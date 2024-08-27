csv2sf <- function(directory){
  
    
    library(tidyverse)
    library(sf)
    
    readr::read_csv(directory,
                    locale = readr::locale(
                        'ko',
                        encoding = 'euc-kr' 
                        ),
                    skip = 4
                    )  -> tmp
    
    
    names(tmp) <- c("st#", "name", "gu", "addr", "y", "x",
                    "startdate", "LCDbike", "QRbike", "runsON")
    
    tmp %>%
    filter_at(vars(x, y), all_vars(!is.na(.))) %>%
    sf::st_as_sf(coords = c("x", "y"), #remove =F,
                         crs = 4326, agr = "constant")
    
}


xlsx2sf <- function(directory){
    
    library(tidyverse)
    library(sf)
    
    readxl::read_xlsx(directory,skip = 4,col_names = F) -> tmp
    
    names(tmp) <- c("st#", "name", "gu", "addr", "y", "x",
                    "startdate", "LCDbike", "QRbike", "runsON")
    
    
    tmp %>%
    filter_at(vars(x, y), all_vars(!is.na(.))) %>%
        sf::st_as_sf(coords = c("x", "y"), #remove =F,
                     crs = 4326, agr = "constant")
    
    
}


library(tidyverse)
library(DescTools)
library(sf)
library(geojsonsf)
library(stars)
library(terra)

library(leafem)
library(tmaptools)
library(ggspatial)
#library(terrainr) caused rendering issue




# import and make Seoul adm boundary data
adm_seoul <-geojson_sf("https://raw.githubusercontent.com/vuski/admdongkor/master/ver20220701/HangJeongDong_ver20220701.geojson") %>%
    filter(sidonm %like% "서울특별시") %>%
    st_make_valid() %>%
    group_by(sidonm) %>%
    summarise()


# import Google map satellite imagery using WMS protocol
# 13: best zoom level for considering trade off between resolution and time
# (22.92 sec elapsed, approx. 20m spatial resolution)

# zoom level(fyi)
#0 - 19

#0 lowest zoom (whole world)

#19 highest zoom (individual buildings, if available)

rs_seoul <- read_osm(adm_seoul,
                     type = "https://mt1.google.com/vt/lyrs=s&hl=en&z={z}&x={x}&y={y}",
                     zoom = 13) %>% terra::rast()

# high performance raster re-projection
#%>%
#terra::project(crs(st_crs(32652)[[2]]),
#               threads = T)


# change projection for plotting(ggplot doesn't support)
adm_seoul <- st_transform(adm_seoul, st_crs(rs_seoul)) 



sfbike <- csv2sf("./data/bikestations/2021_jan_31.csv") %>% st_transform(st_crs(rs_seoul)) %>%
    select(geometry) %>% mutate(`Legend` = "Ddareungi Stations") 





# AQ stations
####################################################################


# encoding: UTF-8

library(readxl)
library(lubridate)
library(tidyverse)
library(doParallel) 


# move all the zipfiles to "./data" directory


# list only zip files
zips <- list.files(path = "./data", pattern = "\\.zip$")
#> zips
#[1] "2018.zip" "2019.zip" "2020.zip" "2021.zip"



# init dataframe
seoulAQ <- tibble()


# importing all observations at once
# might take some times...
{ # start data import
    
    
    no_cores <- 6#detectCores(logical = F) - 1  
    cl <- makeCluster(no_cores, type="PSOCK")  
    registerDoParallel(cl)
    # for loop to import all
    for (i in 1:length(zips)) {
        
        # extract yr from file name by dropping file extention
        yr <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(zips))[i]
        
        # List observation files in the zip
        # Generalized the code for future usability
        # Same as...
        #ls_file <- unzip("./data/2017.zip",list = T)[[1]]
        #ls_file <- c(ls_file[4:12],ls_file[1:3])
        ls_file <- unzip(paste0("./data/",yr,".zip"),list = T)[[1]] %>%
            str_remove(".xlsx") %>%
            parse_date_time(orders = "ym") %>%
            cbind(unzip(paste0("./data/",yr,".zip"),list = T)[[1]]) %>%
            as_tibble(.name_repair = "universal") %>%
            rename(c("V1" = `.`, "V2" = `...2`)) %>%
            arrange(V1) %>% .[[2]]
        
        
        
        # read the data from the compressed file and bind rows into one gargantuan datafrmae
        # used parallel computation due to time consuming uncompression
        library(doParallel)  
        
        
        tmp <- foreach(i=1:length(ls_file),
                       .combine = dplyr::bind_rows,
                       .packages = c("readr","dplyr","tibble","readxl","magrittr")) %dopar% {
                           read_xlsx(unzip(paste0("./data/",yr,".zip"),ls_file[i])) %>%
                               select(-any_of("망"))
                       }
        
        
        
        
        
        file.remove(paste0("./",ls_file))
        
        
        
        seoulAQ <- bind_rows(seoulAQ,tmp)
        
        
    }
    
    stopCluster(cl)
    
} # end data import


# removing korean from dataset
names(seoulAQ) <- c("region", "obs_code","obs_nm","obs_dttm","SO2","CO","O3","NO2","PM10","PM25","addr")
# abbr dict
# M_network: Air Monitoring Network
# obs_code: Observation station code
# obs_nm: observation station name
# obs_dttm: observed date and time
# addr: address of observation station


# data table
library(data.table)

# tibble 2 DT and extract Seoul observations only
dtAQ <-  as.data.table(seoulAQ) %>% .[grepl('^11',obs_code),]





# Date format conversion
# := operator for mutation
dtAQ[,`obs_dttm` := as.character(`obs_dttm`) # not closed brackets "][" act like pipe operator in magrittr
][,date := as.Date(`obs_dttm`,format="%Y%m%d")
][,hrs := data.table::hour(as.ITime(`obs_dttm`,format = "%Y%m%d%H"))]

dtAQ$hrs[dtAQ$hrs==0] <- 24

# index for grouping
dtAQ[,idx := paste0(as.numeric(date),"-",obs_code)]


# group by & summarize using blazing fast data.table
davgAQ <- dtAQ[, lapply(.SD, mean, na.rm = T),
               .SDcols=c("SO2", "CO", "NO2","PM10","PM25"), by=idx]


########################################################################

# Calculating the 8-Hour Ozone Standard
## https://www.asl-associates.com/cal8hr.htm
## calculating rolling averages for all observations
dtAQ[,O3RM := frollmean(dtAQ$O3,
                        n=8,  #integer vector, for adaptive rolling function also list of integer vectors, rolling window size.
                        align="left", # character, define if rolling window covers following rows ("left"), preceding rows ("right") or centered ("center"). Defaults to "right".
                        hasNA = T,
                        na.rm = T,
                        algo = "exact")]

## filter
## 8-hour period from 7:00 a.m. to 3:00 p.m. ~
## 8-hour period from 11:00 p.m. to 7:00 a.m. the following day
dtAQ<- dtAQ[hrs >= 7 & hrs <= 23, ]



## dmax
dmaxO3  <- dtAQ[,by = idx,
                .(O3=max(O3RM,na.rm = T)
                )
]


########################################################################




# split index into date and station codes
davgAQ[, c("date", "station") := tstrsplit(idx, "-", fixed=TRUE)]
dmaxO3[, c("date", "station") := tstrsplit(idx, "-", fixed=TRUE)]

# date format
davgAQ[, date := as.Date(as.numeric(date),origin ="1970-01-01") ]
dmaxO3[, date := as.Date(as.numeric(date),origin ="1970-01-01") ]



# catalog
rawcatalog <- read_xlsx("./data/mStations_2017.xlsx",skip = 3) %>%
    .[-1,]



names(rawcatalog) <- c("region", "city","obs_code","obs_nm","addr","x","y","note")



# removing obsolete stations
for (i in 1:length(rawcatalog$city)) {
    if (!is.na(rawcatalog$note[i])&&is.na(rawcatalog$obs_code[i])) {
        rawcatalog$obs_code[i] <- rawcatalog$obs_code[i-1]
        rawcatalog$obs_code[i-1] <- NA
    }
}



# filtering out obsolete and non-Seoul stations
seoul_obs <- filter(rawcatalog,grepl('^11',obs_code))[c(3,5:7)] %>%
    #https://www.airkorea.or.kr/jfile/readDownloadFile.do?fileId=17d547302cd67&fileSeq=1
    bind_rows(
        tibble(obs_code = "111282",
               addr = "시흥대로 금천구 독산동 996-9 시흥대로 한양수자인아파트 앞",
               x = "126.8987", y = "37.4750")
    )%>%
    distinct(obs_code, .keep_all = T) %>% as.data.table()



# Join
davgAQ_w_coords <- seoul_obs[davgAQ, on = c(obs_code = "station")]
dreduceAQ_w_coords <-  dmaxO3[davgAQ_w_coords, on = "idx"]


# change col names
dreduceAQ_w_coords <- dreduceAQ_w_coords[,c("obs_code","date","idx","x","y", "SO2","CO","O3","NO2","PM10","PM25")]







dreduceAQ_w_coords %>%
    distinct(obs_code, .keep_all = T) %>%
    sf::st_as_sf(coords = c("x", "y"),# remove =F,
                 crs = 4326, agr = "constant")  %>%
    st_transform(st_crs(rs_seoul)) %>% 
    select(geometry) %>%
    mutate(`Legend` = "Air Quality Monitoring Stations") -> final_obs_loc


# making data for point layer
points4plot <- bind_rows(final_obs_loc,sfbike) %>% 
    mutate(`Legend`=as.factor(`Legend`) #%>% fct_rev
           ) %>%
    # for making AQ stations as the top layer(render later)
    arrange(desc(`Legend`)) #%>%
    
    # if using geom_points...
    #mutate(x = st_coordinates(.)[,1],
    #       y = st_coordinates(.)[,2])



# convert to tibble for plotting stacked raster
rs_seoul %>%
    terra::as.data.frame(xy=T) %>%
    as_tibble() %>% rename(Red = red,  
                           Green = green,
                           Blue = blue) -> tib_rseoul

    


# base ggplot frame
ggplot()+ 
    
    # draw satellite raster
    # equivalent to `terrainr::geom_spatial_rgb()` that causes a rendering issue
    geom_raster(data = tib_rseoul,
                aes(x = x, y =y),

                fill = rgb(red = tib_rseoul$Red,
                           green = tib_rseoul$Green,
                           blue = tib_rseoul$Blue,
                           maxColorValue = 255)
                )+

    # draw sf points
    ggplot2::geom_sf(data=points4plot,
                     #size = 1.5,
                     #shape = 21,
                     stroke = .7,
                     aes(col = Legend,
                         fill = Legend,
                         size = Legend,
                         shape = Legend
                         ),
                     na.rm=T
                     )+   
    
    # Change fill & border color
    # Red dots with yellow outline for bike stations,
    # Blue with light cyan outline for air quality monitoring stations
    
    scale_color_manual(values = c("Air Quality Monitoring Stations" = "navy",
                                  "Ddareungi Stations" = "yellow")
                       )+
    scale_fill_manual(values = c("Air Quality Monitoring Stations" = "cyan",
                                "Ddareungi Stations" = "red")
                      ) +
    scale_size_manual(values = c("Air Quality Monitoring Stations" = 4,
                                 "Ddareungi Stations" = 2.5)
                      ) +
    scale_shape_manual(values = c("Air Quality Monitoring Stations" = 24,
                                  "Ddareungi Stations" = 21)
                       )+
    
    # Fix aspect ratio
    coord_fixed()+
    
    # aes for scalebar
    annotation_scale(location = "br", width_hint = 0.3,
                     line_col ="white", text_col = "white", text_cex = 1.3,
                     pad_x = unit(.07, "npc"), pad_y = unit(.07, "npc")) +
    
    # aes for north arrow
    annotation_north_arrow(location = "bl", which_north = "true",
                           pad_x = unit(.87, "npc"), pad_y = unit(.83, "npc"),
                           style = north_arrow_minimal(
                               line_width = 4, text_size = 26,
                               line_col = "white", text_col = "white"
                               
                           )) +
    
    # display tic labels as lon,lat
    ggplot2::coord_sf(datum=st_crs(4326))+
    xlab("Longitude") + ylab("Latitude") +
    
    # you can add a title as well as a subtitle
    #ggtitle("Title", subtitle = "Subtitle") +
    
    # theme setttings
    theme_minimal()+ 
    # fine tune for a legned
    theme(
        
        # I also want to put the legend inside the map
        legend.position = c(0.05, 0.05),
        legend.justification = c("left", "bottom"),
        legend.background = element_rect(fill = 'white'),
        
        # Please remove the axis and labels (i.e., Latitude and Longitude
        # both the number
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        # and labels
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        
        # and background grids
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
        
    ) -> themap #save as a variable



themap # a quick look




# save as a file
ggsave(themap, filename = "map.svg", scale = 2.5, dpi = 512)


    
    
    
    


