###################################################################################
'Count number of people, jobs, studyplaces and cars around a potential'
'metrostation'
###################################################################################

library(dots)
library(tidyverse)
library(sf)
library(tmap)

###################################################################################
'new station lat lon - use google maps'
###################################################################################

new_station_latlon <- c("55.69584415383402, 12.561386229435307")


###################################################################################
'Three functions are used'
'1. Create dot map (people, jobs, studyplaces and cars)'
'2. create solo and shared area of a new station'
'3. count number of people, jobs, studyplaces and cars in the solo and shared area'

'source functions'
###################################################################################

source("Funktioner/function_dotmap_creator_opland.R")
source("Funktioner/function_solo_shared_opland.R")
source("Funktioner/function_count_dots_in_polygon_opland.R")
source("Funktioner/function_closer_station.R")

###################################################################################
'Dotmap takes some time to create (20/30 min in present setup)'
'choose if link to already created dot map is used or new created'
###################################################################################

create_dot_map <- "yes" #yes/no
link_to_dot_map <- "prikkort/dots_equals_100_2035+_5073E_00.gpkg"

if(create_dot_map == "no"){
  dotmap <- st_read(link_to_dot_map)
}

if(create_dot_map == "yes"){
  dotmap <- dot_map_creator_opland(filename_polygons_link = "Input_GIS/OTM_7/OTM7Zoner-behandlet.shp",
                                   filename_zone_data_link = "//nas2/OTMDATA/OTM73/Soceco/5073E_00/Zone.csv",
                                   connector_polygons = "OTM70_ID",
                                   connector_zone_data = "ZONE",
                                   dot_type = c("POP", "JOBS", "STUO16", "CARS"),
                                   year = "2035+",
                                   divisor = 100)
}

###################################################################################
'solo and shared part of new station'

###################################################################################



new_station_solo_polygon <- Create_solo_shared_polygons_opland(latlon = new_station_latlon, #new station geo
                                                               existing_stations_list_links = c("Input_GIS/Stations/DSB st.shp",
                                                                                                "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                                                                                "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                                                                                "Input_GIS/Stations/M4_nord_blue_st.shp"),
                                                               buffer_input = 600, #meters radius
                                                               solo_or_shared = "solo" #solo/shared
)

new_station_shared_polygon <- Create_solo_shared_polygons_opland(latlon = new_station_latlon, #new station geo
                                                               existing_stations_list_links = c("Input_GIS/Stations/DSB st.shp",
                                                                                                "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                                                                                "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                                                                                "Input_GIS/Stations/M4_nord_blue_st.shp"),
                                                               buffer_input = 600, #meters radius
                                                               solo_or_shared = "shared" #solo/shared
)  


###################################################################################
'count dots in solo and shared part of new stations area'
###################################################################################

dots_solo <- data.frame(dots_in_polygon_count_opland(new_station_solo_polygon, link_to_dot_map)) %>% mutate(solo_shared = "solo")
dots_shared <- data.frame(dots_in_polygon_count_opland(new_station_shared_polygon, link_to_dot_map)) %>% mutate(solo_shared = "shared")

dot_count <- rbind(dots_solo, dots_shared)

###################################################################################
'count dots where new station is closer'
###################################################################################


closer_to_new_station <- New_station_closest_station(latlon = new_station_latlon,
                            existing_stations_list_links = c("Input_GIS/Stations/DSB st.shp",
                                                             "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                                             "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                                             "Input_GIS/Stations/M4_nord_blue_st.shp"),
                            dotmap_location = link_to_dot_map)

###################################################################################
'create map visualising dots closer to new station'
###################################################################################

closer_dots_gpkg <- st_read("Closer_dots_data_and_maps/latest_data_for_visual.gpkg") %>% 
  mutate(ny_station_closer = ifelse(nearest == 1, "yes", "no"))


tm_shape(closer_dots_gpkg) +
  tm_dots("ny_station_closer") 
