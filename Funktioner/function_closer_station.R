#########################################################################
'Count dots that gets a closer station'
#########################################################################

library(tidyverse)
library(sf)
library(tmap)

############################################################################
'Function counts dots that gets a closer station'
'This can both be dots close to a new station and dots that are at a further away'
'distance from a new station, as long as the new station becomes the closest station'


'list of shapefiles with existing stations are input'
'together with google maps style coordinates for a new station'
'and a dotmap'


'Result is a data.frame used as input for oplandsanalyser'

''

'Input variables in function:'
'latlon = coordinates from google maps, ex. c("55.65544512828569, 12.59620660312122")'
'existing_stations_list_links = links to shp or gpkg files with existing stations, ex. c("Input_GIS/Stations/DSB st.shp",
                                                                                "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                                                                "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                                                                "Input_GIS/Stations/M4_nord_blue_st.shp")'
'dotmap_link = link to a dotmap with all dots'
############################################################################

New_station_closest_station <- function(latlon = c("55.694719466097624, 12.565575839794342"), 
                                        existing_stations_list_links = c("Input_GIS/Stations/DSB st.shp",
                                                                         "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                                                         "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                                                         "Input_GIS/Stations/M4_nord_blue_st.shp"),
                                               
                                        dotmap_location = "prikkort/dots_equals_100_2035+_5073E_00.gpkg"){
  
  # Split the latitude and longitude values
  lat_lon_split <- strsplit(latlon, ",")
  lat <- as.numeric(lat_lon_split[[1]][1])
  lon <- as.numeric(lat_lon_split[[1]][2])
  
  crs_selected <- st_crs(4326)
  
  # Create an 'sf' data frame with the POINT object
  data <- data.frame(lat = lat, lon = lon)
  sf_data <- st_as_sf(data, coords = c("lon", "lat"), crs = crs_selected) %>% st_transform(25832)
  
  ###############################################################################
  'read and combine existing stations'
  ###############################################################################  
  
  read_existing_stations <- function(file_number){
    #file_number <- 4
    stations <- st_read(existing_stations_list_links[file_number]) %>% 
      select(geometry) %>% 
      st_transform(25832)
    
    return(stations)
  }
  
  all_stations <- map_dfr(1:length(existing_stations_list_links), read_existing_stations)
  
  ###############################################################################
  'fetch dotmap'
  ###############################################################################  
  dotmap_read <- st_read(dotmap_location) %>% st_transform(st_crs(all_stations))
  
  
  ###############################################################################
  'combine new station as first sation with existing stations'
  ###############################################################################
  existing_and_new <- rbind(sf_data %>% st_transform(st_crs(all_stations)), all_stations)
 
  ###############################################################################
  'find closest station for each dot'
  ###############################################################################
   nearest <- st_nearest_feature(dotmap_read, existing_and_new)
  
  st_write(cbind(dotmap_read, nearest), "Closer_dots_data_and_maps/latest_data_for_visual.gpkg")
  
  
  #Which one is new?
  closer_dots <- cbind(dotmap_read, nearest) %>% 
    filter(nearest == 1) %>% 
    group_by(dots_type) %>% 
    summarize(antal = n()) %>% 
    st_drop_geometry() %>% 
    pivot_wider(names_from = dots_type, values_from = antal) 
  
  return(closer_dots)
  
}
  