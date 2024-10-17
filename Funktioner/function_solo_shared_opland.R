#########################################################################
'Create shared and solo polygons'
#########################################################################

library(tidyverse)
library(sf)
library(tmap)

############################################################################
'Function creates areas that are shared with other stations and areas that '
'are solo for the chosen (new) station'


'list of shapefiles with existing stations are input'
'together with google maps style coordinates for a new station'

'shared and solo areas are calculated for a chosen radius'
'where default is 600 meters'

'Result is a gpkg file with info about solo and shared areas for the station'

'Result can be used for calculations or visualisations'

'Input variables in function:'
'latlon = coordinates from google maps, ex. c("55.65544512828569, 12.59620660312122")'
'existing_stations_list_links = links to shp or gpkg files with existing stations, ex. c("Input_GIS/Stations/DSB st.shp",
                                                                                "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                                                                "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                                                                "Input_GIS/Stations/M4_nord_blue_st.shp")'
'buffer_input = radius in meters of buffer, default is 600m'
'solo_or_shared = is it the solo area or the shared area of a new stations, input must be either "solo" og "shared"'
############################################################################

Create_solo_shared_polygons_opland <- function(latlon = c("55.65544512828569, 12.59620660312122"), 
                                               existing_stations_list_links = c("Input_GIS/Stations/DSB st.shp",
                                                                                "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                                                                "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                                                                "Input_GIS/Stations/M4_nord_blue_st.shp"),
                                               buffer_input = 600,
                                               solo_or_shared = c("solo", "shared")){
  
 #latlon = c("55.694747307718195, 12.565489045458035") 
 'existing_stations_list_links = c("Input_GIS/Stations/DSB st.shp",
  "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
  "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
  "Input_GIS/Stations/M4_nord_blue_st.shp") ' 
  
  
  # Split the latitude and longitude values
  lat_lon_split <- strsplit(latlon, ",")
  lat <- as.numeric(lat_lon_split[[1]][1])
  lon <- as.numeric(lat_lon_split[[1]][2])
  
  crs_selected <- st_crs(4326)
  
  # Create an 'sf' data frame with the POINT object
  data <- data.frame(lat = lat, lon = lon)
  sf_data <- st_as_sf(data, coords = c("lon", "lat"), crs = crs_selected) %>% st_transform(25832)
  
  
  
  read_existing_stations <- function(file_number){
    #file_number <- 4
    stations <- st_read(existing_stations_list_links[file_number]) %>% 
      select(geometry) %>% 
      st_transform(25832)
    
    return(stations)
  }
  
  all_stations <- map_dfr(1:length(existing_stations_list_links), read_existing_stations)
  
  ###############################################################################
  'make buffers for all stations and selected station'
  ###############################################################################
  
  buffer_all_stations <- st_buffer(all_stations, buffer_input)
  
  buffer_selected_station <- st_buffer(sf_data, buffer_input)
  
  ###############################################################################
  'One multipolygon with all'
  ###############################################################################
  
  all_stations_multipoly <- st_union(buffer_all_stations)
  
  ###############################################################################
  'shared polygon/area'
  ###############################################################################
  
  shared_area <- st_intersection(buffer_selected_station, all_stations_multipoly)
  
  ###############################################################################
  'solo polygon/area'
  ###############################################################################
  
  solo_area <- st_difference(buffer_selected_station, all_stations_multipoly)
  
  
  if(solo_or_shared == "solo"){
    return(solo_area)
  } 
  
  if(solo_or_shared == "shared"){
    return(shared_area)
  } 
  
}





#testing
'
test_11 <- Create_solo_shared_polygons_opland(
  latlon = c("55.722, 12.5522"),
  existing_stations_list_links = c("Input_GIS/Stations/DSB st.shp",
                                   "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                   "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                   "Input_GIS/Stations/M4_nord_blue_st.shp"),
  buffer_input = 600,
  solo_or_shared = "shared"
)





tmap_mode("view")


  
  #tm_shape(shared_area) +
  #tm_polygons("green") +
  tm_shape(all_stations_multipoly) +
  tm_polygons(alpha = 0.4) +
    tm_shape(test_11) +
    tm_polygons("red", alpha = 0.2) 
    
    '
