#########################################################################
'Function - Isochron for ny station'
#########################################################################


library(osrm)
library(sf)
library(tidyverse)
library(webshot)
library(osmdata)
library(tmap)
library(tictoc)

############################################################################
'Function creates isochrones (selected minutes) for a new station'

'OSM data is used'


'Result is a polygon with the isochron

'

'Input variables in function:'
'latlon = coordinates from google maps, ex. c("55.65544512828569, 12.59620660312122")'
'isochron_time_input = walking time in minutes as a number, ex. 10'
############################################################################

make_isochrone_data <- function(latlon,
                                isochron_time_input = 10){

  #test
  'latlon = c("55.69594067646945, 12.56140641872916")
   isochron_time_input = 10
  '
  

  ############################################################################
  'fetch OSM data'
  ############################################################################
  
  assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
  
  # Check if the OSM data is cached; if not, download and save it
  if(file.exists("OSM_data/osm_pedestrian_data.rds")){
    osm <- readRDS("OSM_data/osm_pedestrian_data.rds")
  } else {
    osm <- opq(bbox = c(12.4, 55.6, 12.7, 55.75)) %>% 
      add_osm_feature(key = "highway") %>% 
      osmdata_sf()
    saveRDS(osm, "OSM_data/osm_pedestrian_data.rds")
  }
  
  
  
  ############################################################################
  'Split the latitude and longitude values'
  ############################################################################
  
  sf_data <- convert_google_coords(latlon)
  
  
  
  ############################################################################
  'Define the function to calculate isochrones'
  ############################################################################
  calculate_isochrones <- function(one_station_input, breaks, profile = "foot") {
    loc <- st_coordinates(one_station_input)
    isochrone <- osrmIsochrone(loc = loc, breaks = breaks, osrm.profile = profile)
    isochrone$station_name <- one_station_input$station_name 
    return(isochrone)
  }
  
  ############################################################################
  'Calculate isochrones and visualize'
  ############################################################################
  
  
  
  ny_station_isochrone <- calculate_isochrones(sf_data, isochron_time_input) %>% st_make_valid() %>% st_zm()
  #ny_station_10_min <- calculate_isochrones(sf_data, 10) %>% st_make_valid() %>% st_zm()
  
  
  return(ny_station_isochrone)
  
}

#test
#make_isochrone_data(latlon = c("55.69594067646945, 12.56140641872916"), isochron_time_input = 5)

