#########################################################################
'Function - Isochron for ny station og omkringliggende stationer'
#########################################################################


library(osrm)
library(sf)
library(tidyverse)
library(webshot)
library(osmdata)
library(tmap)
library(tictoc)

############################################################################
'Function creates isochrones (selected minutes) for a new station and stations within a selected radius the new station'

'default of radius is 1.5 km'

'OSM data is used'


'Result is polygons with the isochrones for new station and all stations within radius'

'Input variables in function:'
'latlon = coordinates from google maps, ex. c("55.65544512828569, 12.59620660312122")'
'isochron_time_input = walking time in minutes as a number, ex. 10'
'radius_input = km. from new station, ex. 1.5 km. (which is deafult)'
'existing_stations_list_links = links to shp or gpkg files with existing stations, ex. c("Input_GIS/Stations/DSB st.shp",
                                                                                "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                                                                "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                                                                "Input_GIS/Stations/M4_nord_blue_st.shp") (which is default input)'
############################################################################

make_isochrone_data_new_and_nearby_stations <- function(latlon,
                                isochron_time_input = 10,
                                radius_input = 1.5,
                                existing_station_list_links = c("Input_GIS/Stations/DSB st.shp",
                                                                "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                                                "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                                                "Input_GIS/Stations/M4_nord_blue_st.shp")){
  
  #test
  '
  latlon = c("55.69594067646945, 12.56140641872916")
   isochron_time_input = 10
   radius_input = 1.5
   existing_station_list_links = c("Input_GIS/Stations/DSB st.shp",
                                                                                        "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                                                                        "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                                                                        "Input_GIS/Stations/M4_nord_blue_st.shp")
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
    dir.create("OSM_data")
    saveRDS(osm, "OSM_data/osm_pedestrian_data.rds")
  }
  
  
  ############################################################################
  'Split the latitude and longitude values'
  ############################################################################
  
  lat_lon_split <- strsplit(latlon, ",")
  lat <- as.numeric(lat_lon_split[[1]][1])
  lon <- as.numeric(lat_lon_split[[1]][2])
  
  crs_selected <- st_crs(4326)
  
  # Create an 'sf' data frame with the POINT object
  data <- data.frame(lat = lat, lon = lon)
  sf_data <- st_as_sf(data, coords = c("lon", "lat"), crs = crs_selected) #%>% st_transform(25832)
  
  
  ############################################################################
  'Define the function to calculate isochrones'
  ############################################################################
  calculate_isochrones <- function(one_station_input, breaks, profile = "foot") {
    loc <- st_coordinates(one_station_input)
    isochrone <- osrmIsochrone(loc = loc, breaks = breaks, osrm.profile = profile)
    #isochrone$station_name <- one_station_input$station_name 
    return(isochrone)
  }
  
  
  ############################################################################
  'Calculate isochrone for new station'
  ############################################################################
  
  
  
  ny_station_isochrone <- calculate_isochrones(sf_data, breaks = isochron_time_input) %>% st_make_valid() %>% st_zm() %>% 
    mutate(station_type = "new")
  
  ############################################################################
  'Fetch existing stations'
  ############################################################################
  
  read_existing_stations <- function(file_number){
    #file_number <- 4
    stations <- st_read(existing_station_list_links[file_number]) %>% 
      select(geometry) %>% 
      st_transform(4326)
    
    return(stations)
  }
  
  all_stations <- map_dfr(1:length(existing_station_list_links), read_existing_stations)
  
  ############################################################################
  'Only existing stations within radius defined by input'
  ############################################################################
  
  
  new_station_buffer <- st_buffer(sf_data, radius_input * 1000) %>% 
    st_transform(st_crs(all_stations))
  

  # Filter all_stations that are within the polygon
  stations_within_polygon <- all_stations %>%
    filter(st_within(geometry, new_station_buffer, sparse = FALSE))
  
  # Cast the geometries explicitly to POINT, in case they were changed
  stations_within_polygon <- st_cast(stations_within_polygon, "POINT")
  
  
  ############################################################################
  'Calculate isochrones for all existing stations'
  ############################################################################
  

  
  
  tic()
  # Use map to iterate over each row in stations_within_polygon
  combined_isochrones <- stations_within_polygon %>%
    split(1:nrow(.)) %>%
    map_dfr(calculate_isochrones)
  
  combined_isochrones <- combined_isochrones %>% st_make_valid() %>% st_zm() %>% 
    mutate(station_type = "existing")
  toc()
  
  
  all_isochrones <- rbind(ny_station_isochrone, combined_isochrones)
  
  
  return(all_isochrones)
  
}
  