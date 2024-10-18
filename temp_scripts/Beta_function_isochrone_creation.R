#########################################################################
'Isochron for ny station'
#########################################################################

library(osrm)
library(sf)
library(tidyverse)
library(webshot)
library(osmdata)
library(tmap)
library(tictoc)

############################################################################
'Function creates isochrones (5 and 10 minutes) for a new station'

'OSM data is used'


'Result is maps with a visualization of Isochrones'
'Visualisation can also include dots from dotmap'
'Visualisation can also include isochrones from existing stations'

''

'Input variables in function:'
'latlon = coordinates from google maps, ex. c("55.65544512828569, 12.59620660312122")'
'existing_stations_list_links = links to shp or gpkg files with existing stations, ex. c("Input_GIS/Stations/DSB st.shp",
                                                                                "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                                                                "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                                                                "Input_GIS/Stations/M4_nord_blue_st.shp")'
'dotmap_link = link to a dotmap with all dots'
############################################################################

############################################################################
'Input'
############################################################################

latlon = c("55.69594067646945, 12.56140641872916")
existing_stations_list_links = c("Input_GIS/Stations/DSB st.shp",
                                 "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                 "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                 "Input_GIS/Stations/M4_nord_blue_st.shp")

dotmap_location = "prikkort/dots_equals_100_2035+_5073E_00.gpkg"


###############################################################################
'fetch dotmap'
###############################################################################  

dotmap_read <- st_read(dotmap_location) %>% st_transform(st_crs(all_stations))


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
calculate_isochrones <- function(one_station_input, breaks = c(5, 10), profile = "foot") {
  loc <- st_coordinates(one_station_input)
  isochrone <- osrmIsochrone(loc = loc, breaks = breaks, osrm.profile = profile)
  isochrone$station_name <- one_station_input$station_name 
  return(isochrone)
}

############################################################################
'Calculate isochrones and visualize'
############################################################################

ny_station_5_min <- calculate_isochrones(sf_data, 5) %>% st_make_valid() %>% st_zm()
ny_station_10_min <- calculate_isochrones(sf_data, 10) %>% st_make_valid() %>% st_zm()


tmap_mode("view")
tm_shape(ny_station_10_min) +
  tm_polygons(alpha = 0.3, col = "darkgreen") +
  tm_shape(sf_data) +
  tm_dots(size = 0.5) +
  tm_shape(ny_station_5_min) +
  tm_polygons(alpha = 0.1, col = "darkblue")
  

############################################################################
'Fetch existing stations'
############################################################################

read_existing_stations <- function(file_number){
  #file_number <- 4
  stations <- st_read(existing_stations_list_links[file_number]) %>% 
    select(geometry) %>% 
    st_transform(4326)
  
  return(stations)
}

all_stations <- map_dfr(1:length(existing_stations_list_links), read_existing_stations)

############################################################################
'Only existing stations within metroland'
############################################################################

# Define the coordinates of the bounding box
coords <- matrix(c(
  12.536853706176348, 55.66394520261706, # nederst/venstre
  12.59684044790393, 55.66394520261706,  # nederst/højre
  12.59684044790393, 55.71784808416263, # øverst/højre
  12.536853706176348, 55.71784808416263, # øverst/venstre
  12.536853706176348, 55.66394520261706    # Close the polygon (back to start)
), ncol = 2, byrow = TRUE)

# Create a polygon from the coordinates, with CRS set to WGS 84 (EPSG:4326)
polygon <- st_polygon(list(coords)) %>%
  st_sfc(crs = st_crs(4326))

# Ensure both the polygon and all_stations are in the same CRS
polygon <- st_transform(polygon, st_crs(all_stations))


# Filter all_stations that are within the polygon
stations_within_polygon <- all_stations %>%
  filter(st_within(geometry, polygon, sparse = FALSE))

# Cast the geometries explicitly to POINT, in case they were changed
stations_within_polygon <- st_cast(stations_within_polygon, "POINT")

############################################################################
'Calculate isochrones for all existing stations'
############################################################################

# Function to calculate isochrone for each station
calculate_isochrone_for_station_10 <- function(station) {
  # station: a single row from stations_within_polygon
  calculate_isochrones(station, 10)
}

calculate_isochrone_for_station_5 <- function(station) {
  # station: a single row from stations_within_polygon
  calculate_isochrones(station, 5)
}


tic()
# Use map to iterate over each row in stations_within_polygon
combined_isochrones_5 <- stations_within_polygon %>%
  split(1:nrow(.)) %>%
  map_dfr(calculate_isochrone_for_station_5)

combined_isochrones_5 <- combined_isochrones_5 %>% st_make_valid() %>% st_zm()
toc()

st_write(combined_isochrones_5, "isochrones/all_stations_combined_isochrones_5.gpkg")

combined_isochrones_10 <- stations_within_polygon %>%
  split(1:nrow(.)) %>%
  map_dfr(calculate_isochrone_for_station_10)

combined_isochrones_10 <- combined_isochrones_10 %>% st_make_valid() %>% st_zm()

st_write(combined_isochrones_10, "isochrones/all_stations_combined_isochrones_10.gpkg")


############################################################################
'Visualise isochrones for new and existing stations'
############################################################################

tm_shape(ny_station_10_min) +
  tm_polygons(alpha = 0.3, col = "darkgreen") +
  tm_shape(sf_data) +
  tm_dots(size = 0.5) +
  tm_shape(combined_isochrones_10) +
  tm_polygons(alpha = 0.3, col = "darkgrey")


############################################################################
'Visualise isochrones for new and existing stations incl. dots'
############################################################################

custom_colors <- c("POP" = "red", "JOBS" = "blue", "STUO16" = "yellow")

tm_shape(ny_station_10_min) +
  tm_polygons(alpha = 0.3, col = "darkgreen") +
  tm_shape(sf_data) +
  tm_dots(size = 0.5) +
  tm_shape(combined_isochrones_10) +
  tm_polygons(alpha = 0.3, col = "darkgrey") +
  tm_shape(dotmap_read %>% filter(dots_type != "CARS")) +
  tm_dots(col = "dots_type", palette = custom_colors, alpha = 0.3)
