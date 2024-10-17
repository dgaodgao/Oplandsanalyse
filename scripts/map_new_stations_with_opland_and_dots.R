#########################################################################
'map one or more new stations and existing stations'
'including dotmap'
#########################################################################

library(tidyverse)
library(sf)
library(tmap)



#########################################################################
'station locations'
#########################################################################


new_station_latlon <- c("55.694625740648604, 12.565656306081491",
                        "55.69581089740258, 12.561455966867971")

# Split the latitude and longitude values
lat_lon_split <- strsplit(new_station_latlon, ",")
lat <- sapply(lat_lon_split, function(x) as.numeric(x[1]))
lon <- sapply(lat_lon_split, function(x) as.numeric(x[2]))

data <- data.frame(lat = lat, lon = lon)
crs_selected <- st_crs(4326)

sf_data <- st_as_sf(data, coords = c("lon", "lat"), crs = crs_selected) %>% st_transform(25832)

nye_stationer_buffer <- st_buffer(sf_data, 600)

#########################################################################
'existing stations'
#########################################################################

existing_stations_list_links = c("Input_GIS/Stations/DSB st.shp",
                                 "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                 "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                 "Input_GIS/Stations/M4_nord_blue_st.shp")


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

all_stations_buffer <- st_buffer(all_stations, 600)
###############################################################################
'fetch dotmap'
###############################################################################  

dotmap_location <- "prikkort/dots_equals_100_2035+_5073E_00.gpkg"

dotmap_read <- st_read(dotmap_location) %>% st_transform(st_crs(all_stations))

###############################################################################
'create map'
###############################################################################  

tmap_mode("view")

custom_colors <- c("POP" = "red", "JOBS" = "blue", "STUO16" = "yellow")

tm_shape(dotmap_read %>% filter(dots_type != "CARS")) +
  tm_dots(col = "dots_type", palette = custom_colors, alpha = 0.3) +
  tm_shape(all_stations) +
  tm_dots(size = 0.2, alpha = 0.2)+
  tm_shape(all_stations_buffer) +
  tm_polygons(col = "darkred", alpha = 0.2) +
  tm_shape(nye_stationer_buffer) +
  tm_polygons(col = "darkgreen", alpha = 0.4) +
  tm_shape(sf_data) +
  tm_dots(size = 0.2, alpha = 0.4, col = "darkgreen")
  

