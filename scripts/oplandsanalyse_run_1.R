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

new_station_latlon <- c("55.6568339086844, 12.50818955944628")




###################################################################################
'functions are used'
'1. create dot map data (people, jobs, studyplaces and cars)'
'2. create solo and shared area of a new station'
'3. count number of people, jobs, studyplaces and cars in the solo and shared area'
'4. identify dots that gets shorter distance to a station'
'5. make isochrone for a new station'
'6. make isochrones for a new and existing stations'
'7. convert google coords'

'source functions'
###################################################################################

source("Funktioner/function_dotmap_creator_opland.R")                         #1
source("Funktioner/function_solo_shared_opland.R")                            #2
source("Funktioner/function_count_dots_in_polygon_opland.R")                  #3
source("Funktioner/function_closer_station.R")                                #4
source("Funktioner/function_make_isochrone_new_station.R")                    #5
source("Funktioner/function_make_isochrone_new_and_existing_stations.R")      #6
source("Funktioner/function_convert_google_coords.R")                         #7

###################################################################################
'Dotmap takes some time to create (20/30 min in present setup)'
'choose if link to already created dot map is used or new created'
###################################################################################

create_dot_map <- "no" #yes/no
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

'data er gemt "Closer_dots_data_and_maps/latest_data_for_visual.gpkg"' #som overskrives hver gang

###################################################################################
'create isochrone for new station'
###################################################################################

isochrone_new_station <- make_isochrone_data(new_station_latlon, 
                                             isochron_time_input = 10)


###################################################################################
'create isochrone for new station and existing stations'
###################################################################################

isochrone_new_station_and_existing_stations <- make_isochrone_data_new_and_nearby_stations(new_station_latlon, 
                                            isochron_time_input = 10,
                                            radius_input = 2,
                                            existing_station_list_links = c("Input_GIS/Stations/DSB st.shp",
                                                                             "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                                                             "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                                                             "Input_GIS/Stations/M4_nord_blue_st.shp"))


###################################################################################
###################################################################################
###################################################################################

'VISUALS'
'tmap is used'

###################################################################################
###################################################################################
###################################################################################

###################################################################################
'dotmap'
###################################################################################

tmap_mode("view")

# Set colors for each type
custom_colors <- c("POP" = "red", "JOBS" = "blue", "STUO16" = "yellow")

quick_map_dots <- tm_shape(dotmap %>% filter(dots_type != "CARS")) +
  tm_dots(col = "dots_type", palette = custom_colors) +
  tm_view(set.view = c(lon = st_coordinates(sf_data)[1], lat = st_coordinates(sf_data)[2], zoom = 14))


###################################################################################
'stationbuffers'
###################################################################################

sf_data <- convert_google_coords(new_station_latlon)


new_station_buffer <- st_buffer(sf_data, 600) %>% mutate(station_type = "new")

existing_stations_list_links = c("Input_GIS/Stations/DSB st.shp",
                                 "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                 "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                 "Input_GIS/Stations/M4_nord_blue_st.shp")

read_existing_stations <- function(file_number){
  #file_number <- 4
  stations <- st_read(existing_stations_list_links[file_number]) %>% 
    select(geometry) %>% 
    st_transform(25832)
  
  return(stations)
}

all_stations <- map_dfr(1:length(existing_stations_list_links), read_existing_stations)

existing_stations_buffers <- st_buffer(all_stations, 600) %>% mutate(station_type = "existing")

combined_stations_buffer <- rbind(new_station_buffer %>% st_transform(st_crs(existing_stations_buffers)), 
                                  existing_stations_buffers)


quick_map_buffers <- tm_shape(combined_stations_buffer) +
  tm_polygons(col = "station_type", alpha = 0.5) +
  tm_shape(sf_data) +
  tm_dots() +
  tm_shape(all_stations) +
  tm_dots() +
  tm_view(set.view = c(lon = st_coordinates(sf_data)[1], lat = st_coordinates(sf_data)[2], zoom = 14))
  



###################################################################################
'create map visualising dots closer to new station'
###################################################################################

closer_dots_gpkg <- st_read("Closer_dots_data_and_maps/latest_data_for_visual.gpkg") %>% 
  mutate(ny_station_closer = ifelse(nearest == 1, "yes", "no"))


quick_map_closer <- tm_shape(closer_dots_gpkg) +
  tm_dots("ny_station_closer") +
  tm_view(set.view = c(lon = st_coordinates(sf_data)[1], lat = st_coordinates(sf_data)[2], zoom = 14))



###################################################################################
'create map visualising isochrons'
###################################################################################

quick_map_isochrones <- tm_shape(isochrone_new_station_and_existing_stations) +
  tm_polygons(col = "station_type", alpha = 0.5) +
  tm_shape(sf_data) +
  tm_dots() +
  tm_shape(all_stations) +
  tm_dots() +
  tm_view(set.view = c(lon = st_coordinates(sf_data)[1], lat = st_coordinates(sf_data)[2], zoom = 14))

 
