library(osrm)
library(sf)
library(tidyverse)
library(webshot)
library(osmdata)
library(tmap)
library(tictoc)

make_isochrone_data_new_and_nearby_stations <- function(latlon,
                                                        isochron_time_input = 10,
                                                        radius_input = 1.5,
                                                        existing_station_list_links = c("Input_GIS/Stations/DSB st.shp",
                                                                                        "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                                                                        "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                                                                        "Input_GIS/Stations/M4_nord_blue_st.shp")) {
  
  
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
  
  
  sf_data <- convert_google_coords(latlon)
  
  # Function to calculate isochrones
  calculate_isochrones <- function(one_station_input, breaks, profile = "foot") {
    loc <- st_coordinates(one_station_input)
    isochrone <- tryCatch({
      osrmIsochrone(loc = loc, breaks = breaks, osrm.profile = profile)
    }, error = function(e) {
      warning(paste("Error calculating isochrone for station:", e$message))
      return(NULL)
    })
    #isochrone$station_name <- one_station_input$station_name
    return(isochrone)
  }
  
  # Calculate isochrone for the new station
  ny_station_isochrone <- calculate_isochrones(sf_data, isochron_time_input)
  if (is.null(ny_station_isochrone)) stop("Isochrone calculation for new station failed.")
  
  ny_station_isochrone <- ny_station_isochrone %>%
    st_make_valid() %>%
    st_zm() %>%
    mutate(station_type = "new")
  
  # Fetch existing stations
  read_existing_stations <- function(file_number) {
    stations <- tryCatch({
      st_read(existing_station_list_links[file_number]) %>%
        select(geometry) %>%
        st_transform(4326)
    }, error = function(e) {
      warning(paste("Error reading station file:", existing_station_list_links[file_number], " - ", e$message))
      return(NULL)
    })
    return(stations)
  }
  
  all_stations <- map_dfr(1:length(existing_station_list_links), read_existing_stations)
  all_stations <- all_stations %>% filter(!is.na(geometry))
  
  # Filter stations within the radius of the new station
  buffer_new_station <- st_buffer(sf_data %>% st_transform(25832), radius_input * 1000)
  all_stations <- all_stations %>% st_transform(25832)
  
  stations_within_radius <- st_filter(all_stations, buffer_new_station)
  stations_within_radius <- st_transform(stations_within_radius, 4326)
  
  # Calculate isochrones for all existing stations within radius
  if (nrow(stations_within_radius) == 0) {
    warning("No existing stations found within the specified radius.")
    return(ny_station_isochrone)
  }
  
  
  combined_isochrones <- stations_within_radius %>%
    st_cast("POINT") %>% 
    split(1:nrow(.)) %>%
    map_dfr(~ calculate_isochrones(.x, isochron_time_input)) %>%
    filter(!is.null(geometry))
  
  combined_isochrones <- combined_isochrones %>%
    st_make_valid() %>%
    st_zm() %>%
    mutate(station_type = "existing")
 
  
  all_isochrones <- rbind(ny_station_isochrone, combined_isochrones)
  
  return(all_isochrones)
}


'
#tjek map

tmap_mode("view")

tm_shape(all_isochrones) +
tm_polygons(col = "station_type") +
tm_shape(sf_data) +
tm_dots() +
tm_shape(stations_within_radius) +
tm_dots()

'
