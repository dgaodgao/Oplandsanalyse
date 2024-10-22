
#########################################################################
'Convert google coordinates to sf_data'
#########################################################################

convert_google_coords <- function(new_station_latlon){

  # Error Check: Ensure latlon has the correct format
  if (!is.character(new_station_latlon) || length(new_station_latlon) != 1) {
    stop("latlon must be a character string in the format 'latitude, longitude'.")
  }
  

  # Split the latitude and longitude values
  lat_lon_split <- strsplit(new_station_latlon, ",")
  lat <- as.numeric(trimws(lat_lon_split[[1]][1]))
  lon <- as.numeric(trimws(lat_lon_split[[1]][2]))
  
  if (is.na(lat) || is.na(lon)) {
    stop("latlon must contain valid numeric latitude and longitude values.")
  }
  
  crs_selected <- st_crs(4326)
  
  # Create an 'sf' data frame with the POINT object for the new station
  data <- data.frame(lat = lat, lon = lon)
  sf_data <- st_as_sf(data, coords = c("lon", "lat"), crs = crs_selected)

return(sf_data)

}



