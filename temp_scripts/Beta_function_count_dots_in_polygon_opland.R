#########################################################################
'Crount dots in a polygon'
#########################################################################

library(tidyverse)
library(sf)
library(tmap)


############################################################################
'Function counts number of dots in a polygon'

'prikkort/dotmap with geographically defined dots is input'
'together with a polygon'




'Result is a gpkg number of count of number of dots'

'Result can be used for calculations or visualisations'

'Input variables in function:'
'polygon = area that dots should be counted within, ex. output from function_solo_shared_opland'
'dotmap_location = location of gpkg file with dotmap'
############################################################################

dots_in_polygon_count_opland <- function(polygon,
                                         dotmap_location){
#dotmap_location <- "prikkort/dots_equals_100_2035+_5073E_00.gpkg"
  'polygon <- Create_solo_shared_polygons_opland(
  latlon = c("55.694654220241354, 12.565618051055882"),
  existing_stations_list_links = c("Input_GIS/Stations/DSB st.shp",
                                   "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                   "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                   "Input_GIS/Stations/M4_nord_blue_st.shp"),
  buffer_input = 600,
  solo_or_shared = "solo"
  )'
  
  
  dotmap_read <- st_read(dotmap_location) %>% st_transform(st_crs(polygon))
  
  
  # Function to count the number of points for each dots_type that intersect with the polygon
  count_points_in_polygon <- function(polygon, points, variable) {
    points_filtered <- points %>% filter(dots_type == variable)
    points_in_polygon <- st_intersects(polygon, points_filtered, sparse = FALSE)
    count <- sum(points_in_polygon)
    return(count)
  }
  
  # Get unique values of dots_type to use as variables
  variables <- unique(dotmap_read$dots_type)
  
  # Function to count the number of points for each dots_type that intersect with the polygon
  count_points_in_polygon <- function(polygon, points, variable) {
    points_filtered <- points %>% filter(dots_type == variable)
    points_in_polygon <- st_intersects(polygon, points_filtered, sparse = FALSE)
    count <- sum(points_in_polygon)
    return(count)
  }
  
  # Create a data frame to store the counts for each variable
  counts <- sapply(variables, function(var) count_points_in_polygon(polygon, dotmap_read, var))
  
  # Convert the counts to a data frame
  result_df <- as.data.frame(t(counts))
  colnames(result_df) <- variables
 
  return(result_df)
   
}


