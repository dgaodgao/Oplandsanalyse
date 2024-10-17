#########################################################################
'Create dot maps for any variable that has a number and geographic info'
#########################################################################
library(dots)
library(tidyverse)
library(sf)
library(tmap)


############################################################################
'Function creates a dotmap based on joining geografic info about OTM zones'
'With data about what is inthe OTM zones'
'This is fx. number of people, jobs, studyplaces'

'Data for each zone is spread randomly within the zone to create dots'
'Result is a gpkg file with geografic data for each otm zone'

'Result can be used for calculations or visualisations'

'Input variables in function:'
'filename_polygons_link = link to geography of OTM zones, ex. "Input_GIS/OTM_7/OTM7Zoner-behandlet.shp"'
'filename_zone_data_link = link to data for OTM zones, ex. "//nas2/OTMDATA/OTM73/Soceco/5073E_00/Zone.csv"'
'connector_polygons = variable name for zone ID in filename_polygons_link, ex. "OTM70_ID"'
'connector_zone_data = variable name for zone ID in filename_zone_data_link, ex. "ZONE"'
'year = prognosis year which is used to name output file, ex. "2035+" '
'dot_type = the variable names from filename_zone_data_link that should be made dots from, ex. ex. c("POP", "JOBS", "STUO16", "CARS")'
'divisor = how many each dot represents - 100 is default'
############################################################################




dot_map_creator_opland <- function(filename_polygons_link, 
                                   filename_zone_data_link, 
                                   connector_polygons, 
                                   connector_zone_data, 
                                   year, 
                                   dot_type, 
                                   divisor = 100,
                                   visual = TRUE){
  #filename_polygons_link <- "Input_GIS/OTM_7/OTM7Zoner-behandlet.shp"
  #filename_zone_data_link <- "//nas2/OTMDATA/OTM73/Soceco/5073E_00/Zone.csv"
  #connector_polygons <- "OTM70_ID"
  #connector_zone_data <- "ZONE"
  #dot_type <- c("POP", "JOBS", "STUO16", "CARS")
  #dot_type <- "POP"
  #year <- "2035+"
  #divisor <- 100
  
  ### loading files ###
  polygon_map <- st_read(filename_polygons_link) %>% 
    st_transform(crs = 25832) %>%
    st_zm() %>% 
    filter(!st_is_empty(geometry))
  
  zonedata <- read.csv(filename_zone_data_link)
  #MISSING - insert checks of shp files and csv files
  ### loading files finished ###
  
  
  ### merge and transform data ###
  #zonedata <- zonedata %>% rename(!!connector_polygons := connector_zone_data) 
 
   polygon_map <- polygon_map %>% 
     select(!!connector_polygons)
  
   zonedata_with_geo <- polygon_map %>%
     left_join(zonedata, by = setNames(connector_zone_data, connector_polygons))
   

  ### merge and transform data  finished###

  
  
  dots_for_map <- dots_points(shp = st_zm(zonedata_with_geo), 
                              cols = dot_type, 
                              divisor = divisor, 
                              min_point = max(divisor / 2, 1),
                              engine = engine_sf_random) #st_zm adjusts any invalid geometries
  
  zonedata_writing <- str_extract(filename_zone_data_link, "(?<=Soceco/).*?(?=/Zone\\.csv)")
  
  st_write(dots_for_map, paste0("prikkort/dots_equals_", divisor, "_", year,"_",  zonedata_writing, ".gpkg"), append = FALSE)
  
  
  ##############################################################################
  'Create visual'
  ##############################################################################
  
  if(visual == TRUE){
  
  tmap_mode("view")
  
  # Set colors for each type
  custom_colors <- c("POP" = "red", "JOBS" = "blue", "STUO16" = "yellow")
  
  quick_map <- tm_shape(dots_for_map %>% filter(dots_type != "CARS")) +
    tm_dots(col = "dots_type", palette = custom_colors)
  
  return(quick_map)
  }
  
  return(paste0("Dotmap saved for soceco id ", zonedata_writing))
  
}


###testing###
'
dot_map_creator_opland(filename_polygons_link = "Input_GIS/OTM_7/OTM7Zoner-behandlet.shp", 
                                   filename_zone_data_link = "//nas2/OTMDATA/OTM73/Soceco/5073E_00/Zone.csv", 
                                   connector_polygons = "OTM70_ID", 
                                   connector_zone_data = "ZONE", 
                                   year = "2035+", 
                                   dot_type = c("POP", "JOBS", "STUO16", "CARS"), 
                                   divisor = 100,
                                   visual = TRUE)
'
