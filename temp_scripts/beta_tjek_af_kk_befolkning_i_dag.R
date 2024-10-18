
  filename_polygons_link = "Input_GIS/roder_KK/rode.shp"
  filename_zone_data_link <- "Input_data/plandata_KK/plandata_roder_beboere.csv"
  connector_polygons <- "rode_nr"
  connector_zone_data <- "roder"
  dot_type <- c("beboere")
  year <- "2024"
  divisor <- 100
  




#########################################################################
'Create dot maps for any variable that has a number and geographic info'
#########################################################################
library(dots)
library(tidyverse)
library(sf)
library(tmap)





  
  ### loading files ###
  polygon_map <- st_read(filename_polygons_link) %>% 
    st_transform(crs = 25832) %>%
    st_zm() %>% 
    filter(!st_is_empty(geometry))
  
  zonedata <- read.csv2(filename_zone_data_link)
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
  
  zonedata_writing <- "KK"
  
  st_write(dots_for_map, paste0("prikkort/dots_equals_", divisor, "_", year,"_",  zonedata_writing, ".gpkg"), append = FALSE)
  
  
  ##############################################################################
  'Create visual'
  ##############################################################################
  
  if(visual == TRUE){
    
    tmap_mode("view")
    
    # Set colors for each type
    custom_colors <- c("beboere" = "red")
    
    quick_map <- tm_shape(dots_for_map %>% filter(dots_type != "CARS")) +
      tm_dots(col = "dots_type", palette = custom_colors)
    
    return(quick_map)
  }
  
 
  dotmap_location <- "prikkort/dots_equals_100_2024_KK.gpkg"
  polygon <- Create_solo_shared_polygons_opland(
  latlon = c("55.694654220241354, 12.565618051055882"),
  existing_stations_list_links = c("Input_GIS/Stations/DSB st.shp",
                                   "Input_GIS/Stations/M1M2 st (EPSG-3044).shp",
                                   "Input_GIS/Stations/M3M4 st (EPSG-3044).shp",
                                   "Input_GIS/Stations/M4_nord_blue_st.shp"),
  buffer_input = 600,
  solo_or_shared = "solo"
  )

  dots_in_polygon_count_opland(polygon = polygon, dotmap_location = dotmap_location)


