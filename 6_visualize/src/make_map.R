make_map <- function(target_name, map_data_sf_fn, date_str) {
  
  # Filter to just the appropriate date
  map_data_ready <- readRDS(map_data_sf_fn) %>% 
    filter(date_str == date_str)
  
  # Create the map image
  png(target_name, width = 11, height = 8, units="in", res=300)
  plot(st_geometry(map_data_ready), col = map_data_ready$color, border=NA, axes=FALSE)
  dev.off()
  
}
