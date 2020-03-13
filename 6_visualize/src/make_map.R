make_map <- function(target_name, data_sf_fn, map_categories, map_colors) {
  
  # Join color data
  data_sf <- readRDS(data_sf_fn)
  map_colors_df <- data.frame(value = map_categories,
                              color = map_colors,
                              stringsAsFactors = FALSE)

  map_data_ready <- left_join(data_sf, map_colors_df, by = "value")
  
  # Create the map image
  png(target_name, width = 11, height = 8, units="in", res=300)
  plot(st_geometry(map_data_ready), col = map_data_ready$color, border=NA, axes=FALSE)
  dev.off()
  
}
