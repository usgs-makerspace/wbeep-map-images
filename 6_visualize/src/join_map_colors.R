join_map_colors <- function(target_name, data_sf_fn, map_categories, map_colors) {
  
  # Filter to just the appropriate date and join color data
  data_sf <- readRDS(data_sf_fn)
  map_colors_df <- data.frame(value = map_categories,
                              color = map_colors,
                              stringsAsFactors = FALSE)
  
  # Select data for the current date (single column) and then merge the colors
  data_with_colors <- left_join(data_sf, map_colors_df, by = "value")
  
  saveRDS(data_with_colors, target_name)
}
