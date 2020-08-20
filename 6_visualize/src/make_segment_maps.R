make_segment_maps <- function(classified_segment_data, out_dir ,
                              categories, colors,
                              geofabric_dir = '1_fetch/in/GFv1.1.gdb') {
  geofabric <- gf <- sf::read_sf(geofabric_dir, 
                                 layer='nsegment_v1_1') %>% 
    mutate(seg_id = as.character(nsegment_v1_1)) %>% 
    sf::st_zm()
  classified_segments_geoms <- classified_segment_data %>% 
    left_join(geofabric, by = "seg_id")
  names(colors) <- categories
  #for each unique date, make a map
  plot_files <- c()
  for(date in unique(classified_segments_geoms$Date)) {
    date_df <- classified_segments_geoms %>% 
      filter(Date == date)
    date <- as.Date(date, origin = "1970-01-01")
    gg <- ggplot(date_df, aes(color = value)) + geom_sf(aes(geometry = Shape), size = 0.1) +
      scale_color_manual(values = colors) + theme_void() +
      labs(color = "", title = date)
    png_path <- file.path(out_dir, paste0("segments_", date, ".png"))
    ggsave(file = png_path, plot = gg)
    plot_files <- c(plot_files, png_path)
  }
  return(combine_to_tibble(plot_files))
}