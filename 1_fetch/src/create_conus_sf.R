create_conus_sf <- function(target_name, proj) {
  
  conus_data <- map("usa", fill = TRUE, plot = FALSE)
  conus_sf <- st_as_sf(conus_data)
  conus_sf_transf <- st_transform(conus_sf, st_crs(proj))
  conus_sf_valid <- lwgeom::st_make_valid(conus_sf_transf)
  
  saveRDS(conus_sf_valid, target_name)
}
