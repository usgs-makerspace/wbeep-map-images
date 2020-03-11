topo_to_sf <- function(target_name, topo_fn, proj_start, proj_target) {
  
  hru_topo <- topojson_read(topo_fn)
  hru_sf <- st_as_sf(hru_topo)
  st_crs(hru_sf) <- proj_start
  hru_sf_transf <- st_transform(hru_sf, proj_target)
  
  # Self-intersections were causing downstream problems with cropping and summarizing for a dissolve
  hru_sf_valid <- st_make_valid(hru_sf_transf) # Needs pkg `lwgeom`
  hru_sf_valid_selfinter <- st_buffer(hru_sf_valid, dist = 0)
  
  saveRDS(hru_sf_valid_selfinter, target_name)
}

crop_hru_to_conus <- function(target_name, hru_sf_fn, conus_sf_fn) {
  
  hru_sf <- readRDS(hru_sf_fn)
  conus_sf <- readRDS(conus_sf_fn)
  
  hru_conus_sf <- st_intersection(hru_sf, conus_sf)
  
  saveRDS(hru_conus_sf, target_name)
}
