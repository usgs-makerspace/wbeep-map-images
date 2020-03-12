join_spatial_with_data <- function(target_name, spatial_sf_fn, model_data_fn) {
  
  spatial_sf <- readRDS(spatial_sf_fn)
  model_data <- readr::read_csv(model_data_fn, col_types = cols()) %>% 
    dplyr::select(hru_id_nat, value) %>%
    filter(value != "Undefined")
  
  # there are some HRUs in spatial_sf that aren't in the model_data
  joined_data <- left_join(spatial_sf, model_data, by = "hru_id_nat")
  
  saveRDS(joined_data, target_name)
}
