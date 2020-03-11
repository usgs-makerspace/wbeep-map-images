
extract_filenames_from_ind <- function(ind_file) {
  filename_hash_list <- readLines(ind_file)
  only_names <- unlist(lapply(strsplit(filename_hash_list, ":"), `[`, 1))
  only_names_noNA <- only_names[!is.na(only_names)]
  return(only_names_noNA)
}

process_and_combine_model_output <- function(target_name, task_ind) {
  files <- extract_filenames_from_ind(task_ind)
  
  combined_df <- purrr::map(files, function(file){ 

    # read and manipulate file, return data.frame
    readr::read_csv(file, col_types = cols()) %>% 
      dplyr::select(hru_id_nat, value) %>%
      mutate(date_str = paste0("date_", gsub(".*([0-9]{8}).csv", '\\1', file))) %>% 
      filter(value != "Undefined")
    
  }) %>% 
    purrr::reduce(dplyr::bind_rows) 
  
  saveRDS(combined_df, target_name)
}

join_spatial_and_data <- function(target_name, spatial_sf_fn, model_data_fn) {
  
  spatial_sf <- readRDS(spatial_sf_fn)
  model_data <- readRDS(model_data_fn)
  
  # there are some HRUs in spatial_sf that aren't in the model_data
  joined_data <- left_join(spatial_sf, model_data, by = "hru_id_nat")
  
  data_for_plotting <- joined_data %>% 
    # filter out NAs just in case (first time, there were actually none ...)
    filter(!is.na(value))
    
  saveRDS(data_for_plotting, target_name)
}
