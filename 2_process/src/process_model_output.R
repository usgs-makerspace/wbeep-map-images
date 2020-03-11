
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
    file_date <- as.Date(gsub(".*([0-9]{8}).csv", '\\1', file), format="%Y%m%d")
    readr::read_csv(file) %>% 
      select(hru_id_nat, value) %>%
      mutate(date = file_date) %>% 
      filter(value != "Undefined")
  }) %>% 
    purrr::reduce(dplyr::bind_rows)
  
  saveRDS(combined_df, target_name)
}
