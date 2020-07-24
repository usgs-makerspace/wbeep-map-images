join_spatial_with_data <- function(target_name, spatial_sf_fn, model_data_fn) {
  
  spatial_sf <- readRDS(spatial_sf_fn)
  model_data <- readr::read_csv(model_data_fn, col_types = cols()) %>% 
    dplyr::select(hru_id_nat, value) %>%
    filter(value != "Undefined")
  
  # there are some HRUs in spatial_sf that aren't in the model_data
  joined_data <- left_join(spatial_sf, model_data, by = "hru_id_nat")
  
  saveRDS(joined_data, target_name)
}

get_nonzero_duplicate_indices <- function(x) {
  zeros <- x == 0
  dups <- duplicated(x, fromLast = TRUE)
  !zeros & dups 
}

find_value_category <- function(value, labels, ...) {
  breaks <- as.numeric(list(...))
  
  # First, make sure actual breaks exist
  if(all(is.na(breaks))) {
    final_label <- "Undefined"
  } else {
    
    # then, check if there are non-zero duplicate quantiles
    dup_indices <- get_nonzero_duplicate_indices(breaks)
    if(any(dup_indices)) {
      breaks <- breaks[!dup_indices]
      labels <- labels[-which(dup_indices)]
    } 
    #if all zeros, mark as undefined, need check for NA in case >2 quantiles are the same
    if(value == 0 && sum(breaks[2:5], na.rm = TRUE) == 0) {
      final_label <- "Undefined"
    } else if(value == 0 && sum(breaks == 0) > 0){ 
      #if only some are zeros and value is zero, use highest zero tier
      high_zero_index <- max(which(breaks == 0))
      if(high_zero_index >= 3) {
        final_label <- labels[3]
      } else {
        final_label <- labels[high_zero_index]
      } 
    } else if(value > 0 && sum(breaks == 0) > 0) {
      high_zero_index <- max(which(breaks == 0))
      breaks <- breaks[high_zero_index:length(breaks)]
      labels <- labels[high_zero_index:length(labels)]
      final_label <- cut(value, breaks, labels, include.lowest = TRUE)
    } else {
      final_label <- cut(value, breaks, labels, include.lowest = TRUE)
    }
  }
  
  final_label <- as.character(final_label)
  return(final_label)
}
