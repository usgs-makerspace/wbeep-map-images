calculate_season_quantiles <- function(outfile, daily_segment_aves_file) {
  ave_temps <- readRDS(daily_segment_aves_file) 
  ave_temp_matrix <- as.matrix(ave_temps[,-1])
  rownames(ave_temp_matrix) <- ave_temps$Date
  
  date_df <- tibble(date = ave_temps$Date) %>% 
    mutate(season = get_season(Date))
  rm(ave_temps)
  gc()
  season_quantiles_all <- tibble()
  assertthat::assert_that(nrow(date_df) == nrow(ave_temp_matrix))
  for(season in c("winter", "spring", "summer", "fall")) {
    cat(message("starting ", season))
    season_indices <- which(date_df$season == season)
    season_matrix <- ave_temp_matrix[season_indices,]
    season_matrix[season_matrix < -90] <- NA
    season_quantiles <- apply(season_matrix, MARGIN = 2, 
                              quantile, 
                              probs = c(0.10, 0.25, 0.75, 0.90), 
                              na.rm = TRUE, type = 6) %>% 
      t(.) %>% 
      as_tibble(rownames = "seg_id") %>% 
      mutate(season = season)
    season_quantiles_all <- bind_rows(season_quantiles_all, season_quantiles)
    message(season, " done")
    gc()
  }
  saveRDS(season_quantiles_all, outfile)
}

get_days_data <- function(daily_segment_aves_file, dates) {
  readRDS(daily_segment_aves_file) %>% 
    filter(Date %in% as.Date(dates))
}
  

quantile_mat_to_df <- function(x) {
  as_tibble(x)
}

#https://stackoverflow.com/questions/36502140/determine-season-from-date-using-lubridate-in-r
get_season <- function(input_date){
  numeric.date <- 100*month(input_date)+day(input_date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}

classify_segments <- function(segment_quantiles_file, days_to_map) {
  percentile_categories <- c("very low", "low", "average", "high", "very high")
  segment_quantiles <- readRDS(segment_quantiles_file)
  data_classified <- days_to_map %>%
    pivot_longer(cols = !Date, names_to = "seg_id", values_to = "tave") %>% 
    mutate(season = get_season(Date)) %>% 
    left_join(segment_quantiles, by = c("seg_id", "season")) %>% 
    mutate(`0%` = -Inf,
           `100%` = Inf) %>% 
    filter(tave > -90) %>% #some lines like this, quantiles are NAs
    rowwise() %>% 
    mutate(value = find_value_category(value = tave, 
                                       labels = percentile_categories,
                                       `0%`, `10%`, `25%`, `75%`, `90%`, `100%`))
    return(data_classified)
}