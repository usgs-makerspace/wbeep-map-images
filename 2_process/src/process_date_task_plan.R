create_process_date_task_makefile <- function(makefile, task_plan, remake_file) {
  scipiper::create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    include=remake_file,
    packages=c('sf', 'dplyr', 'readr'),
    sources=c(
      '2_process/src/process_model_output.R'),
    file_extensions=c('feather','ind')
  )
}

create_process_date_tasks <- function(date_range, log_folder){
  
  # prepare a data.frame with one row per task
  timesteps <- seq(as.Date(date_range$start), as.Date(date_range$end), by = 1)
  tasks <- data_frame(timestep=timesteps) %>%
    mutate(task_name = strftime(timestep, format = '%Y%m%d', tz = 'UTC')) %>% 
    # Skip ones that are failing for now
    filter(!task_name %in% c('20200101', '20200102', '20200206'))
  
  joined_to_spatial <- scipiper::create_task_step(
    step_name = 'joined_to_spatial',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("2_process/out/wbeep_storage_data_sf_%s.rds", task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf("join_spatial_with_data(", 
               "target_name = target_name,", 
               "spatial_sf_fn = '2_process/out/hru_conus_sf.rds',", 
               # File format: 1_fetch/out/model_output_categorized_YYYYMMDD.csv
               "model_data_fn = I('1_fetch/out/model_output_categorized_%s.csv'))" = task_name
      )
    } 
  )
  
  # ---- combine into a task plan ---- #
  
  gif_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=list(
      joined_to_spatial),
    add_complete=FALSE,
    final_steps='joined_to_spatial',
    ind_dir='2_process/log')
}


# helper function to sprintf a bunch of key-value (string-variableVector) pairs,
# then paste them together with a good separator for constructing remake recipes
psprintf <- function(..., sep='\n      ') {
  args <- list(...)
  non_null_args <- which(!sapply(args, is.null))
  args <- args[non_null_args]
  argnames <- sapply(seq_along(args), function(i) {
    nm <- names(args[i])
    if(!is.null(nm) && nm!='') return(nm)
    val_nm <- names(args[[i]])
    if(!is.null(val_nm) && val_nm!='') return(val_nm)
    return('')
  })
  names(args) <- argnames
  strs <- mapply(function(template, variables) {
    spargs <- if(template == '') list(variables) else c(list(template), as.list(variables))
    do.call(sprintf, spargs)
  }, template=names(args), variables=args)
  paste(strs, collapse=sep)
}

