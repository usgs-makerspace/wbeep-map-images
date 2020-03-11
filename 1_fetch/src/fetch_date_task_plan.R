create_fetch_date_task_makefile <- function(makefile, task_plan, remake_file) {
  scipiper::create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    include=remake_file,
    packages=c('aws.signature', 'aws.s3'),
    sources=c(
      '1_fetch/src/download_from_s3.R'),
    file_extensions=c('feather','ind')
  )
}

create_fetch_date_tasks <- function(date_range, log_folder){
  
  # prepare a data.frame with one row per task
  timesteps <- seq(as.Date(date_range$start), as.Date(date_range$end), by = 1)
  tasks <- data_frame(timestep=timesteps) %>%
    mutate(task_name = strftime(timestep, format = '%Y%m%d', tz = 'UTC'))
  
  data_downloaded <- scipiper::create_task_step(
    step_name = 'data_downloaded',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("1_fetch/out/model_output_categorized_%s.csv", task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf("download_s3_file(", 
               "target_name = target_name,", 
               "s3_bucket = s3_bucket,", 
               "s3_file_loc = s3_wbeep_storage_model_loc,", 
               # File format: model_output_categorized_YYYY-MM-DD.csv
               "s3_filename = I('model_output_categorized_%s.csv'))" = format(cur_task$timestep, "%Y-%m-%d")
      )
    } 
  )
  
  # ---- combine into a task plan ---- #
  
  gif_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=list(
      data_downloaded),
    add_complete=FALSE,
    final_steps='data_downloaded',
    ind_dir='1_fetch/log')
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

