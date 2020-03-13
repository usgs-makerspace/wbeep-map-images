create_visualize_date_task_makefile <- function(makefile, task_plan, remake_file) {
  scipiper::create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    include=remake_file,
    packages=c('dplyr', 'sf'),
    sources=c(
      '6_visualize/src/make_map.R'),
    file_extensions=c('feather','ind')
  )
}

create_visualize_date_tasks <- function(date_range, log_folder){
  
  # prepare a data.frame with one row per task
  timesteps <- seq(as.Date(date_range$start), as.Date(date_range$end), by = 1)
  tasks <- data_frame(timestep=timesteps) %>%
    mutate(task_name = strftime(timestep, format = '%Y%m%d', tz = 'UTC')) %>% 
    # Skip ones that are failing for now
    filter(!task_name %in% c('20200101', '20200102', '20200206'))
  
  make_map <- scipiper::create_task_step(
    step_name = 'make_map',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("6_visualize/out/model_map_%s.png", task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf("make_map(", 
               "target_name = target_name,", 
               "data_sf_fn = '2_process/out/wbeep_storage_data_sf_%s.rds'," = task_name, 
               "map_categories = wbeep_storage_categories,",
               "map_colors = wbeep_storage_colors)"
      )
    } 
  )
  
  # ---- combine into a task plan ---- #
  
  gif_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=list(
      make_map),
    add_complete=FALSE,
    final_steps='make_map',
    ind_dir='6_visualize/log')
}
