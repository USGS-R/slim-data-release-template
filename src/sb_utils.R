
#' @param use_task_table logical specifying whether to call `do_item_replace_tasks`
#' which will create a task_table of the files to push to ScienceBase. This prevents them
#' all from failing if one fails. Defaults to `TRUE`.
#' @param sources filepath(s) for where all of the functions that are needed for running 
#' `sb_replace_files` exsit. For example, where `sb_replace_files`, `sb_render_post_xml`, 
#' `do_item_replace_tasks`, `upload_and_record`, and `combine_upload_times` are defined. It 
#' might be easier to put them all in the same file.
#' 
sb_replace_files <- function(sb_id, ..., file_hash, use_task_table = TRUE, sources = c()){
  
  files <- c(...)
  
  # Throw error if there are no files given to push
  stopifnot(length(files) > 0 | !missing(file_hash))
  
  # Keep login check here as well as in `upload_and_record` in case the task table
  # method is not used
  if (!sbtools::is_logged_in()){
    sb_secret <- dssecrets::get_dssecret("cidamanager-sb-srvc-acct")
    sbtools::authenticate_sb(username = sb_secret$username, password = sb_secret$password)
  }
  
  hashed_filenames <- c()
  if (!missing(file_hash)){
    hashed_filenames <- yaml.load_file(file_hash) %>% names %>% sort() %>% rev()
    if(use_task_table) {
      do_item_replace_tasks(sb_id, hashed_filenames, sources)
    } else {
      for (file in hashed_filenames){
        item_replace_files(sb_id, files = file)
      }
    }
  }
  
  if (length(files) > 0){
    if(use_task_table) {
      do_item_replace_tasks(sb_id, files, sources)
    } else {
      item_replace_files(sb_id, files = files)
    }
  }
  
}

# Helper function to create a task_table for the files that need to be pushed to SB
do_item_replace_tasks <- function(sb_id, files, sources) {
  
  # Define task table rows
  task_df <- tibble(filepath = files) %>% mutate(task_name = paste0('uploaded_file_', row_number()))
  
  # Define task table columns
  sb_push <- scipiper::create_task_step(
    step_name = 'push_file_to_sb',
    target_name = function(task_name, step_name, ...){
      sprintf("%s_pushed_to_sb", task_name)
    },
    command = function(task_name, ...){
      sprintf("upload_and_record(I('%s'), '%s')", sb_id, 
              filter(task_df, task_name == !!task_name) %>% pull(filepath))
    } 
  )
  
  # Create the task plan
  task_plan <- create_task_plan(
    task_names = task_df$task_name, 
    task_steps = list(sb_push),
    final_steps = c('push_file_to_sb'),
    add_complete = FALSE)
  
  # Create the task remakefile
  task_yml <- "file_upload_tasks.yml"
  final_target <- "upload_timestamps"
  create_task_makefile(
    task_plan = task_plan,
    makefile = task_yml,
    packages = c('sbtools', 'scipiper', 'dplyr'),
    sources = sources,
    final_targets = final_target,
    finalize_funs = "bind_rows",
    as_promises = FALSE)
  
  # Build the tasks
  loop_tasks(task_plan = task_plan, task_makefile = task_yml, num_tries = 3)
  upload_timestamps <- remake::fetch(final_target, remake_file=task_yml)
  
  # Remove the temporary task makefile for uploading the files to ScienceBase
  file.remove(task_yml)
  
  return(upload_timestamps)
}

upload_and_record <- function(sb_id, file) {
  
  # First verify that you are logged into SB. Need to do this for each task that calls 
  # `upload_and_record` in case there are any long-running uploads that timeout the session.
  if (!sbtools::is_logged_in()){
    sb_secret <- dssecrets::get_dssecret("cidamanager-sb-srvc-acct")
    sbtools::authenticate_sb(username = sb_secret$username, password = sb_secret$password)
  }
  
  # First, upload the file
  item_replace_files(sb_id, file)
  
  timestamp <- Sys.time()
  attr(timestamp, "tzone") <- "UTC"
  timestamp_chr <- format(timestamp, "%Y-%m-%d %H:%M %Z")
  
  # Then record when it happened and return that as an obj
  return(tibble(file = file, sb_id = sb_id, time_uploaded_to_sb = timestamp_chr))
}
