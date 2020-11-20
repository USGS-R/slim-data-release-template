
#' @param use_task_table logical specifying whether to call `do_item_replace_tasks`
#' which will create a task_table of the files to push to ScienceBase. This prevents them
#' all from failing if one fails.
#' @param task_yml only needed if `use_task_table = TRUE`; task makefile name
#' 
sb_replace_files <- function(sb_id, ..., file_hash, use_task_table = FALSE, task_yml){
  
  files <- c(...)
  
  # Throw error if there are no files given to push
  stopifnot(length(files) > 0 | !missing(file_hash))
  
  if (!sbtools::is_logged_in()){
    sb_secret <- dssecrets::get_dssecret("cidamanager-sb-srvc-acct")
    sbtools::authenticate_sb(username = sb_secret$username, password = sb_secret$password)
  }
  
  hashed_filenames <- c()
  if (!missing(file_hash)){
    hashed_filenames <- yaml.load_file(file_hash) %>% names %>% sort() %>% rev()
    if(use_task_table) {
      do_item_replace_tasks(hashed_filenames, sb_id, task_yml)
    } else {
      for (file in hashed_filenames){
        item_replace_files(sb_id, files = file)
      }
    }
  }
  
  if (length(files) > 0){
    if(use_task_table) {
      do_item_replace_tasks(files, sb_id, task_yml)
    } else {
      item_replace_files(sb_id, files = files)
    }
  }
  
}

sb_render_post_xml <- function(sb_id, ..., xml_file = NULL){
  
  if (is.null(xml_file)){
    xml_file <- file.path(tempdir(), paste0(sb_id,'.xml'))
  }
  
  render(filename = xml_file, ...)
  
  sb_replace_files(sb_id = sb_id, xml_file)
  
}

# Helper function to create a task_table for the files that need to be pushed to SB
do_item_replace_tasks <- function(files, sb_id, task_yml) {
  
  # Define task table columns
  sb_push <- scipiper::create_task_step(
    step_name = 'push_file_to_sb',
    target_name = function(task_name, step_name, ...){
      sprintf("%s_pushed_to_sb", task_name)
    },
    command = function(task_name, ...){
      sprintf("item_replace_files(I('%s'), '%s')", sb_id, task_name)
    } 
  )
  
  # Create the task plan
  task_plan <- create_task_plan(
    task_names = files, # Define task table rows
    task_steps = list(sb_push),
    final_steps = c('push_file_to_sb'),
    add_complete = FALSE)
  
  # Create the task remakefile
  create_task_makefile(
    task_plan = task_plan,
    makefile = task_yml,
    packages = c('sbtools', 'scipiper'),
    as_promises = FALSE)
  
  # Build the tasks
  loop_tasks(task_plan = task_plan, task_makefile = task_yml, num_tries = 3)
  
}
