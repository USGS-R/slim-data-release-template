
#' @param filename the csv file to write the upload timestamp file log to
#' @param sb_id the sciencebase identifier which is a string, such as
#' "5faaac68d34eb413d5df1f22"
#' @param ... unnamed arguments are assumed to be files that will be uploaded to sciencebase
#' @param file_hash a yaml file with filepath and hash value pairs. All files named in the 
#' `file_hash` will be uploaded to sciencebase
#' @param sources filepath(s) for where all of the functions that are needed for running 
#' `sb_replace_files` exsit. For example, where `sb_replace_files`, `sb_render_post_xml`, 
#' `do_item_replace_tasks`, `upload_and_record`, and `combine_upload_times` are defined. It 
#' is recommended to put them all in the same file.
#' 
#' @details a `force = TRUE` or `scdel` on the target coming back from this will automatically ping 
#' sciencebase again to check that all of the files exist. This might be helpful behavior if you've done a 
#' manual delete of one or more files on sciencebase, or want to (again) verify all files exist up there.
#' 
#' Also, `sources` does best with a single character filepath, which allows the contents of source file to be 
#' treated as a dependency. If you feel the need to put `sources = I(c('file1.R','file2.R'))` note that 
#' remake won't track the contents of the files to trigger re-builds, just the file names (because of the I()).
#' I would suggest keeping every function and variable you need in one source file for this pattern
sb_replace_files <- function(filename, sb_id, ..., file_hash, sources = c()){
  
  files <- c(...)
  
  if (!missing(file_hash)){
    files <- c(files, names(yaml.load_file(file_hash))) %>% sort() 
  }

  # Throw error if there are no files given to push
  stopifnot(length(files) > 0)

  do_item_replace_tasks(sb_id, files, sources) %>% 
    write_csv(filename)
}

# Helper function to create a task_table for the files that need to be pushed to SB
do_item_replace_tasks <- function(sb_id, files, sources) {
  
  object_tgt_pattern <- 'sb_%s_%s_file'
  task_yml <- "file_upload_tasks.yml"
  # Define task table rows
  task_df <- tibble(filepath = files) %>% 
    mutate(task_name = sprintf(object_tgt_pattern, sb_id, basename(filepath)))
  
  # Define task table columns
  sb_push <- scipiper::create_task_step(
    step_name = 'push_file_to_sb',
    target_name = function(task_name, step_name, ...){
      task_name
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
  
  final_target <- sprintf("upload_%s_timestamps", sb_id)
  
  create_task_makefile(
    task_plan = task_plan,
    makefile = task_yml,
    packages = c('sbtools', 'scipiper', 'dplyr', 'remake'),
    sources = sources,
    final_targets = final_target,
    finalize_funs = "bind_rows",
    as_promises = FALSE)
  
  post_finished <- FALSE
  while (!post_finished){
    # Build the tasks
    loop_tasks(task_plan = task_plan, task_makefile = task_yml, num_tries = 3)
    upload_timestamps <- remake::fetch(final_target, remake_file=task_yml)
    # seems we need a delay before calling item_list_files, since the files need to be indexed first
    Sys.sleep(1.5)
    post_finished <- verify_uploads(upload_timestamps, tgt_names = object_tgt_pattern, remake_file=task_yml)
  }
  
  # Remove the temporary target from remake's DB; it won't necessarily be a 
  # unique name and we don't need it to persist, especially since killing the task yaml
  scdel(final_target, remake_file=task_yml)
  # delete task makefile for uploading the files to ScienceBase
  file.remove(task_yml)
  
  return(upload_timestamps)
}

#' `upload_and_record` in case there are any long-running uploads that timeout the session.
#' 
#' @param sb_id a sciencebase item identifier (character string)
#' @param filepath the path to the file(s) to be uploaded
#' 
#' @return a tibble with `filepath`, `sb_id`, and `time_uploaded_to_sb` with one row per 
#' file uploaded with this single call to `sbtools::item_replace_files``
upload_and_record <- function(sb_id, filepath) {
  
  # First verify that you are logged into SB. Need to do this for each task that calls 
  sb_secret_login()
  
  # Second, upload the file
  item_replace_files(sb_id, files = filepath)
  
  timestamp <- Sys.time()
  attr(timestamp, "tzone") <- "UTC"
  timestamp_chr <- format(timestamp, "%Y-%m-%d %H:%M %Z")
  
  # Then record when it happened and return that as an obj
  return(tibble(filepath = filepath, sb_id = sb_id, time_uploaded_to_sb = timestamp_chr))
}

#' verify the files you are expect to be on sciencebase are indeed on sciencebase
#' 
#' @param file_tbl a `data.frame` containing `filepath`, `sb_id`, and `time_uploaded_to_sb`
#' @param tgt_names the string pattern used to specify the object target names, 
#' assuming `sb_id` and `filepath` are used
#' @param remake_file the filepath to the remakefile yaml which contains the upload targets
#' @return TRUE if all files in `file_tbl` are on sciencebase and FALSE if any file is missing
#' 
#' @details this function call will fail if more than one unique `sb_id` is in the `file_tbl`,
#' or if any duplicated file names exist on sciencebase for this `sb_id`. 
verify_uploads <- function(file_tbl, tgt_names, remake_file){
  
  sb_secret_login()
  sb_id <- unique(file_tbl$sb_id)
  # this call is not robust to a tbl w/ more than one unique sb_id
  stopifnot(length(sb_id) == 1)
  
  # item_list_files _could_ fail, but that is pretty rare. If it does, this pipeline will break
  sb_files <- sbtools::item_list_files(sb_id) %>% pull(fname)
  
  if (any(duplicated(sb_files))){
    stop('there are duplicated files on %scatalog/item/%s', sbtools:::pkg.env$domain, sb_id)
  }
  
  unposted_files <- file_tbl %>% filter(!basename(filepath) %in% sb_files) %>% 
    mutate(target_name = sprintf(tgt_names, sb_id, basename(filepath))) %>% 
    pull(target_name)
  
  if (length(unposted_files) > 0){
    remake::delete(unposted_files, remake_file = remake_file)
    FALSE
  } else{
    TRUE
  }
}

#' a simple function to authenticate R for sciencebase using the "cidamanager" user if
#' you aren't already logged in. 
#' 
#' @details can avoid `dssecrets` usage sbtools::authenticate_sb('[my_sb_user]'), 
#' as long as you have permission to edit the item(s)
sb_secret_login <- function(){
  if (!sbtools::is_logged_in()){
    sb_secret <- dssecrets::get_dssecret("cidamanager-sb-srvc-acct")
    sbtools::authenticate_sb(username = sb_secret$username, password = sb_secret$password)
  }
}
