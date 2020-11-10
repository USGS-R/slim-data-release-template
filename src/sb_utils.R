sb_replace_files <- function(sb_id, ..., file_hash){
  
  if (!sbtools::is_logged_in()){
    sb_secret <- dssecrets::get_dssecret("cidamanager-sb-srvc-acct")
    sbtools::authenticate_sb(username = sb_secret$username, password = sb_secret$password)
  }
  
  hashed_filenames <- c()
  if (!missing(file_hash)){
    hashed_filenames <- yaml.load_file(file_hash) %>% names %>% sort() %>% rev()
    for (file in hashed_filenames){
      item_replace_files(sb_id, files = file)
    }
  }
  files <- c(...)
  if (length(files) > 0){
    item_replace_files(sb_id, files = files)
  }
  
}

sb_render_post_xml <- function(sb_id, ..., xml_file = NULL){
  
  if (is.null(xml_file)){
    xml_file <- file.path(tempdir(), paste0(sb_id,'.xml'))
  }
  
  render(filename = xml_file, ...)
  
  sb_replace_files(sb_id = sb_id, xml_file)
  
}