## Project conf
## This file should be use for all scripts in this project to share the same conf & init
# Paths are relative to this directory
source("../system.R")

add_path_prefix("project", "ecdc")

need_update = function(results.file, last.file) {
  ok = file.exists(results.file) 
  if(!ok && file.exists(last.file)) {
    f = readLines(last.file)
    delay = as.integer(difftime(Sys.time(), file.mtime(f), units = "hours"))
    if(delay < 24) {
      ok = TRUE
    }
  }
  if(ok) {
    if(!file.exists(last.file)) {
      message("Last file updated")
      write(results.file, file=last.file)
    } 
    rlang::abort("Already computed", class="error_already_done")
  }
}