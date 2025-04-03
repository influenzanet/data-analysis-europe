source("../system.R")

if(!exists("local_data_path")) {
  rlang::abort(paste("Variable 'local_data_path' is not defined in configuration"))
}