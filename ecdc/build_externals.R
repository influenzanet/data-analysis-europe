source("conf.R")
options(warn=1) # Show warnings as they occur

library(dplyr)
library(magrittr)
library(rlang)
library(swMisc)

share.lib("incidence") # Default method name fro syndrome

source("externals/utils.R")
source("externals/db.R")

init.path('indicator')

# Initialize local db
db = ExternalDB$new(external_db_path())

source_scripts = c(
  'nl_rivm',
  'de_grippeweb',
  #'dk_covidmeter',
  'dk_influmeter',
  'sweden'
)

for(script in source_scripts) {
    message("Runninng ", script)
    r = try(local({
        source(paste0("externals/", script, ".R") ) 
    }))
    if(is(r, "try-error")) {
      print(paste("Error during execution of", script))
      condition = attr(r, "condition")
      str(condition)
    }
}
