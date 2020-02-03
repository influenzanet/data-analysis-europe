## Ecdc runner
## Catch cli arguments & handle errors
source("conf.R")

library(swMisc)

if(!exists("cli.args")) {
  cli.args = parseArgs(list(
    'season'=list(type="int", default=get_current_season()),
    'country'=list(type="choices", choices=platform_env("COUNTRY_CODES"))
  ))
}

country = cli.args$country
season = cli.args$season

run_script = function(name) {
  cat("-- Running ", name,"\n")
  env = new.env()
  r = try(source(paste0(name, ".R"), local=env))
  
  if(is.error(r)) {
    save(env, file=my.path("error_",name,"_", country, season,".RData"))
    dump(r, file=my.path("error_",name,"_", country, season,".R"))
  }

  if(is(attr(r,"condition"), "error_no_data")) {
    message("No data for this season")
    return()
  }
  
  if( is.error(r) ) {
    warning("Error during computing")
    print(r)
  }
  r
}

run_script("ecdc_indicator")
run_script("ecdc_healthcare")

