# Compute all
source("conf.R")

if(!interactive()) {
  library(swMisc)
  
  compute = parseArgs(list(
    "incidence"=list(type="bool", default=TRUE),
    "healthcare"=list(type="bool", default=TRUE)
  ))
} else {
  if(!exists("compute")) {
    stop("compute variable must be defined")
  }
}

run_script = function(season, country, name) {
  cat("-- Running ", name,"\n")
  r = try({
    source(paste0(name, ".R"), local=TRUE)
  }, silent = TRUE)
  
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

seasons = get_historical_seasons()
for(season in seasons) {
  countries = platform_env("COUNTRY_CODES")
  for(country in countries) {
    cat("=== Computing ", country, " season ", season,"\n")
    
    if(isTRUE(compute$incidence)) {
      run_script(season, country, "ecdc_indicator")
    }
    
    if(isTRUE(compute$episode)) {
     run_script(season, country, "ecdc_healthcare")
    }

  }
}