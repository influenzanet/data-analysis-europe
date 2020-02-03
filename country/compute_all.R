# Compute all
source("conf.R")

run_script = function(season, country, name) {
  cat("-- Running ", name,"\n")
  r = try({
    cli.args = list(country=country, season=season)
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
    run_script(season, country, "intakes")
  }
}