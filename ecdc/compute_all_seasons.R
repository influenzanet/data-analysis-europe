# Compute all
source("conf.R")

for(season in get_historical_seasons()) {
  countries = platform_env("COUNTRY_CODES")
  for(country in countries) {
    cat("=== Computing ", country, " season ", season,"\n")
    r = try(local({
      source("ecdc_indicator.R")
    }), silent = TRUE)
    
    if(is(attr(r,"condition"), "error_no_data")) {
      message("No data for this season\n")
      next()
    }
    
    if( is.error(r) ) {
      warning("Error during computing")
      print(r)
    }
    
  }
}