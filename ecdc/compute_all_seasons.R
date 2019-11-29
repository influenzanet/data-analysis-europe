# Compute all
source("conf.R")

for(season in get_historical_seasons()) {
  countries = platform_env("COUNTRY_CODES")
  for(country in countries) {
    cat("Computing ", country, " season ", season)
    cli.args = list("country"=country, season=season)
    r = try(local({
      source("ecdc_indicator.R")
    }))
    
    if(is(r, "error_no_data")) {
      message("No data for this season\n")
      next()
    }
    
    if( is.error(r) ) {
      warning("Error during computing")
      str(r)
      print(r)
    }
    
  }
}