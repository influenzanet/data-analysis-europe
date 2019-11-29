source("conf.R")

library(dplyr)
library(rlang)

countries = platform_env("COUNTRY_CODES")
seasons = get_historical_seasons()

init.path('indicator')

dir.create(my.path('bundles'), showWarnings = FALSE)

#' @param path path where are by seasons files
#' @param name name of file to create
#' @param country
create_bundle = function(path, name, country) {
  files = paste0(path, name, "_", seasons, ".csv")
  data = NULL
  for(file in files) {
    if(!file.exists(file)){
      next()
    }    
    r = read.csv2(file)
    data = bind_rows(data, r)
  }
  write.csv(data, file=my.path('bundles/', country, '_', name,'.csv'), row.names = FALSE)
}


for(country in countries) {
  path = my.path(country,"/")
  create_bundle(path, "active", country)
  create_bundle(path, "incidence", country)
}