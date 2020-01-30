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
create_bundle = function(path, name, country, sorting, restrict.yw=TRUE) {
  cat("Importing", name, country,"\n")
  data = NULL
  for(season in seasons) {
    file = paste0(path, name, "_", season, ".csv")
    
    if(!file.exists(file)){
      next()
    } 
    
    if(country == "IE" && season == 2018) {
      next()
    }
    
    if(country %in% c("BE", "NL") && season == 2017) {
      next()
    }
    
    
    r = read.csv2(file)
    
    if(restrict.yw) {
      ## Remove 2 first estimation points
      # Computed but given the computation rules they are not significants
      ww = sort(unique(r$yw))
      w = head(ww, n=2)
      cat("Excluding ", w,"\n")
      r = r[ !r$yw %in% w, ]
      
      # Maximum season bounds
      season.range = ((as.integer(season) + c(0, 1)) * 100) + c(40, 18)
      
      r = r[ r$yw >= season.range[1] & r$yw <= season.range[2], ]
    }
    
    if(nrow(r) > 0 && is.null(r[["season"]])) {
      r$season = as.integer(season)
    }
    
    data = bind_rows(data, r)
  }
  if(!is.null(data)) {
    data = data %>% arrange(!!!syms(sorting))
    
    if(name == "incidence") {
      i = is.na(data$upper) & is.na(data$lower)
      data$upper[i] = 0
      data$lower[i] = 0
    }
    write.csv(data, file=my.path('bundles/', country, '_', name,'.csv'), row.names = FALSE)
  }
}


for(country in countries) {
  path = my.path(country,"/")
  create_bundle(path, "active", country, "yw")
  create_bundle(path, "incidence", country, c("yw","syndrome"))
  create_bundle(path, "visits_weekly", country, c('yw','variable'))
  create_bundle(path, "visits_cumul", country, c('yw','variable'), restrict.yw = FALSE)
}