source("conf.R")

library(dplyr)
library(rlang)

countries = platform_env("COUNTRY_CODES")
seasons = get_historical_seasons()

init.path('indicator')

dir.create(my.path('bundles'), showWarnings = FALSE)

#' @param path path where are by seasons files
#' @param name name of file to create
#' @param sorting column to use to sort the data
#' @param restrict.yw restrict the range of weeks according to the weeks of the season
#' @param filter function() to filter data
#' @param country
create_bundle = function(path, name, country, sorting, restrict.yw=TRUE, filter=NULL) {
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
    
    if(!is.null(filter)) {
      data = do.call(filter, list(data))
    }
  
    write.csv(data, file=my.path('bundles/', country, '_', name,'.csv'), row.names = FALSE)
  }
  
  data
}


filter_incidence = function(data) {
  i = is.na(data$upper) & is.na(data$lower)
  data$upper[i] = 0
  data$lower[i] = 0
  data
}

filter_visits = function(data) {
  data %>% filter(estimator == "adj")
}

for(country in countries) {
  path = my.path(country,"/")
  create_bundle(path, "active", country, "yw")
  create_bundle(path, "incidence", country, c("yw","syndrome"), filter=filter_incidence )
  create_bundle(path, "visits_weekly_all", country, c('yw','variable'))
  visits = create_bundle(path, "visits_weekly", country, c('yw','variable'), filter=filter_visits)
  if(!is.null(visits)) {
    last = visits %>% 
              group_by(season) %>%
              mutate(max_yw=max(yw)) %>%
              ungroup() %>%
              filter(yw == max_yw) %>% 
              select(yw, variable, cum_prop, cum_prop_upper, cum_prop_lower, season)
  }  
  write.csv(last, file=my.path('bundles/', country, '_visits_cumul.csv'), row.names = FALSE)
}