##
# Build bundle files from the computed data
# - Build incidence datasets by combining all the available data
# - Build csv outputs for the results website
##
source("conf.R")

library(dplyr)
library(rlang)
library(swMisc)
library(magrittr)

countries = platform_env("COUNTRY_CODES")
seasons = get_historical_seasons()

init.path('indicator')

## Incidence datasets
# Collect all incidence files in their last version
datasets = rlang::new_environment()

# Load dataset and collect it
load_incidence_country = function(country) {
  path = my.path(country, "/")
  ff = list.files(path, pattern="^incidence-.*\\.last$", full.names = TRUE)
  last = unlist(lapply(ff, readLines))
  message(paste(country, " found", length(last) ))
  for(last.file in last) {
    r = try(readRDS(paste0(path, last.file)))
    if(is.error(r)) {
      message(paste0(country, ": Unable to load ", sQuote(last)))
    }
    methods = names(r)
    for(method in methods) {
      rr = r[[method]]
      inc = rr$inc
      season = as.integer(rr$season)
      # Reorganize data to extract active participants
      active = inc %>% filter(syndrome == "active") %>% select(-upper, -lower, -type) %>% rename(active=value)
      count = inc %>% filter(type == "count") %>% select(-upper, -lower, -type) %>% rename(count=value)
      inc = inc %>% filter(type != "count") %>% rename(incidence=value)
      inc = left_join(inc, count, by=c('yw','syndrome'))
      inc$type = factor(inc$type, c('adj','crude'))
      inc$country = country
      inc$method = method
      inc$season = season
      
      active$country = country
      active$method = method
      active$season = season
      datasets$inc = bind_rows(datasets$inc, inc)
      datasets$active = bind_rows(datasets$active, active)
    }
  }
  invisible(list(country=country, count=length(last)))
}

lapply(countries, load_incidence_country)

datasets$active %<>% 
                    mutate_at(c("syndrome", "country", "method"), factor) %>% 
                    arrange(season, yw)

datasets$inc %<>%
    mutate_at(c("syndrome", "country", "method"), factor) %>% 
    arrange(yw, syndrome)

saveRDS(datasets, my.path("incidences.rds"))

dir.create(my.path('bundles'), showWarnings = FALSE)

filters = list(
  list()
  
  
  
  
)


#' @param path path where are by seasons files
#' @param name name of file to create
#' @param sorting column to use to sort the data
#' @param restrict.yw restrict the range of weeks according to the weeks of the season
#' @param filter function() to filter data
#' @param country
create_bundle = function(path, name, country, sorting, restrict.yw=TRUE, filter=NULL, out.name=NULL) {
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
      #cat("Excluding ", w,"\n")
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
    if(is.null(out.name)) {
      out.name = name
    }
    write.csv(data, file=my.path('bundles/', country, '_', out.name,'.csv'), row.names = FALSE)
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
  create_bundle(path, "visits_weekly", country, c('yw','variable'), out.name="visits_weekly_all")
  visits = create_bundle(path, "visits_weekly", country, c('yw','variable'), filter=filter_visits)
  if(!is.null(visits)) {
    last = visits %>% 
              group_by(season) %>%
              mutate(max_yw=max(yw)) %>%
              ungroup() %>%
              filter(yw == max_yw) %>% 
              select(yw, variable, cum_prop, cum_prop_upper, cum_prop_lower, season)
    
    if(nrow(last) > 0) {
      last$cum_prop_upper[!is.na(last$cum_prop_upper) & last$cum_prop_upper > 1] = 1
      last$cum_prop_lower[!is.na(last$cum_prop_lower) & last$cum_prop_lower < 0] = 0
    }
    
    write.csv(last, file=my.path('bundles/', country, '_visits_cumul.csv'), row.names = FALSE)
  } 
  
}