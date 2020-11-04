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

#' Remove first estimated point of the season and restrict range (for season < 2020)
filter_season_weeks = function(data) {
  
  restrict_season = function(r, .keys) {
    season = .keys$season
    
    if(season >= 2020) {
      return(r)
    }
    
    if(season == 2019) {
      season.bounds = c(40, 30) # 2019 : include summer, no season end
    } else {
      season.bounds = c(40, 18) # Before 2019  week 40 to week 18 of y+1
    }
    
    ## Remove 2 first estimation points
    # Computed but given the computation rules they are not significants
    ww = sort(unique(r$yw))
    w = head(ww, n=2)
    #cat("Excluding ", w,"\n")
    r = r[ !r$yw %in% w, ]
    
    # Maximum season bounds
    season.range = ((as.integer(season) + c(0, 1)) * 100) + season.bounds
    
    r = r[ r$yw >= season.range[1] & r$yw <= season.range[2], ]
    r
  }
  data %>% group_by(season) %>% group_modify(restrict_season)
}

#' Incidence data selection
filter_incidence = function(data) {
  
  na_to_zero = function(x) {
    x[is.na(x)] = 0
    x
  }
  
  data %>% 
    mutate(across(c(upper, lower), na_to_zero )) %>%
    filter(type == "adj") %>%
    filter( (syndrome == "ili.ecdc" & method == "w1_s2_if2_ex") | (syndrome == "covid.ecdc" & method == "w0_s2_if2_ex") )
}


filter_visits = function(data) {
  data %>% filter(estimator == "adj")
}

#' Remove non constistent data
filter_base = function(data) {
  data %>% filter(
    !(country == "IE" & season == 2018),
    !(country %in% c("BE","NL") & season == 2017)
  )
  
}

# Bundles definition
bundles = list(
  list(
    name="incidence", 
    sorting=c("yw","syndrome"), 
    dataset="inc", 
    filters = list(
      filter_base,
      filter_season_weeks,
      filter_incidence
      )
  ),
  list(
    name="active",
    sorting="yw",
    dataset="active",
    filters= list(
      filter_base,
      filter_season_weeks
    )
  )
)

# Create a bundle for a country
create_bundle_country = function(data, .keys, bundle) {
  country = .keys$country
  data = data %>% arrange(!!!syms(bundle$sorting))
  message(paste("+  Writing bundle", country, nrow(data), "rows"))
  write.csv(data, file=my.path('bundles/', country, '_', bundle$name,'.csv'), row.names = FALSE) 
}

# Build bundles
for(bundle in bundles) {
  name = bundle$name
  message(paste("Bundle", name))
  data = datasets[[bundle$dataset]]
  if(is.null(data)) {
    message(paste("No data for bundle", name))
    next()
  }
  filters = bundle$filters
  for(f in filters) {
    data = do.call(f, list(data))
  }
  data %>% group_by(country) %>% group_walk(create_bundle_country, bundle=bundle)
}

# for(country in countries) {
#   path = my.path(country,"/")
#   create_bundle(path, "active", country, "yw")
#   create_bundle(path, "incidence", country, c("yw","syndrome"), filter=filter_incidence )
#   create_bundle(path, "visits_weekly", country, c('yw','variable'), out.name="visits_weekly_all")
#   visits = create_bundle(path, "visits_weekly", country, c('yw','variable'), filter=filter_visits)
#   if(!is.null(visits)) {
#     last = visits %>% 
#               group_by(season) %>%
#               mutate(max_yw=max(yw)) %>%
#               ungroup() %>%
#               filter(yw == max_yw) %>% 
#               select(yw, variable, cum_prop, cum_prop_upper, cum_prop_lower, season)
#     
#     if(nrow(last) > 0) {
#       last$cum_prop_upper[!is.na(last$cum_prop_upper) & last$cum_prop_upper > 1] = 1
#       last$cum_prop_lower[!is.na(last$cum_prop_lower) & last$cum_prop_lower < 0] = 0
#     }
#     
#     write.csv(last, file=my.path('bundles/', country, '_visits_cumul.csv'), row.names = FALSE)
#   } 
#   
# }