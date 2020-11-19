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
  last = basename(unlist(lapply(ff, readLines)))
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
      pp = active %>% rename(part=active) %>% select(part, yw)
      inc = left_join(inc, pp, by=c('yw'))
      inc$type = factor(inc$type, c('adj','crude'))
      inc$country = country
      inc$method = method
      inc$season = season
      
      active$country = country
      active$method = method
      active$season = season
      datasets$incidence = bind_rows(datasets$incidence, inc)
      datasets$active = bind_rows(datasets$active, active)
    }
  }
  invisible(list(country=country, count=length(last)))
}

load_healthcare_country = function(country) {
  path = my.path(country, "/")
  ff = list.files(path, pattern="^healthcare-.*\\.last$", full.names = TRUE)
  last = NULL
  if(length(ff)) {
    
    last = basename(unlist(lapply(ff, readLines)))
    message(paste(country, " found", length(last) ))
    for(last.file in last) {
      r = try(readRDS(paste0(path, last.file)))
      if(is.error(r)) {
        message(paste0(country, ": Unable to load ", sQuote(last)))
      }
      meta = attr(r, "meta")
      season = meta$season
      
      # Variables frequencies
      ff = r$vars
      ff$country = country
      ff$season = season
      
      datasets$vars = bind_rows(datasets$vars, ff)
      
      # Cumulated frequency over each season for variables
      ff = r$cumul
      ff$country = country
      ff$season = season
      
      datasets$vars_cumulated =  bind_rows(datasets$vars_cumulated, ff)
      
      ff = r$syndrome
      ff$country = country
      ff$season = season
      
      datasets$freq_syndrome = bind_rows(datasets$freq_syndrome, ff)
    }
  }
  invisible(list(country=country, count=length(last)))
} 

message("Loading Incidence indicators")
ii = lapply(countries, load_incidence_country)

message("Loading Healthcare indicators")
datasets$vars=NULL
datasets$freq_syndrome=NULL
datasets$vars_cumulated=NULL

hh = lapply(countries, load_healthcare_country)

datasets$active %<>% 
    mutate_at(c("syndrome", "country", "method"), factor) %>% 
    arrange(season, yw)

datasets$incidence %<>%
    mutate_at(c("syndrome", "country", "method"), factor) %>% 
    arrange(yw, syndrome)

datasets$vars %<>%
  mutate_at(c("syndrome", "country", "variable", "type","level"), factor) %>% 
  arrange(yw, syndrome, variable) %>%
  mutate(cumulated=FALSE) %>%
  rename(estimator=type)  

datasets$vars_cumulated %<>%
  mutate_at(c("syndrome", "country", "variable","level"), factor) %>% 
  arrange(yw, syndrome, variable) %>%
  mutate(cumulated=TRUE, estimator="episode")

datasets$vars = bind_rows(datasets$vars, datasets$vars_cumulated)
rm("vars_cumulated", envir = datasets)

datasets$freq_syndrome %<>%
  mutate_at(c("syndrome", "country", "variable","level"), factor) %>% 
  arrange(yw, syndrome, variable)


# All computed data, not filtered
saveRDS(datasets, my.path("datasets.rds"))

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
    filter( (syndrome == "ili.ecdc" & method == "w1_s2_if2_ex") | (syndrome == "covid.ecdc" & method == "w0") )
}

#' Remove non constistent data
filter_base = function(data) {
  data %>% filter(
    !(country == "IE" & season == 2018),
    !(country %in% c("BE","NL") & season == 2017)
  )
  
}

filter_visits = function(data) {
  data = data %>% 
          filter(estimator == "episode" & grepl("^visit", variable) & level == "TRUE") %>% 
          select(-level, -estimator)
  dd = data %>% filter(!cumulated) %>% select(-cumulated)
  cum = data %>% filter(cumulated) %>% 
          select(-cumulated) %>%
          rename_with(~paste0("cum_",.), c(starts_with("prop_"), starts_with('n_'),  starts_with('total')))
  
  keys = c('yw','variable','syndrome','season','country')
  
  dd = left_join(dd, cum, by=keys)
 
  r = dd %>% select(!!!syms(keys), starts_with('prop_'), starts_with('cum_prop_'), starts_with('n_'), starts_with('total') )
   
}


# Bundles definition
bundles = list(
  list(
    name="incidence", 
    sorting=c("yw","syndrome"), 
    dataset="incidence", 
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
  ),
  list(
    name="visits_weekly",
    sorting="yw",
    dataset="vars",
    filters= list(
      filter_base,
      filter_season_weeks,
      filter_visits
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


outputs = list()
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
  
  outputs[[bundle$name]] = data
  
  data %>% group_by(country) %>% group_walk(create_bundle_country, bundle=bundle)

}

# Exported data
saveRDS(outputs, my.path("bundles.rds"))