source('conf.R')

library(dplyr)
library(swMisc)
library(rlang)

## Computation parameters
share.lib('incidence')

# Get all parameters sets
eu.params.sets = get_eu_incidence_parameters("all")

if(!exists("country") | is.null(country)) {
  rlang::abort("Country not defined")
}

if(!exists("season") | is.null(season)) {
  rlang::abort("Season not defined")
}

countries = platform_env("COUNTRY_CODES")
seasons = get_historical_seasons()

if(!country %in% countries) {
  rlang::abort(paste("Unknown country ", sQuote(country)))
}

if(!season %in% seasons) {
  rlang::abort(paste("Unknown season ", sQuote(season)))
}

init.path(paste0('indicator/', country))

provider = SyndromeProviderRS2019$new()

r = load_results_for_incidence(season=season, age.categories=NULL, country=country, syndrome.from = list(provider=provider$compute, health.status=FALSE), first.season=T, columns=list(keep.all=TRUE))

if( is.null(r) | is.null(r$weekly) | is.null(r$intake) ) {
  rlang::abort("No data", class = "error_no_data")
}

if( nrow(r$weekly) == 0 ) {
  rlang::abort("No weekly data", class = "error_no_data")
}

# Create a country column, as it will be needed when merging intake
r$intake$country = factor(country) 

h = season_definition(season = season)

results = list()

for(eu.params in eu.params.sets) {
  
  message("Computing ", eu.params$name)
  
  params = eu.params$estimator.params
  age.categories = eu.params$age.categories

  # compute age.cat according to current age groups
  r$intake$age.cat  = cut_age(r$intake$age, age.categories = age.categories)
  
  design = design_incidence(age.categories = age.categories, year.pop = h$year.pop, geo="country", geo_area = toupper(country))
  
  estimator = IncidenceRS2014$new(weekly=r$weekly, intake=r$intake, params=params, syndromes = r$syndromes, design=design, output=c("inc", "participant"))
  
  weeks = sort(unique(iso_yearweek(r$weekly$date)))
  
  inc.data = rlang::with_abort(estimator$compute(weeks = unique(r$weekly$yw), verticalize = TRUE, verbose=FALSE))
  
  if(is.error(inc.data)) {
    rlang::abort("Error during computation", parent=inc.data)
  }
  
  if(is.null(inc.data$inc)) {
    rlang::abort("No incidence data", class = "error_no_data")
  }
  
  inc.data$country = country
  inc.data$season = season

  results[[eu.params$name]] = inc.data
}

attr(results, "methods") <- names(results)
attr(results, "version") <- 2 

fn = paste0('incidence-', season,'-', Sys.Date(),'.Rds')
saveRDS(results, file=my.path(fn))
write(fn, file=my.path("incidence-", season,".last"))