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

update.mode = get0("update.mode", ifnotfound = FALSE)


countries = platform_env("COUNTRY_CODES")
seasons = get_historical_seasons()

if(!country %in% countries) {
  rlang::abort(paste("Unknown country ", sQuote(country)))
}

if(!season %in% seasons) {
  rlang::abort(paste("Unknown season ", sQuote(season)))
}

init.path(paste0('indicator/', country))

results.file = my.path(paste0('incidence-', season,'-', Sys.Date(),'.Rds'))
last.file = my.path("incidence-", season,".last")
if(update.mode && file.exists(results.file)) {
  if(!file.exists(last.file)) {
    message("Last file updated")
    write(results.file, file=last.file)
  }
  rlang::abort("Already computed", class="error_already_done")
}

symptoms = get_symptoms_columns(season)

# Age group will be computed later
r = load_results_for_incidence(
  season=season, 
  age.categories=NULL, 
  country=country, 
  syndrome.from = ecdc_syndrome_from, 
  first.season=T, 
  columns=list(weekly=symptoms, keep.all=TRUE),
  onset = episode_onset_design()
)

if( is.null(r) | is.null(r$weekly) | is.null(r$intake) ) {
  rlang::abort("No data", class = "error_no_data")
}

if( nrow(r$weekly) == 0 ) {
  rlang::abort("No weekly data", class = "error_no_data")
}

# Create a country column, as it will be needed when merging intake
r$intake$country = factor(country) 

h = season_definition(season = season)

ss = symptoms[symptoms != ifnBase::NO_SYMPTOM]
r$weekly['any_symptom'] = rowSums(r$weekly[, ss], na.rm=TRUE) > 0
r$syndromes = c(r$syndromes, 'any_symptom')

results = list()

for(eu.params in eu.params.sets) {
  
  message("Computing ", eu.params$name)
  
  params = eu.params$estimator.params
  age.categories = eu.params$age.categories

  # compute age.cat according to current age groups
  r$intake$age.cat  = cut_age(r$intake$age, age.categories = age.categories)
  
  design = design_incidence(age.categories = age.categories, year.pop = h$year.pop, geo="country", geo_area = toupper(country))
  
  estimator = IncidenceRS2014$new(weekly=r$weekly, intake=r$intake, params=params, syndromes = r$syndromes, design=design, output=c("inc", "participant", "age"))
  
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
attr(results, "file") <- results.file
saveRDS(results, file=results.file)
write(results.file, file=last.file)