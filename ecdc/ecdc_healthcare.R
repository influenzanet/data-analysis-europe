source('conf.R')
library(dplyr)
library(swMisc)
library(rlang)
library(reshape2)

share.lib("episodes")
share.lib('incidence')

# Get all parameters sets
eu.params = get_eu_incidence_parameters("default")

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

update.mode = get0("update.mode", ifnotfound = FALSE)

init.path(paste0('indicator/', country))

results.file = my.path(paste0('healthcare-', season,'-', Sys.Date(),'.Rds'))
last.file = my.path("healthcare-", season,".last")
if(update.mode) {
 need_update(results.file, last.file)
}

# Current season data (population,...)
season.def = season_definition(season = season)

age.categories = eu.params$age.categories

# Year of the reference general population to fetch
year.pop = season.def$year.pop

# Strata columns 
# Can be age.cat and gender (need reference population data in each strata)
strata = c('age.cat')

visits.columns = survey_labels('weekly', 'visit')
medic.columns =  survey_labels('weekly', 'medic')
analysis.columns = survey_labels('weekly', 'analysis.sympt.covid')

# Download columns
cols = c(visits.columns, medic.columns)

# Columns on which compute the frequencies (can be computed after)
frequency.vars = c(visits.columns, medic.columns) 

# Syndromes to compute the frequency on
syndromes = c("ili.ecdc", "covid.ecdc")

if(season > 2019) {
  cols = c(cols, analysis.columns) # Only Qcov16 in db 
  #  "analysis.sympt.result.pcr","analysis.sympt.result.sero","analysis.sympt.flu","analysis.sympt.flu.result"
  
  frequency.vars = c(frequency.vars, analysis.columns)
}

cols = survey_variable_available(cols, survey="weekly", season=season)

if(any(!frequency.vars %in% cols)) {
  # Be sure vars to compute frequency on are still in data
  frequency.vars = frequency.vars[frequency.vars %in% cols]
}


r = load_results_for_incidence(
  season=season, 
  age.categories=age.categories, 
  country=country, 
  syndrome.from = ecdc_syndrome_from, 
  first.season=TRUE, # Get data for first participation censorship
  columns=list(keep.all=TRUE, weekly=cols),
  onset = episode_onset_design()
)

if( is.null(r) | is.null(r$weekly) | is.null(r$intake) ) {
  rlang::abort("No data",class = "error_no_data")
}

if( nrow(r$weekly) == 0 ) {
  rlang::abort("No weekly data", class = "error_no_data")
}

# Data are handled in an environment for episode computation
# Keep it to avoid data duplication
env = list2env(r)
rm(r)

# Create a country column, as it will be needed when merging intake
env$intake$country = factor(country) 

# Ensure syndrome columns are boolean
env$weekly = mutate_at(env$weekly, env$syndromes, ~ . > 0L)

# How to compute episodes
design.episode = episode_design(delay_episode = 15L, strategies = get_default_episode_strategies())

# to get population
#design.pop = design_incidence(age.categories = age.categories, year.pop = year.pop, geo="country", geo_area = toupper(country))

# Prepare data to compute episode
episode_prepare_data(design.episode, env = env)

env$weekly$yw  = iso_yearweek(env$weekly$onset)
env$weekly = survey_recode_all(env$weekly, "weekly")

# Create stratification weights for participants
part.weight = create_participant_weight(age.categories, year.pop, groups="yw", strata=strata, weekly=env$weekly, intake=env$intake)

# Put participants weight in the main weekly
env$weekly = merge(env$weekly, part.weight[, c('person_id','yw','weight')], by=c('person_id','yw'), all.x=TRUE)

results = new.env() # Data to export

collect = function(name, data, syndrome) {
  if(nrow(data) == 0) {
    return()
  }
  data$syndrome = syndrome
  if(!is.null(results[[name]])) {
    results[[name]] = bind_rows(results[[name]], data)
  } else {
    results[[name]] = data
  }
}

meta = list(episode.params=list(), time = Sys.time(), country=country, season=season)

for(syndrome.column in syndromes) {
  
  # Count steps counts
  nn = list()
  
  nn$weekly_all = nrow(env$weekly)

  message(syndrome.column)
  env$weekly$episode = NULL # Be sure we dont have episode column
  
  # Update the episode design with specific info for syndrome, keeping other params
  syndrome.episode = episode_design_syndrome(syndrome.column, design.episode)

  meta$episode.params[[syndrome.column]] = syndrome.episode
  
  # Compute episode for the current syndrome
  episode_compute(env, design=syndrome.episode, syndrome.column = syndrome.column)
  
  # Weekly after all episode data merge (keep only one survey response by *episode*)
  # So we keep only ONE survey response by episode, each variable is an aggregated value of responses
  # named "fusion" here to differentiate from the "merge" operation, usually for data.frame() by columns
  weekly.fusion = episode_fusion_strategy(env$weekly, design = design.episode)
  
  weekly.fusion$yw = iso_yearweek(weekly.fusion$onset)
  
  nn$syndrome_selected = nrow(weekly.fusion)
  
  # Put strata weights
  weekly.fusion = merge(weekly.fusion, part.weight[, c('person_id','yw','weight')], by=c('person_id','yw'), all.x=TRUE)
  
  nn$weekly_complete = sum(!is.na(weekly.fusion$weight))
  
  weekly.fusion = weekly.fusion %>% filter(!is.na(weights))
  
  # Design for stratification
  d.weighted = design_weight(weekly.fusion)
  
  ## Recompute syndrome frequency
  
  # merged weekly doesnt doesnt has syndrome column
  # Mimic full weekly by keeping only fist episode survey
  w.sd = env$weekly[, c('timestamp', syndrome.column, 'yw', 'episode','person_id', 'weight')]
  w.sd = w.sd %>%
            arrange(person_id, episode, timestamp) %>%
             group_by(person_id, episode) %>%
             mutate(order=row_number()) %>%
             ungroup()
            
  # Compute weekly frequency by participant. Reduce all surveys to one by week & participant
  # Each var = any(var), TRUE if at least one survey has true
  freq_bool_weekly = function(ww, vars) {
    ww = ww %>% group_by(person_id, yw) %>% summarize(across(all_of(vars), ~sum(.) >0 ), weight=max(weight)) 
    freq_bool_by(design_weight(ww), vars, "yw")
  }
  
  f_raw = freq_bool_weekly(w.sd, syndrome.column)
  
  w.sd = w.sd %>% filter( !(!!sym(syndrome.column) & order > 1))
  
  f_episode = freq_bool_weekly(w.sd, syndrome.column)
  
  # Frequency 
  freqs_syndrome = bind_freqs(raw=f_raw, episode=f_episode)
  
  collect("syndrome", freqs_syndrome, syndrome=syndrome.column)
  
  rm(w.sd)
  
  # Variable on which to compute the frequency on
  vars = frequency.vars

  ff = freq_bool_by(weekly.fusion, vars = vars, by = "yw", design = d.weighted)
  ff$type = "episode"
  
  collect("vars", ff, syndrome=syndrome.column)
  
  weeks = unique(ff$yw)
  
  # Compute cumulated frequency (with stratification) by episode (not participant !)
  freqs_cumul = NULL
  for(yw in weeks) {
    ww = weekly.fusion %>% filter(yw <= !!yw)
    ww$g = 1L
    freq_yw = freq_bool_by(ww, vars = vars, by = "g", design = design_weight(ww))
    freq_yw$yw = yw
    freq_yw$g = NULL
    freqs_cumul = bind_rows(freqs_cumul, freq_yw)
  }
  
  collect("cumul", freqs_cumul, syndrome=syndrome.column)
  
  ## Count number of surveys by participants by weeks
  ww = env$weekly %>% filter(!!sym(syndrome.column))
  p.raw = ww %>% 
            count(yw, person_id, name = "count") %>% 
            group_by(yw, count) %>% 
            summarize(value=n()) 
  
  p.e = weekly.fusion %>% 
          count(yw, person_id, name = "count") %>% 
          group_by(yw, count) %>% 
          summarize(value=n()) 
  
  pp= bind_freqs(raw=data.frame(p.raw), episode = data.frame(p.e))

  collect("participants", pp, syndrome=syndrome.column)
}

# Transform all char vector to factor, reduce memory footprint
results = lapply(results, function(data) {
  data %>% mutate_if(is.character, factor)
})

attr(results,"meta") <- meta

saveRDS(results, file=results.file)
write(results.file, file=last.file)
