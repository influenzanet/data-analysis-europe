# Prepare and build files to be used by Syndrome Explorer Shiny app
source('conf.R')

library(dplyr)
library(forcats)

add_path_prefix("project","incidence")

share.lib("incidence")

countries = platform_env("COUNTRY_CODES")

# Seasons to compute  
seasons = 2021:2024
verbose = F
file.prefix = "syndromes"

init.path(file.prefix)

syndromes_provider = function(weekly, intake) {
  
  weekly$status = syndromes_influenzanet_2012(weekly)
  weekly$status = ifnBase::regroup.syndrome(weekly$status)
  
  ww = list(id=weekly$id)
  
  for(n in levels(weekly$status)) {
    ww[[paste0(n,".ifn")]] = weekly$status == n
  }
  
  ww = data.frame(ww)
  
  sd = SyndromeProviderRS2019$new()
  
  ww1 = sd$compute(weekly = weekly, intake=intake, use.sudden = FALSE)
  n = names(ww1)
  i = n != "id"
  n[i] = paste0(n[i], ".nosudden")
  names(ww1) <- n
  
  ww = full_join(ww, data.frame(ww1), by="id")
  
  ww2 = sd$compute(weekly = weekly, intake=intake, use.sudden = TRUE)
  
  ww = full_join(ww, data.frame(ww2), by="id")
  
  sdc = SyndromeProviderCovid$new()
  
  ww2 = sdc$compute(weekly = weekly, intake=intake)
  
  ww = full_join(ww, data.frame(ww2), by="id")
  
  ww
}

weekly.all = NULL
sd.all = c()
sympt.all = c()

for(season in seasons) {
  cat("* Starting season", season,"\n")
  for(country in countries) {
    cat("* loading country ", country," data\n")
    
    # If no data in cache & can load data
    syndrome.from = list(provider=syndromes_provider, health.status =FALSE)
    onset = episode_onset_design()
    r = load_results_for_incidence(season=season, age.categories=NULL, country=country, syndrome.from = syndrome.from, first.season = TRUE, onset=onset, columns = list(keep.all=TRUE))
    #saveRDS(r, file=my.path(paste(season, country, "data.rds", sep='.')))
    
    if( is.null(r) ) {
      cat("No data for ", country,"\n")
      next()
    }
    
    weekly = r$weekly
    intake = r$intake
    syndromes = r$syndromes
    
    weekly$yw = iso_yearweek(weekly$onset)
    
    rm(r)
    intake$country = toupper(country) # be sure this column exists (not provided if loaded only for one country)
    intake$country = factor(intake$country)
    weeks = sort(unique(iso_yearweek(weekly$date)))
    gc()
    
    
    symptoms = get_symptoms_columns(season)
    
    sympt.all = unique(c(sympt.all, symptoms))
    sd.all = unique(c(sd.all, syndromes))
    
    state.columns = c(symptoms, syndromes, 'sympt.sudden', 'high.fever', 'moderate.fever', 'fever.sudden')
    
    state.columns = state.columns[state.columns %in% names(weekly) ]
    
    ww = weekly %>% group_by(yw, person_id) %>% summarize(across(all_of(state.columns), ~sum(., na.rm=TRUE)>0), .groups="drop")
    
    ww = left_join(ww, intake[,c('person_id', 'age')], by='person_id')
 
    ww$country = factor(country, countries)
    ww$season = season
    
    weekly.all = bind_rows(weekly.all, ww)
       
  }
}

# Replace participant id with random values
weekly.all$person_id = runif(nrow(weekly.all), min=1, max=nrow(weekly.all))
weekly.all = weekly.all %>% arrange(country, season, yw, person_id)

weekly.all$person_id = NULL # Drop it

weekly.all$age = as.integer(round(weekly.all$age))

saveRDS(weekly.all, my.path("weekly.syndrome.all.rds"))

meta = list(
  symptoms = sympt.all,
  syndromes = sd.all,
  countries = unique(weekly.all$country),
  seasons = unique(weekly.all$season),
  criteria = c('sympt.sudden', 'high.fever', 'moderate.fever', 'fever.sudden'),
  time = Sys.time()
)

saveRDS(meta, my.path("meta.rds"))


