## Check syndrome computation and explore symptoms 
source("conf.R")

library(ggplot2)
library(dplyr)

# Create a specific dir in general output directory
init.path('')

share.lib('upset')

# Compute syndromes indicator columns, by combining several function results
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

# Describe how to compute syndrome columns for load_results_for_incidence()
syndrome.from = list(provider=syndromes_provider, health.status =FALSE)

# How to compute onset date from a survey
onset = episode_onset_design()

countries = c('FR','IT','DK','UK')

previous.seasons = -4

data.all = NULL
participants = NULL
surveys.all = NULL 
symptoms.assoc = NULL
syndromes.previous = NULL

symptoms.all = get_symptoms_aliases()
symptoms.all = symptoms.all[symptoms.all != ifnBase::NO_SYMPTOM]

symptoms.mask = create_binary_mask(symptoms.all)

get_previous_participants = function(season, index, ids, country) {
   ss = min(relative_seasons(season, index))
   def = season_definition(ss)
   min = as.Date(def$dates$start)
   h = season_definition(season)
   max = as.Date(h$dates$start)
   col.time = ifnBase:::db_quote_var("timestamp")
   w = paste0('where ', col.time, ">=", ifnBase:::db_quote_str(min),' and ', col.time, '<', ifnBase:::db_quote_str(max) ) 
   query = paste0("SELECT distinct s.id as person_id from ", h$weekly," p left join ", ifnBase:::join_surveyuser("p","s"), w)
   print(query)
   p = dbQuery(query)
   p$person_id
}

pops = c('all','previous','new')

as_pop = function(p) {
  if(!p %in% pops) {
    rlang::abort(paste(p, "not in pops"))
  }
  factor(p, pops)
}


for(season in 2017:2020) {
  for(country in countries) {

    cat("Season ", season, "country ", country, "\n")
    r = load_results_for_incidence(season=season, age.categories=NULL, country=country, syndrome.from = syndrome.from, first.season = TRUE, onset=onset, columns = list(keep.all=TRUE))
  
    ww = r$weekly
    ww = left_join(ww, r$intake[, c('person_id', 'age')], by="person_id")
    
    previous = get_previous_participants(season, ids=unique(ww$person_id), index=previous.seasons, country = country)
    
    symptoms = get_symptoms_columns(season)
    
    symptoms.all = unique(c(symptoms.all, symptoms))
    
    symptom.yes = symptoms[ !symptoms %in% ifnBase::NO_SYMPTOM ]
    
    if( !hasName(ww, "loss.taste")) {
      ww$loss.taste = NA
      ww$loss.smell = NA
      ww$nose.bleed = NA
    }
    
    #	ILI ECDC = apparition brutale ET fièvre ou frissons ou asthénie ou maux de tête ou douleur musculaire/articulaire (enfants >5 ans) ET maux de gorge ou toux ou dyspnée
    ww = mutate(ww, 
          ili.ecdc2 = ((!is.na(sympt.sudden) & sympt.sudden) | (!is.na(fever.sudden) & fever.sudden)) & (fever | chills | asthenia | headache | pain ) & (cough | sorethroat | dyspnea),
          sensor =  (!is.na(loss.taste) & loss.taste) | (!is.na(loss.smell) & loss.smell),
          sudden = (!is.na(sympt.sudden) & sympt.sudden),
          covid.ecdc2 = cough | fever | dyspnea | (age > 5 &  (sudden & sensor))
    ) 
    
    for(i in seq_along(symptoms.groups)) {
      g = symptoms.groups[[i]]
      name = names(symptoms.groups[i])
      ww[[name]] = rowSums(ww[,g, drop=FALSE], na.rm=TRUE)
    }
   
    ww$sympt_count = rowSums(ww[, symptom.yes], na.rm=TRUE)
    
    ww$any_sympt =  ww$sympt_count > 0
    
    ww$previous = ww$person_id %in% previous
    
    #ww[ ww$covid.ecdc != ww$covid.ecdc2, c('cough','fever','dyspnea','age','sympt.sudden','loss.taste','loss.smell', 'sudden','sensor', 'covid.ecdc','covid.ecdc2')]
    
    syndromes = c("covid.ecdc","ili.ecdc2","ili.ecdc", "covid.ecdc2","no.sympt", "sudden", "sensor", names(symptoms.groups), 'any_sympt')
    
    # Syndrome by week & person
    ww_person = ww %>% 
      group_by(yw, person_id) %>% 
      summarize_at(all_of(syndromes), sum) %>%
      ungroup() %>%
      mutate(n=1L) %>%
      mutate_at(all_of(syndromes), ~ . > 0)
    
    ww_person$previous = ww_person$person_id %in% previous
    
    build_syndrome_set = function(ww_person) {
      d = ww_person  %>%
        group_by(yw) %>%
        summarize_at(c('n', syndromes), sum)
      
      d = tidyr::pivot_longer(d, syndromes)
      
      d$season = season
      d$country = factor(country, countries)
      d
    }
    
    d = build_syndrome_set(ww_person)
    d$pop = as_pop("all")
    
    data.all = bind_rows(data.all, d)      

    d = build_syndrome_set(ww_person %>% filter(previous)) 
    d$pop = as_pop("previous")
    data.all = bind_rows(data.all, d)      
    
    d = build_syndrome_set(ww_person %>% filter(!previous)) 
    d$pop = as_pop("new")
    data.all = bind_rows(data.all, d)      
    
    part = ww %>% group_by(yw, person_id) %>%
      summarize(
        sympt_count=max(sympt_count),
        n_survey=n(),
        .groups='drop'
      ) 

    part$previous = part$person_id %in% previous
    part$season = season
    part$country = factor(country, countries)
    participants = bind_rows(participants, part)
   
    # Reported symptoms using reported surveys
    build_survey_set = function(ww) {
    
      surveys = ww %>%
        group_by(yw) %>%
        summarize(
          n_survey=n(),
          n_with_sympt=sum(sympt_count > 0),
          n_participants = n_distinct(person_id),
          sympt_count_max = max(sympt_count),
          sympt_count_median=median(sympt_count),
          sympt_count_q95=quantile(sympt_count, probs = .95, names=FALSE),
          sympt_count_q25=quantile(sympt_count, probs = .25, names=FALSE),
          sympt_count_q75=quantile(sympt_count, probs = .75, names=FALSE),
          .groups="drop"
        )
      surveys$season = season
      surveys$country = factor(country, countries)
      surveys
    }
    
    surveys = build_survey_set(ww)
    surveys$pop = as_pop("all")
    surveys.all = bind_rows(surveys.all, surveys)
    
    surveys = build_survey_set(ww %>% filter(previous))
    surveys$pop = as_pop("previous")
    surveys.all = bind_rows(surveys.all, surveys)
    
    surveys = build_survey_set(ww %>% filter(!previous))
    surveys$pop = as_pop("new")
    surveys.all = bind_rows(surveys.all, surveys)
    
    build_assoc = function(ww) {
    
      d =  ww %>% filter(!no.sympt)
      
      if(nrow(d) > 0) {
        d = apply_binary_mask(d, mask=symptoms.mask, column="g")
        
        # Flag to differentiate all symptoms (with extra) and olds
        d$all = apply(d[, symptom.yes], 1, function(x) !any(is.na(x)))
        
        #  mutate(yw=iso_yearweek(date)) %>% 
        # Remove week level, as it not used
        d = d %>% 
          group_by(person_id, all, g) %>% 
          summarize(count=n()) %>%
          group_by(all, g) %>%
          summarize(n_person=n(), n_syndrom=sum(count), .groups="drop")
        d = data.frame(d)
        
        d$country = factor(country, countries)
        d$season = season
      } else {
        d = NULL
      }
      d
    }
    
    d = build_assoc(ww)
    d$pop = as_pop("all")
    symptoms.assoc = bind_rows(symptoms.assoc, d)
  
    d = build_assoc(ww %>% filter(previous))
    d$pop = as_pop("previous")
    symptoms.assoc = bind_rows(symptoms.assoc, d)

    d = build_assoc(ww %>% filter(!previous))
    d$pop = as_pop("new")
    symptoms.assoc = bind_rows(symptoms.assoc, d)
    
  } # for Country     
} # for Season

data.all = data.all %>% mutate_if(is.character, factor)
surveys.all = surveys.all %>% mutate_if(is.character, factor)

saveRDS(list(
  symptoms = symptoms.all,
  symptoms.mask= symptoms.mask,
  syndromes=data.all,
  participants=participants,
  surveys=surveys.all,
  seasons = unique(data.all$season),
  countries= unique(data.all$country),
  symptoms.assoc=symptoms.assoc
), my.path('syndromes.rds'))

