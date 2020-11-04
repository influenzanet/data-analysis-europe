##
# Analyse weekly data for a given season
# Country based analyses are stored in the country specific output directory
##

source("conf.R")

library(dplyr)
library(gridExtra)
library(ggplot2)

suppressPackageStartupMessages(library(cowplot))

share.lib("incidence")
share.lib('upset')

season = get_current_season()

eu.params = get_eu_incidence_parameters()

countries = platform_env("COUNTRY_CODES")
age.categories = eu.params$age.categories

data.all = new.env(parent=emptyenv())

# Helper to put data for all countries in data.frame
collect_data = function(name, data) {
  if(is.data.frame(data)) {
    data.all[[name]] = bind_rows(data.all[[name]], data)
  } else {
    if(!is.null(data.all[[name]])) {
      prev = data.all[[name]]
    } else {
      prev = list()  
    }
  }
}

init.path(season)

syndrome.from = list(health.status=TRUE)
onset = episode_onset_design()
symptoms = get_symptoms_columns(season)

symptoms.mask = create_binary_mask(symptoms)

measure.covid = survey_labels('weekly', "measures")
reason.covid = survey_labels('weekly', "reason.covid")
confin.work = survey_labels('weekly', "confin.work")
confinstop.work = survey_labels('weekly', "confinstop.work")

weekly.columns = c(symptoms, measure.covid, reason.covid, confin.work, confinstop.work)

other.questions <- list(
  list(name="measure",  vars= measure.covid, type="bool", min.week=202011),
  list(name="reason",  vars=reason.covid, type="bool", min.week=202011), 
  list(name="confin",  vars=confin.work, type="bool", min.week=202011),
  list(name="confinstop",  vars=confinstop.work, type="bool", min.week=202011)
)

for(country in countries) {
  
  dataset = load_results_for_incidence(season=season, age.categories=age.categories, country=country, syndrome.from = syndrome.from, onset=onset, columns=list(keep.all=TRUE, weekly=weekly.columns))
  
  if(is.null(dataset$weekly) || is.null(dataset$intake) || nrow(dataset$weekly) == 0 || nrow(dataset$intake) == 0) {
    message("not data for", country, "\n")
    next()
  }
  
  dataset$weekly = dataset$weekly %>% arrange(timestamp)
  
  symptoms = symptoms[ symptoms != "no.sympt"]
  syndromes = dataset$syndromes
  syndromes = syndromes[ syndromes != "no.symptom"]
  
  weekly = dataset$weekly 
  
  # Add .ifn suffix to influenzanet syndrom names
  nn <- syndromes
  names(nn) <- paste0(syndromes, ".ifn")
  weekly = rename(weekly, !!!nn)
  syndromes = names(nn)
  
  sd = SyndromeProviderRS2019$new()
  ww = sd$compute(weekly = weekly, intake=dataset$intake, use.sudden = FALSE)
  n = names(ww)
  i = n != "id"
  n[i] = paste0(n[i], ".covid")
  names(ww) <- n
  
  weekly = left_join(weekly, data.frame(ww), by="id")
  
  syndromes.covid = n[ n != "id"]
  
  ww = sd$compute(weekly = weekly, intake=dataset$intake, use.sudden = TRUE)
  n = names(ww)
  weekly = left_join(weekly, data.frame(ww), by="id")
  
  syndromes.ecdc = n[ n != "id"]
  
  sdc = SyndromeProviderCovid$new()
  ww = sdc$compute(weekly=weekly, intake=dataset$intake)
  
  weekly = left_join(weekly, data.frame(ww), by="id")
  
  # Suggested by Ania 
  #  fever and/OR cough (+ any combination of those with shortness of breath and fatigue).
  #
  n1 = c(ifnBase::SYMPT_COUGH, ifnBase::SYMPT_FEVER)
  n2 = c("asthenia","dyspnea" )
  weekly$ch.covid = rowSums(weekly[, n1], na.rm=TRUE) > 0 & rowSums(weekly[, n2], na.rm=TRUE) > 0

  columns = c(symptoms, syndromes, syndromes.covid, syndromes.ecdc)
  
  weekly$yw = iso_yearweek(weekly$onset)
  
  ## Symptom by perceived cause
  ww = weekly %>% filter(!no.sympt)
  
  ww = ww %>%
    group_by(yw, person_id, sympt.cause) %>%
    summarize_at(columns, sum, na.rm=TRUE) %>%
    group_by(yw, sympt.cause) %>%
    summarise_at(columns, ~sum(. > 0))

  ww = data.frame(ww)
    
  ww$country = factor(country, countries)
  collect_data("symptom_causes", ww)

  
 # in this list were predefined by the Israeli MOH (Ministry of Health): muscle pains, shortness of breath,
 #  fatigue, cough and a high fever (body temperature of over 38 degrees celsius. For responders under the
 # age of 18 nausea and vomiting was also included.
  
  
  weekly = left_join(weekly, dataset$intake, by='person_id')
  age = weekly$age
  weekly$nausea_vom =  if_else(!is.na(age) & age < 18, weekly$nausea > 0 | weekly$vomiting > 0, FALSE)
  
  # Compute symptom ratio from Rossman H et al. 2020 (medRxiv)
  cols =  c('asthenia','cough','dyspnea', 'fever', 'pain', 'nausea_vom')
  
  weekly$count = apply(weekly[, cols], 1, sum)
  weekly$ratio = weekly$count / if_else(!is.na(age) & age < 18, 6, 5)
  
  ww = weekly %>% 
        group_by(yw, person_id) %>%
        summarize(ratio=max(ratio, na.rm=TRUE)) %>%
        group_by(yw) %>% 
        summarize(
          mean=mean(ratio, na.rm=TRUE),
          q1=quantile(ratio, na.rm=TRUE, probs=.25),
          q3=quantile(ratio, na.rm=TRUE, probs=.75),
          med=quantile(ratio, na.rm=TRUE, probs=.50)
        )
  
  ww$country = factor(country, countries)
  
  collect_data("rossman", ww)
  
  ww = weekly %>% group_by(yw, person_id) %>% summarize_at(symptoms, sum)
  ww[, symptoms] = ww[, symptoms] > 0
  ww$person = 1L
  ww$person_with_sympt = as.integer(apply( ww[, symptoms], 1, sum, na.rm=TRUE) > 0)
  ww = ww %>% group_by(yw) %>% summarise_at(c('person', 'person_with_sympt', symptoms), sum)
  ww = tidyr::pivot_longer(ww, symptoms)
  ww$name = factor(ww$name)
  
  ww$country = factor(country, countries)
  
  collect_data("symptoms", ww)

  # Influenzanet Syndromes 
  
  ww = weekly %>% group_by(yw, person_id) %>% summarize_at(syndromes, sum)
  ww[, syndromes] = ww[, syndromes] > 0
  ww$person = 1
  ww = ww %>% group_by(yw) %>% summarise_at(c('person', syndromes), sum)
  ww = tidyr::pivot_longer(ww, syndromes)
  ww$name = factor(ww$name)
  
  ww$country = factor(country, countries)
  
  collect_data("syndromes", ww)

  #  Syndromes ECDC
  ss = syndromes.ecdc
  ww = weekly %>% group_by(yw, person_id) %>% summarize_at(ss, sum)
  ww[, ss] = ww[, ss] > 0
  ww$person = 1
  ww = ww %>% group_by(yw) %>% summarise_at(c('person', ss), sum)
  ww = tidyr::pivot_longer(ww, ss)
  ww$name = factor(ww$name)
  
  ww$country = factor(country, countries)
  
  collect_data("syndromes.ecdc", ww)

  # Covid Syndromes 
  ss = syndromes.covid
  ww = weekly %>% group_by(yw, person_id) %>% summarize_at(ss, sum)
  ww[, ss] = ww[, ss] > 0
  ww$person = 1
  ww = ww %>% group_by(yw) %>% summarise_at(c('person', ss), sum)
  ww = tidyr::pivot_longer(ww, ss)
  ww$name = factor(ww$name)
  
  ww$country = factor(country, countries)
  
  collect_data("syndromes.covid", ww)
  
  rm(ww)
  rm(weekly)
  
  ww = dataset$weekly %>% filter(!no.sympt)
  
  ww = apply_binary_mask(ww, mask=symptoms.mask, column="g")
  
  # Flag to differentiate all symptoms (with extra) and olds
  ww$all = apply(ww[, symptoms], 1, function(x) !any(is.na(x)))

  ww = ww %>% 
        mutate(yw=iso_yearweek(date)) %>% 
        group_by(person_id, yw, all, g) %>% 
        summarize(count=n()) %>%
        group_by(yw, all, g) %>%
        summarize(n_person=n(), n_syndrom=sum(count))
  ww = data.frame(ww)

  ww$country = factor(country, countries)

  collect_data("symptom_groups", ww)

  rm(ww)
  
  # Number of survey reported by week
  # Here use reporting date (timestamp) not the onset date for the week
  ww = dataset$weekly %>% 
        mutate(yw=iso_yearweek(date)) %>%
        group_by(person_id, yw) %>%
        summarize(n_survey=n()) 
  
  ww = ww %>% group_by(yw) %>% summarize(n_person=n(), mean_survey=mean(n_survey), max_survey=max(n_survey))
  ww$country = factor(country, countries)
  
  collect_data("participants_week", ww)
  
  ##
  # Other questions
  ##
  
  #' Frequency table for weekly & particpant data
  #' @param data
  freq_weekly = function(data, columns, type="bool") {
    
    #sum_na = function(x) { sum(x, na.rm=TRUE) }
    any_notna = function(x) { any(!is.na(x)) } 
    if(type == "bool") {
      
      d <- data %>% group_by(yw, person_id) %>% summarize_at(columns, sum, na.rm=TRUE) 
      d <- d %>% ungroup() %>% group_by(yw) %>% summarize_at(columns, sum)
      d <- tidyr::pivot_longer(d, -yw, names_to = 'variable', values_to = "count")
      
      # compute total
      total <- data %>% group_by(yw, person_id) %>% summarize_at(columns, any_notna)
      total = total %>% ungroup() %>% group_by(yw) %>% summarise_at(columns, sum)
      total <- tidyr::pivot_longer(total, -yw, names_to = 'variable', values_to = "total")
      
      d$variable = factor(d$variable)
      total$variable = factor(total$variable)
    
      d = full_join(total, d, by=c('yw','variable'))
      
    }
    
    d = data.frame(d)
    attr(d, "type") <- type
    d
  }
  
  for(question in other.questions) {
    
    variables = question$vars
    
    ww <- dataset$weekly %>% select(yw, person_id, !!!variables)

    if(!is.null(question$min.week)) {
      ww = ww %>% filter(yw >= question$min.week)
    }    
  
    if(nrow(ww) == 0) {
      next()
    } 
      
    d = freq_weekly(ww, variables, type=question$type)
    
    d$country = factor(country, countries)
    
    collect_data(question$name, d)
    
  }
  
  rm(ww, d)

  ###
  ## Analyse participants
  ###

  # Number of survey by week & first day of week for each user
  ww = dataset$weekly %>% 
        select(date, person_id) %>% 
        mutate(
            yw=iso_yearweek(date),
            wday=as.integer(format(format="%w", date))
        ) %>%
        group_by(person_id, yw) %>%
        summarize(n_survey=n(), wday=min(wday)) 
  
  # Count number of survey & person by week & weekday
  ww = ww %>%
        ungroup() %>%
        group_by(yw, wday) %>%
        summarize(
          n_person=n(),
          n_survey=sum(n_survey)
  )
  
  ww = ww %>%
        ungroup() %>%
        group_by(yw) %>%
        mutate(
          total_person=sum(n_person),
          total_survey=sum(n_survey)
        )
  ww$country = factor(country, countries)
  collect_data("participants_date", ww)

  rm(ww)

}

attr(data.all, "syndromes") <- list(
  "covid"=structure(syndromes.covid, title="Covid syndromes"),
  "ecdc"=structure(syndromes.ecdc, title="ECDC syndromes"),
  "ifn"=structure(syndromes, title="Influenzanet syndroms")
) 

attr(data.all, "symptoms") <- symptoms
attr(data.all, "symptoms.mask") <- symptoms.mask
attr(data.all, "questions") <- other.questions
saveRDS(data.all, file=my.path("weekly_syndromes.Rds"))
