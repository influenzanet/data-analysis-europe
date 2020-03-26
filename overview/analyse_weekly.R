##
# Analyse weekly data for a given season
##

source("conf.R")
library(dplyr)

share.lib("incidence")

caption = ifn.copyright # get then caption graph 

eu.params = get_eu_incidence_parameters()

countries = platform_env("COUNTRY_CODES")
age.categories = eu.params$age.categories

data.all = new.env(parent=emptyenv())

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

season = get_current_season()

init.path(season)

for(country in countries) {
  syndrome.from = list(health.status=TRUE)
  onset = episode_onset_design()
  dataset = load_results_for_incidence(season=season, age.categories=age.categories, country=country, syndrome.from = syndrome.from, onset=onset, columns=list(keep.all=TRUE))
  
  if(is.null(dataset$weekly) || is.null(dataset$intake) || nrow(dataset$weekly) == 0 || nrow(dataset$intake) == 0) {
    cat("not data for", country, "\n")
    next()
  }
  
  symptoms = get_symptoms_aliases()
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
  
  columns = c(symptoms, syndromes, syndromes.covid, syndromes.ecdc)
  
  weekly = weekly %>% group_by(person_id, onset) %>% summarize_at(columns, sum)
  
  weekly[, columns] = weekly[, columns] > 0
  
 # in this list were predefined by the Israeli MOH (Ministry of Health): muscle pains, shortness of breath,
 #  fatigue, cough and a high fever (body temperature of over 38 degrees celsius. For responders under the
 # age of 18 nausea and vomiting was also included.
  
  weekly$yw = iso_yearweek(weekly$onset)
  
  weekly = left_join(weekly, dataset$intake, by='person_id')
  age = weekly$age
  weekly$nausea_vom =  if_else(!is.na(age) & age < 18, weekly$nausea > 0 | weekly$vomiting > 0, FALSE)
  
  # Compute symptom ratio from Rossman H et al. 2020 (medRxiv)
  cols =  c('asthenia','cough','dyspnea', 'fever', 'pain', 'nausea_vom')
  
  weekly$count = apply(weekly[, cols], 1, sum)
  weekly$ratio = weekly$count / if_else(!is.na(age) & age < 18, 6, 5)
  
  ww = weekly %>% group_by(yw) %>% 
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
  ww$person = 1
  ww = ww %>% group_by(yw) %>% summarise_at(c('person',symptoms), sum)
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

  
}

library(ggplot2)

min.week = 201952

data = data.all$rossman

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=mean, group=country)) +
  geom_line() +
  facet_grid(rows=vars(country), scales="free_y") +
  labs(title="Symptom ratio from (Rossman H et al.), 2019")
ggsave(my.path("symptom_ratio.pdf"), width=12, height = 8)  

data = data.all$symptoms

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=value/person)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_fill_viridis_c(direction = -1, option = "A" ) +
  labs(x="Week", y="Symptom", title="% of symptom reported by participants", caption=caption()) +
  guides(fill=guide_legend("% of Participants"))
ggsave(my.path("symptom_prop.pdf"), width=6, height = 14)  

data = data.all$syndromes

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=value/person)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_fill_viridis_c(direction = -1, option = "A" ) +
  labs(x="Week", y="Syndromes", title="% of Influenzanet syndromes reported by participants", caption=caption()) +
  guides(fill=guide_legend("% of Participants"))
ggsave(my.path("syndrome_prop.pdf"), width=6, height = 14)  

data = data.all$syndromes.covid

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=value/person)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_fill_viridis_c(direction = -1, option = "A" ) +
  labs(x="Week", y="Syndromes", title="% of Influenzanet syndromes (without sudden) reported by participants", caption=caption()) +
  guides(fill=guide_legend("% of Participants"))
ggsave(my.path("syndrome-covid_prop.pdf"), width=6, height = 14)  

data = data.all$syndromes.ecdc

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=value/person)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_fill_viridis_c(direction = -1, option = "A" ) +
  labs(x="Week", y="Syndromes", title="% of Influenzanet syndromes (ECDC sets) reported by participants", caption=caption()) +
  guides(fill=guide_legend("% of Participants"))
ggsave(my.path("syndrome-ecdc_prop.pdf"), width=6, height = 14)  

d1 = data.all$syndromes.covid
d1$name = gsub(".covid", "", d1$name, fixed=TRUE)
d2 = data.all$syndromes.ecdc
data = bind_rows(covid=d1, ecdc=d2, .id="set")

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=value/person, color=set)) +
  geom_line() +
  scale_color_discrete(labels=c('ecdc'="With sudden (ECDC)", "covid"="Without sudden")) +
  facet_grid(rows=vars(country), cols=vars(name), scales="free_y") +
  labs(x="Week", y="Syndromes", title="% of Influenzanet syndromes (ECDC & without-sudden sets) reported by participants", caption=caption()) +
  guides(fill=guide_legend("% of Participants"))
ggsave(my.path("syndrome-covid-ecdc_prop.pdf"), width=14, height = 12)  

data = data.all$participants_date
data = data %>% filter(yw >= min.week)
days = c('1'="Monday",'2'='Tuesday','3'='Wednesday', '4'='Thursday','5'='Friday','6'='Saturday','7'='Sunday')

data = data %>% mutate(
  day = 8- ifelse(wday == 0, 7, wday)
)

ggplot(data, aes(x=monday_of_week(yw), y=factor(wday), fill=n_person/total_person)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_y_discrete(labels=days) +
  scale_fill_viridis_c(direction = -1, option = "A" ) +
  labs(x="Week", y="Day of week", title="% of person by week and weekday, by date of first report of the week", caption=caption()) +
  guides(fill=guide_legend("% of Participants"))
ggsave(my.path("week_participant_prop.pdf"), width=6, height = 14)  

ggplot(data, aes(x=monday_of_week(yw), y=factor(day), fill=n_survey/total_survey)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_y_discrete(labels=days) +
  scale_fill_viridis_c(direction = -1, option = "A" ) +
  labs(x="Week", y="Day of week", title="% of surveys by week and weekday, by date of first report of the week", caption=caption()) +
  guides(fill=guide_legend("% of Participants"))
ggsave(my.path("week_survey_prop.pdf"), width=6, height = 14)  


