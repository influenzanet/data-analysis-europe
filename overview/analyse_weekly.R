##
# Analyse weekly data for a given season
##

source("conf.R")
library(dplyr)

share.lib("incidence")
caption = function() {
  paste(Sys.time(), "Influenzanet 2019, for internal purpose only")
}

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
  syndrome.from = list(health.status=FALSE)
  onset = episode_onset_design()
  dataset = load_results_for_incidence(season=season, age.categories=age.categories, country=country, syndrome.from = syndrome.from, onset=onset, columns=list(keep.all=TRUE))
  
  if(is.null(dataset$weekly) || is.null(dataset$intake) || nrow(dataset$weekly) == 0 || nrow(dataset$intake) == 0) {
    cat("not data for", country, "\n")
    next()
  }
  
  symptoms = get_symptoms_aliases()
  symptoms = symptoms[ symptoms != "no.sympt"]
  
  weekly = dataset$weekly %>% group_by(person_id, onset) %>% summarize_at(symptoms, sum)
  
  weekly[, symptoms] = weekly[, symptoms] > 0
  
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
  labs(x="Week", y="Symptom", title="% of symptom reported by participants", caption=caption())
ggsave(my.path("symptom_prop.pdf"), width=6, height = 14)  

data = data.all$participants_date
data = data %>% filter(yw >= min.week)
days = c('0'='Sunday','1'="Monday",'2'='Tuesday','3'='Wednesday', '4'='Thursday','5'='Friday','6'='Saturday')

ggplot(data, aes(x=monday_of_week(yw), y=factor(wday), fill=n_person/total_person)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_x_discrete(labels=days) +
  scale_fill_viridis_c(direction = -1, option = "A" ) +
  labs(x="Week", y="Day of week", title="% of person by week and weekday, by date of first report of the week", caption=caption())
ggsave(my.path("week_participant_prop.pdf"), width=6, height = 14)  

ggplot(data, aes(x=monday_of_week(yw), y=factor(wday), fill=n_survey/total_survey)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_x_discrete(labels=days) +
  scale_fill_viridis_c(direction = -1, option = "A" ) +
  labs(x="Week", y="Day of week", title="% of surveys by week and weekday, by date of first report of the week", caption=caption())
ggsave(my.path("week_survey_prop.pdf"), width=6, height = 14)  


