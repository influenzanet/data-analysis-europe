##
# Analyse weekly data for a given season
# Country based analyses are stored in the country specific output directory
##

source("conf.R")

library(dplyr)
library(gridExtra)
suppressPackageStartupMessages(library(cowplot))

share.lib("incidence")
share.lib('upset')

season = get_current_season()

min.week = season * 100 + 51

short.term.size = 4
short.term = iso_yearweek(Sys.Date() - short.term.size * 7)

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

init.path(season)

syndrome.from = list(health.status=TRUE)
onset = episode_onset_design()
symptoms = get_symptoms_columns(season)

symptoms.mask = create_binary_mask(symptoms)

for(country in countries) {
  
  dataset = load_results_for_incidence(season=season, age.categories=age.categories, country=country, syndrome.from = syndrome.from, onset=onset, columns=list(keep.all=TRUE, weekly=symptoms))
  
  if(is.null(dataset$weekly) || is.null(dataset$intake) || nrow(dataset$weekly) == 0 || nrow(dataset$intake) == 0) {
    cat("not data for", country, "\n")
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
  
  # Suggested by Ania 
  #  fever and/OR cough (+ any combination of those with shortness of breath and fatigue).
  #
  n1 = c('cough','fever')
  n2 = c("asthenia","dyspnea" )
  weekly$ch.covid = rowSums(weekly[, n1], na.rm=TRUE) > 0 & rowSums(weekly[, n2], na.rm=TRUE) > 0
  
  
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

  ww$country = country
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

saveRDS(data.all, file=my.path("weekly_syndromes.Rds"))

library(ggplot2)

# Graph save shortcut
g_save = function(...,  desc=NULL, plot=FALSE, width=12, height = 8) {
  ggsave(out_path(..., ".pdf", plot=plot, desc=desc), width=width, height = height)  
}


data = data.all$rossman

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=mean, group=country)) +
  geom_line() +
  facet_grid(rows=vars(country), scales="free_y") +
  labs(title="Symptom ratio from (Rossman H et al.), 2019")
g_save("symptom_ratio", plot=TRUE, width=12, height = 8)  

data = data.all$symptoms

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=100*value/person)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_fill_viridis_c(direction = -1, option = "A", na.value="grey80") +
  labs(x="Week", y="Symptom", title="% of symptom reported by participants", caption=caption()) +
  guides(fill=guide_legend("% of Participants"))
g_save("symptom_prop", plot=TRUE, width=6, height = 14)  

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=100*value/person_with_sympt)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_fill_viridis_c(direction = -1, option = "A", na.value="grey80") +
  labs(x="Week", y="Symptom", title="% of symptom reported by participants (with at least 1 symptom)", caption=caption()) +
  guides(fill=guide_legend("% of Participants (with symptom)"))
g_save("symptom_prop_with_symptom", plot=TRUE, width=6, height = 14)  

ggplot(data %>% filter(yw >= min.week), aes(x=name, y=100*value/person_with_sympt, fill=100*value/person_with_sympt)) +
  geom_bar(stat="identity") +
  facet_grid(rows=vars(country), cols=vars(yw)) +
  scale_fill_viridis_c(direction = -1, option = "A",na.value="grey80") +
  coord_flip() + theme_with("x_vertical") +
  theme(axis.text = element_text(size=5)) +
  labs(x="Symptom", y="% of participants of the week", title="% of symptom reported by participants with at least 1 symptom", caption=caption()) +
  guides(fill=guide_legend("% of Participants (with symptom)"))
g_save("symptom_bar_prop_with_symptom", plot=TRUE, width=8, height = 10)  

# Short term graph
ggplot(data %>% filter(yw >= short.term), aes(x=monday_of_week(yw), y=name, fill=100*value/person_with_sympt)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_fill_viridis_c(direction = -1, option = "A",na.value="grey80") +
  labs(x="Week", y="Symptom", title="% of symptom reported by participants", caption=caption()) +
  guides(fill=guide_legend("% of Participants"))
g_save("symptom_prop_with_symptom_shortterm", plot=TRUE, width=6, height = 14)  


ggplot(data %>% filter(yw >= short.term), aes(x=name, y=100*value/person_with_sympt, fill=100*value/person_with_sympt)) +
  geom_bar(stat="identity") +
  facet_grid(rows=vars(country), cols=vars(yw)) +
  scale_fill_viridis_c(direction = -1, option = "A", na.value="grey80") +
  coord_flip() + theme_with("x_vertical") +
  theme(axis.text = element_text(size=5)) +
  labs(x="Symptom", y="% of participants of the week", title="% of symptom reported by participants with at least 1 symptom", caption=caption()) +
  guides(fill=guide_legend("% of Participants"))
g_save("symptom_bar_prop_with_symptom_shortterm", plot=TRUE, width=8, height = 10)  

data = data.all$syndromes

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=value/person)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_fill_viridis_c(direction = -1, option = "A" ) +
  labs(x="Week", y="Syndromes", title="% of syndromes reported by participants, Influenzanet syndromes set, by week", caption=caption()) +
  guides(fill=guide_legend("% of Participants"))
g_save("syndrome_prop.pdf", plot=TRUE, width=6, height = 14)  

data = data.all$syndromes.covid

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=value/person)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_fill_viridis_c(direction = -1, option = "A" ) +
  labs(x="Week", y="Syndromes", title="% of Influenzanet syndromes (without sudden) reported by participants", caption=caption()) +
  guides(fill=guide_legend("% of Participants"))
g_save("syndrome-covid_prop.pdf", plot=TRUE, width=6, height = 14)  

data = data.all$syndromes.ecdc

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=value/person)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_fill_viridis_c(direction = -1, option = "A" ) +
  labs(x="Week", y="Syndromes", title="% of Influenzanet syndromes reported by participants, ECDC syndromes set", caption=caption()) +
  guides(fill=guide_legend("% of Participants"))
g_save("syndrome-ecdc_prop.pdf", plot=TRUE, width=6, height = 14)  

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
g_save("syndrome-covid-ecdc_prop.pdf", plot=TRUE, width=14, height = 12)  

data = data.all$participants_date
data = data %>% filter(yw >= min.week)
days = c('7'="Monday",'6'='Tuesday','5'='Wednesday', '4'='Thursday','3'='Friday','2'='Saturday','1'='Sunday')

data = data %>% mutate(
  day =  8 - ifelse(wday == 0, 7, wday)
)

ggplot(data, aes(x=monday_of_week(yw), y=factor(day), fill=n_survey/total_survey)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_y_discrete(labels=days) +
  scale_fill_viridis_c(direction = -1, option = "A" ) +
  labs(x="Week", y="Day of week", title="% of surveys by week and weekday, by date of first report of the week", caption=caption()) +
  guides(fill=guide_legend("% of Participants"))
g_save("week_survey_prop.pdf", plot=TRUE, width=6, height = 14)  

## Symptoms association

data = data.all$symptom_groups

overall = data %>% filter(yw >= short.term) %>% group_by(g) %>% summarize(count=sum(n_person))
overall = get_labels_from_binary(overall, mask=symptoms.mask, group = "g")

upset_plot(overall, symptoms, n.max=120, title=paste0("Symptom associations for the last ", short.term.size, " weeks"), caption=caption())
g_save("symptom_upset_shortterm", plot=TRUE, width=14, height = 6)  

data = create_binary_groups(overall, symptoms.groups)
upset_plot(data, sets=names(symptoms.groups), title=paste0("Grouped symptoms associations for the last ", short.term.size, " weeks"), caption=caption())
g_save("grouped_symptom_upset_shortterm", plot=TRUE, width=14, height = 6)  


