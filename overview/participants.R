##
# Graph participants
#

# Load framework
source('conf.R')

library(dplyr)
library(ggplot2)

season = get_current_season()

countries = platform_env("COUNTRY_CODES")

data.all = new.env(parent=emptyenv())

collect_data = function(name, data, country) {
  if(is.data.frame(data)) {
    data$country = factor(country, countries)
    data.all[[name]] = bind_rows(data.all[[name]], data)
  } else {
    if(!is.null(data.all[[name]])) {
      prev = data.all[[name]]
    } else {
      prev = list()  
    }
    prev[[name]]
  }
}

init.path(season)

for(country in countries) {

  intake = survey_load_results('intake', cols=c("timestamp"), country =country, season=season)
  
  if(nrow(intake) == 0) {
    next()
  }
  
  weekly = survey_load_results('weekly', cols=c("timestamp"), country =country, season=season)
  
  if(nrow(weekly) == 0) {
    next()
  }
  
  weekly$date = as.Date(trunc(weekly$timestamp, "day")) 
  intake$date = as.Date(trunc(intake$timestamp, "day")) 
  
  ## Count intake : first for each particcpants
  
  part.intake = intake %>% group_by(person_id) %>% summarize(date=min(date))
  p = survey_participant_previous_season(season, ids=part.intake$person_id, country=country)
  part.intake$previous = part.intake$person_id %in% p 
  
  part.intake = part.intake %>% group_by(date, previous) %>% count()
  
  collect_data(part.intake, name="intake", country)
  
  part.weekly = weekly %>% group_by(person_id, date) %>% count() %>% ungroup()
  
  part.weekly = part.weekly %>% group_by(date) %>% count()
  
  collect_data(part.weekly, name="weekly", country)

  part.weekly = part.weekly %>% 
                  mutate(yw=iso_yearweek(date)) %>% 
                  group_by(yw) %>% 
                  summarize(n=sum(n))
  
  collect_data(part.weekly, name="weekly.week", country)
  
}

data = data.all$intake

total = data %>% group_by(date, country) %>% summarize(n=sum(n))
ggplot(total, aes(x=date, y=n, fill=country)) + 
  geom_bar(stat="identity", position="stack") + 
  facet_grid(rows="country", scales = "free_y") +
  ifn_labs(title="Count intake by date (first of each participant)", y="Count") 
ggsave(out_path("intake_count_date_country.pdf", plot=TRUE), width=8, height=12)

ggplot(data, aes(x=date, y=n, fill=previous)) + 
  geom_bar(stat="identity", position="stack") + 
  scale_fill_discrete(labels=c('TRUE'=i18n("Yes"), 'FALSE'=i18n("No"))) +
  guides(fill=guide_legend("In previous seasons")) +
  facet_grid(rows="country", scales = "free_y") +
  ifn_labs(title="Count intake by date (first of each participant) and previous participation", y="Count") 
ggsave(out_path("intake_count_date_previous_country.pdf", plot=TRUE), width=8, height=12)

total = total %>% group_by(country) %>% arrange(date) %>% mutate(cum=cumsum(n))
ggplot(total, aes(x=date, y=cum, fill=country)) + 
  geom_bar(stat="identity", position="stack") + 
  facet_grid(rows="country", scales = "free_y") +
  ifn_labs(title="Cumulated count of intakes by date (first of each participant)", y="Count") 
ggsave(out_path("intake_cumulated_date_country.pdf", plot=TRUE), width=8, height=12)

data = data %>% group_by(country) %>% arrange(date) %>% mutate(cum=cumsum(n))
ggplot(data, aes(x=date, y=cum, fill=previous)) + 
  geom_bar(stat="identity", position="stack") + 
  scale_fill_discrete(labels=c('TRUE'=i18n("Yes"), 'FALSE'=i18n("No"))) +
  guides(fill=guide_legend("In previous seasons")) +
  facet_grid(rows="country", scales = "free_y") +
  ifn_labs(title="Count intake by date (first of each participant) and previous participation", y="Count") 
ggsave(out_path("intake_cumulated_date_previous_country.pdf", plot=TRUE), width=8, height=12)

data = data.all$weekly

total = data %>% group_by(date, country) %>% summarize(n=sum(n))
ggplot(total, aes(x=date, y=n, fill=country)) + 
  geom_bar(stat="identity", position="stack") + 
  facet_grid(rows="country", scales = "free_y") +
  ifn_labs(title="Count of participants with weekly by date", y="Count") 
ggsave(out_path("weekly_count_date_country.pdf", plot=TRUE), width=8, height=12)

data = data.all$weekly.week
ggplot(data, aes(x=monday_of_week(yw), y=n, fill=country)) + 
  geom_bar(stat="identity", position="stack") + 
  facet_grid(rows="country", scales = "free_y") +
  ifn_labs(title="Count of participants with weekly by calendar week", y="Count") 
ggsave(out_path("weekly_count_week_country.pdf", plot=TRUE), width=8, height=12)

