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

  result_desc_filters(auto=TRUE, filter=list())

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
  
  part.intake = intake %>% group_by(person_id) %>% summarize(date=min(date), .groups="drop")
  p = survey_participant_previous_season(season, ids=part.intake$person_id, country=country)
  part.intake$previous = part.intake$person_id %in% p 
  
  pp = part.intake %>% 
    group_by(date) %>%
    count() %>%
    ungroup()
  pp = pp %>% arrange(date) %>% mutate(cum=cumsum(n)) %>% rename(new=n)
  
  part.intake = part.intake %>% group_by(date) %>% summarize(n=n(), n_previous=sum(if_else(previous, 1, 0)))
  
  part.intake = left_join(part.intake, pp, by="date")
  
  collect_data(part.intake, name="intake", country)
  
  # Count participant date, n=1 for 1 participant-date
  part.date = weekly %>% group_by(person_id, date) %>% summarize(n=1, .groups="drop")
  
  # Cumulated version, participant only count on first date
  pp = part.date %>% 
        group_by(person_id) %>% 
        summarize(date=min(date), .groups="drop") %>%
        group_by(date) %>%
        count() %>%
        ungroup()
  pp = pp %>% arrange(date) %>% mutate(cum=cumsum(n)) %>% rename(new=n)
  
  # Compute min week for each participant
  pw = part.date %>% 
          mutate(yw=iso_yearweek(date)) %>% 
          group_by(person_id) %>%
          summarize(yw=min(yw), .groups="drop") %>%
          group_by(yw) %>%
          count() %>%
          ungroup()
  
  pw = pw %>% arrange(yw) %>% mutate(cum=cumsum(n)) %>% rename(new=n)
  
  part.date = part.date %>% group_by(date) %>% count(wt=n) %>% ungroup()
  
  part.date = left_join(part.date, pp, by="date")
  
  collect_data(part.date, name="weekly.date", country)

  part.weekly = part.date %>% 
                  mutate(yw=iso_yearweek(date)) %>% 
                  group_by(yw) %>% 
                  summarize(n=sum(n), .groups="drop")
  
  part.weekly = left_join(part.weekly, pw, by="yw")

  collect_data(part.weekly, name="weekly.week", country)
  
}


context = ResultContext$new()

#' Save helper with default parameters
g_save=function(..., width, height, desc=NULL) {
  save_graph_with_context(paste0(...), formats=c('pdf','svg'), width=width, height=height, context=context, desc=desc)
}


data = data.all$intake

context$set(subject="intake", time="date")

context$set(cumulated="no")

ggplot(data, aes(x=date, y=n, fill=country)) + 
  geom_bar(stat="identity", position="stack") + 
  facet_grid(rows="country", scales = "free_y") +
  ifn_labs(title="Count intake by date (first of each participant)", y="Count") 
g_save("intake_count_date_country", width=8, height=12)

ggplot(data, aes(x=date)) + 
  geom_bar(aes(y=n, fill="all"),stat="identity", position="stack") + 
  geom_bar(aes(y=n_previous, fill="previous"),stat="identity", position="stack") + 
  scale_fill_brewer(labels=c('all'=i18n("all"), 'previous'=i18n("previous_participant")), palette = "Dark2") +
  guides(fill=guide_legend("Participants")) +
  facet_grid(rows="country", scales = "free_y") +
  ifn_labs(title="Count intake by date (first of each participant) and previous participation", y="Count") 
g_save("intake_count_date_previous_country", width=8, height=12)

context$push()
context$set(cumulated="yes")

ggplot(data, aes(x=date, y=cum, fill=country)) + 
  geom_bar(stat="identity", position="stack") + 
  facet_grid(rows="country", scales = "free_y") +
  ifn_labs(title="Cumulated count of intakes by date (first of each participant)", y="Count") 
g_save("intake_cumulated_date_country", width=8, height=12)

context$pop()

data = data.all$weekly.date

context$set(subject="weekly")

context$set(cumulated="no")
ggplot(data, aes(x=date, y=n, fill=country)) + 
  geom_bar(stat="identity", position="stack") + 
  facet_grid(rows="country", scales = "free_y") +
  ifn_labs(title="Count of participants with weekly by date", y="Count") 
g_save("weekly_count_date_country",width=8, height=12)

context$set(cumulated="yes")
ggplot(data, aes(x=date, y=cum, fill=country)) + 
  geom_bar(stat="identity", position="stack") + 
  geom_line(aes(y=new, color="new"), stat="identity", position="stack") + 
  facet_grid(rows="country", scales = "free_y") +
  scale_color_manual(values=c("new"="black"), labels=c(new="Newcomers")) +
  ifn_labs(title="Cumulative count of participants with weekly by date", y="Count") 
g_save("weekly_cumulated_date_country",width=8, height=12)

context$set(cumulated="no", time="week")
data = data.all$weekly.week
ggplot(data, aes(x=monday_of_week(yw), y=n, fill=country)) + 
  geom_bar(stat="identity", position="stack") + 
  facet_grid(rows="country", scales = "free_y") +
  ifn_labs(title="Count of participants with weekly by calendar week", y="Count", x="Week") 
g_save("weekly_count_week_country", width=8, height=12)

context$set(cumulated="yes")
ggplot(data, aes(x=monday_of_week(yw), y=cum, fill=country)) + 
  geom_bar(stat="identity", position="stack") + 
  geom_line(aes(y=new, color="new"), stat="identity", position="stack") + 
  facet_grid(rows="country", scales = "free_y") +
  scale_color_manual(values=c("new"="black"), labels=c(new="Newcomers")) +
  ifn_labs(title="Cumulative count of participants with weekly by date", y="Count", x="Week") 
g_save("weekly_cumulated_week_country",width=8, height=12)

