# Count weekly & Weekly survey by country

source("conf.R")
library(dplyr)

init.path('overview')

participants = NULL

for(season in get_historical_seasons()) {

  #season = 2016

  cat("Season", season,"\n")

  intake = survey_load_results("intake", c('country'), season=season, debug=F)

  intake = intake %>% group_by(country, person_id) %>% summarize(count_intake=n())

  # Load all survey of the season
  # Complete with intake from past seasons if necessary (for ES and IT)
  weekly = survey_load_results("weekly", c('timestamp','country'), season=season)

  weekly = weekly %>% group_by(global_id, country) %>% summarize(count_weekly=n())

  p = full_join(intake, weekly)

  p$season = season

  participants = bind_rows(participants, p)
}

pp = participants %>% group_by(country, season) %>%
  summarize(
    registred=n(),
    has_instake=sum(!is.na(count_intake)),
    has_weekly=sum(!is.na(count_weekly)),
    weekly_wo_intake=sum(is.na(count_intake) & !is.na(count_weekly)),
    intake_wo_weekly=sum(!is.na(count_intake) & is.na(count_weekly)),
    weekly_and_intake=sum(!is.na(count_intake) & !is.na(count_weekly))
  )

write.csv(pp, my.path('participants_by_country.csv'), row.names = FALSE)