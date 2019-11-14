source("../system.R")

library(dplyr)

intake = survey_load_results('intake', cols=c("timestamp",'country','code_zip'), geo=c('nuts2','nuts3','nuts1'), debug=T)

# Nuts 1 level
i = is.na(intake$code_nuts1) & !is.na(intake$code_zip)
tt = table(intake$country, i)

intake$season = calc_season(as.Date(intake$timestamp))
intake$missing = i

# Missing geographic information (unknown code)
intake$survey = 1

miss.survey.season = intake %>%
      group_by(season, country, code_zip) %>%
      summarize(survey=n(), missing=sum(missing))

percent = function(x,n) { round(100 * x/n)}

# Number of missing codes by country and season
missing.codes.season = miss.survey.season %>%
      group_by(season, country) %>%
      summarise(
          code=n(),
          survey=sum(survey),
          missing.code=sum(missing > 0),
          missing.survey=sum(missing)
        ) %>%
      mutate(
        prop.code=percent(missing.code, code),
        prop.survey=percent(missing.survey, survey)
      )

xtabs(cbind(prop.code, prop.survey) ~ country + season, data=missing.codes.season)

## Regardless season

miss.survey = intake %>% group_by(country, code_zip) %>% summarize(survey=n(), missing=sum(missing))

# Number of missing codes by country and season
missing.codes = miss.survey %>%
  group_by(country) %>%
  summarize(
      missing.code=sum(missing > 0),
      missing.survey=sum(missing),
      survey=sum(survey),
      code=n()
  ) %>%
  mutate(
      prop.code=percent(missing.code, code),
      prop.survey=percent(missing.survey, survey)
  )

miss = miss.survey[miss.survey$missing > 0, ]

write.csv2(miss[,c('country','code_zip','survey')], file="missing-postal-codes.csv", row.names=F)
write.csv2(missing.codes, file="missing-postal-codes-summary.csv", row.names=F)


