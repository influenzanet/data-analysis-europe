
colors.web = list('green'="#7AB800","blue"="#007AB8")

# Survey using the european template
platform_define_survey(
  "intake",
  survey_id=NULL,
  single.table=T,
  table = "epidb_results_intake",
  template="eu:intake",
  mapping=list(
    country="country"
 ),
 geo.column="Q3"
)


platform_define_survey(
 "weekly",
  single.table=T,
  survey_id=NULL,
  table = "epidb_results_weekly",
  mapping=list(
    country="country"
  ),
  template = "eu:weekly"
)

geo.levels = c('zip', 'nuts3', 'nuts2', 'nuts1', 'country')

platform_geographic_levels(
  geo.levels,
  # level code of the information in the survey
  level.base = 'zip',
  table = 'geo_zip',
  columns =  c(
    'zip'='code_zip',
    'nuts3'='code_nuts3',
    'nuts2'='code_nuts2',
    'nuts1'='code_nuts1',
    'country'='country'
  ),
  hierarchies=list(
    'europe'=geo.levels
  ),
  country = TRUE,
  default.hierarchy="europe"
)

# List of geographic tables
# name=level code
platform_geographic_tables(
   list(
     'zip'=list(table='geo_zip', title=NULL, column='code_zip'),
     'nuts3'=list(table='geo_nuts3', title="title", column='code_nuts3'),
     'nuts2'=list(table='geo_nuts2', title="title", column='code_nuts2'),
     'nuts1'=list(table='geo_nuts1', title="title", column='code_nuts1'),
     'country'=list(table='geo_country', title="title", column='country')
  )
)

COUNTRY_CODES = c('IE','UK','ES','DK','BE','IT','FR','PT','NL','SE','CH')

#' Determine if a code is in a "type" of a geographic level
#' Can check if a given geographic area has a given feature
#' Compat with some countries
geo_is_type = function(geo, type, code) {
   return( rep(TRUE, length(code)) )
}

## Historical tables

seasons = list(
  list(2011, pop=2012),
  list(2012, pop=2012),
  list(2013, pop=2013),
  list(2014, pop=2014),
  list(2015, pop=2015),
  list(2016, pop=2016),
  list(2017, pop=2017),
  list(2018, pop=2018),
  list(2019, pop=2018)
)

for(season in seasons) {
  y = season[[1]]
  platform_season_history(y,
        weekly='epidb_results_weekly',
        intake='epidb_results_intake',
        health='epidb_health_status_2012',
        dates=list('start'=paste0(y, '-10-01'), 'end'=paste0(y + 1, '-04-30')),
        year.pop=season$pop
  )
}

platform_options(
  first.season.censored=TRUE,
  complete.intake=list(
    max.year=2
  ),
  default_language = 'en',
  use.country = TRUE,
  health.status = list(
    "default"="epidb_health_status_2012",
    "id"="weekly_id"
  ),
  population.age.loader = "db",
  population.loader = "db"
)


#' Get the first season for a country
#' Returns the season year the platform was launched
#' Not necessarly the first season available in the eu DB
#' It should be used to know if we can determine the first season of participation for each participant
#' Season is the year number of the first year of each winter season
get_first_season_country <- function(country) {
  switch(toupper(country),
    "UK"=2009,
    "ES"=2012,
    "DK"=2013,
    "BE"=2003,
    "IT"=2008,
    "FR"=2011,
    "PT"=2005,
    "NL"=2003,
    "IE"=2013,
    "SE"=2011,
    "CH"=2016,
    stop(paste("first season not defined for country ", country))
  )
}

#' Get the season year until wich participant first season should be censored
#' This depends o
get_season_censoring <- function(country) {
  switch(toupper(country),
         "UK"=2011,
         "ES"=NA,
         "DK"=NA,
         "BE"=2011,
         "IT"=2011,
         "FR"=NA,
         "PT"=2011,
         "NL"=2011,
         "IE"=NA,
         "SE"=NA,
         "CH"=NA,
         stop(paste("first season not defined for country ", country))
  )
}
