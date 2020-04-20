
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

# 
recode.y1_n0_dkn2 = list("Yes" = 1, "No" = 0, "DNK" = 2)

platform_define_survey(
 "weekly",
  single.table=T,
  survey_id=NULL,
  table = "epidb_results_weekly",
  mapping=list(
    country="country",
    "loss.smell"=variable_available("Q1_23", rlang::quo(season >= 2019)),
    "loss.taste"=variable_available("Q1_21", rlang::quo(season >= 2019)),
    "nose.bleed"=variable_available("Q1_22", rlang::quo(season >= 2019)),
    
    # covid-19
    "travel"="Qcov1",
    "travel.country1"=variable_available("Qcov1b_multi_row1_col1", rlang::quo(season >= 2019)),
    "travel.country2"=variable_available("Qcov1b_multi_row2_col1", rlang::quo(season >= 2019)),
    "travel.country3"=variable_available("Qcov1b_multi_row3_col1", rlang::quo(season >= 2019)),
    
    "contactperson.foreign"=variable_available("Qcov2", rlang::quo(season >= 2019)),
    "contactperson.foreign.country1"=variable_available("Qcov2b_multi_row1_col1", rlang::quo(season >= 2019)),
    "contactperson.foreign.country2"=variable_available("Qcov2b_multi_row2_col1", rlang::quo(season >= 2019)),
    "contactperson.foreign.country3"=variable_available("Qcov2b_multi_row3_col1", rlang::quo(season >= 2019)),
    
    "contactperson.covid.confirm"=variable_available("Qcov3", rlang::quo(season >= 2019)),
    "contactperson.covid.confirm.house"=variable_available("Qcov3b", rlang::quo(season >= 2019)),
    "contactperson.covid.sympt"=variable_available("Qcov8", rlang::quo(season >= 2019)),
    "contactperson.covid.sympt.house"=variable_available("Qcov8b", rlang::quo(season >= 2019)),
    
    "call.tel.covid"=variable_available("Qcov4", rlang::quo(season >= 2019)),
    "call.tel.emergency"=variable_available("Qcov5", rlang::quo(season >= 2019)),
    
    "wear.mask"=variable_available("Qcov6", rlang::quo(season >= 2019)),
    
    "measure.wash.hands"=variable_available("Qcov7_1", rlang::quo(season >= 2019)),
    "measure.cough.elbow"=variable_available("Qcov7_2", rlang::quo(season >= 2019)),
    "measure.use.tissue"=variable_available("Qcov7_3", rlang::quo(season >= 2019)),
    "measure.wear.mask"=variable_available("Qcov7_4", rlang::quo(season >= 2019)),
    "measure.avoid.shakehands"=variable_available("Qcov7_5", rlang::quo(season >= 2019)),
    "measure.limit.transp"=variable_available("Qcov7_6", rlang::quo(season >= 2019)),
    "measure.avoid.gatherings"=variable_available("Qcov7_7", rlang::quo(season >= 2019)),
    "measure.stay.home"=variable_available("Qcov7_8", rlang::quo(season >= 2019)),
    "measure.work.home"=variable_available("Qcov7_9", rlang::quo(season >= 2019)),
    "measure.avoid.travel"=variable_available("Qcov7_10", rlang::quo(season >= 2019)),
    "measure.stop.hug.kiss"=variable_available("Qcov7_11", rlang::quo(season >= 2019)),
    "measure.avoid.none"=variable_available("Qcov7_12", rlang::quo(season >= 2019)),
    "measure.food.delivered"=variable_available("Qcov7_13", rlang::quo(season >= 2019)),
    "measure.avoid.friend.family"=variable_available("Qcov7_14", rlang::quo(season >= 2019)),
    "measure.avoid.elderly.risk"=variable_available("Qcov7_15", rlang::quo(season >= 2019)),
    "measure.avoid.children"=variable_available("Qcov7_16", rlang::quo(season >= 2019)),
    
    "reason.covid.doctor"=variable_available("Qcov9_1", rlang::quo(season >= 2019)),
    "reason.covid.confirm"=variable_available("Qcov9_2", rlang::quo(season >= 2019)),
    "reason.covid.contact.confirm"=variable_available("Qcov9_3", rlang::quo(season >= 2019)),
    "reason.covid.contact.diagnos"=variable_available("Qcov9_4", rlang::quo(season >= 2019)),
    "reason.covid.contact.sympt"=variable_available("Qcov9_5", rlang::quo(season >= 2019)),
    "reason.covid.event.confirm"=variable_available("Qcov9_6", rlang::quo(season >= 2019)),
    "reason.covid.think"=variable_available("Qcov9_7", rlang::quo(season >= 2019)),
    
    "inform.contact.covid"=variable_available("Qcov9b", rlang::quo(season >= 2019)),
    
    "confin.work.home"=variable_available("Qcov10_1", rlang::quo(season >= 2019)),
    "confin.work.outside"=variable_available("Qcov10_2", rlang::quo(season >= 2019)),
    "confin.work.absence.child"=variable_available("Qcov10_3", rlang::quo(season >= 2019)),
    "confin.work.absence.sick"=variable_available("Qcov10_4", rlang::quo(season >= 2019)),
    "confin.work.other"=variable_available("Qcov10_5", rlang::quo(season >= 2019)),
    
    "confin.work.outside.ndays"=variable_available("Qcov10b", rlang::quo(season >= 2019)),
    
    "confin.shopping"=variable_available("Qcov11", rlang::quo(season >= 2019)),
    
    "confin.exercice"=variable_available("Qcov12", rlang::quo(season >= 2019)),
    
    "confin.contact.close"=variable_available("Qcov13", rlang::quo(season >= 2019)),
    
    "confinstop.work.home"=variable_available("Qcov14_1", rlang::quo(season >= 2019)),
    "confinstop.work.outside"=variable_available("Qcov14_2", rlang::quo(season >= 2019)),
    "confinstop.work.absence.child"=variable_available( "Qcov14_3", rlang::quo(season >= 2019)),
    "confinstop.work.absence.sick"=variable_available("Qcov14_4", rlang::quo(season >= 2019)),
    "confinstop.work.other"=variable_available("Qcov14_5", rlang::quo(season >= 2019)),
    "confinstop.work.DNK"=variable_available("Qcov14_6", rlang::quo(season >= 2019)),
    
    "confinstop.work.outside.ndays"=variable_available("Qcov14b", rlang::quo(season >= 2019)),
    
    "confinextend.follow.reco"=variable_available("Qcov15", rlang::quo(season >= 2019))

  ),
  labels=list(
    "sympt.cause" = list(
      "cause.ili" = "0",
      "cause.cold" = "1",
      "cause.allergy" = "2",
      "cause.gastro" = "3",
      "cause.other" = "4",
      "cause.dkn" = "5",
      "cause.asthma" = "6",
      "cause.ili.doctor" = "7",
      "cause.other.doctor" = "8",
      "cause.covid" = "9"
    ),
    measures="measure.*",
    reason.covid="reason.covid.*",
    confin.work="confin.work.*",
    confinstop.work="confinstop.work.*"
  ),
  recodes=list(
    # covid-19
    "travel"=recode.y1_n0_dkn2,
    # "travel.country1"=
    # "travel.country2"=
    # "travel.country3"=
    
    "contactperson.foreign"=recode.y1_n0_dkn2,
    # "contactperson.foreign.country1"=
    # "contactperson.foreign.country2"=
    # "contactperson.foreign.country3"=
    
    "contactperson.covid.confirm"=recode.y1_n0_dkn2,
    "contactperson.covid.confirm.house"=recode.y1_n0_dkn2,
    "contactperson.covid.sympt"=recode.y1_n0_dkn2,
    "contactperson.covid.sympt.house"=recode.y1_n0_dkn2,
    
    "call.tel.covid"=recode.y1_n0_dkn2,
    "call.tel.emergency"=recode.y1_n0_dkn2,
    
    "wear.mask"= list(
      "Yes" = 1,
      "no.couldnot.find" = 2,
      "No" = 0
    ),
    
    "inform.contact.covid"=list(
      "Yes" = 1,
      "some.of.them" = 2,
      "No" = 0
    ),
    
    "confin.shopping"=list(
      "anymore" = 1,
      "less.1.week" = 2,
      "1.week" = 3,
      "2to6.week"=4,
      "1.day"=5,
      "more.1.day"=6
    ),
    
    "confin.exercice"=list(
      "anymore" = 1,
      "less.1.week" = 2,
      "1.week" = 3,
      "2to6.week"=4,
      "1.day"=5,
      "more.1.day"=6
    ),
    
    "confin.contact.close"=list(
      "0" = 0,
      "1" = 1,
      "2.to.5" = 2,
      "6.to.10"=3,
      "more.10"=4
    ),
    
    "confinextend.follow.reco"=list(
      "no.not.at.all" = 1,
      "no.not.really" = 2,
      "yes.moderately"=3,
      "yes.absolutely"=4,
      "DNK"=99
    )
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
