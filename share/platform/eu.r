
colors.web = list('green'="#7AB800","blue"="#007AB8")

# Helper quosures
until_2020 = quo(season <= 2020)
from_2019 = quo(season >= 2019)
in_2019_20 = quo(season %in% c(2019,2020))

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
  "vaccination",
  survey_id=NULL,
  single.table=T,
  create_id=TRUE,
  table = "epidb_vaccination",
  template="eu:vaccination",
  mapping=list(
    country="country"  
  ),
  labels=list(
    # Override labels because some are not available
    "covid.vacc.reason"=var_labels("covid.vacc.reason.*", exclude=c("covid.vacc.reason.other.txt","covid.vacc.reason.mandatory","covid.vacc.reason.pass")),
    "covid.nvac.reason"=var_labels("covid.nvac.reason.*", exclude="covid.nvac.reason.other.txt")
  )
)

# 
recode.y1_n0_dkn2 = list("Yes" = 1, "No" = 0, "DNK" = 2)

labels_pcr_delay = list(
  "1d"=1,
  "2to4d"=2,
  "5to7d"=3,
  "8to14d"=4,
  "15to28d"=5,
  "over4w"=6,
  "DNK"=99
)

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
    "travel.country1"=variable_available("Qcov1b_multi_row1_col1", rlang::quo(season == 2019)),
    "travel.country2"=variable_available("Qcov1b_multi_row2_col1", rlang::quo(season == 2019)),
    "travel.country3"=variable_available("Qcov1b_multi_row3_col1", rlang::quo(season == 2019)),
    
    "contactperson.foreign"=variable_available("Qcov2", rlang::quo(season == 2019)),
    "contactperson.foreign.country1"=variable_available("Qcov2b_multi_row1_col1", rlang::quo(season == 2019)),
    "contactperson.foreign.country2"=variable_available("Qcov2b_multi_row2_col1", rlang::quo(season == 2019)),
    "contactperson.foreign.country3"=variable_available("Qcov2b_multi_row3_col1", rlang::quo(season == 2019)),
    
    "contactperson.covid.confirm"=variable_available("Qcov3", rlang::quo(season >= 2019)),
    "contactperson.covid.confirm.house"=variable_available("Qcov3b", rlang::quo(season >= 2019)),
    "contactperson.covid.sympt"=variable_available("Qcov8", rlang::quo(season >= 2019)),
    "contactperson.covid.sympt.house"=variable_available("Qcov8b", rlang::quo(season >= 2019)),
    
    "call.tel.covid"=variable_available("Qcov4", rlang::quo(season == 2019)),
    "call.tel.emergency"=variable_available("Qcov5", rlang::quo(season >= 2019)),
    
    "wear.mask"=variable_available("Qcov6", rlang::quo(season >= 2019)),
    
    "measure.wash.hands"=variable_available("Qcov7_1", rlang::quo(season == 2019)),
    "measure.cough.elbow"=variable_available("Qcov7_2", rlang::quo(season == 2019)),
    "measure.use.tissue"=variable_available("Qcov7_3", rlang::quo(season == 2019)),
    "measure.wear.mask"=variable_available("Qcov7_4", rlang::quo(season == 2019)),
    "measure.avoid.shakehands"=variable_available("Qcov7_5", rlang::quo(season == 2019)),
    "measure.limit.transp"=variable_available("Qcov7_6", rlang::quo(season == 2019)),
    "measure.avoid.gatherings"=variable_available("Qcov7_7", rlang::quo(season == 2019)),
    "measure.stay.home"=variable_available("Qcov7_8", rlang::quo(season == 2019)),
    "measure.work.home"=variable_available("Qcov7_9", rlang::quo(season == 2019)),
    "measure.avoid.travel"=variable_available("Qcov7_10", rlang::quo(season == 2019)),
    "measure.stop.hug.kiss"=variable_available("Qcov7_11", rlang::quo(season == 2019)),
    "measure.avoid.none"=variable_available("Qcov7_12", rlang::quo(season == 2019)),
    "measure.food.delivered"=variable_available("Qcov7_13", rlang::quo(season == 2019)),
    "measure.avoid.friend.family"=variable_available("Qcov7_14", rlang::quo(season == 2019)),
    "measure.avoid.elderly.risk"=variable_available("Qcov7_15", rlang::quo(season == 2019)),
    "measure.avoid.children"=variable_available("Qcov7_16", rlang::quo(season == 2019)),
    "measure.wear.mask.inside"=variable_available("Qcov7_17", rlang::quo(season >= 2021)),
    "measure.isolate.home"=variable_available("Qcov7_18", quo(season >= 2021)),
    
    "reason.covid.doctor"=variable_available("Qcov9_1", rlang::quo(season >= 2019)),
    "reason.covid.confirm"=variable_available("Qcov9_2", rlang::quo(season >= 2019)),
    "reason.covid.contact.confirm"=variable_available("Qcov9_3", rlang::quo(season >= 2019)),
    "reason.covid.contact.diagnos"=variable_available("Qcov9_4", rlang::quo(season >= 2019)),
    "reason.covid.contact.sympt"=variable_available("Qcov9_5", rlang::quo(season >= 2019)),
    "reason.covid.event.confirm"=variable_available("Qcov9_6", rlang::quo(season >= 2019)),
    "reason.covid.think"=variable_available("Qcov9_7", rlang::quo(season >= 2019)),
    
    "inform.contact.covid"=variable_available("Qcov9b", rlang::quo(season >= 2019)),
    
    "confin.work.home"=variable_available("Qcov10_1", rlang::quo(season == 2019)),
    "confin.work.outside"=variable_available("Qcov10_2", rlang::quo(season == 2019)),
    "confin.work.absence.child"=variable_available("Qcov10_3", rlang::quo(season == 2019)),
    "confin.work.absence.sick"=variable_available("Qcov10_4", rlang::quo(season == 2019)),
    "confin.work.other"=variable_available("Qcov10_5", rlang::quo(season == 2019)),
    
    "confin.work.outside.ndays"=variable_available("Qcov10b", rlang::quo(season == 2019)),
    
    "confin.shopping"=variable_available("Qcov11", rlang::quo(season == 2019)),
    
    "confin.exercice"=variable_available("Qcov12", rlang::quo(season == 2019)),
    
    "confin.contact.close"=variable_available("Qcov13", rlang::quo(season >= 2019)),
    
    "confinstop.work.home"=variable_available("Qcov14_1", rlang::quo(season >= 2019)),
    "confinstop.work.outside"=variable_available("Qcov14_2", rlang::quo(season >= 2019)),
    "confinstop.work.absence.child"=variable_available( "Qcov14_3", rlang::quo(season >= 2019)),
    "confinstop.work.absence.sick"=variable_available("Qcov14_4", rlang::quo(season >= 2019)),
    "confinstop.work.other"=variable_available("Qcov14_5", rlang::quo(season >= 2019)),
    "confinstop.work.DNK"=variable_available("Qcov14_6", rlang::quo(season >= 2019)),
    
    "confinstop.work.outside.ndays"=variable_available("Qcov14b", rlang::quo(season >= 2019)),
    
    "confinextend.follow.reco"=variable_available("Qcov15", rlang::quo(season >= 2019)),
    
    "analysis.sympt.covid.pcr"=variable_available("Qcov16_1", rlang::quo(season >= 2019)),
    "analysis.sympt.covid.sero"=variable_available("Qcov16_2", rlang::quo(season >= 2019)),
    "analysis.sympt.covid.not.but.plan"=variable_available("Qcov16_3", rlang::quo(season >= 2019)),
    "analysis.sympt.covid.not.plan"=variable_available("Qcov16_4", rlang::quo(season >= 2019)),
    "analysis.sympt.covid.no"=variable_available("Qcov16_0", rlang::quo(season >= 2019)),
    "analysis.sympt.covid.antigenic"=variable_available("Qcov16_5", in_2019_20),
    
    "analysis.sympt.covid.v2.test"=variable_available("Qcov16h", quo(season >= 2021)),
    "analysis.sympt.covid.v2.pcr"=variable_available("Qcov16i_1", quo(season >= 2021)),
    "analysis.sympt.covid.v2.sero"=variable_available("Qcov16i_2", quo(season >= 2021)),
    "analysis.sympt.covid.v2.antigenic.pharynx"=variable_available("Qcov16i_3", quo(season >= 2021)),
    "analysis.sympt.covid.v2.antigenic.nasal"=variable_available("Qcov16i_4", quo(season >= 2021)),
    
     "analysis.sympt.result.pcr"=variable_available("Qcov16b", rlang::quo(season >= 2019)),
     "analysis.sympt.result.sero"=variable_available("Qcov16c", rlang::quo(season >= 2019)),
     "analysis.sympt.result.antigenic"=variable_available("Qcov16f", from_2019), # Only for pharyngeal antigenic test
     "analysis.sympt.result.antigenic.nasal"=variable_available("Qcov16k", from_2019),
    
    # 
     "analysis.sympt.pcr.search.delay"=variable_available("Qcov16d", rlang::quo(season >= 2020)),
     "analysis.sympt.pcr.sample.delay"=variable_available("Qcov16e", rlang::quo(season >= 2020)),
     "analysis.sympt.flu"=variable_available("Qcov19", rlang::quo(season >= 2020)),
     "analysis.sympt.flu.result"=variable_available("Qcov19b", rlang::quo(season >= 2020))
    # 
    # "visit.no.reason"=variable_available("Qcov18", rlang::quo(season >= 2020)),
    # "visit.no.reason.fear"=variable_available("Qcov18b", rlang::quo(season >= 2020))
  
  ),
  labels=list(
    "symptoms.extra"=c('loss.smell', 'loss.taste', 'nose.bleed'),
    measures="measure.*",
    reason.covid="reason.covid.*",
    confin.work=c('confin.work.home','confin.work.outside','confin.work.absence.child','confin.work.absence.sick','confin.work.other'),
    confinstop.work=c('confinstop.work.home','confinstop.work.outside','confinstop.work.absence.child','confinstop.work.absence.sick','confinstop.work.other','confinstop.work.DNK'),
    analysis.sympt.covid="analysis.sympt.covid.*",
    analysis.sympt.result="analysis.sympt.result.*"
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
    ),
    "sympt.cause"=list(
      cause.covid="9"
    ),
    "analysis.sympt.result.pcr"=list(
      "positive" = 1,
      "negative" = 2,
      "not.interpretable"=3,
      "not.yet"=4
    ),
    "analysis.sympt.result.sero"=list(
      "positive" = 1,
      "negative" = 2,
      "not.interpretable"=3,
      "not.yet"=4
    ),
    "analysis.sympt.flu"=list(
      "pcr"=1,
      "not.yet"=2,
      "not.planned"=3,
      "no"=0
    ),
    "analysis.sympt.flu.result"=list(
      "positive" = 1,
      "negative" = 2,
      "not.interpretable"=3,
      "not.yet"=4
    ),
    
    "visit.no.reason"=list(
      "1"="sympt.recently", # My symptoms appeared very recently
      "2"="sympt.mild", # Symptoms are mild
      "3"="sympt.ofen", # I have these symptoms often
      "4"="self.medic", # I think that I know what I have, and I use self-medication 4
      "5"="no.eff.treatment", # I think there is no effective treatment for the disease I have
      "6"="apppointment.delay", # It is too hard to get an appointment quickly
      "7"="no.time", # I do not have time
      "8"="financial", # For financial reasons
      "9"="fear.consequence", #F or fear of consequences if the doctor think I have COVID-19"
      "10"="other", 
      "99"="DNK"
    ),
    "visit.no.reason.fear"=list(
      "1"="diagnostic.test", # Have to do a diagnostic test,
      "2"="isolated", # Have to be isolated",
      "3"="stop.working", # Have to stop working",
      "2"="judged", # Be judged by others",
      "1"="excluded", # Be excluded by others",
      "2"="other" # Another consequence (open field)"
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

# Known platforms
COUNTRY_CODES = c('IE','UK','ES','DK','BE','IT','FR','PT','NL','SE','CH')

# Countries to build ECDC indicator from
ECDC_EXPORT_COUNTRIES = c('UK','DK','IT','FR','CH') 

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
  list(2019, pop=2018, end='2020-08-31'),
  list(2020, pop=2018, start='2020-09-01', end="2021-08-31"),
  list(2021, pop=2018, start='2021-09-01', end="2022-08-31"),
  list(2022, pop=2018, start='2022-09-01', end="2023-08-31"),
  list(2023, pop=2018, start='2023-09-01', end="2024-08-31"),
)

for(season in seasons) {
  y = season[[1]]
  
  dd = list()
  
  dd$start =  if(hasName(season, "start")) season$start else paste0(y, '-10-01')
  dd$end =  if(hasName(season, "end")) season$end else paste0(y + 1, '-04-30')
  
  platform_season_history(y,
        weekly='epidb_results_weekly',
        intake='epidb_results_intake',
        health='epidb_health_status_2012',
        dates=dd,
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
