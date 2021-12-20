# Vaccination utilities`

monthyear_to_date = function(y, min, max) {
  na = is.na(y)
  d = if_else(na, as.Date(NA), as.Date(paste0(y,"-01")))
  d[ !na & d < min] = NA
  d[ !na & d > max] = NA
  d
}

str_to_date = function(y, min, max) {
  na = is.na(y)
  d = if_else(na, as.Date(NA), as.Date(as.character(y)))
  d[ !na & d < min] = NA
  d[ !na & d > max] = NA
  d
}

doses.over.3 = c("three","more_3","more_2")
doses.over.2 = c(doses.over.3, "two")
doses.over.1 = c(doses.over.2,"one")
over.two.doses = doses.over.2

#' Get labels for a variable of the vaccination survey
#' @param name name of the labels set
#' @param keep_from data.frame, if not null only keeps labels available as name in this data.frame
vacc_labels = function(name, keep_from=NULL) {
  labs =survey_labels("vaccination", name)
  if(!is.null(keep_from)) {
    labs = labs[labs %in% names(keep_from)]
  }
  labs
}

#' Get vaccination available columsn for a given country and season
#' @param country country code
#' @param season season number
#' @param reasons if TRUE includes reasons columns
get_vaccination_columns = function(country, season, reasons=FALSE) {
  has.vacc.survey = country %in% c('FR') & season > 2020
  has.vacc.new = country %in% c('FR') & season > 2020

  vacc.columns = c(
    'intake',
    'timestamp',
    'covid.vaccine',
    'covid.vaccine.doses',
    if(!has.vacc.new) c(  
      'covid.vaccine.which', # Vaccine brand for 1st dose
      "covid.vaccine.when1",
      "covid.vaccine.when1.date",
      "covid.vaccine.when2",
      "covid.vaccine.when2.date",
      "covid.vaccine.which2"
    ),
    if(has.vacc.new) c(
      vacc_labels("covid.vaccine.which"),
      'covid.vaccine.last.when',
      'covid.vaccine.last.date',
      'covid.vacc.one.reason',
      if(reasons) c("covid.vaccine.second.plan", vacc_labels("covid.nvac.reason"))
    ),
    if(reasons) vacc_labels("covid.vacc.reason")
  )
  
  attr(vacc.columns, "vacc.survey") <- has.vacc.survey
  attr(vacc.columns, "vacc.survey.new") <- has.vacc.new
  vacc.columns
}

#' Is survey use vaccination (TRUE) or intake survey (FALSE)
#' @param x object returned by get_vaccination_columns()
is_vacc_survey <- function(x) {
  attr(x, "vacc.survey") 
}

#' Is survey use new vaccination survey
#' @param x object returned by get_vaccination_columns()
is_vacc_survey_new <- function(x) {
  attr(x, "vacc.survey.new")
}

#' Recodes vaccination variables
#' @param vacc raw survey data for vaccination survey
#' @param is_new is new vaccination form in use
recode_vaccination <- function(vacc, is_new=FALSE) {
  
  vacc$date = as.Date(trunc(vacc$timestamp, "days"))
  vacc$vacc.time = vacc$date
  i = vacc$covid.vaccine == ifnBase::YES
  if(is_new) {
    vacc$covid.last.date = monthyear_to_date(vacc$covid.vaccine.last.date, min=as.Date("2021-01-01"), max=vacc$date)
    j = i & !is.na(vacc$covid.last.date)
    vacc$vacc.time[j] = vacc$covid.last.date[j]
    
    vacc$full.scheme = vacc$covid.vaccine.doses %in% over.two.doses
    
  } else {
    vacc$covid.dose1.date = str_to_date(vacc$covid.vaccine.when1.date, min=as.Date("2021-01-01"), max=vacc$date)
    vacc$covid.dose2.date = str_to_date(vacc$covid.vaccine.when2.date, min=as.Date("2021-01-01"), max=vacc$date)
    
    # First dose
    j = i & !is.na(vacc$covid.vaccine.doses) & vacc$covid.vaccine.doses == "one" & !is.na(vacc$covid.dose1.date)
    vacc$vacc.time[j] = vacc$covid.dose1.date[j]
    # Second dose or more
    j = i & !is.na(vacc$covid.vaccine.doses) & vacc$covid.vaccine.doses %in% over.two.doses & !is.na(vacc$covid.dose2.date)
    vacc$vacc.time[j] = vacc$covid.dose2.date[j]
    
  }
  
  vacc 
}

vacc_labeller = function(name) {
  survey_labeller("vaccination", name)
}
