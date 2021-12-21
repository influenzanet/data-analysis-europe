source('conf.R')

library(dplyr)
library(swMisc)
library(rlang)

## Computation parameters
share.lib('incidence')
share.lib('vaccination')

vaccinated.endpoints = list(
  "FR"="2021-11-15",
  "IT"="2021-09-01",
  "UK"="2021-09-01",
  "CH"="2021-09-01"
)

# Delay to consider as unvaccinated after the last dose (status.*.5m)
delay.5m = 30 * 5

# Get all parameters sets
eu.params.sets = get_eu_incidence_parameters("all")

if(!exists("country") | is.null(country)) {
  rlang::abort("Country not defined")
}

if(!exists("season") | is.null(season)) {
  rlang::abort("Season not defined")
}

countries = platform_env("COUNTRY_CODES")
seasons = get_historical_seasons()

if(!country %in% countries) {
  rlang::abort(paste("Unknown country ", sQuote(country)))
}

if(!season %in% seasons) {
  rlang::abort(paste("Unknown season ", sQuote(season)))
}

init.path('who')

symptoms = get_symptoms_columns(season)

test.covid.columns = c(survey_labels("weekly", "analysis.sympt.covid"), "analysis.sympt.result.pcr" )

# Age group will be computed later
r = load_results_for_incidence(
  season=season, 
  age.categories=c(0, 13, 200), 
  country=country, 
  syndrome.from = ecdc_syndrome_from, 
  first.season=T, 
  columns=list(weekly=c(symptoms, test.covid.columns), keep.all=TRUE),
  onset = episode_onset_design()
)

if( is.null(r) | is.null(r$weekly) | is.null(r$intake) ) {
  rlang::abort("No data", class = "error_no_data")
}

if( nrow(r$weekly) == 0 ) {
  rlang::abort("No weekly data", class = "error_no_data")
}

# Create a country column, as it will be needed when merging intake
r$intake$country = factor(country)

vacc.columns = get_vaccination_columns(country=country, season=season)

has.vacc.survey = is_vacc_survey(vacc.columns)
has.vacc.new = is_vacc_survey_new(vacc.columns)

vacc = survey_load_results("vaccination", vacc.columns, season=season, country=country, debug=F)

if(!has.vacc.survey) {
  vacc.prev = survey_load_results("vaccination", vacc.columns, season=season-1, country=country, debug=F)
  vacc.prev = vacc.prev %>% 
      arrange(timestamp) %>% 
      group_by(person_id) %>% 
      slice_max(timestamp, n=1, with_ties = FALSE)
  
  vacc = bind_rows(vacc.prev, vacc)
}

if( all(is.na(vacc$covid.vaccine)) ) {
  rlang::abort("No covid vaccination data", class = "error_no_data")
}

if(has.vacc.survey) {
  vacc = vacc[!vacc$intake, ]
}

vacc = survey_recode_all(vacc, "vaccination")
weekly = r$weekly
intake = r$intake
syndromes = r$syndromes
rm(r)

rr = survey_recodings("weekly")
for(column in test.covid.columns) {
  if(hasName(rr, column)) {
    weekly[[column]] = survey_recode(weekly[[column]], column, survey="weekly")
  }
}

intake$has.intake = TRUE # Flag to check intake availability

weekly = left_join(weekly, intake[, c('person_id','age.cat','has.intake')], by='person_id')
weekly = weekly %>% filter(is.na(age.cat) | age.cat != "0-12") 

# Vectorized OR : NA if all is NA, OR otherwise over several columns
anyof = function(...) {
  rr = data.frame(...)
  apply(rr, 1, function(x) {
    if(all(is.na(x))) {
      NA
    } else {
      any(x, na.rm=TRUE)
    }
  })
}

weekly = weekly %>% mutate(
  has.pcr = anyof(analysis.sympt.covid.pcr, analysis.sympt.covid.v2.pcr),
#  has.antigenic = anyof(analysis.sympt.covid.antigenic, analysis.sympt.covid.v2.antigenic.nasal),
  pcr.positive = if_else(!is.na(has.pcr) & has.pcr, analysis.sympt.result.pcr == "positive", NA),
#  antigenic.positive = if_else(!is.na(has.antigenic) & has.antigenic, analysis.sympt.covid.antigenic == "positive", NA)
)

vacc = left_join(vacc, intake[, c('person_id','has.intake')], by='person_id')

vacc = vacc %>% 
  filter(has.intake) %>% # Only keep participant with available intake
  arrange(timestamp)
vacc$yw = iso_yearweek(as.Date(trunc(vacc$timestamp)))

vacc = recode_vaccination(vacc, has.vacc.new)

has.doses = !is.na(vacc$covid.vaccine.doses)

if(has.vacc.new) {
  vacc$full.scheme = vacc$covid.vaccine.doses %in% over.two.doses
  
  j = has.doses & vacc$covid.vaccine.dose == "one" & 
        (vacc$covid.vaccine.which.jonhson |
          !is.na(vacc$covid.vacc.one.reason) & vacc$covid.vacc.one.reason %in% c("covid.infection.before","covid.infection.after","covid.jonhson")
        )
  vacc$full.scheme[j] = TRUE

} else {
  
  vacc$full.scheme = vacc$covid.vaccine.doses %in% over.two.doses
  j = has.doses & vacc$covid.vaccine.doses == "one" & vacc$covid.vaccine.which == "jonhson"
  vacc$full.scheme[j] = TRUE
}

# Check vaccination status report.
# Some participant can report to be unvaccinated after having reported to be vaccinated 
vacc.check = vacc %>% 
  filter(!is.na(covid.vaccine) & covid.vaccine != ifnBase::DONTKNOW) %>%
  group_by(person_id) %>%
  mutate(
    status=covid.vaccine == ifnBase::YES
  ) %>%
  summarize(
    vaccinated.time=max(if_else(status, timestamp, as.POSIXct(NA)), na.rm=TRUE),
    unvaccinated.time=max(if_else(!status, timestamp, as.POSIXct(NA)), na.rm=TRUE),
    status=max(status),
    .groups="drop"
  ) %>%
  mutate(
    problem = status & !is.na(unvaccinated.time) & unvaccinated.time > vaccinated.time
  )  
vacc.check = vacc.check %>% filter(problem)

vaccinated.endpoint = vaccinated.endpoints[[country]]
if(is.null(vaccinated.endpoint)) {
  rlang::abort("Vaccination endpoint not defined for country")
}

vaccinated.endpoint = as.Date(vaccinated.endpoint)

if(is.na(vaccinated.endpoint)) {
  rlang::abort("Vaccination endpoint is NA for country")
}

rm(j, has.doses)

weekly$yw = iso_yearweek(weekly$onset)

pw = weekly %>% group_by(yw, person_id) %>% summarize(n=1)

vacc = arrange(vacc, vacc.time)
cols = c('full.scheme', 'vacc.time')

## Compute vaccination status with last known status at each time step
message("Computing vaccination statuses")
pw = pw %>% 
  group_by(yw, person_id) %>% 
  group_modify(function(rows, .k) {
    pid = .k$person_id
    monday = monday_of_week(.k$yw)
    v = tail(vacc[ vacc$person_id ==  pid & vacc$vacc.time <= monday, cols], n=1) # Last known status 
    if(nrow(v) == 0) {
      return(rows)
    }
    data.frame(rows, v) # Copy state for all rows of the week
    }) %>% 
  ungroup()


pw = pw %>% 
  rename(status.timely=full.scheme) %>%
  select(-n)
 
last.state = vacc %>% 
  group_by(person_id) %>% 
    select(vacc.time, full.scheme) %>% 
    slice_max(vacc.time, n=1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(status.last=full.scheme)

# Cohort approach 
vacc$cohort = NA
vacc$cohort[ vacc$date <= vaccinated.endpoint & vacc$full.scheme ] = 1 # 
vacc$cohort[ !is.na(vacc$covid.vaccine) & vacc$covid.vaccine == ifnBase::NO ] = 0 # Consider Non vaccinated until now

cohort = vacc %>% 
  filter(!is.na(cohort)) %>%
  arrange(desc(timestamp)) %>%
  group_by(person_id) %>%
    summarize(
      status=as.logical(max(cohort)),
      vaccinated.time=max(if_else(cohort > 0, timestamp, as.POSIXct(NA)), na.rm=TRUE),
      unvaccinated.time=max(if_else(cohort == 0, timestamp, as.POSIXct(NA)), na.rm=TRUE),
      .groups="drop"
    ) %>%
  mutate(
    problem = status & !is.na(unvaccinated.time) & unvaccinated.time > vaccinated.time
  )

table(cohort$problem)
vacc[ vacc$person_id %in% cohort$person_id[cohort$problem],] %>% 
    arrange(person_id, timestamp) %>% 
    group_by(person_id) %>% 
    select(!!!vacc.columns, full.scheme) %>%
    group_map(function(d, .k) d)

cohort = cohort %>% filter(!problem) # Cannot keep inconsistent entries

cohort = rename(cohort, status.cohort=status)

syndromes = c('covid.ecdc','ili.ecdc', 'pcr.positive')
vacc.states = c("status.last","status.timely","status.timely.5m","status.last.5m", 'status.cohort')

weekly = left_join(weekly, pw, by=c("person_id", "yw"))
weekly = left_join(weekly, last.state, by="person_id", suffix=c("", ".last_state"))
weekly = left_join(weekly, cohort[, c('person_id','status.cohort')], by="person_id")
weekly = weekly %>% 
  mutate(
    status.timely.5m=status.timely & difftime(date, vacc.time,"days") < delay.5m,
    status.last.5m=status.last & difftime(date, vacc.time.last_state,"days") < delay.5m
  )

weekly[weekly$date < as.Date("2021-01-01"), vacc.states] = FALSE

## Now compute weekly incidence

# Accept only 1 syndrome occurrence & state by week for each participant

# Any only if at least one value set, NA otherwise
any_na = function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  sum(x, na.rm=T) > 0
}

ww = weekly %>% 
  group_by(person_id, yw) %>% 
  summarise_at(c(syndromes, vacc.states), any_na) %>% 
  ungroup()

inc = NULL
ww$active = TRUE
ww$active.pcr.positive = !is.na(ww$pcr.positive)

active.colummns = c('active','active.pcr.positive')

for(vacc.state in vacc.states) {
  w = ww %>% 
        group_by(yw, !!sym(vacc.state)) %>% 
        summarize_at(c(syndromes, active.colummns), ~sum(., na.rm=TRUE))
  w = rename(w, status = !!vacc.state)
  w$status.type = vacc.state
  w = tidyr::pivot_longer(w, all_of(syndromes), names_to = "syndrome", values_to = "count")
  inc = bind_rows(inc, w)
}

library(ggplot2)
ss = pw %>% 
  group_by(yw, status.timely) %>% 
  summarize(count=n())

ggplot(ss, aes(x=monday_of_week(yw), y=count, color=status.timely, group=status.timely)) + geom_line()

ggplot(inc, aes(x=monday_of_week(yw), color=status)) + 
    geom_line(aes(y=count)) + 
    geom_line(aes(y=active), linetype="dashed") +
    facet_grid(rows=vars(syndrome), cols=vars(status.type))

ggplot(inc, aes(x=monday_of_week(yw), color=status)) + 
  geom_line(aes(y=1000*count/active)) + 
  facet_grid(rows=vars(syndrome), cols=vars(status.type)) +
  labs(y="Incidence rate / 1000 participants")

ss = cohort %>%
      mutate(
        time=if_else(status.cohort, vaccinated.time, unvaccinated.time),
        date=as.Date(trunc(time, "days"))
      ) %>%
      group_by(date, status.cohort) %>%
      summarise(
        count=n(),
        .groups = "drop"
      ) %>%
      arrange(date) %>%
      group_by(status.cohort) %>%
      mutate(cum_count=cumsum(count)) %>%
      ungroup()
    
ggplot(ss, aes(x=date, y=cum_count, color=status.cohort)) + geom_line()

data =list(
  inc=inc, 
  time=Sys.time(), 
  syndromes=syndromes, 
  season=season, 
  country=country, 
  part.states=pw,
  cohort=cohort,
  vacc.check=vacc.check
)

saveRDS(data, my.path("who_vacc_",country,"_",season,".rds"))

