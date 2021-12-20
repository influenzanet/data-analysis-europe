# Explore Vaccination data (Vaccination survey & intake)
# This script build by country results (output in /country/[country]/[season]/vaccination) and global results (put in /overview/[season])
# Runnable as cli (see repo README) Rscript vaccination.R season=[season]
source("conf.R")

library(ggplot2)
library(dplyr)
library(swMisc)

share.lib('vaccination')

if(!exists("cli.args")) {
  cli.args = parseArgs(list(
    season=list(type="int", min=2011, max=calc_season(Sys.Date()), default=get_current_season() )
  ))
}

#' Frequency for multiple columns
freq_bool = function(data, columns) {
  columns = select_columns(data, columns)
  dd = bind_rows(lapply(columns, function(column) {
    name = sym(column)
    data %>% 
      summarise(
        total=n(), 
        missing=sum(is.na(!!name)),
        count=sum(!!name)
      ) %>% 
      mutate(total_value=total-missing, var=!!column) %>% 
      ungroup()    
  }))
  dd
}

# Frequency plot for multiple choice
plot_freq_bool <- function(d, var_labels=i18n) {
  g =ggplot(d, aes(x=var, y=100 * count/total_value)) + geom_bar(stat="identity", fill="steelblue") +
    geom_text(aes(x=var, label=count), nudge_y = 1, size=3) +
    theme_with("x_vertical") +
    labs(y="%") +
    scale_x_discrete(labels=var_labels)
  g
}  

#' Compute weekly count & cumulated by week & age.group
#' @param doses data.frame() shot event for all vaccinated user (timestamp, monday, age.cat, [column])
#' @param column column with dose level for each event
#' @param pop population data (age.cat, pop_intake)
compute_dose_weekly = function(doses, column, pop) {
  
  symcol = sym(column)
  
  # Only keep the first reported dose level for each user
  dd = doses %>%
    filter(!is.na(!!symcol)) %>%
    group_by(person_id, !!symcol) %>%
    slice_min(timestamp, n=1, with_ties=FALSE) %>%
    ungroup()

  dd = dd  %>%
    group_by(monday, age.cat, !!symcol) %>%
    count(name = "count") %>% 
    ungroup()
  
  dd = dd %>% 
    arrange(monday) %>% 
    group_by(!!symcol, age.cat) %>% 
    mutate(cum_count=cumsum(count))
  
  dd = left_join(dd, pop, by="age.cat")

  total = dd %>% 
    group_by(!!symcol, monday) %>% 
    summarize(count=sum(count), .groups="drop") %>%
    arrange(monday) %>% # reorder because join can change order
    group_by(!!symcol) %>%
    mutate(
      cum_count=cumsum(count),
      age.cat="all_age"
    ) %>% ungroup()
  
  pt = pop %>% 
        summarize(pop_intake=sum(pop_intake)) %>% 
        mutate(age.cat="all_age")
  total = left_join(total, pt, by="age.cat")

  dd = bind_rows(dd, total)
  
  dd = dd %>% mutate(
    prop=count / pop_intake,
    cum_prop=cum_count / pop_intake
  )
  
  dd = dd %>% 
        arrange(monday) %>% 
        ungroup() 
        
  dd
  
}

season = cli.args$season

countries = platform_env("COUNTRY_CODES")

message(paste("Running season ", season))

theme_set(theme_minimal())

condition.columns = survey_labels('intake', 'condition')
intake.columns = c('timestamp', condition.columns, "date.birth")

all_data = rlang::new_environment()

context = ResultContext$new() # Context for results files

context$set(season=season)

#' Save helper with default parameters
g_save=function(..., width, height, desc=NULL) {
  save_graph_with_context(paste0(...), formats=c('pdf','svg'), width=width, height=height, context=context, desc=desc)
}

countries = c('UK','FR','IT','CH')

add_path_prefix("project", "country")

for(country in countries) {
  
  init.path(file.path(country, season, "vaccination"))

  context$push()
  
  context$set(country=country)
  
  collect = list()
  
  vacc.columns = get_vaccination_columns(country=country, season=season, reasons=TRUE)
  
  has.vacc.survey = is_vacc_survey(vacc.columns)
  has.vacc.new = is_vacc_survey_new(vacc.columns)
  
  intake = survey_load_results("intake", intake.columns, season=season, country=country, debug=F)
  
  intake = recode_intake(intake)
  
  intake$age.cat = cut_age(intake$age, c(0, 19, 65, 200))
  
  intake = keep_last_survey(intake)
  
  intake$has.intake = TRUE
  
  vacc = survey_load_results("vaccination", vacc.columns, season=season, country=country, debug=F)
  
  if( all(is.na(vacc$covid.vaccine)) ) {
    cat("No covid vaccination data for ", country)
    next()
  }

  vacc = survey_recode_all(vacc, "vaccination")
  vacc = recode_vaccination(vacc, is_new=has.vacc.new)
  
  vacc = left_join(vacc, intake[, c('person_id','age.cat','has.intake')], by="person_id")
  
  vacc = vacc %>% filter(has.intake) # Remove participant without intake

  vacc = mutate(vacc, monday=monday_of_date(date))

  vacc.covid.reasons = vacc_labeller("covid.vacc.reason")
  nvacc.covid.reasons = vacc_labeller("covid.nvac.reason")
  
  vaccinated = !is.na(vacc$covid.vaccine) & vacc$covid.vaccine == ifnBase::YES
  
  d = freq_bool(vacc[vaccinated, ], vacc.covid.reasons$list())
  if( nrow(d) > 0) {
    plot_freq_bool(d, var_labels = vacc.covid.reasons$unprefix()) + 
      labs(
        x="Covid vaccination reasons (by report)",
        subtitle=paste(country)
      )
    g_save("covid_vacc_reason", width=12, height=8)
  
    collect$vacc.covid.reasons = d
  }
  
  if(has.vacc.new) {
    d = freq_bool(vacc[!vaccinated, ], nvacc.covid.reasons$list())
    plot_freq_bool(d, var_labels = nvacc.covid.reasons$unprefix()) + 
      labs(
        x="Covid non vaccination reason (by report)",
      title="Covid non vaccination reason",
      subtitle=paste(country)  
    )
    g_save("covid_vacc_reason", width=12, height=8)
    
    collect$nvacc.covid.reasons = d
  }
  
  pop = intake %>% group_by(age.cat) %>% summarize(pop_intake=n(), .groups="drop")
  
  ## Vaccination coverage
  
  dd = vacc %>% 
    select(person_id, covid.vaccine, timestamp, age.cat, monday) %>% 
    arrange(timestamp) %>%
    group_by(person_id, covid.vaccine) %>% 
    slice_min(timestamp, n=1, with_ties=FALSE) # Keep only first report for a vaccine status
  
  dd = compute_dose_weekly(dd, "covid.vaccine", pop)
  
  ggplot(dd, aes(x=monday, y=cum_count, color=covid.vaccine)) + 
    geom_line() +
    labs(
      y="Cumulated count (participant)",
      x="Week",
      title="Vaccination report covid (first report/user), all doses", 
      subtitle=paste(country, ", Cumulated count by week, first report")
    ) + 
    facet_grid(rows=vars(age.cat))
  g_save("covid_vaccine_count", width=12, height=8)
  
  ggplot(dd, aes(x=monday, y=100 * cum_prop, color=covid.vaccine)) + 
    geom_line() +
    labs(
      y="Cumulated count (participant)",
      x="Week",
      title="Vaccination report against covid (first report/user)", 
      subtitle=paste(country, ", Cumulated count by week, first report")
    ) + 
    facet_grid(rows=vars(age.cat))
  g_save("covid_vaccine_prop", width=12, height=8)
  
  dd = mutate_at(dd, vars(age.cat, covid.vaccine), as.factor)
  
  collect$covid.vaccine = dd
  
  # Last report for a dose count for each user
  doses = vacc %>% 
      filter(vaccinated) %>%
      group_by(person_id, covid.vaccine.doses) %>%
      slice_max(timestamp, n=1, with_ties=FALSE) # No ties, several survey can have same timestamp 

  dd = doses %>% 
        group_by(monday, covid.vaccine.doses) %>%
        summarise(count=n()) %>%
        ungroup() %>%
        arrange(monday) %>%
        group_by(covid.vaccine.doses) %>%
        mutate(cum_count=cumsum(count)) %>%
        ungroup()
  
  ggplot(dd, aes(x=monday, y=cum_count, color=covid.vaccine.doses)) + 
    geom_line() +
    labs(
      title="Vaccination report aginst Covid19 / doses count & user", 
      subtitle=paste(country, "Cumulated count by week, last report for each user")
    )
  g_save(country,"_covid_vacc_doses_cumulated", width=10, height=8)
  
  collect$covac.doses.week = dd
  
  doses.levels = c('dose_1','dose_2','dose_3')
  dose.levels.labels = c('dose_1'="1 dose", "dose_2"="2 doses","dose_3"="> 2 doses")
  
  doses = vacc[vaccinated, ] %>% 
    select(timestamp, covid.vaccine.doses, date, person_id, age.cat) %>%
    arrange(timestamp) %>% 
    mutate( 
      dose=case_when(
        is.na(covid.vaccine.doses) ~ as.character(NA),
        covid.vaccine.doses == 'one' ~ 'dose_1',
        covid.vaccine.doses == 'two' ~ 'dose_2',
        covid.vaccine.doses %in% c('more_2','more_3','three') ~ 'dose_3',
        TRUE ~ as.character(NA)
      ),
      any_dose = if_else(!is.na(covid.vaccine.doses) & covid.vaccine.doses != "DNK", "any_dose", as.character(NA)),
      monday=monday_of_date(date)
  )
  
  dd = compute_dose_weekly(doses, "dose", pop=pop)
  
  any_dose = compute_dose_weekly(doses, "any_dose", pop=pop)
  any_dose = rename(any_dose, dose=any_dose)
  
  dd = bind_rows(dd, any_dose)
  
  dd = dd %>% filter(!is.na(dose))
  dd$age.cat[is.na(dd$age.cat)] <- 'age_unknown'
  
  dd = mutate_at(dd, vars(age.cat, dose), as.factor)
  
  ggplot(dd, aes(x=monday, y=cum_count, color=dose, group=dose)) + 
    geom_line() +
    facet_grid(rows=vars(age.cat)) +
    labs(
      x="Week (monday)",
      title="Last Vaccination against Covid19 report by dose, user", 
      subtitle=paste0(country,  ", by date of report, Cumulated count by week"),
      y="Participant count"
    ) + 
    scale_color_discrete(labels=dose.levels.labels)
  g_save(country,"_covid_vacc_dose_level_count", width=10, height=8)
  
  ggplot(dd, aes(x=monday, y=100 * cum_prop, color=dose, group=dose)) + 
    geom_line() +
    facet_grid(rows=vars(age.cat)) +
    labs(
      x="Week (monday)",
      title="Last Vaccination against Covid19 report by dose, user", 
      subtitle=paste0(country,  ", by date of report, Cumulated count by week"),
      y="%"
    ) + 
    scale_color_discrete(labels=dose.levels.labels)
  g_save(country,"_covid_vacc_dose_level_prop", width=10, height=8)
  
  collect$covac.doses.levels = dd
  
  mapply(function(data, name) {
    data$country = country
    data$season = season
    all_data[[name]] = bind_rows(all_data[[name]], data)
  }, collect, names(collect))

  context$pop()
}

add_path_prefix("project", project.prefix) # Get back to current project output path

init.path(paste0(season, '/vaccination'))

dd = all_data$covid.vaccine

ggplot(dd, aes(x=monday, y=cum_count, color=covid.vaccine)) + 
  geom_line() +
  labs(
    y="Cumulated count (participant)",
    x="Week",
    title="Vaccinated against covid, all doses", 
    subtitle=paste("Cumulated count by week, first report/user")
  ) + 
  facet_grid(rows=vars(country), cols=vars(age.cat), scales="free_y")
g_save("covid_vaccine_count", width=12, height=8)

ggplot(dd, aes(x=monday, y=cum_prop, color=covid.vaccine)) + 
  geom_line() +
  labs(
    y="Cumulated count (participant)",
    x="Week",
    title="Vaccinated against covid (first report/user)", 
    subtitle= ", Cumulated count by week, first report"
  ) + 
  facet_grid(rows=vars(country), cols=vars(age.cat))
g_save("covid_vaccine_prop", width=12, height=8)

d = all_data$covac.doses.levels
ggplot(d, aes(x=monday, y=100 *cum_prop, color=dose, group=dose)) + 
  geom_line() +
  facet_grid(rows=vars(age.cat), cols=vars(country)) +
  labs(
    title="First report for each doses level by user", 
    subtitle="Cumulated % of the registred participant by week",
    y="% of partitipant of the age group"
  ) + 
  scale_color_discrete(labels=dose.levels.labels)
g_save("covid_vacc_dose_level_prop", width=10, height=8)

