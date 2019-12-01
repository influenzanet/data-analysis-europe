source('conf.R')
library(dplyr)
library(swMisc)
library(rlang)

if(!exists("cli.args")) {
 cli.args = parseArgs(list(
    'season'=list(type="int", default=get_current_season()),
    'country'=list(type="choices", choices=platform_env("COUNTRY_CODES"))
  ))
}

country = cli.args$country
season = cli.args$season

age.categories = c(0, 20, 65, 200)

init.path(paste0('indicator/', country))

#' Create output file
#' @param .data data to output
#' @param name data set name
output = function(.data, name) {
  write.csv2(.data, file=my.path(paste0(name, '_', season, '.csv')), row.names = FALSE)
}

provider = SyndromeProviderRS2019$new()

r = load_results_for_incidence(season=season, age.categories=age.categories, country=country, syndrome.from = list(provider=provider$compute, health.status=FALSE), first.season=T, columns=list(keep.all=TRUE))

if( is.null(r) | is.null(r$weekly) | is.null(r$intake) ) {
  rlang::abort("No data",class = "error_no_data")
}
if( nrow(r$weekly) == 0 ) {
  rlang::abort("No weekly data",class = "error_no_data")
}

params = list(active.week.before=1, active.week.after=1, active.min.surveys=2, exclude.same=T,ignore.first.delay=6, ignore.first.only.new=T)

estimator = IncidenceRS2014$new(weekly=r$weekly, intake=r$intake, params=params, syndromes = r$syndromes, design=NULL, output="inc")

results = estimator$compute(weeks = unique(r$weekly$yw), verticalize = TRUE)

ii = results$inc %>%
  filter(syndrome == "ari.ecdc" & type %in% c("crude","count")) 
 
count = ii %>%
  filter(type =="count") %>%
  select(-upper, -lower, -type) %>%
  rename(count=value)
inc = ii %>%
  filter(type == "crude") %>%
  select(-type) %>%
  rename(incidence=value)
inc = merge(inc, count, by=c('syndrome','yw'), all=TRUE)

pp = results$inc %>%
  filter(syndrome == "active" & type == "count") %>%
  select(-c(type, syndrome, upper, lower)) %>%
  rename(part=value)

inc = left_join(inc, pp, by=c('yw'))

inc$country = country
inc$season = as.integer(season)

active = results$inc %>%
  filter(syndrome == "active" & type == "count") %>%
  select(-c(type, syndrome, upper, lower)) %>%
  rename(active=value)

active$active = as.integer(active$active)
active$season = as.integer(season)
active$country = country

output(inc, 'incidence')
output(active, 'active')
