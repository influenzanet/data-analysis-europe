source("conf.R")
options(warn=1) # Show warnings as they occur

library(dplyr)
library(rlang)
library(swMisc)
library(magrittr)

share.lib("incidence") # Default method name fro syndrome

done.marker =".done"

find_last_file = function(path, pattern) {
  ff = list.files(path, pattern, recursive = FALSE, full.names = FALSE)
  r = !file.exists(paste0(path, ff, done.marker)) # Exclude file with a .done file
  ff = ff[r]
  ff = sort(ff, decreasing = TRUE)
  ff[1]
}

# List of breaks to compute season number for each week
seasons = sapply(get_historical_seasons(), function(season) {
    iso_yearweek(as.Date(as.character(get_season_dates(season)$start)))
}, USE.NAMES = TRUE, simplify = TRUE)

find_season = function(yw) {
  as.integer(as.character(cut(yw, breaks=c(seasons, Inf), labels=names(seasons), include.lowest = TRUE, right=FALSE)))
}

datasets = new_environment()

## Load NL data
init.path('externals/rivm')

file = find_last_file(path=my.path(), pattern='NL_active_.*\\.csv')

nl.active = read.csv2(my.path(file))
nl.active = nl.active %>% select(-X)
nl.active$country = "NL"
nl.active$syndrome = "active"
nl.active$season = find_season(nl.active$yw)
if(!all(table(nl.active[, c('yw','season','method')]) <= 1)) {
  warning("Problem with keys for NL.active")
}

datasets$active = bind_rows(datasets$active, nl.active)

file = find_last_file(path=my.path(), pattern='NL_incidence_.*\\.csv')

nl.inc = read.csv2(my.path(file))
nl.inc[["X"]] = NULL
nl.inc[["part"]] = NULL
nl.inc$country = "NL"

nl.inc = left_join(nl.inc, nl.active[, c('yw','season','method','active')], by=c('yw','season','method'))
if(!all(table(nl.inc[, c('yw','season','method','syndrome','type')])<= 1)) {
  warning("Problem with keys for NL.incidence")
}
nl.inc$season = find_season(nl.inc$yw)
nl.inc = rename(nl.inc, part=active)

datasets$incidence = bind_rows(datasets$incidence, nl.inc)

## Load DE data
init.path('externals/rki')
file = find_last_file(path=my.path(), pattern="ForInfluenzanet_Germany_GrippeWeb_")

d = read.csv2(my.path(file))
names(d) <- tolower(names(d))
d = rename(d, active=numberparticipants)
syndromes = names(d)
syndromes = syndromes[ !syndromes %in% c('year','week', 'active')] # List of columns containing syndromes

d$yw = d$year * 100 + d$week 
d$season = find_season(d$yw)

de.inc = NULL

for(column in syndromes) {
  syndrome = switch(column, "ili"="ili.ecdc", rlang::abort(paste0("Unknown syndrome", sQuote(column))))
  
  inc = d[, c('yw','season', 'active', column)]
  inc = rename(inc, part=active, incidence=all_of(column))
  inc$syndrome = syndrome
  inc$method = "unknown"
  de.inc = bind_rows(de.inc, inc)

}

de.inc$country = "DE"
de.inc$type = "adj"

de.active = de.inc %>% group_by(season, yw, method, country) %>% summarize(active=max(part))
de.active$syndrome ="active"

datasets$active = bind_rows(datasets$active, de.active)
datasets$incidence = bind_rows(datasets$incidence, de.inc)

init.path('externals/dkcovid')

ff = list.files(my.path(), "Denmark_week_.*\\.csv$")
inc = NULL
for(file in ff) {
   r = read.csv2(my.path(file), header = TRUE)
   r = rename(r, week="Week.Number", active="Active.participants", "inc1000"="Incidence")
   r$X = NULL
   r$yw = gsub("(\\d+)-W(\\d+)", "\\1\\2", r$week)
   r$time = as.numeric(gsub(".*_(\\d+)\\.csv$","\\1", file))
   r$incidence = as.numeric(as.character(r$inc1000)) / 1000 
   r$count = r$incidence * r$active
   r = r %>% select(yw, incidence, count, active, time)
   inc = bind_rows(inc, r)
}

inc$country = "DKC"
inc$syndrome = "covid"
inc$yw = as.integer(inc$yw)
inc$season = find_season(inc$yw)
inc$type = "raw"
inc$method = "unknown"
inc = inc %>% select(-time)

active = inc %>% select(yw, season, method, active, country)
active$syndrome ="active"

datasets$active = bind_rows(datasets$active, active)
datasets$incidence = bind_rows(datasets$incidence, inc)

init.path('indicator')

datasets$meta = list(time = Sys.time(), title="External data")
saveRDS(datasets, my.path('externals.rds'))

