## Ecdc runner
## Catch cli arguments & handle errors
source("conf.R")

library(swMisc)

if(!exists("cli.args")) {
  cli.args = parseArgs(list(
    'season'=list(type="int", default=get_current_season()),
    'country'=list(type="choices", choices=platform_env("COUNTRY_CODES"))
  ))
}

country = cli.args$country
season = cli.args$season

env = new.env()
r = try(source("ecdc_indicator.R", local=env))

if(is.error(r)) {
  save(env, file=my.path("error_indicator_", country, season,".RData"))
  dump(r, file=my.path("error_indicator_", country, season,".R"))
  quit("no", status = 1)
}