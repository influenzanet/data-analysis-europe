source("conf.R")

library(dplyr)
library(rlang)
library(ggplot2)

countries = platform_env("COUNTRY_CODES")

init.path('indicator')

bundles = readRDS(my.path('bundles.rds'))

# Local version
g_labs = ifn_labs

library(rmarkdown)

current_week = iso_yearweek(Sys.Date())
last_week = iso_yearweek(Sys.Date() - 7)

output_dir = my.path('bundles')

outputs = bundles
rmarkdown::render("ecdc_report.Rmd", output_dir =output_dir, output_format = "all")

