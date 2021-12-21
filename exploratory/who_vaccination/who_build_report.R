source("conf.R")

library(dplyr)
library(rlang)
library(ggplot2)


init.path('who')

library(rmarkdown)

output_dir = my.path('')

rmarkdown::render("who_vaccination.Rmd", output_dir =output_dir, output_format = "all")

