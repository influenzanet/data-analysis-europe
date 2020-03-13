# Create population bundles
source("conf.R")

library(dplyr)

file.prefix = '2012-2018'

out.path = function(...) {
  my.path(paste0("population/",file.prefix, "/", ...))
}

files = list.files(out.path(), pattern="\\.csv$", full.names = TRUE)

data = list()
for(file in files) {
  cat("Loading", basename(file),"\n")
  year = as.integer(gsub("(\\d+)_pop_(\\w+)\\.csv$","\\1", basename(file)))
  geo =  gsub("(\\d+)_pop_(\\w+)\\.csv$","\\2", basename(file))
  d = read.csv2(file)
  d$year = year 
  data[[geo]] = bind_rows(data[[geo]], d)
}

dir.create(out.path('bundles'))

lapply(names(data), function(geo) {
  d = data[[geo]]
  write.csv2(d, file=out.path('bundles/',geo, '.csv'), row.names = F)
})
