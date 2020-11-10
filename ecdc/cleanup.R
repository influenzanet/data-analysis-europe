# Cleanup generated files and archive last one
source("conf.R")

library(dplyr)

init.path('indicator')

season = 2019

def = season_definition(season)
max.date = as.Date(def$dates$end)

ff = list.files(path=my.path(), pattern = "\\.rds$", recursive = TRUE, ignore.case = TRUE)

ff = data.frame(file=ff, dir=basename(dirname(ff)))

ff$file = as.character(ff$file)

ff = ff %>% filter(grepl("[A-Z]{2}", dir, perl = TRUE)) 

ff$season = gsub("(.*)\\-([0-9]{4})\\-[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}\\.rds$", "\\2", basename(ff$file), perl=TRUE, ignore.case = TRUE)
ff$group = gsub("(.*)\\-([0-9]{4})\\-[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}\\.rds$", "\\1", basename(ff$file), perl=TRUE, ignore.case = TRUE)
ff$season = as.integer(ff$season)

ff$time = file.mtime(my.path(as.character(ff$file)))

# Only consider before the season datte
ff = ff %>% filter(as.Date(time) <= max.date) 

ff = ff %>% arrange(desc(time)) %>% group_by(dir, group, season) %>% mutate(order=row_number(), last=order==1)

to_delete = ff %>% filter(!last) %>% pull(file)

archive = my.path('../archives')
if(!file.exists(archive)) {
  dir.create(archive)
}

to_archive = ff %>% filter(last) %>% mutate(name=basename(file), to=paste0(dir,"_",name))
file.rename(my.path(to_archive$file), to=paste0(archive,"/", to_archive$to))

file.remove(my.path(to_delete))
