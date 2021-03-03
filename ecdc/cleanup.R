# Cleanup generated files and archive last one
source("conf.R")

library(dplyr)

init.path('indicator')

season = 2019
dry_run = TRUE

def = season_definition(season)
max.date = as.Date(def$dates$end)
min.date = as.Date(def$dates$start)

ff = list.files(path=my.path(), pattern = "\\.rds$", recursive = TRUE, ignore.case = TRUE)

ff = data.frame(file=ff, dir=basename(dirname(ff)))

ff$file = as.character(ff$file)

ff = ff %>% filter(grepl("[A-Z]{2}", dir, perl = TRUE)) 

ff$season = gsub("(.*)\\-([0-9]{4})\\-[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}\\.rds$", "\\2", basename(ff$file), perl=TRUE, ignore.case = TRUE)
ff$group = gsub("(.*)\\-([0-9]{4})\\-[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}\\.rds$", "\\1", basename(ff$file), perl=TRUE, ignore.case = TRUE)
ff$season = as.integer(ff$season)

ff$time = file.mtime(my.path(as.character(ff$file)))

# Only consider in the season dates
ff = ff %>% filter(as.Date(time) <= max.date & as.Date(time) >= min.date) 

ff = ff %>% arrange(desc(time)) %>% group_by(dir, group, season) %>% mutate(order=row_number(), last=order==1)

to_delete = ff %>% filter(!last) %>% pull(file)

archive = my.path('../archives')

to_archive = ff %>% filter(last) %>% mutate(name=paste0(dir, "/", basename(file)), to=paste0(dir,"_",name))

delete_file= function(files) {
  for(f in files) {
    cat("Deleting ", f)
    if(dry_run) {
      cat("[fake]")
    } else {
      ok = file.remove(my.path(f))
      if(ok) {
        cat(" deleted")
      } else {
        cat(" !Error during deletion")
      }
    }
    cat("\n")
  }
}

archive_file= function(files, to) {
  if(length(files) != to) {
    stop("files and to must have same lenght")
  }
  for(i in seq_along(files)) {
    file = files[i]
    target = to[i]
    dest = paste0(archive,"/", target) 
    cat("Moving ", file, "to", dest)
    if(dry_run) {
      cat("[fake]")
    } else {
      ok = file.rename(my.path(file), to=dest)
      if(ok) {
        cat(" OK")
      } else {
        cat(" !Error during move")
      }
    }
    cat("\n")
  }
}

if(dry_run) {
  
} else {
  if(!file.exists(archive)) {
    dir.create(archive)
  }
  archive_file(to_archive$name, to_archive$to)
  delete_file(to_delete)
  
}

