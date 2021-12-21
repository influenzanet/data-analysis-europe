source('conf.R')

library(dplyr)
library(dplyr)

collect = new.env()

for(season in c(2021)) {
  for(country in c('IT','FR','UK', 'CH')) {
    
    local({
      source("who_vaccination.R", local = TRUE)
      inc$country = country
      inc$season = season
      collect$inc = bind_rows(collect$inc, inc) 
      
      cohort$country = country
      cohort$season = season
      collect$cohort = bind_rows(collect$cohort, cohort)
    })
    
  }
}

init.path("who")
file = my.path("who_vacc.rds")
cat("Saving data into ", file, "\n")
saveRDS(list(data=collect, time=Sys.time()), file=file)

write.csv(collect$inc, file=my.path("who_vaccination_incidence.csv"), row.names = FALSE)
