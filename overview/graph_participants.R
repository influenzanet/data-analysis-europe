source('conf.R')

library(dplyr)

# Load internal libraries

intake = survey_load_results('intake', cols=c("timestamp",'country'))

seasons = get_historical_seasons()

# Find season number for each 
intake$season = NA
for(ss in seasons) {
  h = season.def(ss)
  season = as.integer(ss)
  start = as.POSIXct(h$dates$start)
  if( is.null(h$dates$end) ) {
    end = Sys.time()
  } else {
    end = as.POSIXct(h$dates$end)
  }
  intake$season[ intake$timestamp >= start & intake$timestamp <= end ] = season  
}

weekly = survey_load_results('weekly', cols=c("timestamp",'country'))

# Find season number for each 
weekly$season = NA
for(ss in get_historical_seasons()) {
  h = season.def(ss)
  season = as.integer(ss)
  start = as.POSIXct(h$dates$start)
  if( is.null(h$dates$end) ) {
    end = Sys.time()
  } else {
    end = as.POSIXct(h$dates$end)
  }
  weekly$season[ weekly$timestamp >= start & weekly$timestamp <= end ] = season  
}

weekly$i = 1

# Count participants only once by season in each country
# timestamp becomes the first filled survey's time in each season
intake = intake %>% group_by(person_id, season, country) %>% summarize(timestamp=min(timestamp))

intake$i = 1

# Participants in each country (whatever the season)
part = intake %>% group_by(person_id, country) %>% summarize(nbseasons=n())

graph.open(my.path('registered_participants_by_country'), width=800, height=600)
  tt = table(part$country)
  barplot(tt, beside=T, col="steelblue", border="gray", las=1, args.legend=list(bty="n", horiz=T))
  title(main="Participants with intake survey accross seasons in db", sub=sub.text(), cex.sub=.8)
graph.close()
write.csv2(tt, my.path('registered_participants_by_country.csv'))


graph.open(my.path('registered_participants_by_season+country'), width=800, height=600)
  tt = table(part$nbseasons, part$country)
  n = nrow(tt)
  barplot(tt, col=graph.colors(n), legend.text=rownames(tt), border="gray", las=1, args.legend=list(bty="n", horiz=T))
  title(main="Participants with intake survey & count of participated seasons (intake)", sub=sub.text(), cex.sub=.8)
graph.close()
write.csv2(tt, my.path('registered_participants_by_season+country.csv'))

# What we have in the DB for each seasons
graph.open(my.path('participants_intake_by_season'), width=800, height=600)
  tt = table(intake$season, intake$country)
  n = nrow(tt)
  barplot(tt, beside=T, col=graph.colors(n), legend.text=rownames(tt), border="gray", las=1, args.legend=list(bty="n", horiz=T))
  title(main="Participants with intake survey by country and season", sub=sub.text(), cex.sub=.8)
graph.close()
write.csv2(tt, my.path('registered_participants_by_season+country.csv'))


graph.open(my.path('nb_weekly_by_season'), width=800, height=600)
 tt = table(weekly$season, weekly$country)
 n = nrow(tt)
 barplot(tt, beside=T, col=graph.colors(n), legend.text=rownames(tt), border="gray", las=1, args.legend=list(bty="n", horiz=F))
 title(main="Number of weekly accross countries in each season", sub=sub.text(), cex.sub=.8)
graph.close()
write.csv2(tt, my.path('nb_weekly_by_season.csv'))

# i = number of weekly by participant, by season & by country
part.weekly = aggregate(i ~ person_id + country + season, data=weekly, sum)

graph.open(my.path('participants_weekly_by_season'), width=800, height=600)
  tt = table(part.weekly$season, part.weekly$country)
  n = nrow(tt)
  barplot(tt, beside=T, col=graph.colors(n), legend.text=rownames(tt), border="gray", las=1, args.legend=list(bty="n", horiz=F))
  title(main="Count of participants (>=1 weekly) by country & season", sub=sub.text(), cex.sub=.8)
graph.close()
write.csv2(tt, my.path('participants_weekly_by_season.csv'))

# Number of participating season by country & participant
part.weekly.season = aggregate(list(nbseasons=part.weekly$season), as.list(part.weekly[, c('person_id','country')]), length)

tt = table(part.weekly.season$nbseasons, part.weekly.season$country)
n = nrow(tt)

graph.open(my.path('nb_season_weekly_by_season+country'), width=800, height=600)
  barplot(tt, col=graph.colors(n), legend.text=rownames(tt), border="gray", las=1, args.legend=list(bty="n", horiz=T))
  title(main="Number of participating season (weekly) by country", sub=sub.text(), cex.sub=.8)
graph.close()
write.csv2(tt, my.path('nb_season_weekly_by_season+country.csv'))

graph.open(my.path('prop_nb_season_weekly_by_season+country'), width=800, height=600)
  pp = prop.table(tt, 2)
  n = nrow(tt)
  barplot(pp, col=graph.colors(n), legend.text=rownames(tt), border="gray", las=1, args.legend=list(bty="n", horiz=T), ylim=c(0,1.1), axes = F)
  title(main="Number of participating season by country (on weekly)", sub=sub.text(), cex.sub=.8)
  ticks = seq(0,1, by=.1)
  axis(2, at=ticks, labels=round(100 * ticks), las=1)
graph.close()
write.csv2(tt, my.path('prop_nb_season_weekly_by_season+country.csv'))

##
# Try to find for each countries the date of the season starting & ending
# Finding the week with at least N weekly surveys
##

weekly$yw = iso_yearweek(as.Date(weekly$timestamp))

season.bounds = NULL

for(season in seasons) {
  min.date = as.POSIXct(as.Date(paste0(season,"-08-01")))
  max.date = as.POSIXct(as.Date(paste0(season+ 1,"-07-31")))
  ww = weekly %>% 
    filter(between(timestamp, min.date, max.date) ) %>% 
    group_by(country, yw) %>%
    summarise(count=n()) 
  
  b = ww %>%
    filter(count > 50) %>%
    group_by(country) %>%
    summarise(min.yw=min(yw), max.yw=max(yw))
  
  ww = merge(ww, b, by="country", all.x=T)
  
  ww = ww %>% 
        filter(yw >= min.yw & yw <= max.yw) %>% 
        group_by(country) %>%
        summarize(count=sum(count))
  
  b = merge(b, ww, by="country", all.x=T)
  b$season = season  

  season.bounds = rbind(season.bounds, b)                   
}


season.bounds = season.bounds %>% arrange(country)
write.csv2(season.bounds, file=my.path("season-bounds.csv"))
