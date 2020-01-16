source("conf.R")

library(dplyr)
library(rlang)
library(ggplot2)

countries = platform_env("COUNTRY_CODES")
seasons = get_historical_seasons()

init.path('indicator/bundles')

load_bundle = function(country, name) {
  file = my.path(country, '_', name,'.csv')
  if(file.exists(file)) {
    cat("Loading ", country, name, "\n")
    return(read.csv(file))
  }
  NULL
}


inc = NULL
active = NULL

for(country in countries) {
  a = load_bundle(country, "active")
  a$country = as.character(a$country)
  active = bind_rows(active, a)
  ii = load_bundle(country, "incidence")
  ii$country = as.character(ii$country)
  inc = bind_rows(inc, ii)
}

max.active = inc %>% group_by(country, season) %>% summarize(max_active=max(part))
inc = left_join(inc, max.active, by=c("country","season"))
inc = inc %>% mutate(active_limit=.35 * max_active, censored=part < active_limit)

caption = function() {
  paste(Sys.time(), "Influenzanet 2019, for internal purpose only")
}

ii = inc %>% filter(!censored)
ii = ii %>% group_by(country, season) %>% mutate(ymax=max(incidence, na.rm=TRUE))
ii = ii %>% mutate(upper=ifelse(upper > ymax * 2, NA, upper))

saveRDS(data.frame(ii), my.path('inc.rds'))

ggplot(ii, aes(x=monday_of_week(yw), y=incidence, group=syndrome, color=syndrome)) + 
  geom_vline(data=inc %>% filter(censored), aes(xintercept=monday_of_week(yw)), color="grey90") +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="red", color="transparent", alpha=.40) +
  facet_grid(rows=vars(country), cols=vars(season), scales = "free") +
  theme_with("legend_top") +
  labs(x="Week", y="Incidence rate", title="Weekly incidence rate by country and season", caption=caption())
ggsave(my.path("incidence_country+season.pdf"), width=12, height=8)

ii = calc_season_fixed(ii)
ggplot(ii, aes(x=season.index, y=incidence, group=season.year, color=factor(season.year))) + 
  geom_line() +
  facet_grid(rows=vars(country), scales = "free") +
  theme_with("legend_top") +
  labs(x="Season week index (1=Week of last 1st september)", y="Incidence rate", title="Weekly incidence rate by country and season", caption=caption())
ggsave(my.path("incidence_country+season_superpose.pdf"), width=12, height=8)

d = ii %>% 
      group_by(season.index, country) %>% 
      summarize( 
                min=min(incidence, na.rm = TRUE), 
                max=max(incidence, na.rm = TRUE), 
                median=median(incidence, na.rm=TRUE), 
                q1=quantile(incidence, probs=.25, na.rm = TRUE), 
                q3=quantile(incidence, probs=.75, na.rm=TRUE)
      )

labels = c('range'="Min/Max","median"="Median", "current"="Current season", "quantile"="1st, 3rd quantiles")
ggplot(d, aes(x=season.index)) +
  geom_line(aes(y=min, color="range", linetype="range")) +
  geom_line(aes(y=max, color="range", linetype="range")) +
  geom_line(aes(y=median, color="median", linetype="median")) +
  geom_line(aes(y=q1, color="quantile", linetype="quantile")) +
  geom_line(aes(y=q1, color="quantile", linetype="quantile")) +
  geom_line(data=ii[ ii$season == max(seasons), ], aes(y=incidence, color="current", linetype="current")) +
  scale_color_manual(values=c('range'="darkblue","median"="blue", "current"="red", "quantile"="steelblue"), labels=labels)  +
  scale_linetype_manual(values=c('range'="dotted","median"="solid", "current"="solid", "quantile"="dashed"), labels=labels)  +
  facet_grid(rows="country") +
  labs(x="Season week index (1=Week of last 1st september)", y="Incidence rates", caption=caption())
ggsave(my.path("incidence_country+season_distrib.pdf"), width=12, height=8)


d = left_join(active, inc[,c('country','season','yw','incidence','censored') ], by=c('country','season','yw'))
d = d %>%
      group_by(season, country) %>% 
      mutate(
        max_inc=max(incidence, na.rm=TRUE), 
        max_active=max(active),
        inc.r=max_active * incidence / max_inc
      )

ggplot(d, aes(x=monday_of_week(yw), y=active)) + 
  geom_bar(stat="identity", fill="steelblue") +
  geom_line(aes(y= inc.r)) +
  geom_point(data=~filter(., censored),aes(y= inc.r), color="red", size=1) +
  facet_grid(rows=vars(country), cols=vars(season), scales = "free") +
  labs(x="Week", y="Active participant", title="Active participants by country and season (incidence superposed)", caption=caption())
ggsave(my.path("active+inc_country+season.pdf"), width=12, height=8)

ggplot(d, aes(x=monday_of_week(yw), y=active)) + 
  geom_bar(stat="identity", fill="steelblue") +
  facet_grid(rows=vars(country), cols=vars(season), scales = "free") +
  labs(y="Active participants", x="Week", title="Active participants by ", caption=caption())
ggsave(my.path("active_country+season.pdf"), width=12, height=8)


