source("conf.R")

library(dplyr)
library(rlang)
library(ggplot2)

countries = platform_env("COUNTRY_CODES")
seasons = get_historical_seasons()

init.path('indicator')

bundles = readRDS(my.path('bundles.rds'))

# Local version
g_labs = ifn_labs

caption = ifn.copyright

active = bundles$active
inc = bundles$incidence

max.active = inc %>% group_by(country, season) %>% summarize(max_active=max(part))
inc = left_join(inc, max.active, by=c("country","season"))
inc = inc %>% mutate(active_limit=.35 * max_active, censored=part < active_limit)

inc.censored = inc %>% filter(!censored)
inc.censored = inc.censored %>% group_by(country, season) %>% mutate(ymax=max(incidence, na.rm=TRUE))
inc.censored = inc.censored %>% mutate(upper=ifelse(upper > ymax * 2, NA, upper))

methods = unique(inc$method)
syndromes = unique(inc$syndrome)

context = ResultContext$new()

g_save = function(..., width, height, desc=NULL) {
  path = my.path(..., ".pdf")
  desc = context$resolve()
  desc_output(path, desc = desc)
  ggsave(path, width=width, height=height)
}

init.path('indicator/bundles')

result_desc_filters(
  auto=TRUE, 
  filters=list(
    result_filter("syndrome","Syndrome"),
    result_filter("what","Subject")
  )
)


for(syndrome in syndromes) {
  
  context$push()
  
  context$set(syndrome=syndrome)
  
  ii = inc.censored %>% filter(syndrome == !!syndrome & type == "adj")

  method = unique(ii$method)
  if(length(method) > 1) {
    rlang::abort("Several methods are provided for an indicator : problem")
  }
  
  subtitle = paste("Incidence parameters", method)
 
  context$set("what"="incidence")
  
  ggplot(ii, aes(x=monday_of_week(yw), y=incidence, group=syndrome, color=syndrome)) + 
    geom_vline(data=inc %>% filter(censored), aes(xintercept=monday_of_week(yw)), color="grey90") +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper, fill=syndrome), color="transparent", alpha=.40) +
    facet_grid(rows=vars(country), cols=vars(season), scales = "free") +
    theme_with("legend_top") +
    g_labs(x="Week", y="Incidence rate", title="Weekly incidence rate by country and season", subtitle=subtitle)
  g_save(syndrome,"_incidence_country+season", width=12, height=8)

  ii = calc_season_fixed(ii)
  ggplot(ii, aes(x=season.index, y=incidence, group=season.year, color=factor(season.year))) + 
    geom_line() +
    geom_line(data=ii[ ii$season == max(seasons), ], size=1.2) +
    facet_grid(rows=vars(country), cols=vars(syndrome), scales = "free") +
    theme_with("legend_top") +
    g_labs(
      x="Season week index (1=Week of last 1st september)", 
      y="Incidence rate", 
      title="Weekly incidence rate by country and season", 
      subtitle=subtitle) +
    guides(color=guide_legend("Season") )
  g_save(syndrome,"_incidence_country+season_superpose", width=4, height=12)

  d = ii %>% 
    filter(season < max(seasons)) %>%
    group_by(season.index, country, syndrome) %>% 
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
    geom_line(data=ii[ ii$season == max(seasons), ], aes(y=incidence, color="current", linetype="current"), size=1.2) +
    scale_color_manual(values=c('range'="darkblue","median"="blue", "current"="red", "quantile"="steelblue"), labels=labels)  +
    scale_linetype_manual(values=c('range'="dotted","median"="solid", "current"="solid", "quantile"="dashed"), labels=labels)  +
    facet_grid(rows=vars(country), cols=vars(syndrome)) +
    g_labs(x="Season week index (1=Week of last 1st september)", y="Incidence rates", subtitle=subtitle)
  g_save(syndrome,"_incidence_country+season_distrib", width=4, height=12)
  
  context$set(what="active")
  
  active$syndrome = NULL
  
  d = left_join(active, inc[, c('country','season','yw','incidence','censored','syndrome') ], by=c('country','season','yw'))
  d = d %>%
        group_by(season, country, syndrome) %>% 
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
    g_labs(x="Week", y="Active participant", title="Active participants by country and season (incidence superposed)")
  g_save(syndrome,"_active+inc_country+season", width=12, height=8)
  
  ggplot(d, aes(x=monday_of_week(yw), y=active)) + 
    geom_bar(stat="identity", fill="steelblue") +
    facet_grid(rows=vars(country), cols=vars(season), scales = "free") +
    g_labs(y="Active participants", x="Week", title="Active participants by ")
  g_save(syndrome,"_active_country+season.pdf", width=12, height=8)
  
  context$pop()
  
}

context$push()
visits = load_bundles("visits_weekly")

ggplot(visits, aes(x=monday_of_week(yw), y=prop, color=variable)) + 
  geom_line() + 
  facet_grid(rows=vars(country), cols=vars(season), scales="free", labeller=labeller(variable=i18n)) +
  g_labs(y="% of participants with syndrome", x="Week", title="Health care seeking with ari.ecdc") +
  guides(color=guide_legend("Variable"))
ggsave(my.path("visits_weekly_country+season.pdf"), width=12, height=8)

ggplot(visits, aes(x=monday_of_week(yw), y=cum_prop, color=variable)) + 
  geom_line() +
  facet_grid(rows=vars(country), cols=vars(season), scales="free", labeller=labeller(variable=i18n)) +
  g_labs(y="% of participants with syndrome", x="Week", title="Health care seeking with ari.ecdc, weekly % cumulated over the season") +
  guides(color=guide_legend("Variable"))
ggsave(my.path("visits_weekly_cumulated_country+season.pdf"), width=12, height=8)

visits.cumul = load_bundles("visits_cumul")

ggplot(visits.cumul, aes(x=factor(season), color=variable, group=variable)) + 
  geom_ribbon(aes(ymin=cum_prop_lower, ymax=cum_prop_upper, fill=variable), color=NA, alpha=.3) +
  geom_point(aes(y=cum_prop)) +
  facet_grid(rows=vars(country), scales="free", labeller=labeller(variable=i18n)) +
  g_labs(y="% of participants with syndrome", x="Week", title="Health care seeking with ari.ecdc, cumulated over each season") +
  guides(color=guide_legend("Variable"), fill=FALSE) + ylim(0,NA)
ggsave(my.path("visits_season_cumulated.pdf"), width=8, height=8)

