source("conf.R")

library(dplyr)
library(rlang)
library(ggplot2)

countries = platform_env("COUNTRY_CODES")
seasons = get_historical_seasons()

init.path('indicator')

bundles = readRDS(my.path('datasets.rds'))

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

init.path('indicator/all')

result_desc_filters(
  auto=TRUE, 
  filters=list(
    result_filter("syndrome","Syndrome"),
    result_filter("what","Subject")
  )
)

result_desc_readme(
  "This directory contains graphics for all computed incidence (all syndromes and all estimators for incidence)",
  paste("Estimators used are ", paste(methods, collapse = ","))
)

for(syndrome in syndromes) {
  
  context$push()
  
  context$set(syndrome=syndrome)
  
  ii = inc.censored %>% filter(syndrome == !!syndrome & type == "adj")

  context$set("what"="incidence", method="all")
  
  subtitle = i18n(syndrome)
  
  ggplot(ii, aes(x=monday_of_week(yw), y=incidence, group=syndrome, color=method)) + 
    geom_vline(data=inc %>% filter(censored), aes(xintercept=monday_of_week(yw)), color="grey90") +
    geom_line() +
    facet_grid(rows=vars(country), cols=vars(season), scales = "free") +
    theme_with("legend_top") +
    g_labs(x="Week", y="Incidence rate", title="Weekly incidence rate by country and season", subtitle=subtitle)
  g_save(syndrome,"_incidence_allmethods_country+season", width=12, height=8)

  ii = calc_season_fixed(ii)
  
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
  subtitle = syndrome
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
    facet_grid(rows=vars(country), cols=vars(method)) +
    g_labs(x="Season week index (1=Week of last 1st september)", y="Incidence rates", subtitle=subtitle)
  g_save(syndrome,"_incidence_distrib_country+season", width=4, height=12)
  
  context$pop()
  
}

context$push()

context$set(what="active", method="all")

ggplot(active, aes(x=monday_of_week(yw), y=active, color=method)) +
  geom_line() +
  facet_grid(rows=vars(country), cols=vars(season), scales="free") +
  theme_with("legend_top") +
  g_labs(x="Week", y="Incidence rate", title="Weekly active participants count by country and season for all methods")

g_save("active_country+season_distrib", width=4, height=12)

context$pop()

context$push()

for(method in methods) {
  
  context$set(method=method)
  
  ii = inc.censored %>% filter(method == !!method & type == "adj")
  
  context$set("what"="incidence")
  
  
  context$set("syndrome"="all")
  ggplot(ii, aes(x=monday_of_week(yw), y=incidence, group=syndrome, color=syndrome)) + 
    geom_vline(data=inc %>% filter(censored), aes(xintercept=monday_of_week(yw)), color="grey90") +
    geom_line() +
    facet_grid(rows=vars(country), cols=vars(season), scales = "free") +
    theme_with("legend_top") +
    g_labs(x="Week", y="Incidence rate", title="Weekly incidence rate by country and season", subtitle=method)
  g_save(method,"_incidence_allsyndromes_country+season", width=12, height=8)

  context$set("syndrome"="all_ecdc")
  ggplot(ii %>% filter(syndrome %in% ecdc_syndromes), aes(x=monday_of_week(yw), y=incidence, group=syndrome, color=syndrome)) + 
    geom_vline(data=inc %>% filter(censored), aes(xintercept=monday_of_week(yw)), color="grey90") +
    geom_line() +
    facet_grid(rows=vars(country), cols=vars(season), scales = "free") +
    theme_with("legend_top", "x_vertical") +
    g_labs(x="Week", y="Incidence rate", title="Weekly incidence rate by country and season", subtitle=method)
  g_save(method,"_incidence_ecdcsyndromes_country+season", width=12, height=8)

}
context$pop()


