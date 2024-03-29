##
# Builld graphs for indicators using full data (used or not for ecdc report)
# Includes complementary analyses
##
source("conf.R")

library(dplyr)
library(rlang)
library(ggplot2)

share.lib('incidence')

seasons = get_historical_seasons()

init.path('indicator')

bundles = readRDS(my.path('datasets.rds'))

# Local version
g_labs = ifn_labs

caption = ifn.copyright

active = bundles$active
inc = bundles$incidence

# Compute max active count for each country/season
# Used to filter to low estimations (not interpretable or noisy)
max.active = inc %>% group_by(country, season) %>% summarize(max_active=max(part))
inc = left_join(inc, max.active, by=c("country","season"))
inc = inc %>% mutate(active_limit=.35 * max_active, censored=part < active_limit)

inc.censored = inc %>% filter(!censored)
inc.censored = inc.censored %>% group_by(country, season) %>% mutate(ymax=max(incidence, na.rm=TRUE))
inc.censored = inc.censored %>% mutate(upper=ifelse(upper > ymax * 2, NA, upper))

methods = na.omit(unique(inc$method))
syndromes = unique(inc$syndrome)
seasons = as.integer(unique(inc$season))
countries = unique(inc$country)

context = ResultContext$new()

g_save = function(..., width, height, desc=NULL) {
  save_graph_with_context(paste0(...), formats="pdf", width=width, height=height, context = context, desc=desc)
}

init.path('indicator/all')

result_desc_filters(
  auto=TRUE, 
  filters=list(
    result_filter("syndrome","Syndrome"),
    result_filter("what","Subject"),
    result_filter("adj","Adjustment"),
    result_filter("method","Participant selection")
  )
)

result_desc_readme(
  c("This directory contains graphics for all computed incidence (all syndromes and all estimators for incidence) and indicators",
  paste("Estimators used are ", paste(methods, collapse = ","))
  )
)

short.season = max(seasons) - 1
last.season = max(seasons)

spans = list(
  list(name="all", season=NA, title="All seasons"),
  list(name="2_seasons", season=short.season, title="last 2 seasons", width=8),
  list(name="last_season", season=last.season, title="last season", width=6)
)

height = length(countries) + 2

rate_unit   = "Incidence rate (per 1000)"
rate_factor = 1000

for(syndrome in syndromes) {
  
  context$push()
  
  context$set(syndrome=syndrome)
  
  ii = inc.censored %>% filter(syndrome == !!syndrome & type == "adj")

  context$set("what"="incidence", method="all", adj="adj")
  
  subtitle = i18n(syndrome)
  
  for(span in spans) {
    if( !is.na(span$season)) {
      d = ii %>% filter(season >= span$season)
    } else {
      d = ii
    }
    
    mm = d %>% filter(!is.na(method)) %>% pull(method)
    if(length(mm) == 0) {
      # Cannot compute if no method (in case of a syndrome kind only provided by external)
      next()
    }
    
    width = if(hasName(span, "width")) span$width else 12
    
    ss = unique(d$season)
    suffix = if(is.na(span$season)) "" else paste0("_", span$name)
    
    ggplot(d, aes(x=monday_of_week(yw), y=rate_factor * incidence, group=method, color=method)) + 
      geom_vline(data=inc %>% filter(censored & season %in% !!ss), aes(xintercept=monday_of_week(yw)), color="grey90") +
      geom_line() +
      facet_grid(rows=vars(country), cols=vars(season), scales = "free") +
      theme_with("legend_top") +
      g_labs(x="Week", y=rate_unit, title="Weekly incidence rate by country and season", subtitle=paste0(subtitle,", ", span$title))
    g_save(syndrome,"_incidence_allmethods_country+season", suffix, width=width, height=height, desc=list(span=span$name))
  
    ggplot(d, aes(x=monday_of_week(yw), y=count, group=method, color=method)) + 
      geom_vline(data=inc %>% filter(censored & season %in% !!ss), aes(xintercept=monday_of_week(yw)), color="grey90") +
      geom_line() +
      facet_grid(rows=vars(country), cols=vars(season), scales = "free") +
      theme_with("legend_top") +
      g_labs(x="Week", y="Number of cases", title="Weekly incidence count by country and season, methods superposed", subtitle=paste0(subtitle,", ", span$title))
    g_save(syndrome,"_count_allmethods_country+season", suffix, width=width, height=height, desc=list(span=span$name, what="count"))

    ggplot(d, aes(x=monday_of_week(yw), y=rate_factor * incidence, group=country, color=country)) + 
      geom_line() +
      facet_grid(rows=vars(method), cols=vars(season), scales = "free") +
      g_labs(x="Week", y=rate_unit, title="Weekly incidence rate by method and season, countries superposed", subtitle=paste0(subtitle,", ", span$title))
    g_save(syndrome,"_incidence_allcountries_method+season", suffix, width=width + 1, height=height, desc=list(span=span$name))

    for(country in unique(d$country)) {
      ggplot(d, aes(x=monday_of_week(yw), y=rate_factor * incidence, group=country)) + 
        geom_line(data=~filter(., country != .env$country), color="grey",alpha=.30) +
        geom_line(data=~filter(., country == .env$country), color="red", size=2) +
        facet_grid(rows=vars(method), cols=vars(season), scales = "free") +
        g_labs(x="Week", y=rate_unit, title="Weekly incidence rate by method and season, countries superposed", subtitle=paste0(subtitle,", ", span$title))
      g_save(syndrome,"_incidence_country-",country,"_method+season", suffix, width=width + 1, height=height, desc=list(span=span$name))
    }  
    
    
  }
  
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
  
  if(nrow(d) > 0) {
    subtitle = syndrome
    labels = c('range'="Min/Max","median"="Median", "current"="Current season", "quantile"="1st, 3rd quantiles")
    ggplot(d, aes(x=season.index)) +
      geom_line(aes(y=min * rate_factor, color="range", linetype="range")) +
      geom_line(aes(y=max * rate_factor, color="range", linetype="range")) +
      geom_line(aes(y=median * rate_factor, color="median", linetype="median")) +
      geom_line(aes(y=q1 * rate_factor, color="quantile", linetype="quantile")) +
      geom_line(aes(y=q3 * rate_factor, color="quantile", linetype="quantile")) +
      geom_line(data=ii[ ii$season == max(seasons), ], aes(y=incidence * rate_factor, color="current", linetype="current"), size=1.2) +
      scale_color_manual(values=c('range'="darkblue","median"="blue", "current"="red", "quantile"="steelblue"), labels=labels)  +
      scale_linetype_manual(values=c('range'="dotted","median"="solid", "current"="solid", "quantile"="dashed"), labels=labels)  +
      facet_grid(rows=vars(country)) +
      g_labs(x="Season week index (1=Week of last 1st september)", y=rate_unit, subtitle=subtitle)
    g_save(syndrome,"_incidence_distrib_country+season", width=width, height=height)
  }
  context$pop()
  
}

context$push()

context$set(what="active", method="all")

# Build active difference relative to reference method
ref = "w0"
active_ref = active %>% 
  filter(method == !!ref) %>% 
  rename(active_ref=active) %>% 
  select(-syndrome, -method)
aa = active %>% 
  filter(method != !!ref) %>% 
  left_join(active_ref, by=c('yw','country','season')) %>% 
  select(-syndrome)

aa = aa %>% mutate(diff=active - active_ref, cv=diff/active_ref)

for(span in spans) {
  if( !is.na(span$season)) {
    d = active %>% filter(season >= span$season)
  } else {
    d = active
  }
  width = if(hasName(span, "width")) span$width else 12
  ss = unique(d$season)
  suffix = if(is.na(span$season)) "" else paste0("_", span$name)
  
  context$set(span=span$name)

  ggplot(d, aes(x=monday_of_week(yw), y=active, color=method)) +
    geom_line() +
    facet_grid(rows=vars(country), cols=vars(season), scales="free") +
    theme_with("legend_top") +
    g_labs(x="Week", y="Number of participans", title="Weekly active participants count by country and season for all methods", subtitle=span$title)
  g_save("active_country+season_distrib", suffix, width=width, height=height)
  
  # Now use relative to w0 method
  if( !is.na(span$season)) {
    d = aa %>% filter(season >= span$season)
  } else {
    d = aa
  }
  
  title = paste0("Difference of active participants from method ", sQuote(ref))
  ggplot(d, aes(x=monday_of_week(yw), y=diff, color=method)) +
    geom_line() +
    facet_grid(rows=vars(country), cols=vars(season), scales="free") +
    g_labs(x="Week",  y="Number of participants", title=title, subtitle=span$title)
  g_save("active_country+season_method-diff", suffix, width=width, height=height)
  
  title = paste0("% Difference of active participants from method ", sQuote(ref))
  
  ggplot(d, aes(x=monday_of_week(yw), y=100*cv, color=method)) +
    geom_line() +
    geom_rug(data=d %>% filter(cv > 100), color="red", sides="t") +
    facet_grid(rows=vars(country), cols=vars(season), scales="free") +
    scale_y_continuous(limits=c(-NA, 200)) +
    g_labs(x="Week",  y="Percentage", title=title, subtitle=span$title)
  g_save("active_country+season_method-cv", suffix, width=width, height=height)
  
  ggplot(d, aes(x=monday_of_week(yw), y=100*cv, color=country)) +
    geom_line() +
    geom_rug(data=d %>% filter(cv > 100), color="red", sides="t") +
    facet_grid(rows=vars(method), cols=vars(season), scales="free") +
    scale_y_continuous(limits=c(-NA, 200)) +
    g_labs(x="Week",  y="Percentage", title=title, subtitle=span$title)
  g_save("active_method+season_method-cv", suffix, width=width, height=height)

}



context$pop()

context$push()

for(method in methods) {
  
  context$set(method=method)
  
  ii = inc.censored %>% filter(method == !!method & type == "adj")
  
  context$set("what"="incidence")
  
  for(span in spans) {
    context$push()
    context$set(span=span$name)
    
    if( !is.na(span$season)) {
      d = ii %>% filter(season >= span$season)
    } else {
      d = ii
    }
    ss = unique(d$season)
    width = if(hasName(span, "width")) span$width else 12
    suffix = if(is.na(span$season)) "" else paste0("_", span$name)
  
    context$set("syndrome"="all")
    ggplot(d, aes(x=monday_of_week(yw), y=incidence * rate_factor, group=syndrome, color=syndrome)) + 
      geom_vline(data=inc %>% filter(censored & season %in% ss), aes(xintercept=monday_of_week(yw)), color="grey90") +
      geom_line() +
      facet_grid(rows=vars(country), cols=vars(season), scales = "free") +
      theme_with("legend_top") +
      g_labs(x="Week", y=rate_unit, title="Weekly incidence rate by country and season", subtitle=paste0(method,",", span$title))
    g_save(method,"_incidence_allsyndromes_country+season", suffix, width=width, height=height)

    ggplot(d, aes(x=monday_of_week(yw), y=count, group=syndrome, color=syndrome)) + 
      geom_vline(data=inc %>% filter(censored & season %in% ss), aes(xintercept=monday_of_week(yw)), color="grey90") +
      geom_line() +
      facet_grid(rows=vars(country), cols=vars(season), scales = "free") +
      theme_with("legend_top") +
      g_labs(x="Week", y="Number of participants", title="Weekly incidence count by country and season", subtitle=paste0(method,",", span$title))
    g_save(method,"_count_allsyndromes_country+season", suffix, width=width, height=height, desc=list(what="count"))
    
    context$set("syndrome"="all_ecdc")
    dd = d %>% filter(syndrome %in% ecdc_syndromes)
    ggplot(dd) +
      aes(x=monday_of_week(yw), y=incidence * rate_factor, group=syndrome, color=syndrome) +
      geom_vline(data=inc %>% filter(censored & season %in% ss), aes(xintercept=monday_of_week(yw)), color="grey90") +
      geom_line() +
      facet_grid(rows=vars(country), cols=vars(season), scales = "free") +
      theme_with("legend_top", "x_vertical") +
      g_labs(x="Week", y=rate_unit, title="Weekly incidence rate by country and season", subtitle=paste0(method,",", span$title))
    g_save(method,"_incidence_ecdc-syndromes_country+season", suffix, width=width, height=height)

    ggplot(dd) +
      aes(x=monday_of_week(yw), y=count, group=syndrome, color=syndrome) +
      geom_vline(data=inc %>% filter(censored & season %in% ss), aes(xintercept=monday_of_week(yw)), color="grey90") +
      geom_line() +
      facet_grid(rows=vars(country), cols=vars(season), scales = "free") +
      theme_with("legend_top", "x_vertical") +
      g_labs(x="Week", y="Number of participants", title="Weekly incidence count by country and season", subtitle=paste0(method,",", span$title))
    g_save(method,"_count_ecdc-syndromes_country+season", suffix, width=width, height=height, desc=list(what="count"))
    context$pop()
    
  }   
}

context$pop()

context$push()

scale_linetype_adjusted = scale_linetype_manual(values=c('adj'='solid','raw'="dashed"), labels=c('adj'="Adjusted","raw"="Non adjusted"))

context$set(what="visits")
for(syndrome in syndromes) {
  dd = bundles$vars %>% filter(syndrome == !!syndrome & grepl("^visit", variable))
  
  if(nrow(dd) == 0) {
    next()
  }
  
  context$set(syndrome=syndrome, cumulated="no")
  
  d = dd %>% filter(cumulated==FALSE)
  ggplot(d, aes( color=variable, x=monday_of_week(yw))) + 
    geom_line(aes(y=prop_adj, linetype="adj")) +
    geom_line(aes(y=prop_raw, linetype="raw")) +
  facet_grid(country~season, scales = "free") +
  scale_linetype_adjusted +
  theme_with("legend_top", "x_vertical") +
  g_labs(x="Week", y="Healthcare seeking rate", title="Healthcare seeking rate by country and season, weekly value", subtitle=paste(syndrome,", adjusted and not adjusted"))
  g_save(syndrome,"_visits_adj+raw_country+season", width=12, height=8, desc=list(adj="both"))
  
  ggplot(d, aes(color=variable, x=monday_of_week(yw))) + 
    geom_line(aes(y=prop_adj)) +
    facet_grid(country ~ season, scales = "free") +
    theme_with("legend_top", "x_vertical") +
    g_labs(x="Week", y="Healthcare seeking rate", title="Healthcare seeking rate by country and season, weekly value", subtitle=paste(syndrome, ", adjusted"))
  g_save(syndrome,"_visits_adj+raw__country+season", width=12, height=8, desc=list(adj="adj"))
  
  ggplot(d, aes(color=variable, x=monday_of_week(yw))) + 
    geom_ribbon(aes(ymin=prop_adj_low, ymax=prop_adj_up, fill=variable), alpha=.30, color="transparent") +
    geom_line(aes(y=prop_adj)) +
    facet_grid(country~season, scales = "free") +
    theme_with("legend_top", "x_vertical") +
    g_labs(x="Week", y="Healthcare seeking rate", title="Healthcare seeking rate by country and season, weekly value", subtitle=paste(syndrome, ", adjusted with ci"))
  g_save(syndrome,"_visits_adj+raw__country+season", width=12, height=8, desc=list(adj="adj"))
  
  context$set(cumulated="yes")
  
  d = dd %>% filter(cumulated == TRUE)
  ggplot(d, aes( color=variable, x=monday_of_week(yw))) + 
    geom_line(aes(y=prop_adj, linetype="adj")) +
    geom_line(aes(y=prop_raw, linetype="raw")) +
    facet_grid(country~season, scales = "free") +
    scale_linetype_adjusted +
    theme_with("legend_top", "x_vertical") +
    g_labs(x="Week", y="Healthcare seeking rate", title="Healthcare seeking rate by country and season, cumulated values", subtitle=paste(syndrome,", adjusted and not adjusted"))
  g_save(syndrome,"_visits-cumul_adj+raw_country+season", width=12, height=8, desc=list(adj="both"))
  
  ggplot(d, aes(color=variable, x=monday_of_week(yw))) + 
    geom_line(aes(y=prop_adj)) +
    facet_grid(country ~ season, scales = "free") +
    theme_with("legend_top", "x_vertical") +
    g_labs(x="Week", y="Healthcare seeking rate", title="Healthcare seeking rate by country and season, cumulated values", subtitle=paste(syndrome, ", adjusted"))
  g_save(syndrome,"_visits-cumul_adj+raw__country+season", width=12, height=8, desc=list(adj="adj"))
  
  ggplot(d, aes(color=variable, x=monday_of_week(yw))) + 
    geom_ribbon(aes(ymin=prop_adj_low, ymax=prop_adj_up, fill=variable), alpha=.30, color="transparent") +
    geom_line(aes(y=prop_adj)) +
    facet_grid(country~season, scales = "free") +
    theme_with("legend_top", "x_vertical") +
    g_labs(x="Week", y="Healthcare seeking rate", title="Healthcare seeking rate by country and season, weekly values", subtitle=paste(syndrome, ", adjusted with ci"))
  g_save(syndrome,"_visits-cumul_adj+raw__country+season", width=12, height=8, desc=list(adj="adj"))

}

context$pop()

context$set(what="tests")
ylab = "Tests rate"
title = "Test realized for symptoms, rate by country and season, weekly value"

for(syndrome in syndromes) {
  dd = bundles$vars %>% filter(syndrome == !!syndrome & grepl("^analysis\\.sympt\\.covid", variable))
  
  if(nrow(dd) == 0) {
    next()
  }
  
  context$set(syndrome=syndrome, cumulated="no")
  
  
  d = dd %>% filter(!cumulated)
  ggplot(d, aes( color=variable, x=monday_of_week(yw))) + 
    geom_line(aes(y=prop_adj, linetype="adj")) +
    geom_line(aes(y=prop_raw, linetype="raw")) +
    facet_grid(country~season, scales = "free") +
    scale_linetype_adjusted +
    scale_color_discrete(labels=i18n) +
    theme_with("legend_top", "x_vertical") +
    g_labs(x="Week", y=ylab, title=title, subtitle=paste(syndrome,", adjusted and not adjusted"))
  g_save(syndrome,"_tests_adj+raw_country+season", width=12, height=8, desc=list(adj="both"))
  
  ggplot(d, aes(color=variable, x=monday_of_week(yw))) + 
    geom_line(aes(y=prop_adj)) +
    facet_grid(country ~ season, scales = "free") +
    scale_color_discrete(labels=i18n) +
    theme_with("legend_top", "x_vertical") +
    g_labs(x="Week", y=ylab, title=title, subtitle=paste(syndrome, ", adjusted"))
  g_save(syndrome,"_tests_adj+raw__country+season", width=12, height=8, desc=list(adj="adj"))
  
  ggplot(d, aes(color=variable, x=monday_of_week(yw))) + 
    geom_ribbon(aes(ymin=prop_adj_low, ymax=prop_adj_up, fill=variable), alpha=.30, color="transparent") +
    geom_line(aes(y=prop_adj)) +
    scale_color_discrete(labels=i18n) +
    scale_fill_discrete(labels=i18n) +
    facet_grid(country~season, scales = "free") +
    theme_with("legend_top", "x_vertical") +
    g_labs(x="Week", y=ylab, title=title, subtitle=paste(syndrome, ", adjusted with ci"))
  g_save(syndrome,"_tests_adj+raw__country+season", width=12, height=8, desc=list(adj="adj"))

}

context$set(syndrome="all", cumulated="no")

## Compare between syndromes
dd = bundles$vars %>% filter(grepl("^analysis\\.sympt\\.covid", variable) & !cumulated)
if(nrow(dd) > 0) {
  ggplot(dd, aes( color=variable, x=monday_of_week(yw), linetype=syndrome) ) + 
    geom_line(aes(y=prop_adj)) +
    facet_grid(country ~ season, scales = "free") +
    scale_color_discrete(labels=i18n) +
    theme_with("x_vertical") +
    g_labs(x="Week", y=ylab, title=title, subtitle=paste("all syndromes (linetype), adjusted"))
  g_save("all_tests_adj_country+season", width=12, height=8, desc=list(adj="adj"))

}

context$pop()



