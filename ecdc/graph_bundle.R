##
# graph_bundle
# Makes the graphs from the bundled data (selected data to be output on the webssite and sent to ecdc)
##
source("conf.R")

library(dplyr)
library(rlang)
library(ggplot2)

countries = platform_env("COUNTRY_CODES")

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

seasons = unique(inc$season)
countries = unique(inc$country)
methods = unique(inc$method)
syndromes = unique(inc$syndrome)

context = ResultContext$new()

g_save = function(..., width, height, desc=NULL) {
  path = paste0(...) 
  save_graph_with_context(path, 'pdf', width, height, context=context, desc=desc) 
}

init.path('indicator/bundles')

result_desc_filters(
  auto=TRUE, 
  filters=list(
    result_filter("syndrome","Syndrome"),
    result_filter("what","Subject"),
    result_filter("span","Time period")
  )
)

short.season = max(seasons) - 1
last.season = max(seasons)

spans = list(
  list(name="all", season=NA, title="All seasons"),
  list(name="2_seasons", season=short.season, title="last 2 seasons", width=8),
  list(name="last_season", season=last.season, title="last season", width=6)
)

rate_unit   = "Incidence rate (per 1000)"
rate_factor = 1000
height = length(countries) + 2

context$set(span="all") # Default period

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
  
  for(span in spans) {
    
    context$push()
    
    context$set(span=span$name)
    
    if( !is.na(span$season)) {
      d = ii %>% filter(season >= span$season)
    } else {
      d = ii
    }
    
    width = if(hasName(span, "width")) span$width else 12
    
    ss = unique(d$season)
    suffix = if(is.na(span$season)) "" else paste0("_", span$name)
    
    ggplot(d, aes(x=monday_of_week(yw), y=incidence * rate_factor, group=syndrome, color=syndrome)) + 
      geom_vline(data=inc %>% filter(censored & season %in% ss), aes(xintercept=monday_of_week(yw)), color="grey90") +
      geom_line() +
      geom_ribbon(aes(ymin=lower* rate_factor, ymax=upper* rate_factor, fill=syndrome), color="transparent", alpha=.40) +
      facet_grid(rows=vars(country), cols=vars(season), scales = "free") +
      theme_with("legend_top") +
      g_labs(x="Week", y=rate_unit, title="Weekly incidence rate by country and season", subtitle=paste0(subtitle, ", ", span$title))
    g_save(syndrome,"_incidence_country+season", suffix, width=width, height=height)
    
    ggplot(d, aes(x=monday_of_week(yw), y=count, group=syndrome, color=syndrome)) + 
      geom_vline(data=inc %>% filter(censored & season %in% ss), aes(xintercept=monday_of_week(yw)), color="grey90") +
      geom_line() +
      facet_grid(rows=vars(country), cols=vars(season), scales = "free") +
      theme_with("legend_top") +
      g_labs(x="Week", y="Number of participants", title="Weekly incident count by country and season", subtitle=paste0(subtitle, ", ", span$title))
    g_save(syndrome,"_count_country+season", suffix, width=width, height=height, desc=list(what="count"))
    
    context$pop()
    
  }
  ii = calc_season_fixed(ii)
  cur = ii[ ii$season == max(seasons), ]
  ggplot(ii, aes(x=season.index, y=incidence * rate_factor, group=season.year, color=factor(season.year))) + 
    geom_line() +
    geom_line(data=cur, size=1.2) +
    geom_ribbon(data=cur, aes(ymin=lower * rate_factor, ymax=upper * rate_factor, fill=factor(season.year)), alpha=.4, color=NA) +
    facet_grid(rows=vars(country), cols=vars(syndrome), scales = "free") +
    theme_with("legend_top") +
    g_labs(
      x="Season week index (1=Week of last 1st september)", 
      y=rate_unit, 
      title="Weekly incidence rate by country and season", 
      subtitle=subtitle) +
    guides(color=guide_legend("Season"), fill=FALSE )
  g_save(syndrome,"_incidence_country+season_superpose", width=4, height=height)

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
  colors.metric = c('range'="darkblue","median"="blue", "current"="red", "quantile"="steelblue")
  ggplot(d, aes(x=season.index)) +
    geom_line(aes(y=min * rate_factor, color="range", linetype="range")) +
    geom_line(aes(y=max * rate_factor, color="range", linetype="range")) +
    geom_line(aes(y=median * rate_factor, color="median", linetype="median")) +
    geom_line(aes(y=q1 * rate_factor, color="quantile", linetype="quantile")) +
    geom_line(aes(y=q3 * rate_factor, color="quantile", linetype="quantile")) +
    geom_line(data=cur, aes(y=incidence * rate_factor, color="current", linetype="current"), size=1.2) +
    geom_ribbon(data=cur, aes(ymin=lower * rate_factor, ymax=upper * rate_factor, fill="current"), alpha=.4, color=NA) +
    scale_color_manual(values=colors.metric, labels=labels)  +
    scale_fill_manual(values=colors.metric, labels=labels)  +
    scale_linetype_manual(values=c('range'="dotted","median"="solid", "current"="solid", "quantile"="dashed"), labels=labels)  +
    guides(fill=FALSE) +
    facet_grid(rows=vars(country), cols=vars(syndrome)) +
    g_labs(x="Season week index (1=Week of last 1st september)", y=rate_unit, subtitle=subtitle)
  g_save(syndrome,"_incidence_country+season_distrib", width=4, height=height)
  
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
  g_save(syndrome,"_active+inc_country+season", width=12, height=height)
  
  ggplot(d, aes(x=monday_of_week(yw), y=active)) + 
    geom_bar(stat="identity", fill="steelblue") +
    facet_grid(rows=vars(country), cols=vars(season), scales = "free") +
    g_labs(y="Active participants", x="Week", title="Active participants by ")
  g_save(syndrome,"_active_country+season.pdf", width=12, height=height)
  
  context$pop()
  
}

context$push()

visits = bundles$visits_weekly

scale_linetype_adjusted = scale_linetype_manual(values=c('adj'='solid','raw'="dashed"), labels=c('adj'="Adjusted","raw"="Non adjusted"))
  
for(syndrome in syndromes) {
 
  context$set(what="healthcare", syndrome=syndrome)
  
  dd = visits %>% filter(syndrome == !!syndrome)
  
  ggplot(dd, aes(x=monday_of_week(yw), y=prop_adj, color=variable)) + 
    geom_line(aes(y=prop_adj, linetype="adj")) +
    geom_line(aes(y=prop_raw, linetype="raw")) +
    facet_grid(rows=vars(country), cols=vars(season), scales="free", labeller=labeller(variable=i18n)) +
    scale_linetype_adjusted +
    guides(color=guide_legend("Variable")) +
    g_labs(
      y="% of participants with syndrome", x="Week", 
      title=paste("Health care seeking with ", syndrome),
      subtitle="Adjusted and not adjusted values"
    ) 
    
  g_save(syndrome, "_visits_weekly_adj+raw_country+season", width=12, height=height)
  
  ggplot(dd, aes(x=monday_of_week(yw), y=prop_adj, color=variable)) + 
    geom_line() +
    facet_grid(rows=vars(country), cols=vars(season), scales="free", labeller=labeller(variable=i18n)) +
    g_labs(y="% of participants with syndrome", x="Week", title=paste("Health care seeking with ", syndrome), subtitle="Adjusted proportions") +
    guides(color=guide_legend("Variable"))
  g_save(syndrome, "_visits_weekly_adj_country+season", width=12, height=8)
  
  ggplot(dd, aes(x=monday_of_week(yw), y=prop_adj, color=variable)) + 
    geom_ribbon(aes(ymin=prop_adj_low, ymax=prop_adj_up, fill=variable), alpha=.30, color="transparent") +
    geom_line() +
    facet_grid(rows=vars(country), cols=vars(season), scales="free", labeller=labeller(variable=i18n)) +
    g_labs(
      y="% of participants with syndrome", x="Week", 
      title=paste0("Health care seeking with ",syndrome,", weekly % cumulated over the season"),
      subtitle="Adjusted proportions with confidence interval"
    ) +
    guides(color=guide_legend("Variable"), fill=FALSE)
  g_save(syndrome, "_visits_weekly_adj+ci_country+season.pdf", width=12, height=height)

  ggplot(dd, aes(x=monday_of_week(yw), color=variable)) + 
    geom_line(aes(y=cum_prop_adj, linetype="adj")) +
    geom_line(aes(y=cum_prop_raw, linetype="raw")) +
    facet_grid(rows=vars(country), cols=vars(season), scales="free", labeller=labeller(variable=i18n)) +
    scale_linetype_adjusted +
    g_labs(
        y="% of participants with syndrome", x="Week", 
        title=paste0("Health care seeking with ",syndrome,", weekly % cumulated over the season"),
        subtitle="Adjusted and non adjusted proportions"
    ) +
    guides(color=guide_legend("Variable"))
  g_save(syndrome, "_visits_weekly_cumulated_adj+raw_country+season.pdf", width=12, height=height)

  ggplot(dd, aes(x=monday_of_week(yw), y=cum_prop_adj, color=variable)) + 
    geom_ribbon(aes(ymin=cum_prop_adj_low, ymax=cum_prop_adj_up, fill=variable), alpha=.30, color="transparent") +
    geom_line() +
    facet_grid(rows=vars(country), cols=vars(season), scales="free", labeller=labeller(variable=i18n)) +
    g_labs(
      y="% of participants with syndrome", x="Week", 
      title=paste0("Health care seeking with ",syndrome,", weekly % cumulated over the season"),
      subtitle="Adjusted proportions with confidence interval"
    ) +
    guides(color=guide_legend("Variable"), fill=FALSE)
  g_save(syndrome, "_visits_weekly_cumulated_adj+ci_country+season.pdf", width=12, height=height)
}

library(rmarkdown)

current_week = iso_yearweek(Sys.Date())
last_week = iso_yearweek(Sys.Date() - 7)

output_dir = my.path()

outputs = bundles
rmarkdown::render("ecdc_report.Rmd", output_dir =output_dir , output_format =rmarkdown::html_document() )

