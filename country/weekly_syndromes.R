## Weekly syndromes
# cli.args = list('country'="IT", season=2020)

source('conf.R')

library(dplyr)
library(swMisc)
library(ggplot2)
library(cowplot)
library(glue)

share.lib('upset')
source("lib/graph.R", local=TRUE)

if(!exists("cli.args")) {
  cli.args = parseArgs(list(
    'season'=list(type="int", default=get_current_season()),
    'country'=list(type="choices", choices=platform_env("COUNTRY_CODES"))
  ))
}

country = cli.args$country
season = cli.args$season

# Country translations
i18n_load("i18n/", language = tolower(country))

short.term.size = 4
short.term = iso_yearweek(Sys.Date() - short.term.size * 7)

data.path = paste0(create_path(prefixes = list(project='overview'), suffix = season), "weekly_syndromes.Rds")

if (!file.exists(data.path)) {
  rlang::abort("No data", class = "no_data")
}

data.all = readRDS(data.path)

symptoms <- attr(data.all, "symptoms") 
symptoms.mask <- attr(data.all, "symptoms.mask")

# Keep only the country
for(n in ls(data.all)) {
  data.all[[n]] = data.all[[n]] %>% filter(country == !!country)
}

init.path(paste0(country, "/", season))

caption = paste0(i18n("platform.copyright"),", ", Sys.time())

colors = get_platform_colors()

country_title = i18n("country_label")
if(country_title == "") {
  country_title = i18n(paste0("country_", tolower(country)))
}

min.week = get_graph_starting_week(season)

context = ResultContext$new()

g_save = function(...,  desc=NULL, plot=FALSE, width=12, height = 8) {
  p = paste0(...)
  save_graph_with_context(p, formats = c('pdf','svg'), width=width, height=height, context=context, desc=desc)
}

g_labs = function(...) {
  ggplot2::labs(..., caption = caption, subtitle = i18n("country_label")) 
}

date_format = i18n("monthday_format")
week_format = function(d) { format(monday_of_week(d), date_format) }

short.term.title = glue(i18n("on_last_n_weeks"), weeks=short.term.size)

large_breaks = function(range) {
  scales::cbreaks(range, scales::pretty_breaks(10))$breaks
}

scale_sympt_freq = function() {
  scale_fill_viridis_c(direction = -1, option = "A", na.value="grey90", breaks=large_breaks)
}

context$push()

data = data.all$symptoms

dd = data %>% filter(yw >= min.week)
if(nrow(dd) > 0) {
  
  context$set(subject="symptoms")
  
  ggplot(dd, aes(x=monday_of_week(yw), y=name, fill=100 * value/person)) +
    geom_tile() + 
    scale_sympt_freq() +
    scale_x_date(date_labels=date_format) +
    scale_y_discrete(labels=i18n) +
    g_labs(x=titlelize("week"), y=titlelize("symptom"), title=i18n('symptom_reported_by_part')) +
    guides(fill=guide_legend(i18n('percentage_of_participants')))
  g_save("symptom_prop", plot=TRUE, width=7, height = 6)  
  
  ggplot(dd, aes(x=monday_of_week(yw), y=name, fill=100*value/person_with_sympt)) +
    geom_tile() +
    scale_sympt_freq() +
    scale_y_discrete(labels=i18n) +
    scale_x_date(date_labels=date_format) +
    g_labs(x=titlelize("week"), y=titlelize("symptom"), title=i18n('symptom_reported_by_part_with_sympt')) +
    guides(fill=guide_legend(i18n('percentage_of_participants')))
  g_save("symptom_prop_with_symptom", plot=TRUE, width=6, height = 8)  
  
  
  ggplot(dd, aes(x=name, y=100*value/person_with_sympt, fill=100*value/person_with_sympt)) +
    geom_bar(stat="identity") +
    facet_grid(cols=vars(yw), labeller=labeller(yw=week_format)) +
    scale_x_discrete(labels=i18n) +
    scale_sympt_freq() +
    coord_flip() + 
    theme_with("x_vertical") +
    theme(axis.text = element_text(size=5)) +
    g_labs(x=titlelize("symptom"), y=i18n('percentage_of_participants'), title=i18n('symptom_reported_by_part_with_sympt')) +
    guides(fill=guide_legend(i18n('percentage_of_participants')))
  g_save("symptom_bar_prop_with_symptom", plot=TRUE, width=7, height = 7)  
  
  d = data %>% filter(yw >= short.term)
  if(nrow(d) > 0) {
    # Short term graph
    ggplot(d, aes(x=monday_of_week(yw), y=name, fill=100*value/person_with_sympt)) +
      geom_tile() +
      scale_y_discrete(labels=i18n) +
      scale_x_date(date_labels=date_format) +
      scale_sympt_freq() +
      g_labs(x=titlelize("week"), y=titlelize("symptom"), title=paste0(i18n('symptom_reported_by_part_with_sympt'), short.term.title)) +
      guides(fill=guide_legend(i18n('percentage_of_participants')))
    g_save("symptom_prop_with_symptom_shortterm", plot=TRUE, width=6, height = 14)  
    
    ggplot(d, aes(x=name, y=100*value/person_with_sympt, fill=100*value/person_with_sympt)) +
      geom_bar(stat="identity") +
      scale_x_discrete(labels=i18n) +
      facet_grid(cols=vars(yw), labeller=labeller(yw=week_format)) +
      scale_sympt_freq() +
      coord_flip() + 
      theme_with("x_vertical") + theme(axis.text = element_text(size=5)) +
      g_labs(x=titlelize("symptom"), y=i18n('percentage_of_participants'), title=paste0(i18n('symptom_reported_by_part_with_sympt'),", ", short.term.title)) +
      guides(fill=guide_legend(i18n('percentage_of_participants')))
    g_save("symptom_bar_prop_with_symptom_shortterm", plot=TRUE, width=8, height = 14)  
  }
}

context$push()

context$set(subject="syndromes")

data = data.all$syndromes %>% filter(yw >= min.week)

if(nrow(data) > 0) {
  context$set(syndrome_group="ifn")
  ggplot(data, aes(x=monday_of_week(yw), y=name, fill=100 * value / person)) +
    geom_tile() +
    scale_x_date(date_labels=date_format) +
    scale_y_discrete(labels=i18n) +
    scale_sympt_freq() +
    g_labs(x=titlelize("week"), y=titlelize("syndrome"), title=i18n("syndrom_ifn_reported_by_part")) +
    guides(fill=guide_legend(i18n('percentage_of_participants')))
  g_save("syndrome_prop", plot=TRUE, width=7, height = 6)  
}

context$set(syndrome_group="covid")

data = data.all$syndromes.covid %>% filter(yw >= min.week)
if(nrow(data) > 0) {
  ggplot(data, aes(x=monday_of_week(yw), y=name, fill=100 * value / person)) +
    geom_tile() +
    scale_x_date(date_labels=date_format) +
    scale_y_discrete(labels=i18n) +
    scale_sympt_freq() +
    g_labs(x=titlelize("week"), y=titlelize("syndrome"), title=i18n("syndrom_ifncovid_reported_by_part")) +
    guides(fill=guide_legend(i18n('percentage_of_participants')))
  g_save("syndrome-covid_prop", plot=TRUE, width=7, height = 6)  
}

data = data.all$syndromes.ecdc %>% filter(yw >= min.week)
if(nrow(data) > 0) {
  
  context$set(syndrome_group="ecdc")
  
  ggplot(data, aes(x=monday_of_week(yw), y=name, fill=100 * value/person)) +
    geom_tile() +
    scale_x_date(date_labels=date_format) +
    scale_y_discrete(labels=i18n) +
    scale_sympt_freq() +
    g_labs(x=titlelize("week"), y=titlelize("syndrome"), title=i18n("syndrom_ecdc_reported_by_part")) +
    guides(fill=guide_legend(i18n('percentage_of_participants')))
  g_save("syndrome-ecdc_prop", plot=TRUE, width=7, height = 6)  

  d1 = data.all$syndromes.covid %>% mutate(name=gsub(".covid", "", name, fixed=TRUE))
  data = bind_rows(covid=d1, ecdc=data, .id="set")
  
  if(nrow(data) > 0) {
    ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=100 *value/person, color=set)) +
      geom_line() +
      scale_color_discrete(labels=c('ecdc'=i18n("with_sudden"), "covid"=i18n("without_sudden"))) +
      scale_x_date(date_labels=date_format) +
      scale_y_discrete(labels=i18n) +
      facet_grid(cols=vars(name), scales="free_y", labeller = labeller(name=i18n)) +
      theme_with("x_vertical") +
      g_labs(x=titlelize("week"), y=titlelize("syndrome"), title=i18n("syndrom_ecdc_covid_reported_by_part")) +
      guides(fill=guide_legend(i18n('percentage_of_participants')))
    g_save("syndrome-covid-ecdc_prop", plot=TRUE, width=10, height = 6)  
  }

}

context$pop()

context$push()

data = data.all$symptom_groups %>% filter(yw >= short.term)

context$set(subject="symptoms")

if(nrow(data) > 0) {
  
  opts=list(
    matrix=list(label=titlelize("symptoms"), point.color=colors$primary,  line.color=colors$primary, set.labels=i18n, point.size=2.5), 
    freq=list(label=titlelize("frequency"), bar.fill=colors$primary) 
  )
  
  symptoms.groups = lapply(symptoms.groups, function(n) {  n[n %in% symptoms ]})
  overall = data %>% group_by(g) %>% summarize(count=sum(n_person))
  overall = get_labels_from_binary(overall, mask=symptoms.mask, group = "g")

  upset_plot(overall, symptoms, n.max=120, title=paste0("Symptom associations for the last ", short.term.size, " weeks"), opts=opts)
  g_save("symptom_upset_shortterm", plot=TRUE, width=17, height = 6)  
  
  data = create_binary_groups(overall, symptoms.groups)
  opts$matrix$label = "Symptom group"
  upset_plot(data, sets=names(symptoms.groups), title=paste0("Grouped symptoms associations for the last ", short.term.size, " weeks"), opts=opts)
  g_save("grouped_symptom_upset_shortterm", plot=TRUE, width=14, height = 6)  
}

context$pop()

data = data.all$participants_date %>% filter(yw >= min.week)

if(nrow(data) > 0) {
  
  context$set(subject="day_if_participation")
  
  days = c('7'="monday",'6'='tuesday','5'='wednesday', '4'='thursday','3'='friday','2'='saturday','1'='sunday')
  
  data = data %>% mutate(
    day =  8 - ifelse(wday == 0, 7, wday)
  )
  
  data$day = factor(data$day, names(days), days)
  
  ggplot(data, aes(x=monday_of_week(yw), y=day, fill=100 * n_person/total_person)) +
    geom_tile() +
    scale_x_date(date_labels=date_format) +
    scale_y_discrete(labels=i18n) +
    scale_fill_viridis_c(direction = -1, option = "A") +
    g_labs(x=titlelize("week"), y=titlelize("week"), title=i18n("participant_week_by_weekday")) +
    guides(fill=guide_legend(i18n('percentage_of_participants')))
  g_save("week_participant_prop", plot=TRUE, width=7, height = 6)  
  
}

questions = attr(data.all, "questions")

for(question in questions) {
  data = data.all[[question$name]]
  name = question$name
  
  context$set(subject=name)
  
  if(!is.null(data)) {
    data = data %>% filter(total > 0)
  }
  
  if(is.null(data) || nrow(data) == 0) {
    cat("No data for", question$name,"\n")
    next()
  }

  ggplot(data, aes(x=monday_of_week(yw), y=round(count/total * 100, 2))) +
    geom_bar(stat = "identity", fill = colors$primary) +
    g_labs(y=i18n('percentage'), x=i18n('week'), title=i18n(name)) +
    facet_wrap(. ~ variable, labeller=labeller(variable=i18n))
  g_save(name, "_percent", width=14, height=6)
  
  ggplot(data, aes(x=monday_of_week(yw), y=count)) +
    geom_bar(stat = "identity", fill = colors$primary) +
    g_labs(y=i18n("count"), x=i18n('week'), title=i18n(name)) +
    facet_wrap(.~variable, labeller=labeller(variable=i18n))
  g_save(paste0(name, "_value"), width=14, height=6) 

}

## Symptoms and Symptom causes
data = data.all$symptom_causes

context$set(subject="symptom_cause")

if(nrow(data) > 0) {

  data$sympt.cause = survey_recode(data$sympt.cause, "sympt.cause", "weekly")
  
  sets = attr(data.all, "syndromes")
  sets$symptoms = structure(attr(data.all, "symptoms"), title="Symptoms")
  
  #use.causes = c('cause.ili','cause.cold','cause.covid')
  
  for(i in seq_along(sets)) {
    name = names(sets[i])
    columns = sets[[i]]
    title = attr(sets[[i]], "title")
    ww = data %>% select(yw, sympt.cause, !!!syms(columns)) %>% filter(!is.na(sympt.cause))
    ww = ww %>% filter(yw >= min.week) 
    
    if(nrow(ww) == 0) {
      message(paste("No data for set", name))
      next()
    }
    
    ww$sympt.cause = factor(ww$sympt.cause)
    ww = tidyr::pivot_longer(ww, all_of(columns))
    ww = data.frame(ww)
  
    g = ggplot(ww, aes(x=monday_of_week(yw), y=value, color=sympt.cause, group=sympt.cause)) + 
      geom_line() + 
      g_labs(title=paste0("Symptoms self-reported cause vs  ", title), x="Week", y="Count") 
    
      g = g  + facet_wrap(~name, scales="free_y") 
    
    g_save(paste0("symptcause-weekly-", name), plot=TRUE, width=14, height = 8)  

    
    # Radar plot 
    d = ww %>% filter(yw >= short.term) %>% group_by(sympt.cause, name) %>% summarise_at("value", sum)
    if(nrow(d) > 0) {
      d = d %>% ungroup() %>% group_by(name) %>% mutate(total=sum(value))
      ggplot(d, aes(x=name, y=100*value/total, color=sympt.cause, group=sympt.cause)) + 
        geom_line() + 
        coord_radar() +
        g_labs(y=i18n("percentagee"), x=i18n(name), title=paste0(i18n("percentage_of_cause_for_group"),", ", short.term.title) ) +
        guides(color=guide_legend(i18n("sympt.cause")))
      g_save(paste0("symptcause-weekly-", name), plot=TRUE, width=14, height = 8)  
    }    
  }
}



