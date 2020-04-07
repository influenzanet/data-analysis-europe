## Weekly syndromes
#

source('conf.R')
library(dplyr)
library(swMisc)
library(ggplot2)

source("lib/graph.R", local=TRUE)

if(!exists("cli.args")) {
  cli.args = parseArgs(list(
    'season'=list(type="int", default=get_current_season()),
    'country'=list(type="choices", choices=platform_env("COUNTRY_CODES"))
  ))
}

country = cli.args$country
season = cli.args$season

data.path = paste0(create_path(prefixes = list(project='overview'), suffix = season), "weekly_syndromes.Rds")

if (!file.exists(data.path)) {
  rlang::abort("No data", class = "no_data")
}

data.all = readRDS(data.path)

# Keep only the country
for(n in ls(data.all)) {
  data.all[[n]] = data.all[[n]] %>% filter(country == !!country)
}

init.path(paste0(country, "/", season))

i18n_load("i18n/", language = tolower(country))

caption = i18n("platform.copyright")

min.week = season * 100 + 51

g_save = function(...,  desc=NULL, plot=FALSE, width=12, height = 8) {
  p = my.path(..., ".pdf")
  message(basename(p))
  desc_output(p, desc=desc, plot=plot)
  ggsave(p, width=width, height = height)  
}

data = data.all$symptoms

if(nrow(data) > 0) {
  ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=value/person)) +
    geom_tile() +
    scale_fill_viridis_c(direction = -1, option = "A" ) +
    labs(x=titlelize("week"), y=titlelize("symptom"), title=i18n('symptom_reported_by_part'), caption=caption) +
    guides(fill=guide_legend(i18n('percentage_of_participants')))
  g_save("symptom_prop", plot=TRUE, width=7, height = 6)  
}

data = data.all$syndromes

if(nrow(data) > 0) {
  ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=value/person)) +
    geom_tile() +
    scale_fill_viridis_c(direction = -1, option = "A" ) +
    labs(x=titlelize("week"), y=titlelize("syndrome"), title=i18n("syndrom_ifn_reported_by_part"), caption=caption) +
    guides(fill=guide_legend(i18n('percentage_of_participants')))
  g_save("syndrome_prop", plot=TRUE, width=7, height = 6)  
}

data = data.all$syndromes.covid
if(nrow(data) > 0) {
  ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=value/person)) +
  geom_tile() +
  scale_fill_viridis_c(direction = -1, option = "A" ) +
  labs(x=titlelize("week"), y=titlelize("syndrome"), title=i18n("syndrom_ifn_reported_by_part"), caption=caption) +
  guides(fill=guide_legend(i18n('percentage_of_participants')))
  g_save("syndrome-covid_prop", plot=TRUE, width=7, height = 6)  
}

data = data.all$syndromes.ecdc
if(nrow(data) > 0) {
  ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=value/person)) +
    geom_tile() +
    scale_fill_viridis_c(direction = -1, option = "A" ) +
    labs(x=titlelize("week"), y=titlelize("syndrome"), title=i18n("syndrom_ecdc_reported_by_part"), caption=caption) +
    guides(fill=guide_legend(i18n('percentage_of_participants')))
  g_save("syndrome-ecdc_prop", plot=TRUE, width=7, height = 6)  

  d1 = data.all$syndromes.covid %>% mutate(name=gsub(".covid", "", name, fixed=TRUE))
  data = bind_rows(covid=d1, ecdc=data, .id="set")
  
  if(nrow(data) ) {
    ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=value/person, color=set)) +
      geom_line() +
      scale_color_discrete(labels=c('ecdc'=i18n("with_sudden"), "covid"=i18n("without_sudden"))) +
      facet_grid(rows=vars(country), cols=vars(name), scales="free_y") +
      labs(x=titlelize("week"), y=titlelize("syndrome"), title=i18n("syndrom_ecdc_covid_reported_by_part"), caption=caption) +
      guides(fill=guide_legend(i18n('percentage_of_participants')))
    g_save("syndrome-covid-ecdc_prop", plot=TRUE, width=10, height = 6)  
  }

}

data = data.all$participants_date

if(nrow(data) > 0) {
  data = data %>% filter(yw >= min.week)
  days = c('7'="monday",'6'='tuesday','5'='wednesday', '4'='thursday','3'='friday','2'='saturday','1'='sunday')
  
  data = data %>% mutate(
    day =  8 - ifelse(wday == 0, 7, wday)
  )
  
  data$day = factor(data$day, names(days), days)
  
  ggplot(data, aes(x=monday_of_week(yw), y=day, fill=n_person/total_person)) +
    geom_tile() +
    facet_grid(rows=vars(country)) +
    scale_y_discrete(labels=i18n) +
    scale_fill_viridis_c(direction = -1, option = "A" ) +
    labs(x=titlelize("week"), y=titlelize("week"), title=i18n("participant_week_by_weekday"), caption=caption) +
    guides(fill=guide_legend(i18n('percentage_of_participants')))
  g_save("week_participant_prop", plot=TRUE, width=7, height = 6)  
  
}