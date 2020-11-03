source("conf.R")

library(dplyr)
library(gridExtra)
library(tidyr)
library(ggplot2)
suppressPackageStartupMessages(library(cowplot))

share.lib("incidence")
share.lib("upset")

season = get_current_season()

# Graph save shortcut
g_save = function(...,  desc=NULL, plot=FALSE, width=12, height = 8) {
  file = out_path(..., ".pdf", plot=plot, desc=desc)
  cat(basename(file), "\n")
  ggsave(file, width=width, height = height)  
}

# Local function to add season to title & use automatic caption
g_labs = function(title, subtitle=NULL, ...) {
  if(is.null(subtitle)) {
    subtitle = paste0("InfluenzaNet, all countries, season ", season, " / ", (season + 1))
  }
  ifn_labs(title=title, subtitle=subtitle, ...)
}


min.week = 202010

short.term.size = 4
short.term = iso_yearweek(Sys.Date() - short.term.size * 7)

caption = ifn.copyright # get then caption graph 

eu.params = get_eu_incidence_parameters("default")

countries = platform_env("COUNTRY_CODES")
colors.web = platform_env("colors.web")

age.categories = eu.params$age.categories

syndrome.from = list(health.status=TRUE)
onset = episode_onset_design()
symptoms = get_symptoms_columns(season)


for(country in countries) {
  
  country_name = i18n(tolower(paste0('country_', country)))
  
  dataset = load_results_for_incidence(season=season, age.categories=age.categories,
                                       syndrome.from = syndrome.from, onset=onset,
                                       columns=list(keep.all=TRUE, weekly=c(symptoms, other.question)),
                                       country = country)

  if(is.null(dataset$weekly) || is.null(dataset$intake) || nrow(dataset$weekly) == 0 || nrow(dataset$intake) == 0) {
    cat("not data for", country, "\n")
    next()
  }
  
  data.week <- dataset$weekly
  
 

  for(name in names(list.other.question)){

      variables = list.other.question[[name]]
      
      ww <- dataset$weekly %>% select(yw, id, !!!variables) %>% filter(yw >= min.week)
      
      weekly.variable <- ww %>% group_by(yw) %>% summarize_at(variables, sum, na.rm = TRUE)
      weekly.variable <- pivot_longer(weekly.variable, -yw, names_to = 'variable')
      
      answer.variable <- ww %>% group_by(yw) %>% summarize_at(variables, ~sum(!is.na(.)))
      answer.variable <- pivot_longer(answer.variable, -yw, names_to = 'variable',values_to = 'nb')
      
      weekly.variable <- merge(weekly.variable, answer.variable, by=c('yw','variable'), all=TRUE)
      
      
      ggplot(weekly.variable, aes(x=monday_of_week(yw), y=round(value/nb * 100, 2))) +
          geom_bar(stat = "identity", fill = colors.web$blue) +
          g_labs(y=i18n('percentage'), x=i18n('week'), title=country_name) +
          facet_wrap(. ~ variable, labeller=labeller(variable=i18n))
      g_save(paste0(country,"_", name, "_percent")) 
      
      ggplot(weekly.variable, aes(x=monday_of_week(yw), y=value)) +
                    geom_bar(stat = "identity", fill = colors.web$blue) +
                    g_labs(y=i18n("count"), x=i18n('week'), title=country_name) +
                    facet_wrap(.~variable, labeller=labeller(variable=i18n))
      ggsave(paste0(country,"_",name, "_value.pdf")) 

  }
  
  
}


                       