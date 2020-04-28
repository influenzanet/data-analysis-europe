source("conf.R")

library(dplyr)
library(gridExtra)
library(tidyr)
library(ggplot2)
suppressPackageStartupMessages(library(cowplot))

share.lib("incidence")
share.lib("upset")

season = get_current_season()

min.week = season * 100 + 51

short.term.size = 4
short.term = iso_yearweek(Sys.Date() - short.term.size * 7)

caption = ifn.copyright # get then caption graph 

eu.params = get_eu_incidence_parameters()

countries = platform_env("COUNTRY_CODES")
colors.web = platform_env("colors.web")

age.categories = eu.params$age.categories

syndrome.from = list(health.status=TRUE)
onset = episode_onset_design()
symptoms = get_symptoms_columns(season)

symptoms.mask = create_binary_mask(symptoms)

mesure.covid = survey_labels('weekly', "measures")
reason.covid = survey_labels('weekly', "reason.covid")
confin.work = survey_labels('weekly', "confin.work")
confinstop.work = survey_labels('weekly', "confinstop.work")

other.question = c(mesure.covid, reason.covid, confin.work, confinstop.work)

for(country in countries) {
  #country = countries[7]
  
  dataset = load_results_for_incidence(season=season, age.categories=age.categories,
                                       syndrome.from = syndrome.from, onset=onset,
                                       columns=list(keep.all=TRUE, weekly=c(symptoms, other.question)),
                                       country = country)
  
  
  if(is.null(dataset$weekly) || is.null(dataset$intake) || nrow(dataset$weekly) == 0 || nrow(dataset$intake) == 0) {
    cat("not data for", country, "\n")
    next()
  }
  
  data.week <- dataset$weekly
  
  list.other.question <- list(mesure = mesure.covid,
                              reason = reason.covid, 
                              confin = confin.work,
                              confinstop = confinstop.work)
 
    
  for(name in names(list.other.question)){
    #name = names(list.other.question)[1]
    
      variables = list.other.question[[name]]
      
      variable <- data.week[, c("yw", "id", variables)]
      variable <- variable[variable$yw >= 202010,]
      
      weekly.variable <- group_by(variable, yw) %>% 
                              summarize_at(variables, sum, na.rm = TRUE)
      
      answer.variable <- pivot_longer(variable, -c(yw, id), names_to = 'variable')
      answer.variable$nb <- !is.na(answer.variable$value)
      answer.variable <- group_by(answer.variable, yw, variable) %>% summarize(nb = sum(nb))
      
      
      weekly.variable <- pivot_longer(weekly.variable, -yw, names_to = 'variable')
      weekly.variable <- merge(weekly.variable, answer.variable)
      weekly.variable$wid <- week_stamp(weekly.variable$yw)
      
      ww <- unique(select(weekly.variable, wid, yw))
      
      ggplot(weekly.variable, aes(wid, round(value/nb * 100, 2))) +
                    geom_bar(stat = "identity", fill = colors.web$blue) +
                    ylab("Percentage") +
                    xlab("week") +
                    facet_wrap(.~variable) +
                    scale_x_wid(ww, week.sep = "w", breaks = "by", by = 2) +
                    ggtitle(country)
      ggsave(paste0(country,"_",name, "_percent.pdf")) 
      
      ggplot(weekly.variable, aes(wid, value)) +
                    geom_bar(stat = "identity", fill = colors.web$blue) +
                    ylab("Number") +
                    xlab("week") +
                    facet_wrap(.~variable) +
                    scale_x_wid(ww, week.sep = "w", breaks = "by", by = 2) +
                    ggtitle(country)
      ggsave(paste0(country,"_",name, "_value.pdf")) 
      
    
  }
  
  
}


                       