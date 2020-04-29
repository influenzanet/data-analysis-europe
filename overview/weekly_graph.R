source("conf.R")

library(dplyr)
library(gridExtra)
library(ggplot2)
suppressPackageStartupMessages(library(cowplot))

share.lib('upset')

season = get_current_season()

# Population by country
pop = load_population("country", 2018)

min.week = season * 100 + 51

short.term.size = 4
short.term = iso_yearweek(Sys.Date() - short.term.size * 7)

init.path(season)

data.all = readRDS(my.path("weekly_syndromes.Rds"))

init.path(paste0(season,'/weekly'))

symptoms = attr(data.all, "symptoms") 
symptoms.mask = attr(data.all, "symptoms.mask") 

# Graph save shortcut
g_save = function(...,  desc=NULL, plot=FALSE, width=12, height = 8) {
  file = out_path(..., ".pdf", plot=plot, desc=desc)
  cat(basename(file), "\n")
  ggsave(file, width=width, height = height)  
}

large_breaks = function(range) {
  scales::cbreaks(range, scales::pretty_breaks(10))$breaks
}

# Local function to add season to title & use automatic caption
g_labs = function(title, subtitle=NULL, ...) {
  if(is.null(subtitle)) {
    subtitle = paste0("InfluenzaNet, all countries, season ", season, " / ", (season + 1))
  }
  ifn_labs(title=title, subtitle=subtitle, ...)
}

ww = sort(unique(data.all$syndromes$yw))
ww = ww[ ww >= short.term]
short.term.period = paste("weeks", paste(format_week(range(ww), sep='w'), collapse = " to "))

colors = get_platform_colors()

countries = unique(data.all$syndromes$country)

pop = pop %>% filter(country %in% countries)
pop = pop %>% mutate(pop_total=sum(all)) %>% rename(pop_country=all)

# Compute weekly weight for each country base on their total population & 
part = data.all$participants_week %>% select(yw, country, participant=n_person) 
part = left_join(part, pop[, c('country','pop_country','pop_total')], by='country')
part = part %>% group_by(yw) %>% 
          mutate(
            participant_total=sum(participant),
          ) %>%
          ungroup()
# Ratio of expected participants if distribution has to follow pop ratio at european level
# Weight is ration expected / observed participants
part = part %>% mutate(
        expect_person= participant_total * (pop_country/pop_total),
        weight = expect_person / participant
      ) %>%
      group_by(yw) %>% 
        mutate(total_weight=sum(weight)) %>%
      arrange(yw, country)
part = data.frame(part)

# Graph of country weight
ggplot(part, aes(x=monday_of_week(yw), y=weight, color=country)) + 
  geom_line() + facet_grid(rows=vars(country), scales="free_y")

weights = part %>% select(country, yw, weight, total_weight)


# graph height
height = length(countries) * 1.75

data = data.all$rossman

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=mean, group=country, color=country)) +
  geom_line() +
  facet_grid(rows=vars(country), scales="free_y") +
  scale_color_brewer(palette="Dark2") +
  g_labs(title="Symptoms ratio from Rossman H et al. 2019")
g_save("symptom_ratio", plot=TRUE, width=12, height = 8)  

data = data.all$symptoms

scale_sympt_freq = function() {
  scale_fill_viridis_c(direction = -1, option = "A", na.value="grey90", breaks=large_breaks)
}

na.color = "grey90"

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=100 * value/person)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_sympt_freq() +
  g_labs(x="Week", y="Symptom", title="% of symptom reported by participants") +
  guides(fill=guide_legend("% of Participants"))
g_save("symptom_prop", plot=TRUE, width=6, height = height)  

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=100 * value / person_with_sympt)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_sympt_freq() +
  g_labs(x="Week", y="Symptom", title="% of symptom reported by participants (with at least 1 symptom)") +
  guides(fill=guide_legend("% of Participants (with symptom)"))
g_save("symptom_prop_with_symptom", plot=TRUE, width=6, height = height)  

ggplot(data %>% filter(yw >= min.week), aes(x=name, y=100 * value / person_with_sympt, fill=100 * value / person_with_sympt)) +
  geom_bar(stat="identity") +
  facet_grid(rows=vars(country), cols=vars(yw)) +
  scale_sympt_freq() +
  coord_flip() + theme_with("x_vertical") +
  theme(axis.text = element_text(size=5)) +
  g_labs(x="Symptom", y="% of participants of the week", title="% of symptom reported by participants with at least 1 symptom") +
  guides(fill=guide_legend("% of Participants (with symptom)"))
g_save("symptom_bar_prop_with_symptom", plot=TRUE, width=7, height = height)  

# Short term graph
ggplot(data %>% filter(yw >= short.term), aes(x=monday_of_week(yw), y=name, fill=100*value/person_with_sympt)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_sympt_freq() +
  g_labs(x="Week", y="Symptom", title=paste("% of symptom reported by participants"), subtitle=short.term.period) +
  guides(fill=guide_legend("% of Participants"))
g_save("symptom_prop_with_symptom_shortterm", plot=TRUE, width=5, height = height)  

ggplot(data %>% filter(yw >= short.term), aes(x=name, y=100*value/person_with_sympt, fill=100*value/person_with_sympt)) +
  geom_bar(stat="identity") +
  facet_grid(rows=vars(country), cols=vars(yw)) +
  scale_sympt_freq() +
  coord_flip() + theme_with("x_vertical") +
  theme(axis.text = element_text(size=5)) +
  g_labs(x="Symptom", y="% of participants of the week", title="% of symptom reported by participants with at least 1 symptom", subtitle=short.term.period) +
  guides(fill=guide_legend("% of Participants"))
g_save("symptom_bar_prop_with_symptom_shortterm", plot=TRUE, width=4.5, height = height)  

data = data.all$syndromes

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=100*value/person)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_sympt_freq() +
  g_labs(x="Week", y="Syndromes", title="% of syndromes reported by participants, Influenzanet syndromes set, by week") +
  guides(fill=guide_legend("% of Participants"))
g_save("syndrome_prop", plot=TRUE, width=6, height = height )  

data = data.all$syndromes.covid

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=100*value/person)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_sympt_freq() +
  g_labs(x="Week", y="Syndromes", title="% of Influenzanet syndromes (without sudden) reported by participants") +
  guides(fill=guide_legend("% of Participants"))
g_save("syndrome-covid_prop", plot=TRUE, width=6, height = height)  

data = data.all$syndromes.ecdc

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=100*value/person)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_sympt_freq() +
  g_labs(x="Week", y="Syndromes", title="% of Influenzanet syndromes reported by participants, ECDC syndromes set") +
  guides(fill=guide_legend("% of Participants"))
g_save("syndrome-ecdc_prop", plot=TRUE, width=6, height = height)  

d1 = data.all$syndromes.covid
d1$name = gsub(".covid", "", d1$name, fixed=TRUE)
d2 = data.all$syndromes.ecdc
data = bind_rows(covid=d1, ecdc=d2, .id="set")

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=100*value/person, color=set)) +
  geom_line() +
  scale_color_discrete(labels=c('ecdc'="With sudden (ECDC)", "covid"="Without sudden")) +
  facet_grid(rows=vars(country), cols=vars(name), scales="free_y") +
  g_labs(x="Week", y="Syndromes", title="% of Influenzanet syndromes (ECDC & without-sudden sets) reported by participants") +
  guides(fill=guide_legend("% of Participants")) + theme_with("x_vertical")
g_save("syndrome-covid-ecdc_prop", plot=TRUE, width=14, height = 12)  

## Symptoms and Symptom causes
data = data.all$symptom_causes

data$sympt.cause = survey_recode(data$sympt.cause, "sympt.cause", "weekly")

sets = attr(data.all, "syndromes")
sets$symptoms = structure(attr(data.all, "symptoms"), title="Symptoms")

use.causes = c('cause.ili','cause.cold','cause.covid')

for(i in seq_along(sets)) {
  name = names(sets[i])
  columns = sets[[i]]
  title = attr(sets[[i]], "title")
  ww = data %>% select(yw, country, sympt.cause, !!!syms(columns)) %>% filter(!is.na(sympt.cause))
  ww = ww %>% filter(yw >= min.week) 
  
  if(name != "ifn") {
    ww =  ww %>% filter(sympt.cause %in% use.causes)
  }
  
  ww$sympt.cause = factor(ww$sympt.cause)
  ww = tidyr::pivot_longer(ww, columns)
  ww = data.frame(ww)
  
  # For all countries
  ww = left_join(ww, weights, by=c('yw','country'))
  
  wg = ww %>% 
        group_by(yw, sympt.cause, name) %>% 
        summarise(
            value=sum(value), 
            weighted=sum(value*weight, na.rm=TRUE), 
            total_w=min(total_weight)
        ) %>%
        mutate(weighted=weighted/total_w)

  ggplot(wg, aes(x=monday_of_week(yw), color=sympt.cause, group=sympt.cause)) + 
    geom_line(aes(y=value, linetype="value")) +
    geom_line(aes(y=weighted, linetype="weighted")) +
    facet_grid(cols=vars(name), rows=vars(sympt.cause), scales="free_y") +
    scale_linetype_manual(values=c('value'='solid',"weighted"="dashed")) +
    g_labs(title=paste0("Symptoms self-reported cause vs ", title), subtitle="Influenzanet, all countries", x="Week", y="Count") 
    
  g_save(paste0("symptcause-weekly-",name,"-europe-weighted"), plot=TRUE, width=14, height = 8)  

  ggplot(ww, aes(x=monday_of_week(yw), y=value, color=sympt.cause, group=sympt.cause)) + 
    geom_line() + 
    facet_grid(rows=vars(country), cols=vars(name), scales="free_y") +
    g_labs(title=paste0("Symptoms self-reported cause vs  ", title), subtitle="Influenzanet, all countries", x="Week", y="Count") 
  
  g_save(paste0("symptcause-weekly-",name,"-by_country"), plot=TRUE, width=14, height = 8)  
  
  wg = ww %>% 
        filter(yw >= short.term) %>%
        group_by(country, sympt.cause, name) %>%
        summarise(value=sum(value))
  
  wg = wg %>% group_by(country, sympt.cause) %>% mutate(total_cause=sum(value)) %>% ungroup()
  wg = wg %>% group_by(country, name) %>% mutate(total_syndrome=sum(value)) %>% ungroup()
  
  width = length(unique(wg$sympt.cause)) * length(unique(wg$name)) * .25
  
  ggplot(wg, aes(x=sympt.cause, y=100*value/total_syndrome, fill=sympt.cause)) + 
    geom_bar(stat="identity") + 
    facet_grid(rows=vars(country), cols=vars(name), scales="free_y") +
    theme_with('x_vertical') +
    g_labs(title=paste0("% Symptoms self-reported cause vs ", title, " by country"), subtitle=paste("Influenzanet, all countries", short.term.period), x="Symptom cause", y=paste("Percentage of ",name,"set")) 
  g_save(paste0("symptcause-freq-",name,"-by_cause_country"), plot=TRUE, width=width, height = 8)  
  
  ggplot(wg, aes(x=name, y=100*value/total_cause, fill=name)) + 
    geom_bar(stat="identity") + 
    facet_grid(rows=vars(country), cols=vars(sympt.cause), scales="free_y") +
    theme_with('x_vertical') +
    g_labs(title=paste0("Symptoms self-reported cause vs ", title, " by country"), subtitle=paste("Influenzanet, all countries,", short.term.period), x="Week", y="Percentage of reported cause") 
  g_save(paste0("symptcause-freq-",name,"-by_syndrome_country"), plot=TRUE, width=width, height = 8)  
  
  wg = wg %>% select(-country) %>% group_by(sympt.cause, name) %>% summarise_all(sum)
  
  ggplot(wg, aes(x=sympt.cause, y=100*value/total_syndrome, fill=sympt.cause)) + 
    geom_bar(stat="identity") + 
    facet_grid(cols=vars(name), scales="free_y") +
    theme_with('x_vertical') +
    g_labs(title=paste0("% Symptoms self-reported cause vs ", title), subtitle=paste("Influenzanet, all countries", short.term.period,", non weighted"), x="Symptom cause", y=paste("Percentage of ",name,"set")) 
  g_save(paste0("symptcause-freq-",name,"-europe-by_cause"), plot=TRUE, width=width, height = 8)  
  
  ggplot(wg, aes(x=name, y=100*value/total_cause, fill=name)) + 
    geom_bar(stat="identity") + 
    facet_grid(rows=vars(sympt.cause), scales="free_y") +
    theme_with('x_vertical') +
    g_labs(title=paste0("Symptoms self-reported cause vs ", title), subtitle=paste("Influenzanet, all countries,", short.term.period,", non weighted"), x="Week", y="Percentage of reported cause") 
  g_save(paste0("symptcause-freq-",name,"-europe-by_syndrome"), plot=TRUE, width=length(unique(wg$name))*.4, height = 8)  
  
  
}

## Day of week of provided data

data = data.all$participants_date
data = data %>% filter(yw >= min.week)
days = c('7'="Monday",'6'='Tuesday','5'='Wednesday', '4'='Thursday','3'='Friday','2'='Saturday','1'='Sunday')

data = data %>% mutate(
  day =  8 - ifelse(wday == 0, 7, wday)
)

ggplot(data, aes(x=monday_of_week(yw), y=factor(day), fill=100*n_survey/total_survey)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_y_discrete(labels=days) +
  scale_fill_viridis_c(direction = -1, option = "A" ) +
  g_labs(x="Week", y="Day of week", title="% of surveys by week and weekday, by date of first report of the week") +
  guides(fill=guide_legend("% of Participants"))
g_save("week_survey_prop", plot=TRUE, width=6, height = height)  


## Symptoms association

data = data.all$symptom_groups

overall = data %>% filter(yw >= short.term)
overall = overall %>% group_by(g) %>% summarize(count=sum(n_person))
overall = get_labels_from_binary(overall, mask=symptoms.mask, group = "g")

opts=list(
  matrix=list(label=titlelize("symptoms"), point.color=colors$primary,  line.color=colors$primary, point.empty="gray83"), 
  freq=list(label="Frequency", bar.fill=colors$primary) 
)

n.max = 150
upset_plot(overall, symptoms, n.max=n.max, title=paste0(n.max, " most frequent symptom associations, ", short.term.period), caption=ifn.copyright(FALSE), subtitle="Influenzanet, all countries", opts=opts)
g_save("symptom_upset_shortterm", plot=TRUE, width=16, height = 6)  

data = create_binary_groups(overall, symptoms.groups)
opts$matrix$label = "Symptoms group"
opts$matrix$point.size = 4
opts$point.empty="gray60"
upset_plot(data, sets=names(symptoms.groups), title=paste0("Grouped symptoms associations, ", short.term.period), caption=ifn.copyright(FALSE), subtitle="Influenzanet, all countries", opts=opts)
g_save("grouped_symptom_upset_shortterm", plot=TRUE, width=13, height = 6)  

questions = attr(data.all, "questions")

for(question in questions) {
  data = data.all[[question$name]]
  name = question$name
  if(is.null(data)) {
    cat("No data for", question$name,"\n")
  }
  
  data = left_join(data, weights, by=c('yw','country'))
  
  wg = data %>% 
        select(-country) %>% 
        group_by(yw, variable) %>% 
        summarize(
            count=sum(count), 
            total=sum(total), 
            weighted=sum(count*weight), 
            total_weight=min(total_weight)
        ) %>%
        mutate(weighted=weighted/total_weight)
 
  ggplot(wg, aes(x=monday_of_week(yw), y=round(count/total * 100, 2))) +
    geom_bar(stat = "identity", fill = colors$primary) +
    g_labs(y=i18n('percentage'), x=i18n('week'), title=question$name) +
    facet_wrap(. ~ variable, labeller=labeller(variable=i18n))
  g_save(paste0(name, "-europe-percent"), width=10, height=10) 

  ggplot(wg, aes(x=monday_of_week(yw))) +
    geom_line(aes(y=100 * count/total, linetype="value") ) +
    geom_line(aes(y=100 * weighted/total, linetype="weighted") ) +
    g_labs(y=i18n('percentage'), x=i18n('week'), title=question$name) +
    scale_linetype_manual(values=c('value'='solid',"weighted"="dashed")) +
    facet_wrap(. ~ variable, labeller=labeller(variable=i18n))
  g_save(paste0(name, "-europe-weighted-percent"), width=10, height=10) 
  
  ggplot(data, aes(x=monday_of_week(yw), y=round(count/total * 100, 2), color=variable)) +
    geom_line() +
    g_labs(y=i18n('percentage'), x=i18n('week'), title=question$name) +
    facet_grid(rows=vars(country))
  g_save(paste0(name, "-weekly-percent-By_country"), width=10, height=10) 

}
