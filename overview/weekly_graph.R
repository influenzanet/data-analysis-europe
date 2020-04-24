source("conf.R")

library(dplyr)
library(gridExtra)
library(ggplot2)
suppressPackageStartupMessages(library(cowplot))

share.lib('upset')

season = get_current_season()

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
g_save("syndrome_prop.pdf", plot=TRUE, width=6, height = height )  

data = data.all$syndromes.covid

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=100*value/person)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_sympt_freq() +
  g_labs(x="Week", y="Syndromes", title="% of Influenzanet syndromes (without sudden) reported by participants") +
  guides(fill=guide_legend("% of Participants"))
g_save("syndrome-covid_prop.pdf", plot=TRUE, width=6, height = height)  

data = data.all$syndromes.ecdc

ggplot(data %>% filter(yw >= min.week), aes(x=monday_of_week(yw), y=name, fill=100*value/person)) +
  geom_tile() +
  facet_grid(rows=vars(country)) +
  scale_sympt_freq() +
  g_labs(x="Week", y="Syndromes", title="% of Influenzanet syndromes reported by participants, ECDC syndromes set") +
  guides(fill=guide_legend("% of Participants"))
g_save("syndrome-ecdc_prop.pdf", plot=TRUE, width=6, height = height)  

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
g_save("syndrome-covid-ecdc_prop.pdf", plot=TRUE, width=14, height = 12)  

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
g_save("week_survey_prop.pdf", plot=TRUE, width=6, height = height)  

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