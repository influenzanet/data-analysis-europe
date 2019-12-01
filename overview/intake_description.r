# Description of registred participants
source('conf.R')

library(ggplot2)
library(dplyr)
library(swMisc)
library(rlang)

if(!exists("cli.args")) {
  cli.args = parseArgs(list(
    season=list(type="int", min=2011, max=calc_season(Sys.Date()), default=get_current_season() )
  ))
}

season = cli.args$season

graph.type = "pdf"

message(paste("Running season ", season))

theme_set(theme_minimal())

init.path(paste0('intake/', season))

out.path = function(x) {
  my.path(paste(season,'_', x, sep=''))
}

# Get list of columns for questions in survey
condition.columns = survey_labels('intake', 'condition')
hear.columns = survey_labels('intake', 'hear.about')
allergy.columns = survey_labels('intake', 'allergy')
pets.columns = survey_labels('intake', 'pets')

time.dependents = c('vacc.curseason', 'pregnant')

columns = c('timestamp','country', 'gender','date.birth','vacc.curseason',"main.activity","occupation", "often.ili", hear.columns,'transport','smoker', 'pregnant', condition.columns, allergy.columns, pets.columns)

intake = survey_load_results("intake", columns, season=season, debug=F)

# Load all survey of the season
# Complete with intake from past seasons if necessary (for ES and IT)
weekly = survey_load_results("weekly", c('timestamp','country'), season=season)

table(weekly$country[ !weekly$person_id %in% intake$person_id])

p = unique(weekly$person_id[!weekly$person_id %in% intake$person_id])
cat("Completing with intake from past seasons for ", length(p),"users\n")

def = season.def(season)

start.date = def$dates$start
ii = survey_load_results("intake", columns, survey.users=p, debug=F, date=list(max=start.date))

if( nrow(ii) > 0 ) {
  ii = keep_last_survey(ii)
  # Cancel time-dependent variables
  for(v in time.dependents) {
    ii[, v] = NA
  }
  intake = bind_rows(intake, ii)
}

intake$country = factor(intake$country)

rm(p, ii, weekly)

gc()

# Now keep only the last intake of each participant
intake = keep_last_survey(intake)

intake = recode_intake(intake)
intake$age[intake$age < 0] = NA

age.categories = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 200)
# age.categories = c(0, 20,  40, 60, 80, 200)

intake$age.cat = cut_age(intake$age, age.categories)

# Population by country, age-group & sex
pop = load_population_age(geo="country", age.breaks=age.categories, year=def$year.pop)

# Overall graph for Europe

season.title = paste("\nSeason ", season,"-", season + 1)

g_title = function(...) {
  labs(..., caption = ifn.copyright())
}

gg_barplot_percent(intake$age.cat, col="darkgreen", label.size =2) +
  g_title(title=paste("Participants by age-group (all countries)",season.title))
graph.save(out.path('europe-age'), width=8, height=6, type=graph.type)

gg_barplot_percent(intake$gender, col="darkgreen", label.size=2) +
  g_title(title=paste("Participants by gender (all countries)", season.title))
graph.save(out.path('europe-gender'), width=8, height=6, type=graph.type)

gg_barplot_percent(intake$vacc.curseason, col="darkgreen", label.size = 2) +
  labs(title=paste("Flu vaccination for the current season (all countries)",season.title))
graph.save(out.path('europe-vacc-curseason'), width=8, height=6, type=graph.type)

# Graph by country

# Create a frequency plot
freq_plot = function(column, trans=NULL, width, title, file=NULL, h=NA, data=intake, no.empty=FALSE, subtitle=NULL, ...) {
  if(length(column) > 1) {
    trans = if(is.null(trans)) TRUE else trans
    if(!is.logical(trans)) {
      stop("trans arg should be logical with multiple columns")
    }
    y = multiple_freq(data[, column], translate = trans)
  } else {
    if(!is.null(trans) ) {
      if( isTRUE(trans) ) {
        trans = column
      }
      y = i18n(y)
    } else {
      y = data[[column]]
    }
    if(no.empty) {
      y = factor(y)
    }
  }
  g = gg_barplot_percent(y, label.size = 2, ...) +
    g_title(title=i18n(title), x="", subtitle=subtitle, y=i18n('percentage'))
  h = ifelse(is.na(h), width / 1.618, h)
  file = if(is.null(file)) column else file
  graph.save(out.path(file), width=width, height=h, type=graph.type)
  g
}


freq_country = function(.data, name, trans=NULL) {
  freq = .data %>% group_by(country, !!sym(name)) %>% summarize(count=n()) %>% rename(var=!!name)
  freq = freq %>% group_by(country) %>% mutate(total=sum(count), prop=count/total)

  if(!is.null(trans) ) {
    if( isTRUE(trans) ) {
      trans = name
    }
    freq$var = survey_recode(freq$var, survey='intake', trans)
  }

  freq
}

plot_by_country = function(name, trans=NULL, title, x.rotate=90, x.vjust=NULL, file=paste0('country-', name), width=8) {
  freq = freq_country(intake, name=name, trans=trans)
  g = ggplot(freq, aes(x=var, fill=country, y=count)) +
      geom_bar(stat="identity", position="dodge") +
      scale_fill_brewer(palette="Dark2") +
      g_title(title=i18n(title))

  if( !is.na(x.rotate) ) {
    g = g + theme(axis.text.x=element_text(angle=x.rotate, vjust = x.vjust))
  }

  if( !is.null(file) ) {
    h = width / 1.68
    graph.save(out.path(file), width=width, height=h, type=graph.type)
  }

  g
}

freq_plot('main.activity', trans="activities", title='graph_main_activity', width=8, file="europe-activity")

freq_plot('transport', trans=TRUE, title='graph_transport', width=8, file="europe-transport")

plot_by_country("transport", trans=TRUE, title='graph_transport')

freq_plot('often.ili', trans=TRUE, title='graph_often.ili', width=8, file="europe-transport")

plot_by_country("often.ili", trans=TRUE, title='graph_often.ili')

freq_plot('smoker', trans=TRUE, title='graph_smoker', width=8, file="europe-smoker")

plot_by_country("smoker", trans=TRUE, title='graph_smoker')

freq_plot('pregnant', trans=NULL, title='graph_pregnant', width=8, file="europe-pregnant")

plot_by_country("pregnant", trans=NULL, title='graph_pregnant')

g = freq_plot(condition.columns,  width=8, title="graph_condition", file="european-condition")

g = freq_plot(allergy.columns,  width=8, title="graph_allergy", file="european-allergy")

pop.age = rbind(
  data.frame(age.cat=pop$age.cat, country=pop$country, count=pop$male, gender="male"),
  data.frame(age.cat=pop$age.cat, country=pop$country, count=pop$female, gender="female")
)

pop.age$pop = factor("pop")

intake.age = intake %>%
              group_by( age.cat, country, gender) %>%
              summarize(count=n()) %>%
              mutate(pop=factor("cohort"))

ages = bind_rows(intake.age, pop.age)

ages = ages %>% group_by(pop, country) %>% mutate(prop=count / sum(count))

# Expression to select female rows
q_female = quo(gender == "female")

europe.ages = ages %>%
          group_by(gender, age.cat, pop) %>%
          summarize(count=sum(count))

europe.ages = europe.ages %>% group_by(gender, pop) %>% mutate(prop=count/sum(count))

g = plot_age_pyramid(europe.ages, female=q_female, w=.5)
graph.save(out.path('age-pyramid-eu-overall.pdf'), width=6, height=5)

countries = levels(intake$country)
for(v in countries) {
  g = plot_age_pyramid(ages[ ages$country == v,], female=q_female) + ggtitle(paste(v, "- Influenzanet by age-group"))
  graph.save(out.path(paste0('age-pyramid-',v)), width=6, height=5, type=graph.type)
}

g = plot_age_pyramid(ages, female=q_female) + facet_grid(~country)
graph.save(out.path("age-pyramid-eu-country"), width=20, height=5, type=graph.type)

intake.gender = intake.age %>%
            group_by(country, gender) %>%
            summarize(count=sum(count)) %>%
            ungroup() %>%
            group_by(country) %>% 
            mutate(total=sum(count))
            

ggplot(intake.gender, aes(x=country, fill=gender, y=count/total)) +
  geom_bar(stat="identity", position="dodge") +
  ylab("% of participant count") +
  g_title("Gender by country")

graph.save(out.path("gender-country.pdf"), width=20, height=8)


