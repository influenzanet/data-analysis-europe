# Description of registred participants
source('conf.R')

library(ggplot2)
library(dplyr)
library(swMisc)
library(rlang)
library(tidyr)

if(!exists("cli.args")) {
  cli.args = parseArgs(list(
    season=list(type="int", min=2011, max=calc_season(Sys.Date()), default=get_current_season() )
  ))
}

season = cli.args$season

message(paste("Running season ", season))

theme_set(theme_minimal())

init.path(paste0(season, '/intake'))

result_desc_filters(auto=TRUE, filters=list())

# Get list of columns for questions in survey
condition.columns = survey_labels('intake', 'condition')
hear.columns = survey_labels('intake', 'hear.about')
allergy.columns = survey_labels('intake', 'allergy')
pets.columns = survey_labels('intake', 'pets')
education.columns = survey_labels("intake","education")
diet.columns = survey_labels("intake", "diet")
time.dependents = c('vacc.curseason', 'pregnant')

columns = c('timestamp','country', 'gender','date.birth','vacc.curseason',"main.activity","occupation", "often.ili", hear.columns,'transport','smoker', 'pregnant', condition.columns, allergy.columns, pets.columns, education.columns, diet.columns)

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

# Load default translations 
i18n_load("../country/i18n/",language = "en")

season.title = paste("\nSeason ", season,"-", season + 1)

g_title = ifn_labs

context = ResultContext$new()

#' Save helper with default parameters
g_save=function(..., width, height, desc=NULL) {
  save_graph_with_context(paste0(...), formats=c('pdf','svg'), width=width, height=height, context=context, desc=desc)
}

context$push()

context$set(subject="age")

gg_barplot_percent(intake$age.cat, col="darkgreen", label.size =2) +
  g_title(title=paste("Participants by age-group (all countries)",season.title))
g_save('europe-age', width=8, height=6)

gg_barplot_percent(intake$gender, col="darkgreen", label.size=2) +
  g_title(title=paste("Participants by gender (all countries)", season.title))
g_save('europe-gender', width=8, height=6)

gg_barplot_percent(intake$vacc.curseason, col="darkgreen", label.size = 2) +
  g_title(title=paste("Flu vaccination for the current season (all countries)",season.title))
g_save('europe-vacc-curseason', width=8, height=6)

context$pop()

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
      y = i18n(data[[column]])
    } else {
      y = data[[column]]
    }
    if(no.empty) {
      y = factor(y)
    }
  }
  title = i18n(title)
  g = gg_barplot_percent(y, label.size = 2, ...) +
    g_title(title=title, x="", subtitle=subtitle, y=i18n('percentage'))
  h = ifelse(is.na(h), width / 1.618, h)
  file = if(is.null(file)) column else file
  g_save(file, width=width, height=h, desc=list(level="europe", title=title))
  g
}

#' Compute frequency by country
freq_country = function(.data, name, trans=NULL) {
  
  if(length(name) > 1) {
    freq = .data %>% 
              group_by(country) %>%
              summarise_at(name, sum)
    freq = tidyr::pivot_longer(freq, name)
    freq = rename(freq, var=name, count=value)
  } else {
    freq = .data %>% 
            group_by(country, !!sym(name)) %>% 
            summarize(count=n()) %>% 
            rename(var=!!name)
    freq = freq %>%
            group_by(country) %>% 
            mutate(total=sum(count), prop=count/total)
  }
  if(!is.null(trans) ) {
    freq$var = i18n(freq$var)
  }
  freq
}

#' Plot a by country frequency graph
plot_by_country = function(name, trans=NULL, title, x.rotate=90, x.vjust=NULL, file=paste0('country-', name), width=8, group="var", type="count") {
  freq = freq_country(intake, name=name, trans=trans)
  freq = freq %>% group_by(country) %>% mutate(total=sum(count))
  
  if(type == "count") {
    y = sym("count")
    ylab = "count"
  } else {
    y = expr(100 * count / total)
    ylab = "percentage"
  }
  
  title = i18n(title)
  g = ggplot(freq, aes(x=var, fill=country, y=!!y)) +
      geom_bar(stat="identity", position="dodge") +
      scale_fill_brewer(palette="Dark2") +
      g_title(title=title, y=i18n(ylab), x=i18n(name))
  
  height = width / 1.68
  
  if(group == "country") {
    g = g + facet_wrap(~country)
    height = width * 0.95 
  }
  
  if( !is.na(x.rotate) ) {
    g = g + theme(axis.text.x=element_text(angle=x.rotate, vjust = x.vjust))
  }

  if( !is.null(file) ) {
    if(group == "country") {
      title = paste(title, "by country")
    }
    desc = list(title=title, level="country")
    g_save(paste0(file,'-', type, if(group == "country") "-by_country"), desc=desc, width=width, height=height)
  }
  attr(g, "freqs") <- freq
  g
}

trans = TRUE

# Main Activity
context$set(subject="main.activity")
freq_plot('main.activity', trans="activities", title='graph_main_activity', width=8, file="europe-activity")
plot_by_country("main.activity", trans=trans, title='graph_main_activity', type="prop")
plot_by_country("main.activity", trans=trans, title='graph_main_activity', type="prop", group="country")

# Transports
context$set(subject="transport")
freq_plot('transport', trans=trans, title='graph_transport', width=8, file="europe-transport")
plot_by_country("transport", trans=trans, title='graph_transport', type="prop")
plot_by_country("transport", trans=trans, title='graph_transport', type="prop", group="country")

# How often ili
context$set(subject="often.ili")
freq_plot('often.ili', trans=trans, title='graph_often.ili', width=8, file="europe-transport")
plot_by_country("often.ili", trans=trans, title='graph_often.ili', type="prop")
plot_by_country("often.ili", trans=trans, title='graph_often.ili', type="prop", group="country")

# Smoker
context$set(subject="smoker")
freq_plot('smoker', trans=trans, title='graph_smoker', width=8, file="europe-smoker")
plot_by_country("smoker", trans=trans, title='graph_smoker', type="prop")
plot_by_country("smoker", trans=trans, title='graph_smoker', type="prop", group="country")

# Pregnant
context$set(subject="pregnant")
freq_plot('pregnant', trans=trans, title='graph_pregnant', width=8, file="europe-pregnant")
plot_by_country("pregnant", trans=trans, title='graph_pregnant', type="prop")
plot_by_country("pregnant", trans=trans, title='graph_pregnant', type="prop", group="country")

# Condition columns
context$set(subject="condition")
g = freq_plot(condition.columns,  width=8, title="graph_condition", file="european-condition")
plot_by_country(condition.columns, trans=trans, title='graph_condition', type="prop", file="country-condition")
plot_by_country(condition.columns, trans=trans, title='graph_condition', type="prop", group="country", file="country-condition")

# Allergies
context$set(subject="allergy")
g = freq_plot(allergy.columns,  width=8, title="graph_allergy", file="european-allergy")
plot_by_country(allergy.columns, trans=trans, title='graph_allergy', type="prop", file="country-allergy")
plot_by_country(allergy.columns, trans=trans, title='graph_allergy', type="prop", group="country", file="country-allergy")

# Education
context$set(subject="education")
g = freq_plot(education.columns,  width=8, title="graph_education", file="european-education")
plot_by_country(education.columns, trans=trans, title='graph_education', type="prop", file="country-education")
plot_by_country(education.columns, trans=trans, title='graph_education', type="prop", group="country", file="country-education")

# Pets
context$set(subject="pets")
g = freq_plot(pets.columns,  width=8, title="graph_pets", file="european-pets")
plot_by_country(pets.columns, trans=trans, title='graph_pets', type="prop", file="country-pets")
plot_by_country(pets.columns, trans=trans, title='graph_pets', type="prop", group="country", file="country-pets")

# Diet
context$set(subject="diet")
g = freq_plot(diet.columns,  width=8, title="graph_diet", file="european-diet")
plot_by_country(diet.columns, trans=trans, title='graph_diet', type="prop", file="country-diet")
plot_by_country(diet.columns, trans=trans, title='graph_diet', type="prop", group="country", file="country-diet")

pop.age = rbind(
  data.frame(age.cat=pop$age.cat, country=pop$country, count=pop$male, gender="male"),
  data.frame(age.cat=pop$age.cat, country=pop$country, count=pop$female, gender="female")
)

context$set(subject="age")

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

p_labs = function(title) {
  g_title(y=i18n("percentage"), x=i18n("age_group"), title=title)
}

g = plot_age_pyramid(europe.ages, female=q_female, w=.5)
g_save('age-pyramid-eu-overall', width=6, height=5, desc=list(level="europe", title="Age pyramid (all countries)"))

countries = levels(intake$country)

# Graph by country
context$push()
context$set(level="country")
for(v in countries) {
  g = plot_age_pyramid(ages[ ages$country == v,], female=q_female) + p_labs(title=paste(v, "- Influenzanet by age-group"))
  g_save(paste0('age-pyramid-',v), desc=paste("Age-gender population pyramid for country", v), width=6, height=5)
}

g = plot_age_pyramid(ages, female=q_female) + 
  facet_grid(~country) + 
  theme_with("x_vertical") + 
  p_labs(title="Age-gender population pyramid by country")
g_save("country-age-pyramid", desc="Age-gender population pyramid by country", width=20, height=5)

intake.gender = intake.age %>%
            group_by(country, gender) %>%
            summarize(count=sum(count)) %>%
            ungroup() %>%
            group_by(country) %>% 
            mutate(total=sum(count))
            

ggplot(intake.gender, aes(x=country, fill=gender, y=count/total)) +
  geom_bar(stat="identity", position="dodge") +
  g_title(ttle="Gender by country", y=i18n('percentage_of_participants'))
g_save("country-gender", width=20, height=8)

context$pop()

