## Intake description
source('conf.R')
library(dplyr)
library(swMisc)

source("lib/graph.R", local=TRUE)

if(!exists("cli.args")) {
  cli.args = parseArgs(list(
    'season'=list(type="int", default=get_current_season()),
    'country'=list(type="choices", choices=platform_env("COUNTRY_CODES"))
  ))
}

country = cli.args$country
season = cli.args$season

init.path(paste0(country, "/", season))

i18n_load("i18n/", language = tolower(country))

sub.text = i18n("platform.copyright")

has.population = TRUE

# Get list of columns for questions in survey
condition.columns = survey_labels('intake', 'condition')
hear.columns = survey_labels('intake', 'hear.about')
allergy.columns = survey_labels('intake', 'allergy')
pets.columns = survey_labels('intake', 'pets')

colors.web = get_platform_colors()

columns = c('timestamp','gender','date.birth','vacc.curseason',"main.activity","occupation","often.ili", 'pregnant',hear.columns,'transport','smoker', condition.columns, allergy.columns, pets.columns )

intake = survey_load_results("intake", columns, season=season, country=country)

if( is.null(intake) || nrow(intake) == 0 ) {
  rlang::abort("No data for this season/country",class = "error_no_data")
}

intake = keep_last_survey(intake)

# Recode variables in a standard way
intake = recode_intake(intake)

g_barplot(intake$gender, x.rotate = 0) +
  g_title(title=i18n("gender_of_participants"), y=titlelize('percentage'), x=titlelize('gender'))
g_save(my.path('gender'), width=3, height=3)

age.categories = c(seq(0, 90, by=5), 200)

intake$age.cat = cut_age(intake$age, age.categories)

g_barplot(intake$age.cat, order=FALSE, label.size = 1.5) +
  g_title(title=i18n("age_of_participants"), x=i18n('age_group')) +
  theme(axis.text.x=element_text(angle=-45, vjust=.5))
g_save(my.path('age'), width=9, height=4)

if( has.population) {
  # National population
  pop = load_population_age(geo="country", age.breaks=age.categories, year=2016)
  pop = pop %>% filter(country == .env$country)
  # Agregate by age & merge pop
  age_sexe_structure = intake %>% filter(!is.na(age.cat)) %>% group_by(age.cat, gender) %>% summarize(count=n())
  age_sexe_structure$pop = "cohort"

  v = c('male','female')
  p = reshape(pop[, c('age.cat',v)], idvar="age.cat", timevar="gender", direction="long", varying=list(v), times=v, v.names="count")
  p$gender = factor(p$gender,v, i18n(v))
  p$pop = "pop"

  age_sexe_structure = bind_rows(age_sexe_structure, p)

  age_sexe_structure = age_sexe_structure %>%
        group_by(gender, pop) %>%
        mutate(all=sum(count), prop=100 * count/all)

  # Compute by age structure
  age_structure =  age_sexe_structure %>%
      group_by(age.cat, pop) %>%
      summarize(count=sum(count), all=sum(all))
  age_structure = age_structure %>%
    mutate(prop=100 * count / all)

  cohort.title = i18n('cohort.title')
  population.title = i18n('population.title')
  pop.labels = c('cohort'=cohort.title, 'pop'=population.title)
  pop.colors = c("pop"=colors.web$primary, "cohort"=colors.web$secondary)

  ggplot(age_structure, aes(x=age.cat, group=pop, fill=pop, y=prop)) +
    geom_bar(stat="identity", position = "dodge") +
    scale_fill_manual(values=pop.colors, labels=pop.labels) +
    guides(fill=guide_legend("")) +
    g_title(title=i18n("age_of_participants_and_pop"), y=i18n('percentage'), x=i18n('age_group'))
  g_save(my.path('age_of_participants_pop'), width=6, height=4)

  female = age_sexe_structure$gender == i18n("female")
  scales = list(label=c(cohort=cohort.title, pop=population.title), color= pop.colors)
  plot_age_pyramid(age_sexe_structure, female=female, prop=FALSE, scales=scales) +
    g_title(x="age_group", y=i18n("age_gender_proportion")) + guides(fill=guide_legend(""), alpha=FALSE)
  g_save(my.path('age_pyramid'), width=6, height=5)

}

# Hear about
simple_plot(hear.columns, title="graph.hear.about", width=6.7, file="hearabout", x.rotate = 45, x.vjust=.3)

# Main occupation
simple_plot('main.activity', width=6, title='graph_main_activity', file='main-activity')

# Vaccination for the current season
simple_plot('vacc.curseason', trans=NULL, width=6, title='graph_vacc_curseason', file='vaccination-curseason' )

simple_plot('transport', width=6, title='graph_transport')

simple_plot('often.ili', width=6, title='graph_often.ili', file='often_ili')

simple_plot(condition.columns, title="graph_condition", width=6, file="condition")

simple_plot('pregnant', width=5, title='graph_pregnant', data=intake[ intake$gender == 'female', ])

simple_plot('smoker',  width=7, title='graph_smoker', no.empty = TRUE)

simple_plot(allergy.columns, title="graph_allergy", width=6.8, file="allergy")

simple_plot(pets.columns, title="graph_pets", width=6.8, file="pets")

# Save graph list
save_outputs()
