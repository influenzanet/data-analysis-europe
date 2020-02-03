source('conf.R')
library(dplyr)
library(swMisc)
library(rlang)
library(reshape2)

share.lib("episodes")

if(!exists("country") | is.null(country)) {
  rlang::abort("Country not defined")
}

if(!exists("season") | is.null(season)) {
  rlang::abort("Season not defined")
}

countries = platform_env("COUNTRY_CODES")
seasons = get_historical_seasons()

if(!country %in% countries) {
  rlang::abort(paste("Unknown country ", sQuote(country)))
}

if(!season %in% seasons) {
  rlang::abort(paste("Unknown season ", sQuote(season)))
}

init.path(paste0('indicator/', country))

#' Create output file
#' @param .data data to output
#' @param name data set name
output = function(.data, name) {
  write.csv2(.data, file=my.path(paste0(name, '_', season, '.csv')), row.names = FALSE)
}

provider = SyndromeProviderRS2019$new()

age.categories = c(0, 21, 65, 200)

visits.columns = survey_variable_available(survey_labels('weekly', 'visit'), survey="weekly", season=season)
medic.columns =  survey_variable_available(survey_labels('weekly', 'medic'), survey="weekly", season=season)

cols = c(visits.columns, medic.columns)

r = load_results_for_incidence(season=season, age.categories=age.categories, country=country, syndrome.from = list(provider=provider$compute, health.status=FALSE), first.season=T, columns=list(keep.all=TRUE,weekly=cols))

if( is.null(r) | is.null(r$weekly) | is.null(r$intake) ) {
  rlang::abort("No data",class = "error_no_data")
}

if( nrow(r$weekly) == 0 ) {
  rlang::abort("No weekly data", class = "error_no_data")
}

env = list2env(r)
rm(r)

# Create a country column, as it will be needed when merging intake
env$intake$country = factor(country) 

# Ensure syndrome columns are boolean
env$weekly = mutate_at(env$weekly, env$syndromes, ~ . > 0L)

design = episode_design(delay_episode = 15L, strategies = get_default_episode_strategies())

# Get population for stratification
h = season_definition(season)
pop = load_population_age(geo="country", year=h$year.pop, age.breaks=age.categories)
pop = pop %>% 
        filter(country == .env$country) %>% 
        select(age.cat, all) %>% 
        rename(population=all)
 
# Prepare data to compute episode
episode_prepare_data(design, env = env)

env$weekly$yw  = iso_yearweek(env$weekly$onset)
env$weekly = survey_recode_all(env$weekly, "weekly")
env$weekly = left_join(env$weekly, env$intake[, c('person_id','age.cat')])

freqs.ecdc = NULL # Data to export

#syndromes = env$syndromes
syndromes = "ari.ecdc"

for(syndrome.column in syndromes) {
  cat(syndrome.column,"\n")
  env$weekly$episode = NULL # Be sure we dont have episode column

  episode_compute(env, design=design, syndrome.column = syndrome.column)
  
  weekly.fusion = episode_fusion_strategy(env$weekly, design = design)
  
  weekly.fusion$yw = iso_yearweek(weekly.fusion$onset)
  
  weekly.fusion = left_join(weekly.fusion, env$intake[, c('person_id','age.cat')])
  
  # Frequency tables
  freqs = list()
  
  # Compute syndrome frequency using episode (keep only first survey episode)
  w.sd = env$weekly[, c('timestamp', syndrome.column, 'yw', 'episode','person_id', 'age.cat')]
  w.sd = w.sd %>%
            arrange(person_id, episode, timestamp) %>%
             group_by(person_id, episode) %>%
             mutate(order=row_number()) %>%
             filter( !(!!sym(syndrome.column) & order > 1))
  
  # Compute syndrome frequency by strata
  freqs_syndrome = bind_freqs(
     raw=freq_episode(env$weekly, syndrome.column, type="bool", strata="age.cat"),
     episode=freq_episode(w.sd, syndrome.column, type="bool", strata="age.cat")
  )
  rm(w.sd)
  
  # Crude (without stratification) rates
  freqs$syndrome = freqs_syndrome %>% 
      group_by(variable, type, yw) %>% 
      summarize(count=sum(count), total=sum(total), missing=sum(missing)) %>%
      data.frame()
  
  freqs_syndrome = left_join(freqs_syndrome, pop, by="age.cat")
  
  # Compute ILI population using general population and the proportion of ILI in the total of these ILI population on all age groups
  # i.e. compute RIR(ILI, population) by age
  freqs_syndrome = freqs_syndrome %>% 
      filter(!is.na(age.cat)) %>%
      mutate(
        prop.syndrome =  count/total,
        pop_adj = prop.syndrome * population # Extrapolated population, with syndrome in each strata
      ) %>%
      group_by(yw, type) %>%
      mutate(
        prop_pop_adj=pop_adj/sum(pop_adj, na.rm=TRUE) # Proportion of extrapolated population in each strata
      ) %>%
      ungroup()

  vars = list(
    list(name="visit", cols=visits.columns),
    list(name="medic", cols=medic.columns),
    list(name="sympt", cols=get_symptoms_aliases())
  )
  
  ww = env$weekly %>% filter(!!sym(syndrome.column))
  
  #' Compute crude & ajusted proportions from frequency table by strata
  #' @param data frequency data.frame for one type (episode, raw)
  #' @param type type name
  compute_props = function(data, type) {
    freqs_strata = left_join(data, freqs_syndrome[ freqs_syndrome$type == type, c('yw','age.cat','prop_pop_adj')], by=c("yw",'age.cat'))
    
    f_crude = freqs_strata %>% 
      arrange(variable, yw) %>%
      group_by(variable, yw) %>%
      summarize(
        count=sum(count), 
        total=sum(total), 
        missing=sum(missing)
      )
    # Compute prop & cumulated prop
    f_crude = f_crude %>%
      mutate(
        prop=count/total
      ) %>%
      group_by(variable) %>%
      mutate(
        cum_count=cumsum(count),
        cum_total=cumsum(total),
        cum_prop=cum_count / cum_total
      )
    
    f_crude = calc_confint_crude(f_crude, "count", "total", prefix="prop_")
    f_crude = calc_confint_crude(f_crude, "cum_count", "cum_total", prefix="cum_prop_")
    
    f_adj = freqs_strata %>%
      filter(!is.na(age.cat)) %>%
      ungroup() %>%
      mutate(
          prop_adj=prop_pop_adj * (count/total),
          wi=(1/total) * prop_pop_adj, 
          w2i=(wi^2) * count
          # Adjusted variance in strata
      ) %>%
      group_by(yw, variable) %>%
      summarize(
        count=sum(count), 
        total=sum(total), 
        missing=sum(missing), 
        prop=sum(prop_adj),
        w2 = sum(w2i, na.rm = TRUE) # Variance estimator
      )
    
    f_adj = calc_adjusted_confint.df(f_adj, col.count="count", col.w2="w2", col.prop="prop", prefix="prop_")
    
    freqs_strata = freqs_strata %>%
      arrange(variable, age.cat, yw) %>%
      group_by(variable, age.cat) %>%
      mutate(
        cum_count=cumsum(count), 
        cum_total=cumsum(total)
      ) %>%
      ungroup()
    
    f_adj_cumul = freqs_strata %>% 
      filter(!is.na(age.cat)) %>%
      mutate(
        prop=prop_pop_adj * cum_count / cum_total,
        w2=((1/cum_total) * prop_pop_adj)^2 * cum_count
      ) %>%
      group_by(variable, yw) %>%
      summarize(
        cum_prop=sum(prop),
        cum_count=sum(cum_count),
        cum_total=sum(cum_total),
        cum_w2=sum(w2)
      )
    
    f_adj_cumul = calc_adjusted_confint.df(f_adj_cumul, col.count="cum_count", col.w2="cum_w2", col.prop="cum_prop", prefix="cum_prop_")

    f_adj = left_join(f_adj, f_adj_cumul, by=c("yw", "variable"))
    
    f_adj = f_adj %>% select(-w2, -cum_w2)
    
    ff = bind_rows(
      crude=f_crude,
      adj=f_adj,
      .id="estimator"
    )
    ff = data.frame(ff)
    
    ff$estimator = factor(ff$estimator)
    ff
  }

  for(var in vars) {
    name = var$name
    
    episode.strata = freq_episode(weekly.fusion, var$cols, type="bool", strata="age.cat")
    
    episode = compute_props(episode.strata, type="episode")
    
    raw.strata = freq_episode(ww, var$cols, type="bool", strata="age.cat")

    raw = compute_props(raw.strata, type="raw")
    
    # Frequencies by strata
   
    freqs[[name]] = bind_freqs(episode=episode, raw = raw)
  }
  
  ## Count number of surveys by participants by weeks
  p.raw = ww %>% 
            count(yw, person_id, name = "count") %>% 
            group_by(yw, count) %>% 
            summarize(value=n()) 
  
  p.e = weekly.fusion %>% 
          count(yw, person_id, name = "count") %>% 
          group_by(yw, count) %>% 
          summarize(value=n()) 
  
  freqs[["participants"]] = bind_freqs(raw=data.frame(p.raw), episode = data.frame(p.e))
   
  attr(freqs,"meta") <- list(
    time=Sys.time(),
    country=country,
    season=season,
    syndrome=syndrome.column
  )
  
  saveRDS(freqs, file=my.path('healthcare-', syndrome.column,"-", season,'-', Sys.Date(),'.Rds'))
  cat("\n")
  
  if(syndrome.column == "ari.ecdc") {
    freqs.ecdc = freqs
  }
}

## Extract for ecdc indicators
if(!is.null(freqs.ecdc)) {

  visits = freqs.ecdc$visit
  
  if(is.null(visits)) {
    rlang::abort("Unable to find visits for ecdc syndrome")
  }
  
  visits = visits %>% 
            filter(
                type =="episode" & 
                variable %in% c('visit.GP','visit.emergency','visit.hospital','visit.other') &
                estimator == "adj"
            )
  
  visits = visits %>% group_by(variable) %>% 
    arrange(yw) %>%
    select(-type)
  
  output(visits, "visits_weekly")
}