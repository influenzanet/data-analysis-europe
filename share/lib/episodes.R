
get_default_episode_strategies = function() {

   list(
    # Any:
    # Q9=medic.*, Q7=visit.*, Q8=contactmed, Q9c=antiviro.med, antiviro.home, Q9d=antibio.med, antibio.home
    episode_strategy("any",
                     get_symptoms_aliases(), "moderate.fever","high.fever",
                     survey_labels("weekly", "medic"),
                     survey_labels("weekly", "visit"),
                     survey_labels("weekly", "contactmed"),
                     survey_labels("weekly", "antiviro"),
                     survey_labels("weekly", "antibio"),
                     survey_labels('weekly', 'analysis.sympt.covid')
    ),
    episode_strategy("worst",
                     take.temp=c(YES, DONTKNOW, NO), # Q6c
                     highest.temp=raw_value(5L, 4L, 3L, 2L, 1L, 0L), # Q6d
                     hospitalization=c(YES, NO), # Q14
                     website=c(YES, NO), # Q8c
                     change.routine=raw_value(2L, 1L, 0L), #Q10
                     off.workdelay=c("over_15" ,"11_15d", "6_10d", "5d","4d", "3d", "2d", "1d") # Q10c
    ),
    episode_strategy("first",
                     "location", "sympt.sudden", "fever.sudden","antiviro.when"
    ),
    episode_strategy("last",
                     "off.work","sympt.cause"
    )
  )
  
}

episode_design_defaults = list(
  "ili.ecdc"=list(delay_episode = 11, max_episode_duration = 11, median_episode_duration = 7),
  "covid.ecdc"=list(delay_episode = 17, max_episode_duration = 17, median_episode_duration=11)
)

update_episode_design = function(design, delay_episode, max_episode_duration, median_episode_duration) {
  if(!is(design, "episode_design")) {
    rlang::abort("Not an episode_design object, did you used episode_design() ?")
  }
  design$max_episode_duration = max_episode_duration
  design$delay_episode_max = delay_episode
  design$median_episode_duration = median_episode_duration
  design
}

episode_design_syndrome = function(syndrome, design=NULL) {
  p = episode_design_defaults[[syndrome]]
  if(is.null(p)) {
    rlang::abort(paste("Unable to find episode design defaults for syndrome", syndrome))
  }
  if(!is.null(design)) {
    p$design = design
    design = do.call(update_episode_design, p) 
  } else {
    do.call(episode_design, p)
  }
  design
}


#' Compute Frequency for a variable on survey data
#' @param data survey data
#' @param vars list of variables to compute the frequency on
#' @param type kind of variable ("bool", "discrete")
#' @param strata supplementary columns to use as strata (added to "yw" )
#' @param pop data.frame() population used to compute adjusted proporition
#' @return data.frame() see details
#' 
#' @details 
#' 
#' 
#' \describe{
#'  \item{yw}{Yearweek value}
#'  \item{variable}{variable name used}
#'  \item{value}{considered value of the variable, only for discrete type}
#'  \item{count}{count of response of the value (TRUE if type is bool)}
#'  \item{total}{count of response with non NA value of the variable}
#'  \item{missing}{count of response to NA for the variable}
#'  \item{prop}{Computed proportion, count/total for crude estimator}
#'  \item{estimator}{Kind of estimator, "crude" or "adj"}
#' 
#' }
#' 
freq_episode = function(data, vars, type, strata=NULL) {
  groups = c("yw", strata)
  if(type == "bool") {
    # Count values
    d = data %>% 
          group_by(!!!syms(groups)) %>% 
          summarize_at(vars, sum)
    d = reshape2::melt(d, id.vars=groups, value.name = "count")
    
    # count total rows
    dn = data %>% group_by(!!!syms(groups)) %>% summarize(total=n())
    dd = left_join(d, dn, by=groups)
    
    # Count rows with value
    dn = data %>% group_by(!!!syms(groups)) %>% summarize_at(vars, ~sum(!is.na(.)))
    dn = reshape2::melt(dn, id.vars=groups, value.name = "total_with_value")
    dd = left_join(dd, dn, by=c(groups,"variable"))
  }
  
  if(type == "discrete") {
    dd = NULL
    for(var in vars) {
      d = data %>% 
            group_by(!!!groups, value=!!sym(var)) %>% 
            summarize(count=n())
      #d = rename(d)
      d$variable = var
      dd = bind_rows(dd, d)
    }
    dd = dd %>% 
          group_by(!!!groups, variable) %>% 
          mutate(total_with_value=sum(ifelse(is.na(value), 0, count)), total=sum(count))
  }
  
  dd = dd %>%
        mutate(
          missing=total - total_with_value,
          total=total_with_value
        ) %>%
        select(-total_with_value)

  dd = data.frame(dd)
  attr(dd, "type") <- type
  dd
}

compute_strata = function() {
  
  if(!is.null(strata)) {
    if(is.null(pop)) {
      rlang::abort("Population is required with strata")
    }    
    
    dd = left_join(dd, pop[, c(strata, 'prop.pop')], by=strata)
    
    dd = ungroup(dd)
    
    groups = c("variable")
    if(type == "discrete") {
      groups = c(groups, 'value')
    }
    groups = c(groups, "yw")
    
    # Compute adjusted frequency by strata
    freq.adj = dd %>% 
      group_by(!!!syms(groups)) %>% 
      filter(!is.na(prop.pop)) %>% # On known strata
      mutate(
        prop = count /total_with_value
      ) %>%
      summarize(
        count=sum(count),
        missing=sum(total)-sum(total_with_value),
        total=sum(total_with_value),
        prop=sum(prop.pop * prop)
      ) 
    
    # Compute crude frequency
    freq.crude = dd %>% 
      group_by(!!!syms(groups)) %>% 
      summarize(
        count=sum(count),
        missing=sum(total)-sum(total_with_value),
        total=sum(total_with_value),
      ) %>% 
      mutate(prop=count/total)
    
    dd = bind_rows(adj=freq.adj, crude=freq.crude, .id="estimator")
    dd$estimator = factor(dd$estimator)
  } else {
    dd$estimator = factor("crude", c("adj","crude"))
    dd = dd %>% 
      mutate(
        missing=total-total_with_value,
        total=total_with_value,
        prop=count/total
      ) %>%
      select(-total_with_value)
  }
}


bind_freqs = function(raw, episode) {
  d = bind_rows(raw=raw, episode=episode, .id="type")
  d$type = factor(d$type)
  if(!identical(attr(raw, "type"), attr(episode, "type"))) {
    stop("type are not the same in raw & episode")
  }
  attr(d, "type") <- attr(raw, "type")
  d
}

calc_adjusted_confint = function(x, w2, prop, conf.level = .95) {
  # Now compute adjusted confidence interval
  # Based on DKES estimator Fay & Feuer, 1997, Stat In Med (16) p791-801
  # Based on Poisson confidence interval using a rescaled distribution
  # y = weighted rate, xi = count in strata; wi = weight in strata; Ni = Sample size in (i)th strata;
  # x = sum(xi); popi = population in strata i; popT = Total population (all strata = sum(popi))
  # Up(x) & Lp(x) 1/2* Chisq quantile for respectively (1 - a / 2, DF=2(x+1)) and (a/2, DF=2x)
  # wi = 1/Ni * (popi / popT)
  # v = sum(wi^2 * xi)
  # U(y) = y + ( sqrt(v) / sqrt(x) ) * ( Up(x) - x)
  # L(y) = y + ( sqrt(v) / sqrt(x) ) * ( Lp(x) - x)
  
  N. = 1 - ((1 - conf.level)/2)
  #w2 = paste0(v, '.w2') # Variance of count
  #v.adj = paste0(v, '.adj')
  
  x.up = 0.5 * stats::qchisq(1 - N., 2 * (x + 1), lower.tail= F)
    
  upper = prop + ( sqrt(w2) / sqrt(x) ) * (x.up - x)
    
  x.low = 0.5 * stats::qchisq(N., 2 * x, lower.tail=F)
    
  lower = prop + ( sqrt( w2) / sqrt(x) ) * (x.low - x)
  
  cbind(upper=upper, lower=lower)
}

calc_adjusted_confint.df = function(data, col.count, col.w2, col.prop, prefix=NULL) {
  d = calc_adjusted_confint(x=data[[col.count]], w2=data[[col.w2]], prop=data[[col.prop]])
  d = data.frame(d)
  if(!is.null(prefix)) {
    colnames(d) <- paste0(prefix, colnames(d))
  }
  bind_cols(data, d)
}

calc_confint_crude=function(data, col.count, col.total, unit=1, prefix=NULL, method="binom.exact") {
  y = data[[col.count]]
  i = is.na(y)
  y[i] = 0 # Fake 0 value will be replaced by NA
  
  fun = switch(method,
               "binom.exact"=epitools::binom.exact,
               "pois.exact"=epitools::pois.exact
        )

  d = fun(y, data[[col.total]])

  cc = c('upper','lower')
  d = d[, cc]
  d[i, cc] = NA
  d$upper = d$upper * unit
  d$lower = d$lower * unit
  
  col.upper = 'upper'
  col.lower = 'lower'
  if(!is.null(prefix)) {
    col.upper = paste0(prefix, col.upper)
    col.lower = paste0(prefix, col.lower)
  }
  names(d) <- c(col.upper, col.lower)
  
  bind_cols(data, d)
}


