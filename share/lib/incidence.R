
default.age.categories = c(0, 21, 65, 200)

#' Common parameters
.eu_parameters_sets = list(
  "w1_s2_if2_ex"=list(
    design.type = "age",
    estimator.params = list(
      active.week.before=1, 
      active.week.after=1, 
      active.min.surveys=2, 
      exclude.same=TRUE,
      ignore.first.delay=6, 
      ignore.first.only.new=TRUE
    ),
    age.categories = default.age.categories
  ),
  "w0_s2_if2_ex"=list(
    design.type = "age",
    estimator.params = list(
      active.week.before=0, 
      active.week.after=0, 
      active.min.surveys=2, 
      exclude.same=TRUE,
      ignore.first.delay=6, 
      ignore.first.only.new=TRUE
    ),
    age.categories = default.age.categories
  ),
  "w0"=list(
    design.type = "age",
    estimator.params = list(
      active.week.before=0,
      active.week.after=0
    ),
    age.categories = default.age.categories
  )
)

# Build name field
.eu_parameters_sets = Map(function(p,n) { p$name = n; return(p)}, .eu_parameters_sets, names(.eu_parameters_sets))

#' Common Incidence computation parameters for European analysis
#' 
#' @param which what kind of output (see which section)
#' 
#' @details Which:
#' 
#' \describe{
#'   \item{default}{Default parameter set}
#'   \item{all}{All parameters sets}
#'   \item{syndrome}{Syndrome specific parameter, second parameter is required}
#' }
#' 
#' @return list
get_eu_incidence_parameters = function(which="default", syndrome=NULL) {
  sets = .eu_parameters_sets
  
  # Add name entry according to list value
  
  if(which == "all") {
    return(sets)
  }
  
  if(which == "default") {
    which = "w1_s2_if2_ex"
  }
  
  if(which =="syndrome") {
    if(is.null(syndrome)) {
      rlang::abort("Missing syndrome parameter")
    }
    if(!hasName(ecdc_default_methods, syndrome)) {
      rlang::abort(paste0("Unknow syndrome",sQuote(syndrome)," in ecdc_default_methods"))
    }
    which = ecdc_default_methods[[syndrome]]
  }
  
  if(!hasName(sets, which)) {
    rlang::abort(paste0("Unknown parameters set ",sQuote(which), " in knowns parameters (.eu_parameters_sets in share/lib/incidence)"))
  }
  
  sets[[which]]
}

## Syndrome functions
# Common function to compute syndromes for ecdc indicators
##

#' Syndrome to compute from weekly & intake data
#' @param weekly weekly survey data
#' @param intake intake survey data
ecdc_syndrome_provider = function(weekly, intake) {
  sd = SyndromeProviderRS2019$new()
  sdc = SyndromeProviderCovid$new()
  
  w1 = sd$compute(weekly=weekly, intake=intake)
  w2 = sdc$compute(weekly=weekly, intake=intake)
  full_join(data.frame(w1), data.frame(w2), by="id")
}

ecdc_syndrome_from = list(
  provider=ecdc_syndrome_provider, 
  health.status=FALSE
)

ecdc_syndromes = c('ili.ecdc', 'covid.ecdc')

ecdc_default_methods = list(
  'ili.ecdc'='w1_s2_if2_ex',
  'covid.ecdc'='w0'
)
