#' Common Incidence computation parameters for European analysis
#' @return list
get_eu_incidence_parameters = function() {
  list(
    design.type = "age",
    estimator.params = list(
      active.week.before=1, 
      active.week.after=1, 
      active.min.surveys=2, 
      exclude.same=TRUE,
      ignore.first.delay=6, 
      ignore.first.only.new=TRUE
    ),
    age.categories = c(0, 21, 65, 200)
  )
}

## Syndrome functions
# Common function to compute syndromes for ecdc indicators
##


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