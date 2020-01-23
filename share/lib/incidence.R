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