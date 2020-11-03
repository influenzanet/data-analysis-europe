
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
    age.categories = c(0, 21, 65, 200)
  ),
  "w0_s2_if2_ex"=list(
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
#' }
#' 
#' @return list
get_eu_incidence_parameters = function(which="default") {
  sets = .eu_parameters_sets
  
  # Add name entry according to list value
  
  if(which == "all") {
    return(sets)
  }
  
  if(which == "default") {
    which = "w1_s2_if2_ex"
  }
  sets[[which]]
}