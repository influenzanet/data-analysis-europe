##
# Bootstrap Loader of global configuration
##

options(ifn=.ifnOptions)

if(!library(ifnBase, logical.return = TRUE)) {
  if( !interactive() ) {
    stop("Missing library ifnBase")
  }
}

load_platform()

if(!"methods" %in% loadedNamespaces()) {
  # methods Lazy loading
  is <- function(...) {

    if(!isTRUE(ifnBase::get_option("methods.loaded"))) {
      library(methods)
      share.option(methods.loaded=TRUE)
    }
    methods::is(...)
  }
}

setHook(packageEvent("ggplot2", "attach"),
 function(...) {
  cat("Extending ggplot2")
  with_ggplot()
})

is_try_error = function(x) {
  "try-error" %in% class(x)
}


get_current_season = function() {
  calc_season(Sys.Date())
}

#' Get list of existing symptoms
#' They depends on seasson now
get_symptoms_columns = function(season) {
  sympt.extra = survey_labels('weekly','symptoms.extra')
  survey_variable_available(c(get_symptoms_aliases(), sympt.extra ), survey="weekly", season=season)
}

# Create a file to describe the output
# Descriptive file are stored in the directory of the file prefixed by '.d_'
desc_output = function(path, desc=NULL, plot=FALSE) {
  if(isTRUE(plot)) {
    plot = ggplot2::last_plot()
    desc = plot$labels$title  
  }
  if(!is.null(desc)) {
    dir = dirname(path)
    desc_file = paste0(".d_", basename(path))
    write(desc, file=paste0(dir, "/", desc_file))
  }
}



