##
# Bootstrap Loader of global configuration
##

options(ifn=.ifnOptions)

if(!library(ifnBase, logical.return = TRUE)) {
  if( !interactive() ) {
    stop("Missing library ifnBase")
  }
}

library(swResults)

load_platform()

# Load default translations
i18n_load(platform_path("i18n", platform = TRUE), language = 'en')

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

#' Return a list of colors for the platform
get_platform_colors = function() {
  list(
    primary=i18n('color.web.primary'), 
    secondary=i18n('color.web.secondary')
  )
}

get_current_season = function() {
  calc_season(Sys.Date())
}

#' Starting week for analysis
get_graph_starting_week = function(season) {
  if(season < 2020) {
    w = 51
  } else {3
    w = 41
  }
  season * 100 + 41
}

#' Get list of existing symptoms
#' They depends on seasson now
get_symptoms_columns = function(season) {
  sympt.extra = survey_labels('weekly','symptoms.extra')
  survey_variable_available(unique(c(get_symptoms_aliases(), sympt.extra )), survey="weekly", season=season)
}

#' Default copyrght text
ifn.copyright = function(internal=TRUE) {
  cp = "Influenzanet, http://influenzanet.info"
  if(internal) {
    cp = paste0(cp, ", for internal purpose only")
  }
  paste0(cp, ", ", Sys.time())
}

# ' Helper function to replace ggplot2::labs() and put automatic caption 
ifn_labs = function(..., caption=NULL, internal=TRUE) {
  if(is.null(caption)) {
    caption = ifn.copyright(internal=internal)
  }
  ggplot2::labs(..., caption=caption)
}



