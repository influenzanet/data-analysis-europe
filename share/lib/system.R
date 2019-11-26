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


get_current_season = function() {
  calc_season(Sys.Date())
}