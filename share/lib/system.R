##
# Bootstrap Loader of global configuration
##

options(ifn=.ifnOptions)

if(!library(ifnBase, logical.return = TRUE)) {
  if( !interactive() ) {
    stop("Missing library ifnBase")
  }
}

options("swResults"=list("path_provider"=ifnBase::my.path))

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

#' Create a descriptive structure to path to desc_output()
#' Merge context data with the ones enventualy directly with the graph output (desc param)
#' @param context ResultContext instance
#' @param desc desc param, title or list passed to the graph output functions
as_output_desc = function(context, desc=NULL) {
  ctx = context$resolve()
  if(!is.null(desc)) {
    if(is.character(desc)) {
      ctx$desc = desc
      desc = ctx
    } else {
      desc = modifyList(ctx, desc)
    }    
  } else {
    desc = ctx
  }
  if(hasName(desc, "title") && !hasName(desc, "desc")) {
    desc$desc = desc$title # Current field for title is "desc"
    desc$title = NULL
  }
  desc
}

#' Save graph helper
#' Output a graph file with desc
#' @param path relative name (will be passed to my.paths)
#' @param formats list of extensions to use as output
#' @param context ResultContext instance with current context
#' @param desc graph specific extra desc
#' @param verbose logical if TRUE show the current graph name
save_graph_with_context = function(path, formats, width, height, context, desc=NULL, verbose=TRUE, dpi=300) {
  desc = as_output_desc(context, desc)
  if(!hasName(desc, "desc")) {
    desc$desc = result_desc_plot(ggplot2::last_plot())
  }
  if(verbose) {
    message(path)
  }
  p = my.path(path)
  for(format in formats) {
    file = paste0(p, '.', format)  
    result_desc_output(file, desc=desc, plot=plot)
    ggplot2::ggsave(file, width=width, height=height, dpi=dpi)
  }
}

external_db_path = function() {
  paste0(local_db_path, "externals.db")
}
