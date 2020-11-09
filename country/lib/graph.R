library(ggplot2)

g_title = function(...) {
  labs(..., caption=sub.text)
}

g_barplot = function(..., label.size=2, ylab=i18n('percentage'), x.rotate=0) {
  gg_barplot_percent(..., x.rotate = x.rotate, label.size=label.size) + ylab(ylab)
}

simple_plot = function(column, trans=NULL, width, title, file=NULL, h=NA, data=intake, no.empty=FALSE, subtitle=NULL, ...) {
  if(length(column) > 1) {
    trans = if(is.null(trans)) TRUE else trans
    if(!is.logical(trans)) {
      stop("trans arg should be logical with multiple columns")
    }
    y = multiple_freq(data[, column], translate = trans)
  } else {
    if(is.null(trans) ) {
        trans = TRUE
    }
    if(isTRUE(trans)) {
      y = i18n(data[[column]])
    } else {
      y = data[[column]]
    }
    
    if(no.empty) {
      y = factor(y)
    }
  }
  g_barplot(y, ...) +
    g_title(title=i18n(title), x="", subtitle=subtitle)
  h = ifelse(is.na(h), width/1.618, h)
  file = if(is.null(file)) column else file
  context$set(subject=file)
  g_save(file, width=width, height=h)
  last_plot()
}
