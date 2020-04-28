library(ggplot2)

# Register outputs
.outputs = new.env(parent = emptyenv())

# List of graphs in output
.outputs$graphs = list()

out_graph = function(file) {
  .outputs$graphs[length(.outputs$graphs)+1] = basename(file)
}

save_outputs = function() {
  g = unlist(.outputs$graphs)
  writeLines(g, my.path('graphs.lst'))
}

g_title = function(...) {
  labs(..., caption=sub.text)
}

g_barplot = function(..., label.size=2, ylab=i18n('percentage'), x.rotate=0) {
  gg_barplot_percent(..., x.rotate = x.rotate, label.size=label.size) + ylab(ylab)
}

g_save = function(file, width, height=NA, r=1.618) {
  if( is.na(height) ) {
    height = width / r
  }
  f = paste0(file, '.png')
  ggsave(f, width = width, height = height, dpi=110)
  out_graph(f)
  f = paste0(file, '.svg')
  ggsave(f, device="svg", width = width, height = height)
  out_graph(f)
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
  g_save(my.path(file), width=width, height=h)
  last_plot()
}