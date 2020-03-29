# All scripts in a workspace's project know only one file in the upper directory
source('../system.R')

add_path_prefix("project","overview")

# need the db for all scripts of this project
dbConnect()

ifn.copyright = function() {
  paste(Sys.time(), "Influenzanet 2019, for internal purpose only")
}

gg_ifn = function() {
  ggplot2::labs(caption=ifn.copyright())
}

sub.text = function(cex.sub=.6) {
 title(sub=ifn.copyright, cex.sub=cex.sub)
}

# Create description file alongside with the output
out_path = function(..., desc=NULL, plot=FALSE) {
  p = paste0(...)
  if(isTRUE(plot)) {
      plot = ggplot2::last_plot()
      desc = plot$labels$title  
  }
  
  if(!is.null(desc)) {
    dir = my.path(dirname(p))
    desc_file = paste0(".d_", basename(p))
    write(desc, file=paste0(dir, "/", desc_file))
  }
  return(my.path(p))
}

