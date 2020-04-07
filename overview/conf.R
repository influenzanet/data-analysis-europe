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
  path = my.path(...)
  desc_output(path, desc=desc, plot=plot)
  return(path)
}

