# All scripts in a workspace's project know only one file in the upper directory
source('../system.R')

project.prefix = "overview" # Convention, each project inside repo defines it project.prefix

add_path_prefix("project", project.prefix)

# need the db for all scripts of this project
dbConnect()

sub.text = function(cex.sub=.6) {
 title(sub=ifn.copyright(), cex.sub=cex.sub)
}

# Create description file alongside with the output
out_path = function(..., desc=NULL, plot=FALSE) {
  path = my.path(...)
  desc_output(path, desc=desc, plot=plot)
  return(path)
}

