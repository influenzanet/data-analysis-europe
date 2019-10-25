# All scripts in a workspace's project know only one file in the upper directory
source('../system.R')

# need the db for all scripts of this project
dbConnect()

ifn.copyright = function() {
  paste("Influenzanet.eu -", Sys.time())
}

gg_ifn = function() {
  ggplot2::labs(caption=ifn.copyright())
}

sub.text = function(cex.sub=.6) {
 title(sub=ifn.copyright, cex.sub=cex.sub)
}
