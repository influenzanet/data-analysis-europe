# Global configuration of scripts

.ifnOptions = list() # global config

find_workspace = function(mark.file=".Rworkspace", max.depth=10) {
  depth = 0
  dir = getwd()
  found = FALSE
  while(!found && depth < max.depth) {
    found = file.exists(file.path(dir, mark.file))
    if(!found) {
      dir = normalizePath(file.path(dir,".."))
      depth = depth + 1
    }
  }
  if(found) {
    return(dir)
  }
  rlang::abort("Unable to find workspace")
}

# Local config scripts
source(file.path(find_workspace(), 'location.R')) # should always in the upper directory from working dir

if( is.null( getOption("ifn") ) ) {
  # load system bootstrap script
  source(paste0(BASE_PATH,'share/lib/system.R'))
}

