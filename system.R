# Global configuration of scripts

.ifnOptions = list() # global config

# Local config scripts
source('../location.R') # should always in the upper directory from working dir

if( is.null( getOption("ifn") ) {
  # load system bootstrap script
  source(paste0(BASE_PATH,'share/lib/system.R'))
}

