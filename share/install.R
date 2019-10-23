
dependencies = list(
  need= c("RColorBrewer", "reshape2", "ggplot2", "dplyr", "maps", "epitools","swMisc", "ifnBase", "methods", "jsonlite", "wordcloud"),
  suggested = c("swOutput")
)

system_template = "# Global configuration of scripts

.ifnOptions = list() # global config

# Local config scripts
source('../location.R') # should always in the upper directory from working dir

if( is.null( getOption(\"ifn\") ) ) {
  # load system bootstrap script
  source(paste0(BASE_PATH,'share/lib/system.R'))
}
"


install_workspace = function() {

  location_file = "location.R"
  system_file = "system.R"

  if( file.exists(location_file) ) {
    cat("File ",location_file," already exists\n")
    return()
  }

  cat("Installing workspace\n")

  cat("Installing system bootstrap file\n")
  write(system_template, file=system_file)

  BASE_PATH = paste0(getwd(), "/")
  OUT_PATH = quote(paste0(getwd(),'/'))

  drivers = c('RODBC','RPostgreSQL')

  d = menu(drivers, title="Choose the DB driver to use")
  if(d > 0) {
    DB_DRIVER = drivers[d]
  }

  platforms = gsub("\\.r", "", list.files('share/platform', pattern='*.R'))
  platforms = platforms[ !grepl("^i18n", platforms)]


  PLATFORM = ''
  d = menu(platforms, title="Choose the platform to use for this workspace")
  if(d > 0) {
    PLATFORM = platforms[d]
  }

  ww = function(line) {
    cat(line, "\n", file=location_file, append=T)
  }

  wp = function(name, value) {
    if(is.character(value)) {
      value = paste0('"', value,'"')
    }
    if(is.language(value)) {
      value = deparse(value)
    }
    ww(paste0(name,' = ', value, "\n"))
  }

  ww("# Location of this workspace on this machine (absolute path, ended by a /)")
  wp('BASE_PATH', BASE_PATH)
  ww("# Location of output files (absolute path, ended by a /), used by init.path() & my.path()")
  wp('.ifnOptions$base.out.path', OUT_PATH)
  ww("# Name of the DB driver interface to use (name of one file in share/lib/db/)")
  wp('.ifnOptions$db_driver', DB_DRIVER)
  ww("# Connexion string or list of parameters used by the DB driver")
  wp('.ifnOptions$db_dsn', "")
  ww("# Name of the platform file to use (county code by convention), should be in share/platform/")
  wp('.ifnOptions$platform', PLATFORM)

  cat("Workspace is almost ready. Please edit location.r file to set up DB_DSN variable\n")
}

install_dependencies = function() {

  # Ensure Sentiweb repos in defined
  repos = options("repos")$repos
  repos['sw'] = "https://cran.sentiweb.fr/"
  options(repos=repos)

  rr = installed.packages()
  installed = rr[, 'Version']

  use_suggested = menu(choices = c('Yes','No'), title = "Should you use suggested package")

  to_install = dependencies$need
  if(use_suggested == 1) {
    to_install = c(to_install, dependencies$suggested)
  }

  to_install = to_install[ !to_install %in% names(installed)]

  if(length(to_install) > 0) {
    cat("Installing needed packages ", paste(to_install, collapse = ","), "\n")
    install.packages(to_install)
  } else {
    cat("All packages are installed\n")
  }

}

install_workspace()
install_dependencies()

