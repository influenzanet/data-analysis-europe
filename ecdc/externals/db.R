library(R6)
library(RSQLite)
library(DBI)


# active (yw , active , method , country , updated, file)")
# incidence (yw , syndrome, type, country, method, incidence, lower, upper, count)")
# imports (country, file, min_yw, max_yw , imported_at)")

ExternalDB <- R6Class(
  public=list(
    db=NULL,
    initialize=function(db.file) {
      need.create = !file.exists(db.file)
      self$db <- DBI::dbConnect(RSQLite::SQLite(), db.file)
      if(need.create) {
        self$create()
      }
    },
    create=function() {
      self$exec("CREATE TABLE active (yw INT  NOT NULL, active INT  NOT NULL, method TEXT  NOT NULL, country TEXT  NOT NULL, updated DATETIME  NOT NULL, file TEXT NOT NULL, PRIMARY KEY(yw, method, country))")
      self$exec("CREATE TABLE incidence (yw INT NOT NULL, syndrome TEXT NOT NULL, type TEXT NOT NULL, country TEXT NOT NULL, method TEXT, incidence REAL, lower REAL, upper REAL, count INT, updated DATETIME  NOT NULL, file TEXT NOT NULL, PRIMARY KEY(yw, syndrome, type, country))")
      self$exec("CREATE TABLE imports (country TEXT, file TEXT, min_yw INT, max_yw INT, imported_at DATETIME)")
    },
    exec=function(...) {
      dbExecute(self$db, paste0(...))
    },
    fetch=function(...) {
      rs <- dbSendQuery(self$db, paste0(...))
      data = dbFetch(rs)
      dbClearResult(rs)
      data
    }
  )
)

#' Build SQL values tuples from row of data
row_to_values = function(row) {
  r = sapply(seq_along(row), function(i) {
    v = row[[i]]
    if(is.na(v)) {
      return("NULL")
    }
    if(is.integer(v)) {
      return(as.character(v))
    }
    if( is.double(v) ) {
      return(formatC(v, format="f", digits=10))
    }
    return(dQuote(as.character(v), FALSE))
  })
  paste(r, collapse = ",")
}

#' Structure to generate insert query
create_insert_query = function(prefix, values) {
  structure(list(
    prefix=prefix,
    values=values 
  ), class="insert_db_query")
}

print.insert_db_query = function(x, ...) {
  cat("Insert Query with ", length(x$values), " rows\n")
  cat("prefix: ", x$prefix, "\n")
  lapply(x$values, function(v) cat("- ", v, "\n"))
  invisible(NULL)
}

as.character.insert_db_query = function(x, ...) {
  paste(x$prefix," VALUES ", paste(x$values, collapse = ","))
}

#' Generate Query to insert active participant data
insert_active_query = function(data, country, file, method=NULL) {
  if(!is.null(method)) {
    data$method = method
  }
  data$country = country
  data$file = file
  columns = c('yw', 'country','method', 'active', 'file')
  rows = lapply(seq_along(data$country), function(row) {
      d = data[row, columns]
      vv = row_to_values(d)
      vv = paste0('(', vv,', datetime("now"))')
      vv
  })
  create_insert_query("INSERT OR REPLACE INTO active(yw, country, method, active, file, updated)", rows)#
}

#' incidence (yw , syndrome, type, country, method, incidence, lower, upper, count)
insert_incidence_query = function(data, country, file, method=NULL) {
  if( !is.null(method) ) {
    data$method = method
  }
  if(!hasName(data, 'lower')) {
    data$lower = NA
  }
  if(!hasName(data, 'upper')) {
    data$upper = NA
  }
  if(!hasName(data, 'count')) {
    data$count = NA
  }
  data$country = country
  data$file = file
  columns = c('yw', 'country', 'syndrome', 'type', 'method','incidence','lower','upper', 'count', 'file')
  n = hasName(data, columns)
  if(any(!n)) {
    rlang::abort(paste("Missing columns in data ", paste(columns[!n], collapse = ",")))
  }
  rows = lapply(seq_along(data$country), function(row) {
      d = data[row, columns]
      vv = row_to_values(d)
      vv = paste0('(', vv,', datetime("now"))')
      vv
  })
  create_insert_query(paste("INSERT OR REPLACE INTO incidence(",paste(columns, collapse = ','),", updated)"), rows)
}

mark_imported = function(db, file, country, weeks) {
  query = paste0('INSERT INTO imports (country,file,min_yw, max_yw,imported_at) VALUES ("',country,'","',file,'",',weeks[1],',',weeks[2],', datetime("now"))')
  db$exec(query)
}

#' Load data from externals db and mimic the old rds structure
load_externals = function(db) {
  inc = db$fetch("select yw,syndrome,type,country,method,incidence, lower,upper,count from incidence")
  active = db$fetch("select yw, active, method, country from active")
  inc$season = calc_season(inc$yw)
  active$season = calc_season(active$yw)
  list(
    incidence=inc,
    active=active
  )
}

