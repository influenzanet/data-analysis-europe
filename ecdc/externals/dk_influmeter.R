
init.path('externals/dk2')

dkc.country = "DKI"

#' rename columns to a given name, but each rename can be a list
#' @param .data data.frame()
#' @param ... list renaming spec name is target column, value can be a list of columns 
rename_lazy = function(.data, ...) {
  nn = tolower(names(.data))
  renames = list(...)
  for(i in seq_along(renames)) {
    from = unique(renames[[i]])
    to = names(renames)[i]
    i = which(from %in% nn) 
    if(length(i) == 1) {
       n = from[i]
       message("Found ", i, ": ", n, " -> ", to)
       nn[ nn == n ] = to
    }
  }
  names(.data) <- nn
  .data
}

files_catalog = function(path, reader) {
  files = list.files(path, full.names = TRUE)
  nn = lapply(files, function(file) {
    d = try(reader(file))
    if(inherits(d, "try-error")) {
      warning(paste("Unable to parse", file))
      return(c())
    }
    names(d)
  })
}

ff = find_last_file(my.path(), "DK_data.*\\.csv$", use.suffix=TRUE)
inc = NULL
if(length(ff) > 0) {
  file = ff[1]
  
  already = check_file_imported(db, file, dkc.country)
  
  if(nrow(already) == 0) {
    message("Loading ", file)
    r = read.csv2(my.path(file), header = TRUE)
    
    r = rename_lazy(r, week=c('yearweek', 'week', 'weekly_yearweek'), active=c('weekly_responses','responses', 'antal_svar'), count='ili_cases')
    
    expected.columns = c('week', 'active', 'count')
    
    if(!all(expected.columns %in% names(r))) {
      e = expected.columns[!expected.columns %in% names(r)]
      stop(paste("Some columns are missing", paste(sQuote(e), collapse = ","), " found", paste(sQuote(names(r)), collapse = ",")))
    }
    r = r[, expected.columns]
    r$yw = yw_from_isoweek(r$week)
    r$time = as.numeric(gsub(".*_(\\d+)\\.csv$","\\1", file))
    r$incidence = r$count / r$active 
    r = r %>% select(yw, incidence, count, active, time)
    r$file = file
    inc = bind_rows(inc, r)
  } else {
    message("File already imported ", file)
    print(already)
  }
}

if(!is.null(inc) && nrow(inc) > 0) {
  inc$syndrome = "ili.ecdc"
  inc$yw = as.integer(inc$yw)
  inc$type = "raw"
  inc$method = "w0"
  inc = inc %>% select(-time)

  active = inc %>% select(yw, method, active, file)

  r = insert_active_query( active , dkc.country, file=file)
  db$exec(r)

  r = insert_incidence_query(inc, country=dkc.country, file=file)
  db$exec(r)
  
  weeks = range(inc$yw)
  mark_imported(db, country=dkc.country, file, weeks)

}

# Last evaluated expression is returned to calling script as result (here number of rows)
nrow(inc)


