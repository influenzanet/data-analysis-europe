
init.path('externals/dk2')

dkc.country = "DKI"

ff = find_last_file(my.path(), "DK_data.*\\.csv$", use.suffix=TRUE)
inc = NULL
if(length(ff) > 0) {
  file = ff[1]
  
  already = check_file_imported(db, file, dkc.country)
  
  if(nrow(already) == 0) {
    message("Loading ", file)
    r = read.csv2(my.path(file), header = TRUE)
    r = rename(r, week="Yearweek", active="Weekly_Responses", "count"="ILI_cases")
    r$X = NULL
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


