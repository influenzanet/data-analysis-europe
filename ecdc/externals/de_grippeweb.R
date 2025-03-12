## Load DE data
init.path('externals/rki')

rki.country = "DE"

file = find_last_file(path=my.path(), pattern="ForInfluenzanet_Germany_GrippeWeb_", use.suffix = TRUE)

already = check_file_imported(db, file, rki.country)
if(nrow(already) == 0) {

  message("Loading ", file)
  
  d = read.csv2(my.path(file))
  names(d) <- tolower(names(d))
  d = rename(d, active=numberparticipants)
  syndromes = names(d)
  syndromes = syndromes[ !syndromes %in% c('year','week', 'active')] # List of columns containing syndromes
  
  d$yw = as.integer(d$year * 100 + d$week)
  
  de.inc = NULL
  
  for(column in syndromes) {
    syndrome = switch(column, "ili"="ili.ecdc", rlang::abort(paste0("Unknown syndrome", sQuote(column))))
    
    inc = d[, c('yw','active', column)]
    inc = rename(inc, part=active, incidence=all_of(column))
    inc$syndrome = syndrome
    inc$method = "unknown"
    inc$incidence = inc$incidence / 100 # Provided as percentage
    de.inc = bind_rows(de.inc, inc)
  }
  
  de.inc$type = "adj"
  
  de.active = de.inc %>% group_by(yw, method) %>% summarize(active=max(part)) %>% ungroup()
  de.active$active = as.integer(de.active$active)
  
  r = insert_active_query( de.active , rki.country, file=file)
  db$exec(r)
  
  r = insert_incidence_query(de.inc, country=rki.country, file=file)
  db$exec(r)
  
  weeks = range(de.inc$yw)
  mark_imported(db, country=rki.country, file, weeks)
  # Last evaluated expression is returned to calling script as result (here number of rows)
  nrow(de.inc)
  
} else {
  0
}

