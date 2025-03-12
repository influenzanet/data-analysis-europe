## Sweden

init.path('externals/se')

se.country = "SE"
ff = find_last_file(my.path(), "Influenzanet_.*\\.csv$", use.suffix=TRUE)
r = NULL
file = NULL
if(length(ff) > 0) {
  file = ff[1]
  message("Loading ", file)
  r = read.csv2(my.path(file), header = TRUE, na.strings="-")
  n = names(r)
  # Normalize names to better names
  n <- tolower(n)
  n[ n == "numberparticipants"] = "active"
  n = gsub("._", "_", n, fixed=TRUE)
  n = gsub("\\.$", "", n)
  n = gsub(".", "_", n, fixed=TRUE)
  names(r) <- n
  r$yw = as.integer(r$year * 100 + r$week)
  
  r = r %>% filter(!is.na(active))
}

indicators = list(
  list(name="ili_per1000", syndrome="ili.ecdc"),
  list(name="ari_per1000", syndrome="ari.ecdc")
)

if(!is.null(r) && nrow(r) > 0) {
  method = "unknow"
  
  active = r %>% select(yw, active)
  query = insert_active_query( active , country=se.country, file=file, method=method)
  db$exec(query)
  factor = 1000
  for(ind in indicators) {
    cols = paste0(ind$name, c('', '_up','_low'))
    names(cols) <- c('incidence','upper','lower')
    inc = r %>% select(yw, !!!(cols)) %>% mutate(incidence=incidence/factor, upper=upper/factor, lower=lower/factor)
    
    inc$syndrome = ind$syndrome
    inc$type ="raw"
    query = insert_incidence_query(inc, country=se.country, file=file, method=method)
    db$exec(query)
  }  

  weeks = range(inc$yw)
  mark_imported(db, country=se.country, file, weeks)
  # Last evaluated expression is returned to calling script as result (here number of rows)
  
  mark_file_done(my.path(file))
  
}