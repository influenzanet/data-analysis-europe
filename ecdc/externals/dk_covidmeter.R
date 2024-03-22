
init.path('externals/dkcovid')

ff = find_files_pattern(my.path(), "Denmark_week_.*\\.csv$", use.suffix=TRUE, decreasing = FALSE)
inc = NULL
for(file in ff) {
   r = read.csv2(my.path(file), header = TRUE)
   message("Loading ", file)
   r = rename(r, week="Week.Number", active="Active.participants", "inc1000"="Incidence")
   r$X = NULL
   r$yw = gsub("(\\d+)-W(\\d+)", "\\1\\2", r$week)
   r$time = as.numeric(gsub(".*_(\\d+)\\.csv$","\\1", file))
   r$incidence = as.numeric(as.character(r$inc1000)) / 1000 
   r$count = r$incidence * r$active
   r = r %>% select(yw, incidence, count, active, time)
   r$file = file
   inc = bind_rows(inc, r)
}

dkc.country = "DKC"

inc$syndrome = "covid.ecdc"
inc$yw = as.integer(inc$yw)
inc$type = "raw"
inc$method = "unknown"
inc = inc %>% select(-time)

active = inc %>% select(yw, method, active, file)

r = insert_active_query(active , dkc.country, file=file)
db$exec(r)

r = insert_incidence_query(inc, country=dkc.country, file=file)
db$exec(r)
weeks = range(inc$yw)
mark_imported(db, country=dkc.country, file, weeks)

