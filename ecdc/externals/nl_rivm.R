## Load NL data
init.path('externals/rivm')

country ="NL"

file = find_last_file(path=my.path(), pattern='active_.*\\.csv', use.suffix = "D_T")
message("Loading ", file)

nl.active = read.csv2(my.path(file))
nl.active = nl.active %>% select(yw, active, method)

r = insert_active_query( nl.active, country, file=file)
db$exec(r)

file = find_last_file(path=my.path(), pattern='incidence_.*\\.csv', use.suffix = "D_T")

nl.inc = read.csv2(my.path(file))
nl.inc = nl.inc %>% select(yw, syndrome, incidence, type, upper, lower, count, method)

r = insert_incidence_query(nl.inc, country=country, file=file)
db$exec(r)

weeks = range(nl.inc$yw)
mark_imported(db, country=country, file, weeks)