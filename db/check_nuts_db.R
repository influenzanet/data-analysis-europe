##
# Check if all NUTS levels are availables
##
source("conf.R")

countries = platform_env("COUNTRY_CODES")

data = read.csv("data/NUTS_AT_2013.csv", encoding = "UTF8")

data = rename(data, "country"="CNTR_CODE","code"="NUTS_ID", "label"="NUTS_NAME")

data$level = nchar(as.character(data$code))-2

geo.levels = 1:3

missings = NULL

for(level in geo.levels) {
  
  d = data %>% filter(country %in% countries & level == .env$level) %>% select(country, code)
  d$nuts = TRUE
  level_name = paste0("nuts", level)
  def = geo_level_table(level_name)
  
  entries = dbQuery('select country, ', def$column, " as code from ", def$table)
  entries$db = TRUE
  
  entries = merge(entries, d, by=c('country','code'), all=TRUE, suffixes=c('.db','.eu'))
  
  entries$db[is.na(entries$db)] = FALSE
  entries$nuts[is.na(entries$nuts)] = FALSE
  
  m = entries %>% filter(!db) %>% select(country, code) %>% mutate(where="db", level=level)
  missings = bind_rows(missings, m)

  m = entries %>% filter(!nuts) %>% select(country, code) %>% mutate(where="nuts", level=level)
  missings = bind_rows(missings, m)

}