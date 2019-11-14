##
# Build Nuts levels
##
source("conf.R")
library(rlang)
library(dplyr)
library(stringr)
library(glue)

country = "CH"

# Data Files should be found on EUROSTAT
# https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/nuts/
# https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/nuts/csv/NUTS_AT_2013.csv
# https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/nuts/csv/NUTS_AT_2016.csv
data = read.csv("data/NUTS_AT_2013.csv", encoding = "UTF8")

data = rename(data, "country"="CNTR_CODE","code"="NUTS_ID", "label"="NUTS_NAME")

data$level = nchar(as.character(data$code))-2

geo.levels = 1:3

geo_nuts = function(level) {
  paste0('nuts', level)
}

out.file = paste0("data/nuts-",country,".sql") 
if(file.exists(out.file)) {
  file.remove(out.file)
}

out_query = function(...) {
  w = paste0(...,"\n")
  Encoding(w) <- "UTF8"
  write(w, file=out.file, append=TRUE)
}

for(lev in geo.levels) {
  d = data %>% filter(country == .env$country & level==lev)
  
  d$label = as.character(d$label)
  Encoding(d$label) <- "UTF-8"
  
  level_name = geo_nuts(lev)
  geo = geo_level_table(level_name) # Geographic level Table definition
  
  d = rename(d, !!geo$column :="code", !!geo$title := "label")
  
  columns = c('country', geo$column, geo$title)
  
  if(lev > 1) {
    for(g in unique((lev-1):1)) {
      up_level = geo_nuts(g)
      column = geo_column(up_level)
      d[[column]] = str_sub(d[[geo$column]], 1, 2 + g)
      columns = c(columns, column)
    }
  }
  
  query = paste0("insert into ", geo$table,' ("', paste(columns, collapse = '","'),'") values ')
  query_data = paste0("('{", paste(columns, collapse = "}','{"), "}')")
  query_data = glue_data(d, query_data)
  query = paste0(query, paste(query_data, collapse = ",\n"),";")

  out_query('-- level ',lev, ' ', level_name)
  out_query("delete from ", geo$table, " where country='", country,"';")
  out_query(query)
    
}

