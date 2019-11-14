## 
# Import level data into db
##
source('conf.R')

library(glue)

file = 'nuts2013-postalcodes-CH.csv'
level = 'zip'

hh = geo_hierarchy()
hh = hh[which(hh == level):length(hh)]
names(hh) <- hh
hh = lapply(hh, geo_column)

data = read.csv(paste0('data/', file))

nn = names(data)
stopifnot(all(hh %in% nn))

columns = unlist(hh)

data = data[, columns]

query = paste0("insert into geo_",level,'("', paste(columns, collapse = '","'),'") values ')
query_data = paste0("(", paste(paste0("'{", columns,"}'"), collapse = ","),")")

qq = paste(query, paste(glue_data(data, query_data), collapse = ","))

dbQuery(qq)