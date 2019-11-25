## Check DB Quality

source('conf.R')

library(dplyr)
library(reshape2)


check_global_id = function(table) {
  r = dbQuery('select count(*) as total, sum(case when global_id is null or global_id=\'\' then 1 else 0 end) missing_gid, "country",  extract(year from timestamp) as "year" from epidb_results_',table,' group by "country", "year" ')
  r = r %>% mutate(prop=100 * missing_gid/total, lab=paste0(missing_gid," (", round(prop),"%)"))
}

r_intake = check_global_id("intake")
r_weekly = check_global_id("weekly")


ht = function(data, row, col, value) {
  rr = by(data, data[, col, drop=FALSE], function(r) {
    r[, c(row, value), drop=FALSE]
  })
  o = NULL
  for(i in 1:length(rr)) {
    d = rr[[i]]
    names(d) <- c(row, names(rr[i]))
    if(is.null(o)) {
      o = d
    } else {
      o = merge(o, d, by=row, all=TRUE)
    }
  }
  o
}

ht(r_intake, "country","year", "lab")

ht(r_weekly, "country","year", "lab")
