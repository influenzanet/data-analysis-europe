## Check DB Quality

source('conf.R')

library(dplyr)
library(reshape2)
library(ggplot2)

init.path("quality")

check_global_id = function(table) {
  r = dbQuery('select count(*) as total, sum(case when global_id is null or global_id=\'\' then 1 else 0 end) missing_gid, "country",  extract(year from timestamp) as "year" from epidb_results_',table,' group by "country", "year" ')
  r = r %>% mutate(prop=100 * missing_gid/total, lab=paste0(missing_gid," (", round(prop),"%)"))
}

r_intake = check_global_id("intake")
r_weekly = check_global_id("weekly")

rr = bind_rows(r_intake, r_weekly, .id = 'survey')

ggplot(rr, aes(x=factor(year), y=prop, fill=survey)) + 
    geom_bar(stat="identity", position = "dodge") + 
    facet_grid(rows=vars(country)) +
    labs(y="% of missing global_id", x="Year", title="Missing global_id by country, year and survey") +
    scale_fill_discrete(labels=c("1"="Intake","2"="Weekly")) 
ggsave(my.path('missing_global_id.pdf'), width=8, height=15)

write.csv2(rr, my.path('missing_global_id.csv'), row.names = FALSE)

