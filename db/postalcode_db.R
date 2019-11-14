## PostCode upgrades
##
library(dplyr)
countries = c("BE", "DK",  "ES", "IE", "IT", "NL", "PT", "SE", "UK", "CH")

paths = list(
    list(path='pc2016_NUTS-2013', prefix='pc(2016)?', version=2013),
    list(path='pc2018_NUTS-2016_v3.0', prefix='pc2018', version=2016)
)

data = NULL

for(p in paths) {
  cat(p$path,"\n")
  for(country in countries) {
    cat("Country", country, "\n")
    file = list.files(paste0('data/postal/', p$path,'/'), paste0("^", p$prefix,'_', tolower(country)), full.names = TRUE)
    if(length(file) > 1) {
      stop("Only one file by country")
    }
    if(length(file) == 0) {
      cat("No data for ", country,"\n")
      next()
    }
    zip = read.csv2(file)


    if(ncol(zip) == 1) {
      zip = read.csv(file)
    }
    n = names(zip)
    # Rename columns (differents accross files & version, oh gosh.)
    n[ grepl('CODE', n)] = "zip"
    n[ grepl('NUTS.?3',n)] = "nuts3"
    names(zip) <- n
    zip$zip = gsub('[" \\\']','', zip$zip)
    zip$nuts3 = gsub('[" \\\']','', zip$nuts3)
    zip$zip = factor(zip$zip)
    zip$version = p$version
    zip$country = country
    data = bind_rows(data, zip)
  }
}

data$nuts3 = gsub('[" \\\']','', data$nuts3)

data$nuts3 = factor(data$nuts3)
data$country = factor(data$country)

saveRDS(data, file="data/postal_codes.rds")

r1 = data %>% filter(version==2013) %>% rename(nuts3_2013=nuts3) %>% select(-version)
r2 = data %>% filter(version==2016) %>% rename(nuts3_2016=nuts3) %>% select(-version)

rr = merge(r1, r2, by=c('zip','country'), all=TRUE)

ftable(table(is.na(rr$nuts3_2013), is.na(rr$nuts3_2016), rr$country, dnn=c('2013','2016','country')))
