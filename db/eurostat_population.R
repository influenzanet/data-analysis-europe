source("conf.R")
library(eurostat)
library(dplyr)
library(reshape2)
library(glue)

options(warn=1)

output.years = 2012:2018

file.prefix = paste(range(output.years),collapse = "-")

out.path = function(...) {
  my.path(paste0("population/",file.prefix, "/", ...))
}

dir.create(out.path())

file.remove(Sys.glob(out.path('*')))

countries = c("BE", "DK",  "ES", "FR", "IE", "IT", "NL", "PT", "SE", "UK", "CH")

exclude.levels = c("Z+$","^FR9","^FRA")

pop_eu = get_eurostat("demo_r_pjangroup", type="code")

# NUTS2 level (2010 version !)
nuts2 = dbQuery("select country, code_nuts1, code_nuts2 from geo_nuts2")

pop_eu$year = as.integer(format(pop_eu$time, "%Y"))

pop_eu$country = substr(pop_eu$geo, 1, 2)

pop_eu = pop_eu %>% filter(country %in% countries)

pop_eu$level = nchar(as.character(pop_eu$geo)) - 2

min.year = 2010

# Load demographic data
for(year in output.years) {

  nuts.levels = nuts2$code_nuts2
  cat("Computing ", year,"\n")
  d = pop_eu %>% filter(year <= .env$year & year >= .env$min.year)

  d = rename(d, value=values)

  d$geo = as.character(d$geo)

  if(nrow(d) == 0) {
    stop("Unable to compute year", year)
  }

  d = d[, c('sex', 'age', 'geo', 'value', 'year')]

  ## Recode geo UK for NUTS2010 compatibility with 2013
  d$geo[ d$geo %in% c('UKI3', 'UKI4')] = 'UKI1'
  d$geo[ d$geo %in% c('UKI5', 'UKI6', 'UKI7')] = 'UKI2'

  d = d %>% filter(geo %in% nuts2$code_nuts2)

  x1 = dcast(d, year + geo + age ~ sex, fun.aggregate=sum)
  x1 =  rename(x1, "female"="F", "male"="M", "all"="T")

  x1 = merge(x1, nuts2, by.x="geo", by.y="code_nuts2", all=T)

  stopifnot( all(!is.na(x1$country)))

  stopifnot(all(!is.na(x1$age)))

  # Get last available year for each country
  y = x1 %>% group_by(geo) %>% filter(!is.na(all)) %>% summarize(max_year=max(year))

  if( !all(is.finite(y$max_year)) ) {
    print( y[ !is.finite(y$max_year), ] )
    cat("No data for this year in some area. Population cannot be computed\n")
    next()
  }

  y = merge(y, nuts2, by.x='geo', by.y='code_nuts2', all.x=T)
  y$country = factor(y$country)
  y$geo = factor(y$geo)

  available.years = tapply(y$max_year, y$country, function(x) length(unique(x)))
  if(! all(available.years == 1)) {
    country = names(available.years[available.years > 1])
    warning(paste(paste(country, collapse = ",")," has different year available for different levels"))
  }

  country.year = aggregate(max_year ~ country, data=y, min)

  i = country.year$max_year < max(country.year$max_year)
  if(any(i)) {
    cat(paste("Using ", country.year$max_year[i], "for country ", country.year$country[i]),sep="\n")
  }

  country.year = rename(country.year, "year.ref"="max_year")

  x1 = merge(x1, country.year, by='country', all.x=T)

  # Only keep reference year for each country
  pop = x1[ x1$year == x1$year.ref, ]

  # Check that's ok
  table(pop$country, pop$year.ref)

  pop$i = 1
  check = aggregate(i ~ geo + age , data=pop, sum)
  stopifnot(  all(check$i == 1) )

  max.age = 200

  pop$age.min = NA
  pop$age.max = NA

  pop.allages = pop[ pop$age == "TOTAL", ]

  pop = pop[ !pop$age == "TOTAL", ]
  pop$age = factor(pop$age)

  # Recode age group label to age.min and age.max columns
  i = grepl("^Y([0-9]+)\\-([0-9]+)", pop$age)
  group = strsplit(gsub("^Y([0-9]+)\\-([0-9]+)","\\1;\\2", pop$age[i]), split=";",fixed=T)
  group = do.call(rbind, lapply(group, function(x) { x = as.numeric(x); data.frame(min=x[1], max=x[2]) }))

  pop$age.min[i] = group$min
  pop$age.max[i] = group$max

  i = pop$age == "Y_LT5"
  pop$age.min[i] = 0
  pop$age.max[i] = 4

  i = grepl("^Y_GE([0-9]+)", pop$age)
  group = as.numeric(gsub("^Y_GE([0-9]+)","\\1", pop$age[i]))
  pop$age.min[i] = group
  pop$age.max[i] = NA

  # Select optimal bound (greatest age with all countries)
  pop$bound = is.na(pop$age.max) & !is.na(pop$age.min)

  bounds = table(pop$country[pop$bound], pop$age.min[pop$bound])
  bounds = bounds > 0

  # Which bound is available for all countries
  bounds.all = apply(bounds, 2, all)
  bounds.all = max(as.numeric(names(bounds.all)[which(bounds.all)]))

  cat("Selected bounds = ", bounds.all,"\n")

  # Remove other kind of bounds
  pop$bounds.ok = pop$bound & pop$age.min == bounds.all
  pop = pop[ !(pop$bound & !pop$bounds.ok), ]

  pop$age = factor(pop$age)

  age.groups = unique(pop[, c('age','age.min', 'age.max')])
  age.groups = age.groups[ order(age.groups$age.min), ]

  i = age.groups$age != "UNK"
  age.groups$min.expected[i] = seq(0, bounds.all, by=5)
  age.groups$max.expected[i] = age.groups$min.expected[i] + 5
  age.groups$max.expected[ age.groups$min.expected == bounds.all] = NA

  # Check all expected groups are ok
  stopifnot(all(age.groups$min[i] == age.groups$min.expected[i]))

  check = aggregate(i ~ geo + age , data=pop, sum)

  pop.frame = merge(data.frame(geo=nuts2$code_nuts2), data.frame(age=age.groups$age))

  pop = merge(pop, pop.frame, by=c('geo','age'), all=T)
 
  pop = pop[ pop$age != "UNK", ] # Remove unknown age-group category (actually always 0)

  if( any(is.na(pop$all)) ) {
    print( pop[ is.na(pop$all),])
    stop("Some population are unknown in expected geo levels or age groups")
  }

  # Now that's ok

  pop = rename(pop, code_nuts2=geo)

  cat("Reference year for this year")
  print(table(pop$country, pop$year.ref))
  #print(table(pop$country, pop$year.ref, pop$age))


  # Aggregate to a given level
  agg_pop = function(data, groups) {
    p = data %>%
      group_by(!!!syms(groups)) %>%
      summarize(all=sum(all), male=sum(male), female=sum(female), year.ref=min(year.ref))
  }

  agg_pop_age = function(data, groups) {
    p =  data %>%
      group_by(!!!syms(groups), age) %>%
      summarize(all=sum(all), male=sum(male), female=sum(female), year.ref=min(year.ref))
    p = merge(p, age.groups[,c('age','age.min','age.max')], by='age', all.x=T)
    p = subset(p, select=-age)
    p
  }


  pop_country = agg_pop(pop, 'country')

  pop_check = pop_eu %>% filter(age == "TOTAL" & geo %in% pop_country$country) %>% select(sex, geo, year, values)
  pop_check$geo = factor(pop_check$geo)
  pop_check = rename(pop_check, country=geo)
  pop_check = dcast(pop_check, year + country ~ sex, fun.aggregate=sum, value.var = "values")
  pop_check = rename(pop_check, "female"="F","male"="M","all"="T")

  suffixes = c('.agg','.check')
  pop_check = merge(pop_country, pop_check, by.x=c('country','year.ref'), by.y=c('country','year'), all.x=T, suffixes =suffixes )

  for(v in c('male','female','all')) {
    i = apply(pop_check[, paste0(v, suffixes)], 1, diff) > 0
    if(any(i)) {
      cat("Difference for male  for")
      print(pop_check[i, c('country', paste0(v, suffixes)) ])
    }
  }
  
  output_pop = function(data, level, age=FALSE) {
    f = paste0('pop_', ifelse(age, "age_", ""), level)
    
    if(level == "country") {
      column = "country"
      
      columns = column
      
    } else {
      column = paste0('code_', level)
      columns = c("country", column)
    }

    if(!hasName(data, "country")) {
      data$country = substring(data[[column]], 1, 2)
    } 
    
    if(age) {
      data = data[ order(data[[column]], data$age.min), ]
      data = rename(data, age_min="age.min")
    } else {
      data = data[ order(data[[column]]), ]
    }
    
    
    write.csv2(data, file=out.path(year,'_', f, '.csv'), row.names=F)

    build_query = function(data, columns, table) {
      qq = paste0('insert into ',table, ' ("', paste(columns,collapse='","'),'")  values \n')
      
      cc = lapply(columns, function(column) {
        if(is.character(data[[column]]) || is.factor(data[[column]])) {
          quote ="'"
        } else {
          quote = ""
        }
        paste0(quote, '{', column,'}', quote)
      })
      
      template = paste0("(", paste(cc, collapse=","),")")
      
      qq = paste(qq, paste(glue_data(data, template), collapse = ",\n"), ";\n")
      qq
    }
    
    data = rename(data, "year_ref"="year.ref")
    
    columns = c(columns, "year", "year_ref", "all", "male", "female")
    
    if(age) {
      table = paste0("pop_age5_", level)
      
      columns = c("age_min", columns)
      qq = build_query(data, columns, table)
    
    } else {
      table = paste0("pop_",level)
      qq = build_query(data, columns, table)
    }
    fn = out.path(f, '.sql')
    write(paste("\n\n-- year ", year, " level ", level," table ",table,"\n"), file=fn, append=TRUE)
    write(paste0("delete from ", table, " where \"year\"=",year,";\n"), file=fn, append=TRUE)
    write(qq, file=fn, append=TRUE)
  }

  output_pop(pop[, c('age.min','age.max','all','male','female','year.ref','country', 'code_nuts2')], 'nuts2', age=TRUE)

  p = agg_pop(pop, 'code_nuts2')

  output_pop(p, 'nuts2')

  p = agg_pop_age(pop, 'code_nuts1')
  output_pop(p, 'nuts1', age=TRUE)

  p = agg_pop(pop, 'code_nuts1')
  output_pop(p, 'nuts1')

  p = agg_pop(pop, 'country')
  output_pop(p, 'country')

  p = agg_pop_age(pop, 'country')
  output_pop(p, 'country', age=TRUE)

}

## Now create bundles
## All age population in one
pattern = "(\\d+)_pop_age_(\\w+)\\.csv$"
files = list.files(out.path(), pattern=pattern, full.names = TRUE)

data = list()
for(file in files) {
  cat("Loading", basename(file),"\n")
  year = as.integer(gsub(pattern,"\\1", basename(file)))
  geo =  gsub(pattern,"\\2", basename(file))
  d = read.csv2(file)
  d$year = year 
  data[[geo]] = bind_rows(data[[geo]], d)
}

dir.create(out.path('bundles'))

lapply(names(data), function(geo) {
  d = data[[geo]]
  
  by(d, d[, 'country', FALSE], function(dd) {
      country = dd$country[1]
      dd$country = NULL
      write.csv2(dd, file=out.path('bundles/',country,'_pop_age5_',geo, '.csv'), row.names = F)
  })

  write.csv2(d, file=out.path('bundles/all_',geo, '.csv'), row.names = F)
})


