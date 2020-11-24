# Index 
## Manage output directory index 
## This  script doesnt do data analysis, just prepare the output diretories

source("conf.R")

add_path_prefix("project","")

dirs = list(
  list(
    dir="", 
    nofile=TRUE, 
    nodir=TRUE,
    readme=list(
      title="Influenzanet internal results",
      desc=c(
          "Welcome on Influenzanet website for internal results",
          "These pages presents the analysis run with the data shared by participating platforms",
          "They are intented for being used and shared only by the participating partners,",
          "please do not share the link of these website outside the INfluenzanet group."
      ),
      links=list(
        "country"="By country results",
        "overview"="Global results at european levels and comparing countries",
        "ecdc"="ECDC indicators"
      )
    )
  ),
  list(
    dir="country", 
    nofile=TRUE, 
    readme=list(
      title="Results by country"
    )
  ),
  list(
    dir="overview", 
    nofile=TRUE, 
    readme=list(
      title="Global results and country comparisons",
      desc=c(
        "Analysis are done by season, select a sub directory for the season you want to see"
      )
    )
  ),
  list(
    dir="ecdc", 
    nofile=TRUE, 
    readme=list(
      title="ECDC Results",
      links=list(
        "./indicator"="ECDC Indicators",
        "./archives"="Archives"
      )
    )
  ),
  list(
    dir="ecdc/indicator", 
    nofile=TRUE, 
    readme=list(
      title="ECDC Results",
      links=list(
        "./all"="All results",
        "./bundles"="Selected data for ECDC and website"
      )
    )
  )
  
  
)

create_readme = function(r, dir) {
  out = c()
  if(hasName(r, "title")) {
    out = c(out, paste0('# ', r$title, "\n"))
  }
  if(hasName(r, "desc")) {
    out = c(out, paste0(paste(r$desc, collapse="\n"), "\n"))
  }
  if(hasName(r, "links")) {
    ll = Map(function(text, link) {
      if(grepl("^\\./", link)) {
        link = gsub("^\\.", "", link)
        link = paste0(dir, link)
      }
      paste0('- [',text,'](',link,')')  
    }, r$links, names(r$links))
    out = c(out, "\n", paste(ll, collapse = "\n"),"\n")
  }
  out = paste(out, collapse = "")
  out
}

readme_file = function(from) {
  f = try(readLines(from))
  readme = NULL
  if(is(f, "try-error")) {
    print(f)
  } else {
    if(length(f) > 0) {
      f = f[1]
      readme = readLines(f)
      readme = paste(readme, collapse="\n")
    }
  }
  readme
}
 
init.path('')

for(def in dirs) {
  message(def$dir)
  init.path(def$dir)
  print(my.path())
  if(isTRUE(def$nofile)) {
    write("", my.path('.nofile'))
  }
  
  if(isTRUE(def$nodir)) {
    write("", my.path('.nodir'))
  }
  
  if(hasName(def, "readme")) {
    if(is.list(def$readme)) {
      readme = create_readme(def$readme, def$dir)
    } else {
      readme = readme_file(def$readme)
    }
    write(readme, my.path('README.md'))
  }

}