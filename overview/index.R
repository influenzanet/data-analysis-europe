## Build field index
source("../system.R")

library(htmltools)

# base_analysis_url defined in location.R
base_url = function(u) {
  paste0(base_analysis_url, u)
}

build_index = function(dir) {
  desc_files = list.files(dir, pattern="^\\.d_", all.files = TRUE, full.names = FALSE)
  descriptions = list()
  lapply(desc_files,function(file) {
    p = file.path(dir, file)
    desc = readLines(p)
    org_file = gsub("^\\.d_","", file)
    descriptions[[org_file]] <<- list(file=org_file, desc=desc)
  })
  
  files = list.files(dir, pattern="\\.(pdf|png|svg|csv)$", all.files = FALSE, full.names = FALSE, include.dirs = TRUE)
  
  files = lapply(files, function(file) {
    target = basename(file)
    info = file.info(file.path(dir, file))
    d = descriptions[[target]]
    desc = NULL
    if(!is.null(d)) {
      desc = d$desc
    }
    withTags(
      li(class="list-group-item graph",
        a(href=target, target),
        span(class="badge badge-light", format(info$mtime, "%Y-%m-%d %T")),
        if(!is.null(desc)) small(desc, class="")
      )
    )
  })
  tags$ul(class="list-group graph-index", tagList(files))
}

build_dirs = function(dir) {
  dirs = list.dirs(dir, full.names = FALSE, recursive = FALSE)
  dirs = dirs[!grepl("\\.+", dirs)]
  dirs = dirs[dirs != ""]
  dirs = lapply(dirs, function(d) {
    tags$a(class="nav-link active", href=d, paste0("[", d,"]"))
  })
  tags$div("Subdirectories", class="subdirs",
    tags$nav(class="nav", tagList(dirs)) 
  )
}


build_navbar = function() {
  
  navs = list(
    "country"="Country results",
    "ecdc"="ECDC Indicators",
    "overview"="Overview results"
  )
  
  navs = Map(function(nav, link) {
    tags$a(class="nav-item nav-link", href=base_url(link), nav)
  }, navs, names(navs))
  
  withTags(
    nav("Influenzanet Results", class="navbar navbar-expand-lg navbar-light bg-light",
        div(class="collapse navbar-collapse", id="navbarNav",
         div(class="navbar-nav", tagList(navs))
        )
    )
  )
}

build_scripts=function() {
  tagList(
    tags$script(src="https://code.jquery.com/jquery-3.4.1.min.js", integrity="sha256-CSXorXvZcTkaix6Yvo6HppcZGetbYMGWSFlBw8HfCJo=", crossorigin="anonymous"),
    tags$script(src=base_url("pdf.js"))
  )
  #   <script src="https://cdn.jsdelivr.net/npm/popper.js@1.16.0/dist/umd/popper.min.js" integrity="sha384-Q6E9RHvbIyZFJoft+2mJbHaEWldlvI9IOYy5n3zV9zzTtmI3UksdQRVvoxMfooAo" crossorigin="anonymous"></script>
  #   <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/js/bootstrap.min.js" integrity="sha384-wfSDF2E50Y2D1uUdj0O3uMBJnjuUD4Ih7YwaYd1iqfktj0Uod8GCExl3Og8ifwB6" crossorigin="anonymous"></script>
} 


build_page = function(dir, name="index.html") {
 index = build_index(dir)
 dirs = build_dirs(dir) 
 page = withTags(
   html(
     head(
       link(rel="stylesheet", href="https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css",integrity="sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh", crossorigin="anonymous"),
       link(rel="stylesheet", href=base_url("pdf.css"))
     ),
     body(
       build_navbar(),
       div(class="alert alert-warning","All graphics and results available on these pages are for internal purposes of Influenzanet only, please do not link diretly these files on any website"),
       h1(basename(dir)),
       div(class="container",
         dirs,
        index
       ),
       build_scripts()
     )
   )
 )
 save_html(page, file.path(dir, name))
}

add_path_prefix("project", "")
init.path('')

libs = c('pdf.js','pdf.css')
for(f in libs) {
  lib = share_path(file.path("resources/", f))
  target =my.path(f)
  if(!file.exists(target)) {
    file.symlink(lib, target)
  }  
}

dirs = list.dirs(my.path(), recursive = TRUE, full.names = TRUE)
for(dir in dirs) {
  cat(dir,"\n")
  build_page(dir)
}


