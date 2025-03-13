#' Mark file already imported
done.marker =".done"

find_files_pattern = function(path, pattern, use.suffix=NULL, decreasing=TRUE) {
  ff = list.files(path, pattern, recursive = FALSE, full.names = FALSE)
  r = !file.exists(paste0(path, ff, done.marker)) # Exclude file with a .done file
  ff = ff[r]
  if( !is.null(use.suffix) ) {
      suffix = sub("([^.]+)\\.[[:alnum:]]+$", "\\1", ff)
      if(use.suffix == "D_T") {
        suffix.pattern = ".*_(\\d{8}_?\\d{4})$"
      } else {
        suffix.pattern = ".*_(\\d{14})$"
      }
      suffix = gsub(suffix.pattern, "\\1", suffix, perl=TRUE)
      ff = ff[order(suffix, decreasing = decreasing)]
  } else {
    ff = sort(ff, decreasing = decreasing)
  }
  ff
}

# Create file marker to set the file as done
mark_file_done = function(file) {
  writeLines(as.integer(Sys.time()), con=paste0(file, done.marker))
}

#' Parse week from iso format to numeric yearweek (year * 100 + week)
#' Can parse {year}W{week}, {year}-W{week}, {year}W{week}-{day}, {year}-W{week}-{day}
#' @return int[]
yw_from_isoweek = function(week, warn=FALSE) {
  pattern = "(\\d+)-?W(\\d+)(-\\d+)?"
  i = grepl(pattern, week)
  w = rep(NA, length(week))
  w[i] = gsub(pattern, "\\1\\2", week[i])
  j = is.na(w) & !is.na(week)
  if(warn && any(j)) {
    warning("Some week number has not been parsed : ", paste(which(j), collapse = ","))
  }
  w
} 

find_last_file = function(path, pattern, use.suffix=NULL) {
  ff = find_files_pattern(path=path, pattern=pattern, use.suffix = use.suffix)
  ff[1]
}

# List of breaks to compute season number for each week
seasons = sapply(get_historical_seasons(), function(season) {
    iso_yearweek(as.Date(as.character(get_season_dates(season)$start)))
}, USE.NAMES = TRUE, simplify = TRUE)

find_season = function(yw) {
  as.integer(as.character(cut(yw, breaks=c(seasons, Inf), labels=names(seasons), include.lowest = TRUE, right=FALSE)))
}
