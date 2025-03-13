
incidence_generic_parser = function(file) {
  rows = read.csv(file, header = TRUE, comment.char = "#")
  names(rows) <- tolower(rows)
  mandatory.columns = c('yearweek','active_count','incidence_rate')
  i = mandatory.columns %in% names(rows)
  if(any(!i)) {
    stop(paste(
          "Missing columns", paste(sQuote(mandatory.columns[i]), collapse = ","), 
          " found: ", 
          paste(sQuote(names(rows), collapse = ","))
      ))  
  }
  rows = parser_yearweek(rows)
}

parse_yearweek = function(rows) {
  rows$yw = yw_from_isoweek(rows$yearweek, warn=FALSE)
  i = grepl("[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}", rows$yearweek)
  if( any(i) ) {
    rows$yw[i] = iso_yearweek(monday_of_date(as.Date(rows$yearweek[i])))
  }
  i = grepl("[0-9]{6}", rows$yearweek)
  if( any(i) ) {
    rows$yw[i] = rows$yearweek[i]
  }
  i = is.na(rows$yw)
  if(any(i)) {
    print(rows[i, c('yw','yearweek')])
    stop(paste("Unable to parse yearweek for some columns at rows", paste(which(i), collapse = ",")))
  }
  rows
}

test_yearweek_parser = function(values, expected, expect.error=FALSE) {
  rows = data.frame(yearweek=values)
  r = try(parse_yearweek(rows))
  if(inherits(r, "try-error")) {
    if(expect.error) {
      return(TRUE)
    }
    print(r)
    stop("Problem parsing yearweek")
  }
  if(any(r$yw != expected)) {
    print(r)
    stop("Invalid parsed yearweek values")
  }
  TRUE
}

test_parser = function() {
  test_yearweek_parser(c('202110','2024W10','2023-W04'), expected=c("202110","202410","202304"))
  test_yearweek_parser(c('2025-02-10','2024-01-01','2023-01-02'), expected=c("202507","202401","202301"))
}



