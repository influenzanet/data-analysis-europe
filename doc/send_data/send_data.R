library(curl)
library(jsonlite)

#' @param kind Kind of data to send, for example 'incidence' for incidence dataset
#' @param file path to the file to send
#' @param token authentication token to use (provided by Influenzanet tech team)
#' @param debug show debug information (curl will become very verbose)
#' @param print.result show information about result
#' @param validate.only if TRUE, only run file validation file will not be stored on the server
#' @return list with response values (see details)
#' 
#' @details
#' Response is a list with the following elements
#' \describe{
#'  \itemize{url}{URL of the server used for the request}
#'  \itemize{status_code}{HTTP status code returned in response, 200-299 is OK}
#'  \itemize{type}{Mime type of the response, usually 'application/json'}
#'  \itemize{content}{Response body as a byte vector}
#'  \itemize{response}{list with parsed element from the body response}
#' }
#' 
#' 
send_to_influenzanet = function(kind, file, token, debug=FALSE, print.result=TRUE, validate.only=FALSE) {

  curl = curl::new_handle()
  
  curl::handle_setform(curl, file=curl::form_file(file))
  
  opts = list()
  
  if(debug) {
    #debugger = debug_gatherer()
    opts$verbose = TRUE
    #opts$debugfunction = debugger$handler
  } 
  
  httpheader= c("Authorization"=paste("Bearer", token))
  
  curl::handle_setheaders(curl, .list=httpheader)
  
  curl::handle_setopt(curl, .list = opts)
  
  u = paste0("https://send.influenzanet.info/import/", kind)
  
  if(validate.only) {
    u = paste0(u, "?validate_only=1")
  }
  
  r = curl::curl_fetch_memory(u, handle=curl)
  
  r$headers = curl::parse_headers_list(r$headers)
  
  responseText = NULL
  parsedResponse = NULL
  if(is.raw(r$content)) {
    responseText = rawToChar(r$content)
    
    # Check header and parse JSON if the body claims to be in json format
    if(is.list(r$header) && hasName(r$headers, "content-type") && r$headers[["content-type"]] == "application/json") {
      parsedResponse = try(jsonlite::fromJSON(responseText))
      if(inherits(parsedResponse, "try-error")) {
        warning("Unable to parse JSON response")  
      }
      r$responseJSON = parsedResponse
    }
    
    r$responseText = responseText
    
  }
  
  if(r$status_code %in% c(200,202,201)) {
    if(print.result) {
      message("Transfer is OK")
      if(!is.null(parsedResponse)) {
        w = function(value, name) {
          if(!is.null(value)) {
            paste(" ", name,"=", value)
          } else {
            ""
          }
        }
        
        message("Server replied ", 
                w(parsedResponse$file, "file"),
                w(parsedResponse$user, "user"),
                w(parsedResponse$message, "message")
        )
      } else {
        message("Server responded ", responseText)
      }
    }
  } else {
   error = "An error occured during transfert"
   if( is.list(parsedResponse) & hasName(parsedResponse, "error") ) {
     error = paste(error, ":", parsedResponse)
   } else {
     if(!is.null(responseText)) {
       error = paste(error, ":", responseText)
     }
   }
   attr(error, "response") <- r # response as attribute
   stop(error) 
  }
  invisible(r)
}

# Path to the incidence data file (here it's an example, expecting the file in the working directory, )
file ="examples/incidence1.csv"

# The authentication token (you need to contact Influenzanet tech team to have one)
token = "demo"

r = send_to_influenzanet("incidence", file, token, debug=FALSE, validate.only=FALSE)
