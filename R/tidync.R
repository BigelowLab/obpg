#' Open a connection to an OPBG resource
#' 
#' @export
#' @param x chr a single row obpg_url table
#' @param try_nrt logical, if TRUE and the uri fails, try the same as NRT
#'  (near real time)
#' @return a tidync object
open_obpg <- function(x = obpg_url(),
                      try_nrt = TRUE){
  
  silent = options("tidync.silent")
  options(tidync.silent = TRUE)
  on.exit({
    options(tidync.silent = silent[[1]])
    })
  
  http_bad = function(uri){
    resp = httr::GET(uri)
    httr::http_error(resp)
  }
  
  bad = http_bad(x$meta)
  if (bad && try_nrt){
    y = obpg_make_nrt(x)
    bad = http_bad(y$meta)
    if (bad) {
      warning("neither URL not NRT connection successful: ", y$opendap)
      uri = NULL
    } else {
      uri = y$opendap
    }
  } else if (bad && !try_nrt){
    warning("URL connection failed: ", x$opendap)
    uri = NULL
  } else {
    uri = x$opendap
  }
  if (!is.null(uri)){
    r = tidync::tidync(uri)
  } else {
    r = NULL
  }
  r
}