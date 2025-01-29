#' Retrieve a bounding box for the Pacific Northwest
#' 
#' @export
#' @param form chr, one of "vector", "bbox" or "polygon" to determine the 
#'    return type
#' @return named vector, sf bbox or sf polygon as requested
pnw_bb = function(form = c("vector", "bbox", "polygon")[3]){
  bb = c(xmin = -136, xmax = -123, ymin = 42, ymax = 50) 
  if(tolower(form[1]) %in% c("bbox", "polygon")){
    bb =  sf::st_bbox(bb, crs = 4326)  
  }
  if(tolower(form[1]) == "polygon"){
    bb =  sf::st_as_sfc(bb, crs = 4326)  
  }
  bb
}

#' Given a URL, test if the server throws an error
#' 
#' @export
#' @param x chr, URL to test
#' @return logical, TRUE if an error is thrown
http_bad = function(x){
  resp = httr::GET(x)
  httr::http_error(resp)
}


#' Make a directory as needed
#' 
#' @export
#' @param x directory name(s)
#' @param recursive logical, if TRUE builds missing path segments as needed
#' @param ... other arguments for [base::dir.create]
#' @return named logical vector (TRUE for success(s))
make_path = function(x, recursive = TRUE, ...){
  
  sapply(x,
       function(d){
         ok = dir.exists(d)
         if (!ok) ok = dir.create(d, recursive = recursive, ...)
         ok
       }) |>
    rlang::set_names(basename(x))
}