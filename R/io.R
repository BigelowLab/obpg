#' A function to apply a function (like \code{log10}) to an attribute of a stars
#' object.
#' 
#' @export
#' @param x stars object
#' @param att character, one attribute name
#' @param fun a function that can be applied to the attribute
#' @param ... other arguments to \code{fun}
#' @return the update inpout oject
attr_fun <- function(x, att = 'chlor_a', fun = log10, ...){
  if (!inherits(x, 'stars')) stop("input must be stars object")
  if (!att %in% names(x)) stop("attribute not found in object:", att)
  dplyr::mutate(!!att := fun(.data[[att]], ...))
}


#' Given a database, guess which type of \code{along} might be most appropriate
#' 
#' If the database has only one type of param, then along is a list of times to indicate
#' laters for one attribute, otherwise along is \code{NA} to indicate multiple attributes.
#' @export
#' @param x database (a tibble)
#' @return either a list of NA_integer_
guess_along <- function(x){
  if (length(unique(x$param)) == 1){ 
    along = list(time = x$date)
  } else {
    along = NA_integer_
  }
  along
}

#' Read raster formatted OBPG file(s).
#'
#' @export
#' @param x  obpg database
#' @param path character path
#' @param along see [stars::read_stars] and [guess_along]
#' @param tolerance num, for binding to allow for some slop
#' @return stars object
read_obpg <- function(x, 
                      path = NULL, 
                      along = guess_along(x),
                      tolerance = sqrt(.Machine$double.eps)){
  
  if (is.null(path)) stop("path must be provided")
  filename <- compose_filename(x, path)
  if (!all(file.exists(filename))) {
    stop("one or more files not found:", paste(filename, collapse = ", "))
  }
  if (is.na(along)){
    # we hope the user provides the same number of dates for each param
    S <- dplyr::group_by(x, .data$param) |>
      dplyr::group_map(
        function(tbl, key){
          read_obpg(tbl, path = path) |>
            rlang::set_names(tbl$param[1])
        }, .keep = TRUE
      ) 
    S <- do.call(c, append(S, list(along = NA, tolerance = tolerance)))
  } else {
    S <- stars::read_stars(filename, along = along, tolerance = tolerance)
    names(S) <- x$param[1]
  }
 
  
    
  
  S
}