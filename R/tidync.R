#' Pseudo-crop a tidync object using a [sf::st_bbox] (or object from which we can determine a [sf::st_bbox])
#'
#' Note that the input is "hyper-filtered" not actually cropped. This
#' establishes limits on the grids (dimensions) for subsequent extractions.
#' One can always restore the original grid dimensions.
#' 
#' @export
#' @param x tidync object
#' @param y object from which a [sf::st_bbox] can be derived
#' @param pad numeric vector, a two element vector (x, y) requesting a slight
#'   padding (if possible) of the bounding box by (x,y) pixels.  Set to 0
#'   to skip padding in a particular dimension.  If only one valuye is provided it is 
#'   used in each direction (x,y).
#' @return the output of [tidync::hyper_filter]
crop_obpg = function(x = open_obpg(), y = pnw_bb(),
                     pad = c(1,1)){
  
  
  b = sf::st_bbox(y)
  if (length(pad) == 1) pad = c(pad,pad)
  if (sum(pad) > 1){
    tr = transform_summary(x)
    if (pad[1] > 0){
      dx = tr |> 
        dplyr::filter(tolower(.data$name) %in% c("x", "lon", "longitude")) |> 
        dplyr::pull(dplyr::all_of("delta"))
      b[['xmin']] = b[['xmin']] - (dx * pad[1])
      b[['xmax']] = b[['xmax']] + (dx * pad[1])
    }
    if (pad[2] > 0){
      dy = tr |> 
        dplyr::filter(tolower(.data$name) %in% c("y", "lat", "latitude")) |> 
        dplyr::pull(dplyr::all_of("delta"))
      b[['ymin']] = b[['ymin']] - (dy * pad[2])
      b[['ymax']] = b[['ymax']] + (dy * pad[2])
    }
  }
  
  
  tidync::hyper_filter(x,
                       lon = .data$lon >= b[['xmin']] & .data$lon <= b[['xmax']],
                       lat = .data$lat >= b[['ymin']] & .data$lat <= b[['ymax']])
}

#' Retrieve a summary table of each spatial (x and y) transform
#' for the **selected** space.
#' 
#' The meaning **selected** space is that defined by [tidync::hyper_filter], 
#' defaulting to the full spatial extent if no prior `hyper_filter`ing has been done.
#' 
#' @export
#' @param x a [tidync::tidync] object
#' @return a table providing a spatial summary
transform_summary = function(x){
  
  transforms <- tidync::hyper_transforms(x)[1:2]
  
  summary_one = function(name, tr = NULL){
    x =  dplyr::filter(tr, .data$selected) |> dplyr::pull(1)
    lim = range(x)
    n = length(x)
    d = (lim[2]-lim[1])/(n-1)
    dplyr::tibble(name = name,
                  len = n,
                  min = lim[1],
                  max = lim[2],
                  delta = d)
  }
  
  lapply(names(transforms),
              function(nm){
                r = if (tolower(nm) %in% c("x", "y", "lon", "lat", "longitude", "latitude")){
                  summary_one(nm, tr = transforms[[nm]])
                } else {
                  NULL
                }
                r
              }) |>
    dplyr::bind_rows()
}


#' Cast a tidync object to stars
#'
#' @export
#' @param x tidync object - typically filtered with [tidync::activate]
#'   and suitably activated grids with [tidync::activate]
#' @param ... other arguments (ignored)
#' @return stars object
as_stars <- function(x, ...) {

  db = attr(x, "db")
  ## ignore unit details for the moment
  data <- lapply(tidync::hyper_array(x, drop = FALSE), 
                 units::as_units)

  transforms <- tidync::hyper_transforms(x)
  
  lon = dplyr::filter(transforms[[1]], .data$selected) |> dplyr::pull(1)
  xlim = range(lon)
  nx = length(lon)
  dx = (xlim[2]-xlim[1])/(nx-1)
  
  lat = dplyr::filter(transforms[[2]], .data$selected) |> dplyr::pull(1)
  ylim = range(lat)
  ny = length(lat)
  dy = (ylim[2]-ylim[1])/(ny-1)
  
  
  bb = sf::st_bbox(c(xmin = xlim[1] - dx/2,
                     xmax = xlim[2] + dx/2,
                     ymin = ylim[1] - dy/2,
                     ymax = ylim[2] + dx/2),
                   crs = 4326)
  
  stars::st_as_stars(bb,
                         nx = nx,
                         ny = ny,
                         values = unclass(data[[1]])) |>
    rlang::set_names(db$param)
}


#' Open a connection to an OPBG resource
#' 
#' @export
#' @param x chr a single row obpg_url table
#' @return a tidync object or NULL
open_obpg <- function(x = obpg_url()){
  
  silent = options("tidync.silent")
  options(tidync.silent = TRUE)
  on.exit({
    options(tidync.silent = silent[[1]])
    })
  

  x = dplyr::filter(x, !.data$error)
  if (nrow(x) == 0){
    message("no opendap service available")
    return(NULL)
  }

  db = decompose_filename(x$opendap[1])
  r = tidync::tidync(x$opendap[1]) |>
    tidync::activate(db$param[1])
  attr(r, "db") <- db
    
  r
}

#' Retrieve the tidync data source
#' 
#' @export
#' @param x tidync object
#' @return filename or url
tidync_source = function(x){
  x$source$source
}
