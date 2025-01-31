#' Pseudo-crop a tidync object using a [sf::st_bbox] (or object from which we can determine a [sf::st_bbox])
#'
#' Note that the input is "hyper-filtered" not actually cropped. This
#' establishes limits on the grids (dimensions) for subsequent extractions.
#' One can always restore the original grid dimensions.
#' 
#' @export
#' @param x tidync object
#' @param y object from which a [sf::st_bbox] can be derived
#' @return the output of [tidync::hyper_filter]
crop_obpg = function(x = open_obpg(), y = pnw_bb()){
  b = sf::st_bbox(y)
  
  tidync::hyper_filter(x,
                       lon = lon >= b[['xmin']] & lon <= b[['xmax']],
                       lat = lat >= b[['ymin']] & lat <= b[['ymax']])
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

  transforms <- tidync:::active_axis_transforms(x)
  
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
#' @param try_nrt logical, if TRUE and the uri fails, try the same as NRT
#'  (near real time)
#' @return a tidync object or NULL
open_obpg <- function(x = obpg_url()){
  
  silent = options("tidync.silent")
  options(tidync.silent = TRUE)
  on.exit({
    options(tidync.silent = silent[[1]])
    })
  

  x = dplyr::filter(x, !error)
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
