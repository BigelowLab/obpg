#' Retrieve the spatial resolution stated in the global metadata
#' 
#' @export
#' @param X ncdf4 object
#' @return 2 element [lon, lat] resolution vector
obpg_res <- function(X){
  
  g <- ncdf4::ncatt_get(X, 0)
  nm <- names(g)
  
  ix <- grep("geospatial_lon_res", nm, fixed = TRUE)
  if (length(ix) > 0){
    if (is.numeric(g[[ix]][1])){
      lon <- g[[ix]][1]
    } else {
      #lon <- strsplit(g[[ix]][1], " ")[[1]][1]
      lon <- mean(abs(diff(X$dim$lon$vals[seq_len(100)])))
    }
    #lon <- sub(" degrees", "", g[[ix]][1], fixed = TRUE)
  } else {
    lon <- 0.1
  }

  ix <- grep("geospatial_lat_res", nm, fixed = TRUE)
  if (length(ix) > 0){
    if (is.numeric(g[[ix]][1])){
      lat <- g[[ix]][1]
    } else {
      lat <- mean(abs(diff(X$dim$lat$vals[seq_len(100)])))
      #lat <- strsplit(g[[ix]][1], " ")[[1]][1]
      #lat <- sub(" degrees", "", g[[ix]][1], fixed = TRUE)
    }
  } else {
    lat <- 0.1
  }
  
  as.numeric(c(lon, lat))
}

#' Retrieve the time epoch stated in the dimension attribute
#' 
#' @export
#' @param X ncdf4 object
#' @return POSIXct time epoch
obpg_t0 <- function(X){
  #dim$time$units units: seconds since 1981-01-01 00:00:00 UTC
  as.POSIXct(X$dim$time$units,
             format = "seconds since %Y-%m-%d %H:%M:%S UTC",
             tz = "UTC")
}

#' Retrieve the time dimension of the NCDF object
#'
#' @export
#' @param X ncdf4 object
#' @param t0 POSIXct, the origin of the time dimension
#' @param form character, output format. One of "Date" or "POSIXct" (default)
#' @return vector fo time stamps as determined by \code{form} argument
obpg_time <- function(X,
                     t0 = obpg_t0(X),
                     form = c("Date", "POSIXct")[2]){
  stopifnot(inherits(X, 'ncdf4'))
  if (!("time" %in% names(X$dim))) stop("time dimension not found")
  time <- X$dim$time$vals + t0
  switch(tolower(form[1]),
         "date" = as.Date(time),
         time)
}

#' Retrieve obpg variables
#' 
#' @export
#' @param X ncdf4 object
#' @param drop character vector of variables to drop
#' @return character vector
obpg_vars <- function(X, drop = c("palette", "qual_sst")){
  
  x <- names(X$var)
  x <- x[!(x %in% drop)]
  
  x
}

#' Retrieve obpg navigation values (start, count, lons, lats)
#'
#' @export
#' @param X ncdf4 object
#' @param g geometry object that defines point locations
#' @param res numeric, 2 element resolution \code{[res_x,res_y]}
#' @param varname character the name of the variable
#' @return data frame 
#' \itemize{
#'   \item{g the requested lonlats}
#'   \item{res the resolution}
#'   \item{start vector of start values for \code{\link[ncdf4]{ncvar_get}}}
#'   \item{count vector of count values \code{\link[ncdf4]{ncvar_get}}}
#'   \item{ext vector of extent see \code{\link[raster]{raster}}}
#'   \item{crs character, proj string for \code{\link[raster]{raster}}}
#'   \item{varname character}
#' }
obpg_nc_nav_point <- function(X, g,
                          res = obpg_res(X),
                          varname = obpg_vars(X)){
  
  stopifnot(inherits(X, 'ncdf4'))
  if (!(varname[1] %in% names(X$var))) stop("varname not known:", varname[1])
  if (length(res) == 1) res <- c(res[1],res[1])
  half <- res/2
  
  
  locate_xy <- function(tbl, key, X = NULL){
    ix <- sapply(tbl[[1]],
                 function(x){
                   which.min(abs(X$dim$lon$vals - x))[1]
                 })
    iy <- sapply(tbl[[2]],
                 function(y){
                   which.min(abs(X$dim$lat$vals - y))[1]
                })
    start <- list(cbind(ix,iy))
    count <- list(matrix(1, ncol = length(start[[1]]), nrow = nrow(tbl)))
    tbl |>
      dplyr::mutate(start = start, count = count)
  }
  
  xy <- sf::st_coordinates(g) |>
    dplyr::as_tibble() |>
    dplyr::rowwise() |>
    dplyr::group_map(locate_xy, X = X) |>
    dplyr::bind_rows() |>
    dplyr::mutate(varname = paste(varname, collapse = ",")) |>
    tidyr::separate_rows(.data$varname, sep = ",")
  
  xy
}




#' Retrieve obpg navigation values (start, count, lons, lats)
#'
#' @export
#' @param X ncdf4 object
#' @param g geometry object that defines a bounding box
#' @param res numeric, 2 element resolution \code{[res_x,res_y]}
#' @param varname character the name of the variable
#' @return list with
#' \itemize{
#'   \item{bb the requested bounding box}
#'   \item{res the resolution}
#'   \item{start vector of start values for \code{\link[ncdf4]{ncvar_get}}}
#'   \item{count vector of count values \code{\link[ncdf4]{ncvar_get}}}
#'   \item{ext vector of extent see \code{\link[raster]{raster}}}
#'   \item{crs character, proj string for \code{\link[raster]{raster}}}
#'   \item{varname character}
#' }
obpg_nc_nav_bb <- function(X, g,
                       res = obpg_res(X),
                       varname =  obpg_vars(X)){
  
  stopifnot(inherits(X, 'ncdf4'))
  if (!(varname[1] %in% names(X$var))) stop("varname not known:", varname[1])
  if (length(res) == 1) res <- c(res[1],res[1])
  half <- res/2
  
  bb <- sf::st_bbox(g) |> as.numeric()
  bb <- bb[c(1,3,2,4)]

  bb2 <- bb + c(-half[1], half[1], -half[2], half[2])
  ix <- sapply(bb2[1:2],
               function(xbb) which.min(abs(X$dim$lon$vals-xbb)))
  we <- X$dim$lon$vals[ix]
  iy <- sapply(bb2[3:4],
               function(ybb) which.min(abs(X$dim$lat$vals-ybb)))
  iy <- sort(iy)
  sn <- X$dim$lat$vals[iy]
  
  list(bb = bb,
       res = res,
       start = c(ix[1], iy[1]),
       count = c(ix[2] - ix[1] + 1, iy[2] - iy[1] + 1),
       ext = c(we + (half[1] * c(-1,1)), sn + (half[2] * c(-1,1)) ),
       crs = 4326,
       varname = varname)
}
