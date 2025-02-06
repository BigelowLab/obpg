#' Browse the metadata OPeNDAP page for a given URL
#' 
#' @export
#' @param x table with 'meta' and 'opendap' variables.
#' @return the output, invisibly
browse_obpg = function(x = obpg_url()){
  x = dplyr::filter(x, !.data$error)
  if (nrow(x) == 0){
    message("no metadata service available")
  } else {
    httr::BROWSE(x$meta[1])
  }
  invisible(x) 
}

#' Given an opgp_url table, convert to NRT
#' 
#' @export
#' @param x output of obpg_url()
#' @return table of NRT urls
obpg_make_nrt = function(x = obpg_url()){
  meta_nrt = function(y){
    y = sub("http", "https", y)
    sub("NRT.nc", "NRT.nc.dmr.html", y, fixed = TRUE)
  }
  dplyr::mutate(x,
    opendap = sub(".nc", ".NRT.nc", .data$opendap, fixed = TRUE),
    meta = meta_nrt(.data$opendap))
}

#' Test the http service for one or more rows of the output
#' of \code{\link{obpg_url}}
#' 
#' @export
#' @param x table produced by \code{\link{obpg_url}}
#' @return the input table, with possible midifications to the *error*
#'   variable.  TRUE means the http check returns an error code,
#'   FALSE means no error, and NA means untested.
obpg_test_http = function(x){
  dplyr::rowwise(x) |>
    dplyr::group_map(
      function(tbl, key){
        dplyr::mutate(tbl, error = http_bad(.data$meta))
      }) |>
    dplyr::bind_rows()
}


#' Craft a OBPG URL for a given date
#' 
#' Not all OBPG products are supported. If there is one missing that you would
#' like to add, please contact the package maintainer.
#' 
#' @seealso \href{https://oceandata.sci.gsfc.nasa.gov/opendap/}{OBPG OPeNDAP catalog} 
#' @export
#' @param date character, POSIXt or Date the date to retrieve
#' @param where character ignored (for now)
#' @param root character, the root URL
#' @param level character level component of path
#' @param mission character mission component of path
#' @param instrument character instrument component of path
#' @param period character period component of path
#' @param product character, provides version and extend info, leave as default
#' @param resolution character resolution component of path
#' @return one or more URLs
obpg_url <- function(date = Sys.Date() - 2,
                     where = "opendap",
                     root = "http://oceandata.sci.gsfc.nasa.gov/opendap",
                     level = c("L3", "L3SMI")[2],
                     mission = c("MODIS",  "S3A", "SNPP", "ADEOS", "SEASTAR", "PACE")[3],
                     instrument = c("AQUA", "TERRA", "SEAWIFS", "VIIRS")[4],
                     period = c("DAY", "MONTH")[1],
                     product =   "SST.sst",
                     resolution = "9km"){
  
  if (FALSE){
    # contents never run, for debugging purposes
    date = Sys.Date() - 2
    where = "opendap"
    root = "http://oceandata.sci.gsfc.nasa.gov/opendap"
    level = c("L3", "L3SMI")[2]
    mission = c("MODIS",  "S3A", "SNPP", "ADEOS", "SEASTAR", "PACE")[3]
    instrument = c("AQUA", "TERRA", "SEAWIFS", "VIIRS")[4]
    period = c("DAY", "MONTH")[1]
    product =   "SST.sst"
    resolution = "9km"
  }
  
  if (inherits(date, "character")) date <- as.Date(date)  
  
  src <- switch(instrument,
                "AQUA" = "AQUA_MODIS",
                "TERRA" = "TERRA_MODIS",
                "VIIRS" = "SNPP_VIIRS",
                "PACE" = "PACE_OCI")
  
  product <- sprintf("L3m.%s.%s", period, product)
  
  root_mission <- switch(instrument,
                         "AQUA" = "http://oceandata.sci.gsfc.nasa.gov/opendap/MODISA",
                         "TERRA" = "http://oceandata.sci.gsfc.nasa.gov/opendap/MODIST",
                         "VIIRS" = "http://oceandata.sci.gsfc.nasa.gov/opendap/VIIRS",
                         "PACE" = "http://oceandata.sci.gsfc.nasa.gov/opendap/PACE_OCI")
  
  pattern = "%s.%s.%s.%s.nc"
  name <- sprintf(pattern,
                  src,
                  format(date, "%Y%m%d"),
                  product,
                  resolution)
  #http://oceandata.sci.gsfc.nasa.gov/opendap/VIIRS/L3SMI/2025/0111/SNPP_VIIRS.20250111.L3m.DAY.SST.sst.9km.NRT.nc
  #https://oceandata.sci.gsfc.nasa.gov/opendap/VIIRS/L3SMI/2025/0111/SNPP_VIIRS.20250111.L3m.DAY.SST.sst.9km.NRT.nc.dmr.html
  
  opendap = file.path(root_mission, level, format(date, "%Y"), format(date, "%m%d"), name)  
  
  meta = sub("http", "https", opendap)
  meta = sub(".nc", ".nc.dmr.html", meta, fixed = TRUE)
  
  r = dplyr::tibble(meta = meta, opendap = opendap, error = NA)
  r = r |>
    dplyr::bind_rows(obpg_make_nrt(r)) |>
    obpg_test_http()
  
  r
}
