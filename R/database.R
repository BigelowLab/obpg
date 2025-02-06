#' Retrieve an expanatory table about the database
#' 
#' @export
#' @return tibble of descriptors (name, type and description) for each field
explain_database = function(){
  dplyr::tribble(
    ~name, ~type, ~description,
    "date", "Date", "We only use L3 which is by day, so no POSIXct",
    "year", "numeric", "date formatted as YYYY",
    "mmdd", "character", "date formatted as mmdd",
    "mit", "character", "mission-instrument-type",
    "level", "character", "string indicating the level",
    "period", "charcater", "period indicator for original L3",
    "suite", "character", "suite identifier",
    "param", "character", "parameter name chlor_a, sst, etc",
    "per", "character", "compositing period such as DAY, MO, 8D",
    "res", "character", "resolution in km",
    "nrt", "character", "optional Near Real-Time identifier",
    "file", "character", "basename of the input file less the extension")
}

#' Decompose one or more obpg filenames into the obpg database
#'
#' @export
#' @param x character, vector of filenames to decompose. Path is discarded.
#' @return a database table
#'  \itemize{
#'    \item{date, Date  We only use L3 which is by day, so no POSIXct}
#'    \item{year, numeric date formatted as YYYY}
#'    \item{mmdd, character date formatted as mmdd}
#'    \item{mit, character mission-instrument-type}
#'    \item{level, character string indicating the level}
#'    \item{period, charcater period indicator for L3}
#'    \item{suite, character suite identifier}
#'    \item{param, character parameter name chlor_a, sst, etc}
#'    \item{per, compositing period such as DAY, MO, 8D}
#'    \item{res, character resolution in km}
#'    \item{nrt, character optional Near Real-Time identifier}
#'    \item{file, basename of the input file less the extension}
#'  }
decompose_obpg <- function(
    x = c("AQUA_MODIS.20191130.L3m.DAY.NSST.sst.4km.NRT.nc",
          "NPP_VIIRS.20190703.L3m.DAY.SST.sst.4km.nc",
          "AQUA_MODIS.20030401_20110430.L3m.MC.CHL.chlor_a.9km.nc",
          "AQUA_MODIS.20020701_20020731.L3m.MO.CHL.chlor_a.4km.nc", 
          "AQUA_MODIS.20220301_20220331.L3m.MO.CHL.chlor_a.4km.NRT.nc",
          "AQUA_MODIS.20230401_20230430.L3m.MO.POC.poc.4km.NRT.nc")){
  
  f     <- basename(x)
  ss    <- strsplit(f, ".", fixed = TRUE)
  len   <- sapply(ss, length)
  ss    <- sapply(seq_along(len), function(i) ss[[i]][-len[i]], simplify = FALSE)
  len   <- len - 1
  date  <- as.Date(sapply(ss, function(s) substring(s[2],1,8)),
                   format = "%Y%m%d")
  nrt   <- rep(NA_character_, length(ss))
  ix <- len > 7
  if (any(ix)) nrt[ix] <- sapply(ss[ix], "[[", 8)
  dplyr::tibble(
    date,
    year  = format(date, "%Y") |> as.numeric(),
    mmdd  = format(date, "%m%d"),
    mit   = sapply(ss, "[[", 1),
    lvl   = sapply(ss, "[[", 3),
    per   = sapply(ss, "[[", 4),
    suite = sapply(ss, "[[", 5),
    param = sapply(ss, "[[", 6),
    res   = sapply(ss, "[[", 7),
    nrt,
    file  = gsub(".NRT", "", sapply(ss, paste, collapse = "."), fixed = TRUE)
  )
}

#' @export
#' @rdname decompose_obpg
#' @param ... other arguments for \code{decompose_obpg}
decompose_filename <- function(x, ...){
  decompose_obpg(x, ...)
}



#' Given a database, build filenames ala \code{yyyy/mmdd/filenames}
#'
#' @export
#' @param x a tibble database
#' @param path a path to raster files typically through opbg2/platform/region
#' @param ext character - the extension to apply - by default ".tif"
#' @param from_scratch logical, if \code{TRUE}, build the name from components
#' @param include_nrt logical, if \code{TRUE} then include the NRT flag when building from scratch.
#'        Ignored if from_scratch is \code{FALSE}
#' @return character, filenames
compose_obpg <- function(x,
                         path = ".",
                         ext = '.tif',
                         from_scratch = FALSE,
                         include_nrt = TRUE){
  
  if (from_scratch){
    fname <- paste(x$mit,
                   format(x$date, '%Y%m%d'),
                   x$lvl,
                   x$per,
                   x$suite,
                   x$param,
                   x$res,
                   sep = ".")
    if (include_nrt){
      ix <- !is.na(x$nrt)
      if (any(ix)) fname[ix] <- paste(fname[ix], x$nrt[ix], sep = ".")
    }
  } else {
    fname <- x$file
    ix <- x$per != 'DAY'
    if (any(ix)) fname[ix] <- gsub(".NRT", "", fname[ix], fixed = TRUE)
  }
  fname <- paste0(fname, ext[1])
  file.path(path, x$year, x$mmdd, fname)
}

#' @export
#' @rdname compose_obpg
#' @param ... other arguments for \code{compose_obpg}
compose_filename <- function(x, ...){
  compose_obpg(x, ...)
}


#' Given a directory, build a database
#'
#' @export
#' @param path a path to raster files
#' @param save_db logical, if TRUE then write the database
#' @param pattern regular expression of the file pattern to search for
#' @param ... arguments passed to write_database **if** `save_db` is `TRUE`.
#' @return tibble or NULL
build_database <- function(
    path = ".",
    pattern = glob2rx("*.tif"),
    save_db = FALSE, 
    ...){
  
  if (!dir.exists(path[1])) stop("path not found", path[1])
  ff <- list.files(path[1], pattern = pattern,
                   recursive = TRUE, full.names = TRUE)
  
  if (length(ff) > 0){
    x <- decompose_obpg(ff)
    if (save_db) x <- write_database(x, path, ...)
  } else {
    warning("no files found to buid database")
    x <- NULL
  }
  
  x
}


#' Read a file-list database
#'
#' @export
#' @param path character the directory with the database
#' @param filename character, the name of the database file
#' @return a tibble
read_database <- function(path = ".", filename = "database"){
  if (!dir.exists(path[1])) stop("path not found:", path[1])
  filename <- file.path(path,filename[1])
  stopifnot(file.exists(filename))
  readr::read_csv(filename,  
                  col_types = readr::cols(.default = readr::col_character(),
                                          date = readr::col_date(format = ""),
                                          year = readr::col_double() ))
}

#' Write the file-list database
#'
#' @export
#' @param x the tibble or data.frame database
#' @param path character the directory to where the database should reside
#' @param filename character, the name of the database file
#' @param append logical, if TRUE try to append to the existing database
#' @return a tibble
write_database <- function(x, path, filename = "database", append = FALSE){
  if (!dir.exists(path[1])) stop("path not found:", path[1])
  
  if (append){
    return(append_database(x, path, filename = filename))
  }
  
  
  filename <- file.path(path,filename[1])
  readr::write_csv(x, filename)
}

#' Append to the file-list database
#'
#' @export
#' @param x the tibble or data.frame database
#' @param path character the directory to where the database should reside
#' @param filename character, the name of the database file
#' @return a tibble with appended data
append_database <- function(x, path, filename = "database"){
  if (!dir.exists(path[1])) stop("path not found:", path[1])
  origfilename <- file.path(path,filename[1])
  if(!file.exists(origfilename)){
    return(write_database(x, path, filename = filename))
  }
  orig = read_database(path, filename = filename)
  orig_info = colnames(orig)
  x_info =colnames(x)
  ident = identical(orig_info, x_info)
  if (!isTRUE(ident)){
    print(ident)
    stop("input database doesn't match one stored")
  }
  dplyr::bind_rows(orig, x) |>
    dplyr::distinct() |>
    write_database(path, filename = filename)
}

#' Given a database with one or more parameters retrieve a listing of missing dates in the
#' sequence form first date to last date.
#'
#' @export
#' @param x a tibble database
#' @param last_date either NULL or a Date object to use as the last date of the
#'    date range to examine. If NULL then the last date in the database is used.
#'    By default we use 'yesterday'
#' @return named list of date vectors of mising days (possibly empty)
db_missing_days <- function(x = read_database(), last_date = Sys.Date()-1){
  
  xx = split(x, x$param)
  
  lapply(xx,
         function(x){
           d = as.Date(x$date)
           r = range(d)
           if (!is.null(last_date)) r[2] <- last_date[1]
           dd = seq(from = r[1], to = r[2], by = 'day')
           dd[!(dd %in% d)]
         })
}

#' Given per parameter missing days listing, generate a named logical vector
#' indicating which are missing.
#'
#' @export
#' @param x a named list of the missing days per parameter generated by
#'        \code{\link{db_missing_days}}
#' @param last_date either NULL or a Date object to use as the last date of the
#'    date range to examine. If NULL then the last date in the database is used.
#'    By default we use 'yesterday'.
#' @return a named logical vector indicating which parameters need updating
db_needs_update <- function(x = db_missing_days(), last_date = Sys.Date()-1){
  sapply(names(x),
         function(n){
           length(x[[n]] > 0) & any(x[[n]] <= last_date)
         })
}

#' Retrieve a subset of most recent entries in a database
#'
#' @export
#' @param x the database
#' @param n numeric, the number of most recent items
#' @return most_recent n of the database by param/per/date
db_most_recent <- function(x, n = 1){
  n_ <- function(N, n = 2) seq(N-(n-1), N, by = 1)
  x |>
    dplyr::group_by(.data$param, .data$per) |>
    dplyr::arrange(.data$date) |>
    dplyr::slice(n_(n(), n)) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$param, .data$per, .data$date)
}




#' Parse climatology name to extract end date from data sets that have 
#' start-stop in the file name which includes climatologies among other
#' things.
#'
#' @export
#' @param x database tibble
#' @param filter logical, if TRUE then filter the database for just the
#'   periods that have two dates. These periods include DAY, 8D, MO, SNAU, SNWI, 
#'   SNSP, SNSU, YR, R32, R3QL, MC, SCSU, SCAU, SCWI, SCSP, and CU 
#' @return input database with added end_date, end_year, and end_mmdd columns
database_to_climatology <- function(x, filter = FALSE){
  
  pcodes = c("MO", "SNAU", "SNWI", "SNSP", "SNSU", "YR", "MC",
             "SCSU", "SCAU", "SCWI", "SCSP", "CU")
  
  if (filter) x = dplyr::filter(x, (.data$per %in% pcodes))
  
  if (!all(x$per %in% pcodes)){
    stop("only these periods can be used: ", paste(pcodes, collapse = ","))
  }
  
  ss <- strsplit(x$file, ".", fixed = TRUE)
  dd <- strsplit(sapply(ss, "[[", 2), "_", fixed = TRUE)
  x |>
    dplyr::mutate(end_date = as.Date(sapply(dd, "[[", 2), format = "%Y%m%d"), .after = 1) |>
    dplyr::mutate(end_year = format(.data$end_date, "%Y"), .after = 'year') |>
    dplyr::mutate(end_mmdd = format(.data$end_date, "%m%d"), .after = 'mmdd')
}
