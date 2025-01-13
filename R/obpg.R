#' Given an opgp_url table, convert to NRT
#' 
#' @export
#' @param x output of obpg_url()
#' @param table of NRT urls
obpg_make_nrt = function(x = obpg_url()){
  meta_nrt = function(y){
    y = sub("http", "https", y)
    sub("NRT.nc", "NRT.nc.dmr.html", y, fixed = TRUE)
  }
  dplyr::mutate(x,
    opendap = sub(".nc", ".NRT.nc", .data$opendap, fixed = TRUE),
    meta = meta_nrt(.data$opendap))
}

#' Craft a OBPG URL for a given date
#' 
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
#' @param nrt logical if TRUE add "NRT" as the trailing segment meaning "near real time"
#' @return one or more URLs
obpg_url <- function(date = Sys.Date() - 2,
                     where = "opendap",
                     root = "http://oceandata.sci.gsfc.nasa.gov/opendap",
                     level = c("L3", "L3SMI")[2],
                     mission = c("MODIS",  "S3A", "SNPP", "ADEOS", "SEASTAR", "PACE")[3],
                     instrument = c("AQUA", "TERRA", "OLCI", "SEAWIFS", "VIIRS", "OCTS", "PACE")[5],
                     period = c("DAY", "MONTH")[1],
                     product =   "SST.sst",
                     resolution = "9km",
                     nrt = FALSE){
  
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
  
  r = dplyr::tibble(meta = meta, opendap = opendap)
  if (nrt) r = obpg_make_nrt(r)
  r
}


# http://oceandata.sci.gsfc.nasa.gov/opendap/MODISA/L3SMI/2021/0713/AQUA_MODIS.20210713.L3m.DAY.CHL.chlor_a.9km.nc

#"https://oceandata.sci.gsfc.nasa.gov/opendap/MODISA/L3SMI/2021/0713/AQUA_MODIS.20210713.L3m.DAY.CHL.chlor_a.9km.nc"


# https://oceandata.sci.gsfc.nasa.gov/opendap/VIIRS/L3SMI/2022/0806/SNPP_VIIRS.20220806.L3m.DAY.SST.sst.9km.nc


#' Query OBPG for available climatology resources
#' 
#' Climatologies are stored at the first date upon which that climatology can
#' be computed for the mission.  For example, seasonal climatology for summer is
#' June 21, 2002 (for June, July and August).  These carry two dates (start and 
#' stop) with the latter subject to updates.  So we search by pattern matching
#' the climatology period, the parameter and the resolution.  We break the search
#' when a match is found.  If more than one match is found then the latter is 
#' retrieved under the assumption that it is the most recent update. 
#' 
#' @export
#' @param years numeric or character, the years to query
#' @param climatology char, the climatology to seek, inluding "CU", "SCSU", 
#' "SCAU", "SCWI", "SCSP", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
#' "Aug", "Sep", "Oct", "Nov", "Dec"
#' @param platform char, the name of the mission/platform
#' @param product char, the name of the product
#' @param param char, the two part parmater name
#' @param res char, the resolution as "9km" or "4km"
#' @param top_uri char, the path to the OBPG thredds catalog 
#' @param verbose logical, if TRUE output messages
#' @return zero or more character URLs (most likely just one)
query_obpg_climatology <- function(
    years = 2002:2003,
    climatology = c("CU", "SCSU", "SCAU", "SCWI", "SCSP", month.abb)[1],
    platform = "MODISA",
    product = "L3SMI",
    param = "SST.sst",
    res = "9km",
    top_uri = "https://oceandata.sci.gsfc.nasa.gov/opendap",
    verbose = FALSE){
  
  
  if (FALSE){
    years = 2002:2003
    #climatology = c("CU", "SCSU", "SCAU", "SCWI", "SCSP", month.abb)[1]
    climatology = "Jun"
    platform = "MODISA"
    product = "L3SMI"
    param = "SST.sst"
    res = "9km"
    top_uri = "https://oceandata.sci.gsfc.nasa.gov/opendap"
  }
  
  if (tolower(climatology) %in% tolower(month.abb)){
    # AQUA_MODIS.20030601_20220630.L3m.MC.SST.sst.9km.nc
    lut <- c(
      "jan" = "20030101",
      "feb" = "20030201",
      "mar" = "20030301",
      "apr" = "20030401",
      "may" = "20030501",
      "jun" = "20030601",
      "jul" = "20020701",
      "aug" = "20020801",
      "sep" = "20020901",
      "oct" = "20021001",
      "nov" = "20021101",
      "dec" = "20221201")
    pattern <-  glob2rx(sprintf("*%s_*.MC.%s.%s.nc", lut[tolower(climatology)], param, res))
  } else {
    pattern <- glob2rx(sprintf("*.%s.%s.%s.nc", climatology, param, res))
  }
  
  if (verbose) cat("search pattern:", pattern, "\n")
  
  uri <- sprintf("%s/%s/%s/catalog.xml", top_uri, platform[1], product[1])
  Top <- thredds::get_catalog(uri)
  f <- character()
  i <- 0
  for (year in years){
    if (verbose) cat("searching year:", year, "\n")
    Year <- Top$get_catalogs(as.character(year))[[1]]
    if (is.null(Year)) next
    nm <- Year$get_catalog_names()
    ix <- nchar(nm) > 3
    Days <- Year$get_catalogs(nm[ix])
    if (is.null(Days)) next
    for (Day in Days){
      dd <- Day$list_datasets()
      ix <- grepl(pattern, names(dd))
      if (any(ix)){
        id <- unname(sapply(dd[ix], "[[", "ID"))
        f <- id[length(id)]   # get the last one
        break
      }
    } # Day loop
    if (length(f) > 0) break
  } # year loop
  
  if (length(f) > 0){
    # I hate this part
    f <- sub("/opendap/hyrax/", "", f, fixed = TRUE)
    f <- file.path(top_uri,f)
  }
  f
}

