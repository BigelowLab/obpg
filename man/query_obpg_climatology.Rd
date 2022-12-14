% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obpg.R
\name{query_obpg_climatology}
\alias{query_obpg_climatology}
\title{Query OBPG for available climatology resources}
\usage{
query_obpg_climatology(
  years = 2002:2003,
  climatology = c("CU", "SCSU", "SCAU", "SCWI", "SCSP", month.abb)[1],
  platform = "MODISA",
  product = "L3SMI",
  param = "SST.sst",
  res = "9km",
  top_uri = "https://oceandata.sci.gsfc.nasa.gov/opendap",
  verbose = FALSE
)
}
\arguments{
\item{years}{numeric or character, the years to query}

\item{climatology}{char, the climatology to seek, inluding "CU", "SCSU", 
"SCAU", "SCWI", "SCSP", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
"Aug", "Sep", "Oct", "Nov", "Dec"}

\item{platform}{char, the name of the mission/platform}

\item{product}{char, the name of the product}

\item{param}{char, the two part parmater name}

\item{res}{char, the resolution as "9km" or "4km"}

\item{top_uri}{char, the path to the OBPG thredds catalog}

\item{verbose}{logical, if TRUE output messages}
}
\value{
zero or more character URLs (most likely just one)
}
\description{
Climatologies are stored at the first date upon which that climatology can
be computed for the mission.  For example, seasonal climatology for summer is
June 21, 2002 (for June, July and August).  These carry two dates (start and 
stop) with the latter subject to updates.  So we search by pattern matching
the climatology period, the parameter and the resolution.  We break the search
when a match is found.  If more than one match is found then the latter is 
retrieved under the assumption that it is the most recent update.
}
