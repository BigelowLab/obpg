% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{database_to_climatology}
\alias{database_to_climatology}
\title{Parse climatology name to extract end date from data sets that have
start-stop in the file name which includes climatologies among other
things.}
\usage{
database_to_climatology(x, filter = FALSE)
}
\arguments{
\item{x}{database tibble}

\item{filter}{logical, if TRUE then filter the database for just the
periods that have two dates. These periods include DAY, 8D, MO, SNAU, SNWI,
SNSP, SNSU, YR, R32, R3QL, MC, SCSU, SCAU, SCWI, SCSP, and CU}
}
\value{
input database with added end_date, end_year, and end_mmdd columns
}
\description{
Parse climatology name to extract end date from data sets that have
start-stop in the file name which includes climatologies among other
things.
}
