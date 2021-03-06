% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/examples.R
\name{GetPopulationPyramidKSH}
\alias{GetPopulationPyramidKSH}
\title{Scrape the population pyramid of Hungary.}
\usage{
GetPopulationPyramidKSH(Type = "Jan1", Years = 2013:2016,
  Gender = "Both", AgeGroup = "Main", GeographicArea = "NUTS2",
  na.rm = TRUE)
}
\arguments{
\item{Type}{Type of population (possible values are "Jan1" and "MidYear").}

\item{Years}{Numeric vector containing the years (from 2015 to 2017, inclusive).}

\item{Gender}{Stratification according to gender (possible values are "Total" and "Both").}

\item{AgeGroup}{Stratification according to age (possible values are "Total", "Main" and
"FiveYear").}

\item{GeographicArea}{Stratification according to geographic area (possible values are "Total",
"NUTS2", "NUTS3" and "LAU1").}

\item{na.rm}{Remove rows with missing population counts? (logical)}
}
\value{
Neatly formatted population pyramid.
}
\description{
Scrape the population pyramid of Hungary possibly stratified according to gender, age group
(main groups, five-year groups or one-year groups) and geographic area (NUTS2, NUTS3 or LAU1).
}
\details{
Data available from 1990 to 2017, but LAU1 geographical resoulation is available only after
2014, and yearly age groups are only available for NUTS2 and NUTS3 resolution.
}
\examples{
GetPopulationPyramidKSH( "MidYear", Years = 2015, AgeGroup = "Main" )
}
