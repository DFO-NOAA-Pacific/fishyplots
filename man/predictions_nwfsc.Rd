% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictions_document.R
\docType{data}
\name{predictions_nwfsc}
\alias{predictions_nwfsc}
\title{Modeled prediction data for NWFSC region}
\format{
A data frame with 2007632 rows and 9 variables:
\describe{
\item{lon}{Original longitude coordinate}
\item{lat}{Original latitude coordinate}
\item{X}{New UTM coordinate}
\item{Y}{New UTM coordinate}
\item{prediction}{Model estimate}
\item{species}{Species specification}
\item{sanity}{Check if the model/prediction has any issues}
\item{region}{Region NWFSC}
\item{crs}{UTM crs used in coordinate transformation}
}
}
\usage{
predictions_nwfsc
}
\description{
Contains prediction data from spatial models on NWFSC surveys for select species.
}
\keyword{datasets}
