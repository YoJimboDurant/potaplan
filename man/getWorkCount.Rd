% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getWorkCount.R
\name{getWorkCount}
\alias{getWorkCount}
\title{get and check park history against call sign and return a data.frame of number of attempts, number of successful activations, and number of qso's.}
\usage{
getWorkCount(call = "KE4MKG", park = "US-2177")
}
\arguments{
\item{call}{is call to check}

\item{park}{park identifier to be queried}
}
\value{
A list of data.frames for activator
}
\description{
get and check park history against call sign and return a data.frame of number of attempts, number of successful activations, and number of qso's.
}
\examples{


park <- "US-2177" 
getParkHist(park)
}
