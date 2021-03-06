% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/getCountData.R
\name{timeBins}
\alias{timeBins}
\title{Defining timebins.}
\usage{
timeBins(x, binsize, pos = 0.5)
}
\arguments{
\item{x}{Numerical vector with timestamp information.}

\item{binsize}{Size of the timebin, measured in the same units (often ms)
as \code{x}.}

\item{pos}{Numerical value that determines the label of the binsize
as proportion of the binsize. A value of 0 will provide the minimum
timestamp within the bin as label, a value of 1 will provide the maximum
value within the bin as label. Defaults to 0.5, the center of the bin.}
}
\value{
Anumerical vector of the same size as \code{x} with timebin
information.
}
\description{
Function for calculating timebins.
}
\author{
Jacolien van Rij
}
\seealso{
\code{\link{getCountData}}

Other Functions for gaze data: \code{\link{getCountData}}
}

