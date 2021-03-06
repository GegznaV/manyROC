% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-fluorescence.R
\docType{data}
\name{fluorescence}
\alias{fluorescence}
\title{Dataset of simulated fluorescence spectra}
\format{
\code{\link[=hyperSpec-class]{hyperSpec}} objects with spectroscopic
data and additional variables.

Spectra and these nonspectroscopic variables:

\describe{
  \item{class}{A factor variable with 4 classes.}
  \item{gr}{A factor variable with 3 classes.}
}
}
\source{
Artificially generated in \code{R}.
}
\usage{
fluorescence
}
\description{
\code{fluorescence} is a dataset of simulated spectroscopic data.
}
\details{
Artificially generated dataset.
}
\examples{
library(hyperSpec)

fluorescence
plot(fluorescence[30:60, ])
}
\author{
Vilmantas Gegzna
}
\keyword{datasets}
