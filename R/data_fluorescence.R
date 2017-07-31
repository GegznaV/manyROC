# Datasets-Spectra --------------------------------------------------------
#' @name DataSets-fluorescence
#'
#' @title Datasets of simulated spectroscopic data
#' @description \code{fluorescence} is a dataset of simulated spectroscopic data.
#'
#' @format \code{\link[=hyperSpec-class]{hyperSpec}} objects with spectroscopic
#' data and additional variables.
#'
#' Spectra and these nonspectroscopic variables:
#'
#' \describe{
#'   \item{class}{A factor variable with 4 classes.}
#'   \item{gr}{A factor variable with 3 classes.}
#' }
#'
#' @examples
#' library(hyperSpec)
#'
#' fluorescence
#' plot(fluorescence[30:60,])
#'
#' @source Artificially generated in \code{R}.
#' @author Vilmantas Gegzna
#'

'fluorescence'