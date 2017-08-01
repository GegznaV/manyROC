# Datasets-Spectra --------------------------------------------------------
# @name DataSets-fluorescence
#'
#' Dataset of simulated fluorescence spectra
#'
#' \code{fluorescence} is a dataset of simulated spectroscopic data.
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
#' @details
#' Artificially generated dataset.
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