#' Set seeds for reproducible parallel computing with 'parallelMap' package
#'
#' A wrapper for \pkg{parallel}::\code{\link[parallel]{clusterSetRNGStream}}.
#'
#' @inheritParams parallel::clusterSetRNGStream
#'
#' @export
parallelSetSeed <- function(iseed) {
    parallel::clusterSetRNGStream(iseed = NULL)
}