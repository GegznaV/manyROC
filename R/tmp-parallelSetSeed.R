#' Set seeds for reproducible parallel computing with 'parallelMap' package
#'
#' A wrapper for \pkg{parallel}::\code{\link[parallel]{clusterSetRNGStream}}.
#' @param ... Arguments to be passed to `set.seed()`.
#' @inheritParams parallel::clusterSetRNGStream
#' @inheritParams base::set.seed
#'
#' @export
parallelSetSeed <- function(iseed = NULL, seed = iseed, cl = NULL, ...) {
    base::set.seed(iseed, ...)
    parallel::clusterSetRNGStream(iseed = iseed)
}
