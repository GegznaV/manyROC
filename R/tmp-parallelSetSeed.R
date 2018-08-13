#' Set seeds for reproducible parallel computing with 'parallelMap' package
#'
#' A wrapper for \pkg{parallel}::\code{\link[parallel]{clusterSetRNGStream}}.
#'
#' @param ... Arguments to be passed to \code{set.seed()}.
#' @param cl A cluster from either \pkg{parallel}  or package \pkg{snow},
#'           or (if \code{NULL}) the registered cluster.
#' @inheritParams parallel::clusterSetRNGStream
#' @inheritParams set.seed
#' @export
parallelSetSeed <- function(iseed = NULL, seed = iseed, cl = NULL, ...) {
    set.seed(seed = seed, ...)
    parallel::clusterSetRNGStream(iseed = iseed, cl = cl)
}
