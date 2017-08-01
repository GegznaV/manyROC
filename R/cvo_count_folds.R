# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Count folds in a cvo object
#'
#' Function \code{cvo_count_folds} returns total number of folds in a
#'  \code{cvo} object.
#' @export
cvo_count_folds <- function(cvo) {
    UseMethod("cvo_count_folds")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cvo_count_folds
#' @export
cvo_count_folds.cvo_caret <- function(cvo) {
    length(names(cvo))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cvo_count_folds
#' @export
cvo_count_folds.cvo_mlr <- function(cvo) {
    cvo$desc$iters
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~