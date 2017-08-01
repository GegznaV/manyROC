#' Access information in a cvo object
#'
#' Function \code{cvo_get_info} returns information about the
#' cross-validation object \code{cvo}.
#'
#' @export
#' @seealso \code{\link{cvo_create_folds}}
#'
#' @name cvo_get_info
#'
cvo_get_info <- function(cvo) {
    attr(cvo, "info")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cvo_get_info
#' @export
#' @description Function \code{cvo_get_sample_size} returns sample
#'  size of the object used to create the
#' cross-validation object \code{cvo}.
#'
cvo_get_sample_size <- function(cvo) {
    attr(cvo, "info")$sample_size
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cvo_get_info
#' @export
#' @description Function \code{cvo_get_seeds} returns seeds of (pseudo)random number
#' generator used for each repetition of splitting to folds.
cvo_get_seeds <- function(cvo) {
    attr(cvo, "seeds")
}

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