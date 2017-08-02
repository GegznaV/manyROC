#' Access information in a cvo object
#'
#' Utility functions for \code{cvo} object. More in section "Details."
#'
#' @details
#'
#' Function \code{cvo_get_info} returns information about the
#' cross-validation object \code{cvo}.\cr
#'
#' Function \code{cvo_get_sample_size} returns sample
#' size of the object used to create the cross-validation
#' object \code{cvo}.\cr
#'
#' Function \code{cvo_count_folds} returns total number of folds in a
#' \code{cvo} object.\cr
#'
#' Function \code{cvo_get_inds} extracts indices of indicated
#' set (either training or test) from cross-validation object (\code{cvo})
#' created with function \code{cvo_create_folds} when fold of interest is
#' indicated as \code{fold} (it can be either a name of fold or an index
#' of fold). If \code{cvo} does not contain indices of chosen
#' \code{type}, it returns a \bold{complement} to those indices and this
#' information is indicated in attributes of returned object.
#'
#' Function \code{cvo_get_seeds} returns information about seeds of
#' (pseudo)random number generator used for each repetition of
#' splitting to folds.\cr
#'
#'
#' @name cvo_get_info
#'
#' @param cvo a cross-validation object (\code{cvo}) created with
#'            function \code{cvo_create_folds}.
#'
#' @param fold either a name or an index of fold of interest.
#'
#' @param type (string) one of options, indicating the kind of
#'             indices you are interested in:
#' \itemize{
#'      \item \code{"AsIs"} - indices as they are in the object,
#'      \item \code{"Train"} - indices of training set,
#'      \item \code{"Test"} - indices of test set.
#' }
#'
#'
#'
#' @export
#'
#' @seealso \code{\link{cvo_create_folds}}
#'
#' @examples
#' cvo <- cvo_create_folds(fluorescence, folds = 10)
#' cvo
#'
#' cvo_get_info(cvo)
#'
#' cvo_get_seeds(cvo)
#'
#' cvo_get_sample_size(cvo)
#'
#' cvo_count_folds(cvo)
#'
#' cvo_get_fold_names(cvo)
#'
#' cvo_get_inds(cvo, 1)
#'
#' cvo_get_inds(cvo, 1, "train")
#'
#' cvo_get_inds(cvo, 1, "test")
#'
cvo_get_info <- function(cvo) {
    attr(cvo, "info")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cvo_get_info
#' @export
cvo_get_sample_size <- function(cvo) {
    attr(cvo, "info")$sample_size
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cvo_get_info
#' @export
cvo_get_seeds <- function(cvo) {
    attr(cvo, "seeds")
}
# =============================================================================
#' @rdname cvo_get_info
#' @export
cvo_count_folds <- function(cvo) {
    UseMethod("cvo_count_folds")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cvo_get_info
#' @export
cvo_count_folds.cvo_caret <- function(cvo) {
    length(names(cvo))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cvo_get_info
#' @export
cvo_count_folds.cvo_mlr <- function(cvo) {
    cvo$desc$iters
}
# =============================================================================
# =============================================================================
#' @rdname cvo_get_info
#' @export
cvo_get_fold_names <- function(cvo) {
    UseMethod("cvo_get_fold_names")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cvo_get_info
#' @export
cvo_get_fold_names.cvo_caret <- function(cvo) {
    names(cvo)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cvo_get_info
#' @export
cvo_get_fold_names.cvo_mlr <- function(cvo) {
    names(cvo$train.inds)
}
# =============================================================================

