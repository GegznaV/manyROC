# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' [!] Access elements of roc_result_list object
#'
#' [!!!] not tested yet.
#'
#' Acces elements of \code{roc_result_list} and \code{roc_results} objects.
#' @name access_elements
#' @param obj Either an \code{roc_result_list} or an \code{roc_results} object.
#' @param what (\code{string(1)})\cr Name of element.
#'
#' @export
#'
#' @author Vilmantas Gegzna
#' @family functions for ROC
roc_get <- function(obj, what) {
    assert_string(what)
    UseMethod("roc_get")
}
# -----------------------------------------------------------------------------
#' @rdname access_elements
#' @export
roc_get.roc_result_list <- function(obj, what) {
    roc_get(obj$all_results, what)
    # obj$roc_elements$tp
}
# -----------------------------------------------------------------------------
#' @rdname access_elements
#' @export
roc_get.roc_results <- function(obj, what) {
    obj[, colnames(obj) == what]
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname access_elements
#' @export
roc_get_all_results <- function(obj, ...) {
    obj$all_results
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname access_elements
#' @export
roc_cutoff <- function(obj) {
    roc_get(obj, "cutoff")
    # obj$roc_elements$cutoff
    # obj$roc_elements[, 1]
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname access_elements
#' @export
roc_tp <- function(obj) {
    roc_get(obj, "tp")
    # obj$roc_elements[, 2]
    # obj$roc_elements$tp
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname access_elements
#' @export
roc_fn <- function(obj) {
    roc_get(obj, "fn")
    # obj$roc_elements[, 3]
    # obj$roc_elements$fn
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname access_elements
#' @export
roc_fp <- function(obj) {
    roc_get(obj, "fp")
    # obj$roc_elements[, 4]
    # obj$roc_elements$fp
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname access_elements
#' @export
roc_tn <- function(obj) {
    roc_get(obj, "tn")
    # obj$roc_elements[, 5]
    # obj$roc_elements$tn
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname access_elements
#' @export
roc_sens <- function(obj) {
    roc_get(obj, "sens")
    # obj$roc_elements[, 5]
    # obj$roc_elements$tn
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname access_elements
#' @export
roc_spec <- function(obj) {
    roc_get(obj, "spec")
    # obj$roc_elements[, 5]
    # obj$roc_elements$tn
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
