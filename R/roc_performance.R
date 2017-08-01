#' @name roc_performance_measures
#' @title [!!!] Performance measures
#'
#' @description Calculate various performance measures for classificatory analysis \cr
#'   [!!!] (...The description is incomplete...)
#'
#' @param obj Either an \code{roc_result_list} or an \code{roc_results} object.
#'
#' @param TP (\code{numeric}) Number of true positives.
#' @param FN (\code{numeric}) Number of false negatives.
#' @param FP (\code{numeric}) Number of false positives.
#' @param TN (\code{numeric}) Number of true negatives.
#' @param SE (\code{numeric}) Vector of sensitivities.
#' @param SP (\code{numeric}) Vector of specificities
#'
#' @author Vilmantas Gegzna
#' @family functions for ROC
#
# last review: 2017-07-31

calculate_acc <- function(TP, FN, FP, TN) {
    (TP + TN) / (TP + FP + TN + FN)
}
# -----------------------------------------------------------------------------
#' @rdname roc_performance_measures
#' @export
roc_calculate_acc <- function(obj) {
    calculate_acc(roc_tp(obj), roc_fp(obj), roc_fn(obj), roc_tn(obj))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_performance_measures
#' @export
calculate_bac <- function(SE, SP) {
    (SE + SP) / 2
}
# -----------------------------------------------------------------------------
#' @rdname roc_performance_measures
#' @export
roc_calculate_bac <- function(obj) {
    (roc_sens(obj) + roc_spec(obj)) / 2
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_performance_measures
#' @export
calculate_auc <- function(SE, SP) {
    pracma::trapz(SE, SP)
}
# -----------------------------------------------------------------------------
#' @rdname roc_performance_measures
#' @export
roc_calculate_auc <- function(obj) {
    pracma::trapz(roc_sens(obj), roc_spec(obj))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_performance_measures
#' @export
calculate_npv <- function(TN, FN) {
    TN / (TN + FN)
}
# -----------------------------------------------------------------------------
#' @rdname roc_performance_measures
#' @export
roc_calculate_npv <- function(obj) {
    TN <- roc_tn(obj)
    FN <- roc_fn(obj)
    TN / (TN + FN)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_performance_measures
#' @export
calculate_ppv <- function(TP, FP) {
    TP / (TP + FP)
}
# -----------------------------------------------------------------------------
#' @rdname roc_performance_measures
#' @export
roc_calculate_ppv <- function(obj) {
    TP <- roc_tp(obj)
    FP <- roc_fp(obj)
    TP / (TP + FP)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_performance_measures
#' @export
calculate_sensitivity <- function(TP, FN) {
    TP / (TP + FN)
}
# -----------------------------------------------------------------------------
#' @rdname roc_performance_measures
#' @export
roc_calculate_se <- function(obj) {
    TP <- roc_tp(obj)
    FN <- roc_fn(obj)
    TP / (TP + FN)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_performance_measures
#' @export
calculate_specificity <- function(TN, FP) {
    TN / (TN + FP)
}
# -----------------------------------------------------------------------------
#' @rdname roc_performance_measures
#' @export
roc_calculate_sp <- function(obj) {
    TN <- roc_tn(obj)
    FP <- roc_fp(obj)
    TN / (TN + FP)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_performance_measures
#' @export
calculate_youdens_j <- function(SE, SP) {
    SE + SP - 1
}
# -----------------------------------------------------------------------------
#' @rdname roc_performance_measures
#' @export
roc_youdens_j <- function(obj) {
    (roc_sens(obj) + roc_spec(obj)) - 1
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_performance_measures
#' @export
calculate_kappa <- function(TP, FN, FP, TN) {
    kappa_calculation_helper(TP, FN, FP, TN, measure_kappa)
}
# -----------------------------------------------------------------------------
#' @rdname roc_performance_measures
#' @export
roc_calculate_kappa <- function(obj) {
    roc_kappa_calculation_helper(obj, measure_kappa)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_performance_measures
#' @export
calculate_wkappa <- function(TP, FN, FP, TN) {
    kappa_calculation_helper(TP, FN, FP, TN, measure_wkappa)
}
# -----------------------------------------------------------------------------
#' @rdname roc_performance_measures
#' @export
roc_calculate_wkappa <- function(obj) {
    roc_kappa_calculation_helper(obj, measure_wkappa)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kappa_calculation_helper <- function(TP, FN, FP, TN, FUN_) {
    # @param FUN_ Function to apply (either \code{\link{measure_kappa}} or
    #                               \code{\link{measure_wkappa}})
    #
    # Helper for Kappa and Wkappa functions.
    # It creates a square matrix `matrix(c("TP", "FN", "FP", "TN"), 2)`
    # on which approprite kappa value (indicated in the `FUN`)
    # is calculated.
    apply(X = cbind(TP, FN, FP, TN),
          MARGIN = 1,
          FUN = function(v) {
              FUN_(conf_mat = matrix(v, nrow = 2))
          }
    )
}
# -----------------------------------------------------------------------------
roc_kappa_calculation_helper <- function(obj, FUN_) {
    kappa_calculation_helper(roc_tp(obj),
                             roc_fn(obj),
                             roc_fp(obj),
                             roc_tn(obj), FUN_ = FUN_)
}
# =============================================================================
