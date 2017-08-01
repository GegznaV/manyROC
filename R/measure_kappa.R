#' Cohen's kappa
#'
#' This function is based on function `measureKAPPA` from `mlr` package.
#'
#' @param truth a vector with true (reference) values.
#' @param response a vector with response (predicted) values.
#' @param conf_mat a table similar to (\code{table(truth, response, useNA = "no")}).
#'
#' @return [!!!]
#'
#' @export
#' @family measures_
#'
#' @examples
#' # [!!!] // none //
#'
#' # matrix(c("TP", "FN", "FP", "TN"), 2)
#'
#' make_conf_matrix <- function(TP, FN, FP, TN)
#'                  matrix(c(TP, FN, FP, TN), 2)
#'

measure_kappa <- function(truth = NULL, response = NULL, conf_mat = NULL) {
    if (is.null(conf_mat)) {
        conf_mat <- table(truth, response)
    }
    if (nrow(conf_mat) != ncol(conf_mat)) {
        stop("Confusion matrix `conf_mat` must be square.")
    }

    conf_mat <- conf_mat / sum(conf_mat)
    # p observed:
    p0 <- sum(diag(conf_mat))
    # p expectrd:
    rowsum <- rowSums(conf_mat)
    colsum <- colSums(conf_mat)
    pe <- sum(rowsum * colsum) / sum(conf_mat)^2

    # Cohen's kappa:
    (1 - (1 - p0) / (1 - pe))
}


