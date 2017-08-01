#' Weighted Cohen's kappa
#'
#' This function is based on function `measureWKAPPA` from `mlr` package.
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
measure_wkappa <- function(truth = NULL, response = NULL, conf_mat = NULL) {

    # "wkappa" might be incorrect if NA values exist in any
    #  of `truth`, `response`, `conf_mat`

    if (is.null(conf_mat)) {
        conf_mat <- table(truth, response)
    }
    if (nrow(conf_mat) != ncol(conf_mat)) {
        stop("Confusion matrix `conf_mat` must be square.")
        class_values <- seq_along(1:nrow(conf_mat)) - 1L
        ## Original code line:
        # class_values <- seq_along(levels(truth)) - 1L

    }

    conf_mat <- conf_mat/sum(conf_mat)
    rowsum <- rowSums(conf_mat)
    colsum <- colSums(conf_mat)
    expected_mat <- rowsum %*% t(colsum)

    weights <- outer(class_values,
                     class_values,
                     FUN = function(x, y) (x - y)^2)

    # Weighted Cohen's kappa
    (1 - sum(weights * conf_mat)/sum(weights * expected_mat))
}