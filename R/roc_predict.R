
# predict method

#' Predict outcome for new data
#'
#' @param object Either an object which inherits from one of following classes:
#'               \code{roc_result_list},
#'               \code{roc_info},
#'               \code{roc_info_multi}, \bold{or} a data frame with exactly
#'               one row, which contains columns:
#'              "cutoff" (numeric),
#'              "below" (character),
#'              "above" (character).
#'
#' @param x_new A numeric vector with data to predict on.
#' @param ... Arguments to further methods.
#'
#' @family functions for ROC
#' @export
#' @examples
#' library(multiROC)
#'
#' set.seed(1)
#' x <- rnorm(10)
#' gr <- gl(n = 2, k = 5, length = 10, labels = c("H","S"))
#' object <- roc_analysis(x, gr)
#'
#' roc_predict(object, x_new = 1)
#'
#' roc_predict(object, x_new = -1)
#'
#' roc_predict(object, x_new = rnorm(20))
#'
roc_predict <- function(object, x_new, ...) {
    UseMethod("roc_predict")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @method roc_predict roc_result_list
#' @export
roc_predict.roc_result_list <- function(object, x_new, ...) {
    assert_class(object$info, "roc_info")
    roc_predict(object$info, x_new, ...)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @method roc_predict default
#' @export
roc_predict.default <- function(object, x_new, ...) {

    object <- as.data.frame(object, stringsAsFactors = FALSE)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check the input
    # assert_class(object, "roc_info")
    assert_data_frame(object, nrows = 1)

    if (sum(names(object)  %in%  c("below", "cutoff", "above")) != 3)
        stop('The `object` must contain variables ',
             'called "below", "cutoff", and "above".')

    assert_numeric(x_new)
    assert_vector(x_new, strict = TRUE)

    # Warn if missing values are present
    if (any(is.infinite(x_new)))
        warning("Variable `x_new` has missing values.\n",
                "The results may be inprecise.\n")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Make prediction
    prediction <-
        ifelse(x_new < object$cutoff,
               # label of group with smaller values (below cutoff)
               yes = as.character(object$below),
               # label of group with larger values (above cutoff)
               no  = as.character(object$above))


    # Output
    prediction
}
# =============================================================================
