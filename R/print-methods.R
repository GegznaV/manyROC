# =============================================================================
# Print methods
# =============================================================================
#' @rdname roc_analysis
#' @export
#' @method print roc_results
print.roc_results <- function(x, ...) {
    # [!!!] reikia tobulinti funkciją ir jos aprašymą
    print(head_tail(x, ...))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_analysis
#' @param digits (\code{integer(1)})\cr Number of significant digits to display.
#' @export
#' @method print roc_optimal
print.roc_optimal <- function(x, digits = 3, ...) {
    # [!!!] reikia tobulinti funkciją ir jos aprašymą
    opt_by <-  attr(x, "optimized_by")
    x <- signif(x, digits = digits)
    x <- as.data.frame(x)
    rownames(x) <- NULL

    if (nrow(x) == 1) print(x, row.names = FALSE, ...) else print(x, ...)
    if (!is.null(opt_by))
        cat(paste0("\n* The optimal cut-off value selected by: max ",
                   toupper(opt_by),"\n"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_analysis
#' @export
#' @method print roc_info
print.roc_info <- function(x,  ...) {
    # [!!!] reikia tobulinti funkciją ir jos aprašymą
    if (nrow(x) == 1) {
        print.data.frame(x, row.names = FALSE, ...)
    } else {
        print.data.frame(x, ...)
    }
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# @param x \code{multiroc_result} object for method \code{print}.
#' @rdname roc_multiroc
#'
#'
#' @param ... Arguments passed for further methods.
#'
#' @param show_all (\code{logical(1)})\cr A flag if whole dataset should be
#'                printed. If \code{FALSE}, only a few fisrt and last rows
#'                will be printed.
#'
#' @param perf_digits (\code{integer(1)})\cr A number of decimals to display
#'            for \emph{performance measures}. Default is 2.
#'
#' @param fmt (\code{character(1)})\cr A string with a number display other
#'            format for \emph{cut-off values}, \code{mean_neg}
#'             and \code{mean_pos}.
#'            Will be passed to \code{\link[base]{sprintf}}. Default is
#'            \code{"\%.3g"}.
#'
#' @export
#' @method print multiroc_result
print.multiroc_result <- function(x, ..., show_all = FALSE,
                                  perf_digits = 2, fmt = "%.3g") {

    perf_names <- intersect(
        c("sens","spec","PPV","NPV","BAC","Youden","Kappa","AUC"),
        colnames(x))

    # cutoff_names <- intersect(c("cutoff","mean_neg", "mean_pos"), colnames(x))

    x %<>%
        dplyr::mutate_at(perf_names,
                         sprintf, fmt = glue::glue("%.{perf_digits}f")) %>%
        dplyr::mutate_if(is.numeric, sprintf, fmt = fmt)

    # For long dataframes anly a few lines are printed
    # [!!!] 10 may be converted to a parameter.
    #
    if (nrow(x) > 10) {
        print(head_tail(x, ...))
    } else {
        print(x, ...)
    }
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
