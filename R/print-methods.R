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
    if (nrow(x) == 1) print.data.frame(x, row.names = FALSE, ...) else print.data.frame(x, ...)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# @param x \code{multiroc_result} object for method \code{print}.
#' @rdname roc_multiroc
#'
#'
#' @param ... Arguments passed for further methods.
#'
#' @param digits (\code{integer(1)})\cr A number of decimals to display
#'            for \emph{performance measures}. Default is 2.
#'
#' @param cutoff_fmt (\code{character(1)})\cr A string with a number display
#'            format for \emph{cut-off values}, \code{mean_neg}
#'             and \code{mean_pos}.
#'            Will be passed to \code{\link[base]{sprintf}}. Default is
#'            \code{"\%.4g"}.
#'
#' @export
#' @method print multiroc_result
print.multiroc_result <- function(x, ..., digits = 2, cutoff_fmt = "%.4g") {

    perf_names <- c("sens","spec","PPV","NPV","BAC","Youden","Kappa","AUC")
    cutoff_names <- c("cutoff","mean_neg", "mean_pos")

    x %>%
        dplyr::mutate_at(perf_names, sprintf, fmt = glue::glue("%.{digits}f")) %>%
        dplyr::mutate_at(cutoff_names, sprintf, fmt = cutoff_fmt)  %>%
        print(...)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
