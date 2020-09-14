# =============================================================================
# Print methods
# =============================================================================
#' @rdname roc_analysis
#' @export
#' @method print as_str
print.as_str <- function(x, ...) {
  tibble::glimpse(x, ...)
}

#' @rdname roc_analysis
#' @inheritParams roc_manyroc
#' @export
#' @method print roc_df
print.roc_df <- function(x, ..., show_all = FALSE,
                         perf_digits = 2, fmt = "%.3g") {
  # [!!!] reikia tobulinti funkcija ir jos aprašyma
  # print(head_tail(x, ...))

  x <- as.data.frame(x)

  perf_names <- intersect(
    c("sens", "spec", "ppv", "npv", "bac", "youden", "kappa", "auc", "acc"),
    colnames(x))

  x %<>%
    dplyr::mutate_at(perf_names,
      sprintf, fmt = glue::glue("%.{perf_digits}f")) %>%
    dplyr::mutate_if(is.numeric, sprintf, fmt = fmt)

  # For long dataframes anly a few lines are printed
  # [!!!] 10 may be converted to a parameter.
  #
  if (nrow(x) > 10) {
    print(head_tail(x), ...)
  } else {
    print(x, ...)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_analysis
#' @param digits (\code{integer(1)})\cr Number of significant digits to display.
#' @export
#' @method print roc_opt_result
print.roc_opt_result <- function(x, digits = 3, ...) {
  # [!!!] reikia tobulinti funkcija ir jos aprašyma
  opt_by <-  attr(x, "optimized_by")
  x <- signif(x, digits = digits)
  x <- as.data.frame(x)
  rownames(x) <- NULL


  if (nrow(x) == 1) {
    print(x, row.names = FALSE, ...)
  } else {
    print(x, ...)
  }
  if (!is.null(opt_by))
    cat(paste0("\n*The optimal cut-off value selected by: max ",
      toupper(opt_by), "\n"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_analysis
#' @export
#' @method print roc_info
print.roc_info <- function(x,  ...) {
  # [!!!] reikia tobulinti funkcija ir jos aprašyma
  if (nrow(x) == 1) {
    print.data.frame(x, row.names = FALSE, ...)
  } else {
    print.data.frame(x, ...)
  }
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# @param x \code{manyroc_result} object for method \code{print}.
#' @rdname roc_manyroc
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
#' @param fmt (\code{character(1)})\cr A string indicating number display
#'            format for other numeric columns excluding performance measures.
#'            The string will be passed to \code{\link[base]{sprintf}}.
#'            Default is \code{"\%.3g"}.
#'
#' @export
#' @method print manyroc_result
print.manyroc_result <- function(x, ..., show_all = FALSE,
                                 perf_digits = 2, fmt = "%.3g") {

  perf_names <- intersect(
    unique(c(
      "sens", "spec", "PPV", "NPV", "BAC", "Youden", "Kappa", "AUC", "ACC",
      "sens", "spec", "ppv", "npv", "bac", "youden", "kappa", "auc", "acc")),
    colnames(x)
  )

  x <-
    x %>%
    dplyr::mutate_at(
      perf_names, sprintf, fmt = glue::glue("%.{perf_digits}f")
    ) %>%
    dplyr::mutate_if(is.numeric, sprintf, fmt = fmt) %>%
    as.data.frame()

  # For long dataframes, only a few lines are printed
  # [!!!] 10 may be converted to a parameter.

  if (nrow(x) > 10) {
    print(head_tail(x), ...)
  } else {
    print(x, ...)
  }
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ----------------------------------------------------------------------------
#' @inheritParams base::print
#'
#' @rdname sp_manyroc_with_cv_by_variable
#' @export
#' @method print hide_it
print.hide_it <- function(x, ...) {
  cat("*** First non-empty element: ***\n")
  ind <- purrr::map_lgl(x, ~ !is.null(.x)) %>% which()  %>% .[1]
  print(x[[ind]])
  if (length(x) > 1)
    cat("\n*** Other elements are not shown ***\n\n")
}
