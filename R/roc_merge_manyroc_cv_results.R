#' Merge 2 lists with manyROC CV results
#'
#' @param x,y lists to merge
#'
#' @return A list wil results from \code{x} and \code{y}
#' @export
#'
#' @examples
#'
#' # x <- readr::read_rds(path = "rez_366_a.RDS")
#' # y <- readr::read_rds(path = "rez_366_b.RDS")
#' #
#' # roc_merge_manyroc_cv_results(x, y)
#'
#' # [!!!] unit test are needed.
roc_merge_manyroc_cv_results <- function(x, y) {
  assert_list(x, len = 7)
  assert_list(y, len = 7)
  classes <- map(x, class)

  z <- list()

  z$n_included         <- bind_cols(x$n_included,         y$n_included)
  z$ind_included_rows  <- bind_cols(x$ind_included_rows,  y$ind_included_rows)
  z$cvo                <- c(x$cvo,                y$cvo)

  z$results <- bind_rows(x$results, y$results)

  z$variables_included <- c(x$variables_included, y$variables_included)
  z$variables_errored  <- c(x$variables_errored,  y$variables_errored)
  z$error_messages     <- c(x$error_messages,     y$error_messages)

  out <- map2(z, classes, add_class_label)

  out
}
