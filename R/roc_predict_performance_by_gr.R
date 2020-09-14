# =============================================================================
#' Calculate perfornamce for each pair of groups
#'
#' This function works as combination of \code{\link{roc_predict}} and
#' \code{\link{calculate_performance}}, just \code{object} may have more than
#' 1 row.
#'
#' @param object Either an object of class \code{roc_info} or a data frame,
#'               which contains columns:
#'              "cutoff" (numeric),
#'              "below" (character),
#'              "above" (character).
#' @param x_new  A numeric vector to predict on.
#' @param gr_new A factor vector associated with `x_new`
#'
#' @return A dataframe with performance calculated on test data
#'
roc_predict_performance_by_gr <- function(object, x_new, gr_new) {

  object <- as.data.frame(object, stringsAsFactors = FALSE)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check the input
  if (sum(names(object)  %in%  c("below", "cutoff", "above")) != 3)
    stop("The `object` must contain variables ",
      'called "below", "cutoff", and "above".')

  assert_numeric(x_new)
  assert_vector(x_new, strict = TRUE)

  # Warn if missing values are present
  if (any(is.infinite(x_new)))
    warning("Variable `x_new` has missing values.\n",
      "The results may be inprecise.\n")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  prediction <- purrr::pmap(list(object$cutoff, object$below, object$above),
    .f = predict_and_evaluate_performance,
    new_x = x_new,
    true_gr = gr_new)

  # names(prediction) <- object$compared_groups

  DF <- as.data.frame(purrr::reduce(prediction, rbind))

  # Output
  dplyr::bind_cols(compared_groups = object$compared_groups, DF)
  # dplyr::bind_rows(prediction, .id = "compared_groups")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper function
predict_and_evaluate_performance <-
  function(cutoff, below, above, new_x, true_gr) {
    assert_string(below)
    assert_string(above)
    assert_number(cutoff)

    # Use only appropriate `new_x` and `true_gr` values
    ind_ok <- true_gr %in% c(below[1], above[1])
    pred_gr <-
      ifelse(new_x[ind_ok] < cutoff,
        # label of group with smaller values (below cutoff)
        yes = as.character(below),
        # label of group with larger values (above cutoff)
        no  = as.character(above)
      )

    cbind(cutoff = cutoff, calculate_performance(true_gr[ind_ok], pred_gr))
  }


# =============================================================================
