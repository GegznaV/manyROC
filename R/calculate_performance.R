# =============================================================================
#' Performance measures for two-class classification
#'
#'
#' @param truth (\code{factor}) \cr A factor vector with "true"/reference
#'               classes.
#' @param prediction A vector with predicted classes.
#'
#' @param pos_label (\code{character(1)}) \cr A string with the name of
#'                  positive group. By default, the second level of factor
#'                  \code{truth} is treated as positive.
#'
#'
#' @return A matrix with the following columns:
#' \itemize{
#' \item \code{tp} (number of true positives),
#'       \code{fn} (number of false negatives),
#'       \code{fp} (number of false positives),
#'       \code{tn} (number of true negatives),
#'       \code{sens} (sensitivity),
#'       \code{spec} (specificity),
#'       \code{ppv} (positive predictive value),
#'       \code{npv} (negative predictive value),
#'       \code{youden} (Youden's j index),
#'       \code{kappa} (Cohen's kappa).
#'
#' \item
#'       attribute \code{labels} contains a named vector with elements
#'       \code{pos_label} and \code{neg_label}, which indicate labels of
#'        positive and negative groups respectively.
#' }\cr\cr
#'
#'
#' @export
#'
#' @author Vilmantas Gegzna
#' @family functions for ROC
#'
#' @examples
#' (truth <- gl(n = 2, k = 3, length = 20, labels = c("H", "S")))
#'
#' (prediction <- rev(truth))
#'
#'
#' calculate_performance(truth, prediction)
#'
#' calculate_performance(truth, prediction, pos_label = "S")
#'
#' calculate_performance(truth, prediction, pos_label = "H")
calculate_performance <- function(truth,
                                  prediction,
                                  pos_label = levels(truth)[2]
) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Check the input
  truth <- droplevels(truth)
  assert_factor(truth, n.levels = 2)

  assert_subset(levels(prediction), levels(truth))
  assert_subset(pos_label, levels(truth))

  # Warn if missing values are present
  if (any(is.infinite(truth)))
    warning("Variable `truth` has missing values.\n",
      "The results may be inprecise.\n")

  if (any(is.infinite(prediction)))
    warning("Variable `prediction` has missing values.\n",
      "The results may be inprecise.\n")

  # Lengths of inputs must agree
  if (length(truth) != length(prediction)) {
    warning("Number of cases in `truth` and `prediction` must agree.")
  }
  assert_set_equal(length(truth), length(prediction))
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Reorder levels to get predictabe results:
  ##  first -  label of negative group,
  ##  second - label of positive group.
  neg_label  <- setdiff(levels(truth), pos_label)
  levels     <- c(pos_label, neg_label)
  truth      <- ordered(truth, levels = levels)
  prediction <- ordered(prediction, levels = levels)
  # ==========================================================================
  # Calculate performance measures
  conf_mat <- table(truth, prediction)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tp <- conf_mat[1, 1]
  fp <- conf_mat[2, 1]
  fn <- conf_mat[1, 2]
  tn <- conf_mat[2, 2]
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SE <- calculate_sensitivity(tp, fn)
  SP <- calculate_specificity(tn, fp)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  performance <- cbind(tp = tp,
    fn = fn,
    fp = fp,
    tn = tn,
    sens = SE,
    spec = SP,
    ppv  = calculate_ppv(tp, fp),
    npv  = calculate_npv(tn, fn),
    bac  = calculate_bac(SE, SP),
    youden = calculate_youdens_j(SE, SP),
    kappa  = calculate_kappa(tp, fn, fp, tn)

    # , stringsAsFactors = FALSE

  )
  # ==========================================================================
  # Output
  attr(performance, "labels") <- c(pos_label = pos_label,
    neg_label = neg_label)
  add_class_label(performance, c("two_class_perform", "roc_df"))
  # ==========================================================================
}
# =============================================================================
