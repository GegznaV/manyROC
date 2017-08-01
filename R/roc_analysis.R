# =============================================================================
# COMMENTS:
#
#  .compute.unnormalized.roc.curve --------------------------------------------
# prediction     --------------------------------------------------------------
# in the original function decreasing = TRUE  ---------------------------------
# =============================================================================

#' [!] Carry out the ROC analysis
#'
#' Do the ROC (receiver operating characteristic) analysis and calculate
#' vector of cut-off values and associated number of
#' true positives (TP),
#' false negatives (FN),
#' false positives (FP), and
#' true negatives (TN) as well as performance measures such as sensitivity,
#' specificity, etc.
#' \cr[!!!]
#'
#' @name roc_analysis
#' @param x (\code{numeric}) \cr A numeric vector.\cr
#'          (in \code{print} function) An object to print.
#' @param ... [!!!] Passed to further methods.
#'
#' @param gr (\code{factor}) \cr A factor vector with two levels.
#' @param pos_label (\code{character(1)}) \cr A string with the name of
#'                  positive group.
#' @param pos_is_larger (\code{NULL}|\code{TRUE}|\code{FALSE}) \cr
#'        A flag indicating, if values of positive group are (on average) larger.
#'        If \code{NULL}, this option is determined basing on data using 10\%
#'        trimmed mean.
#' @param optimize_by (\code{string(1)}) \cr [!!!] Method to determine the
#'                    optimal cut-off value.
#'                    Current options: \code{"bac"},
#'                                    \code{"youden"},
#'                                    \code{"kappa"}.
#'
#' @param results (\code{character(1)}) \cr A string indicating which results
#'                should be returned: either \code{"all"} (as described in
#'                section "Values") or just \code{"optimal"}.
#'
#'
#' @return A list (which also inherits from class \code{"roc_result_list"})
#' with three fields: \code{$info}, \code{$optimal}, \code{$all_results}.
#' \itemize{
#'
#' \item \bold{\code{$info}} is a data frame with columns
#'      \code{var_name} - empty string reserved for variable name,
#'      \code{neg_label}, \code{pos_label} labels of negative and positive
#'                              groups respectively,
#'      \code{n_neg}, \code{n_pos}, \code{n_total} - number of negative
#' and positive cases as well as number of cases in total.
#'
#' \item \bold{\code{$optimal}} one row from \code{$all_results}, which was
#' determined as having optima threshold (cut-off) value. Sometimes it can be
#' several rows, if the performance is equally good. \cr\cr
#'
#' \item \bold{\code{$all_results}} is a data frame with columns
#'       \code{cutoffs} for cutoff values,
#'       \code{tp} (number of true positives),
#'       \code{fn} (number of false negatives),
#'       \code{fp} (number of false positives),
#'       \code{tn} (number of true negatives),
#'       ... [!!!]
#'
#' }\cr\cr
#'
#'
#' @export
#'
#' @note
#' This function is inspired by functions \code{predict} and
#' \code{.compute.unnormalized.roc.curve} from \pkg{ROCR} package.
#'
#' @author Vilmantas Gegzna
#' @family functions for ROC
#'
#' @examples
#' # Make some data
#' set.seed(1)
#' (x <- rnorm(10))
#'
#' (gr <- gl(n = 2, k = 5, length = 10, labels = c("H","S")))
#'
#'
#' # Explore the functions
#'
#' roc_analysis(x, gr)
#'
#' roc_analysis(x, gr, pos_label = "H")
#'

roc_analysis <- function(x,
                         gr,
                         pos_label  = levels(gr)[2],
                         pos_is_larger = NULL,
                         optimize_by = "bac",
                         results = "all") {

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check the input

    # [!!!] Inputs `optimize_by = NULL` and `results = "optimize"` are
    # incompatible. An assertion and warning are needed

    assert_numeric(x)
    assert_vector(x, strict = TRUE)

    assert_factor(gr, n.levels = 2)

    if (length(x) != length(gr)) {
        warning("Number of cases in `x` and `gr` must agree." )
    }
    assert_set_equal(length(x), length(gr))

    assert_string(pos_label)
    assert_choice(pos_label, levels(gr))

    assert_flag(pos_is_larger, null.ok = TRUE)

    assert_string(optimize_by, null.ok = TRUE)
    assert_choice(optimize_by, c("bac", "youden", "kappa"), null.ok = TRUE)

    assert_choice(results, c("all", "optimal"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Remove missing and infinite values
    finite_bool <- is.finite(x) & is.finite(gr)
    x  <-  x[finite_bool]
    gr <- gr[finite_bool]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Order factor levels so that the first was negative
    # and the second was positive
    neg_label <- setdiff(levels(gr), pos_label)

    ## Reorder levels to get predictabe results:
    ##  first - always negative, second - positive
    levels <- c(neg_label, pos_label)
    gr <- ordered(gr, levels = levels)

    n_neg <- sum(gr == neg_label)
    n_pos <- sum(gr == pos_label)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Automatically determine, which group has  smaller `x` values on average
    means <- tapply(x, gr, mean, trim = .10)
    if (is.null(pos_is_larger)) {
        decreasing <- means[1] < means[2]  # mean__neg_gr < mean__pos_gr
    } else {
        decreasing <- pos_is_larger
    }

    # ==========================================================================
    # # Do the ROC analysis
    x_order <- order(x, decreasing = decreasing)
    x_sorted <- x[x_order]

    tp <- cumsum(gr[x_order] == pos_label)
    fp <- cumsum(gr[x_order] == neg_label)

    # [!!!] Why is `rev()` called at all? Is it necessary?
    dups <- rev(duplicated(rev(x_sorted)))
    tp <- c(0, tp[!dups])
    fp <- c(0, fp[!dups])

    SIGN <- if (decreasing == TRUE) 1 else -1
    cutoffs <- c(SIGN * Inf, x_sorted[!dups])

    fn = n_pos - tp
    tn = n_neg - fp
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    SE <- calculate_sensitivity(tp, fn)
    SP <- calculate_specificity(tn, fp)

    ppv <- calculate_ppv(tp, fp)
    npv <- calculate_npv(tn, fn)
    bac <- calculate_bac(SE, SP)
    youden <- calculate_youdens_j(SE, SP)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    all_results <- cbind(cutoff = cutoffs,
                         tp = tp,
                         fn = fn,
                         fp = fp,
                         tn = tn,
                         sens = SE,
                         spec = SP,
                         ppv  = ppv,
                         npv  = npv,
                         bac  = bac,
                         youden = youden
                          # pos = tp + fp,
                          # neg = tn + fn
    )
    all_results <- class_add(all_results, c("roc_essentials","roc_results"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(optimize_by)) {
        max_ind <-  switch(tolower(optimize_by),
                           "bac"    = bac == max(bac),
                           "youden" = youden == max(youden),
                           "kappa"  = {
                               kappa =  calculate_kappa(tp, fn, fp, tn)
                               kappa == max(kappa)
                           }
        )

        # If several equally optimal values are present, the ties are resolved
        # by choosing the middle (rounded) value a follow
        if (sum(max_ind) > 1)
            max_ind <- which(max_ind)[round(sum(max_ind)/2)]

        # Next, the kappa and AUC values are calculated.
        optimal <- c(all_results[max_ind, ],
                     kappa = calculate_kappa(tp[max_ind],
                                             fn[max_ind],
                                             fp[max_ind],
                                             tn[max_ind]),
                     auc = calculate_auc(SE, SP),
                     mean_neg = means[1],
                     mean_pos = means[2]
        )
        optimal <- t(as.matrix(optimal))
        attr(optimal, "optimized_by") <- optimize_by
        optimal <- class_add(optimal, c("roc_optimal","roc_results"))

    } else {
        # If optimize_by = NULL
        optimal <- "*** Not calculated ***"
    }
    # ==========================================================================
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Output
    if (results == "optimal")
        return(optimal)

    # otherwise:
    res <- list(
        info =
            class_add(
                data.frame(
                    var_name  = "",
                    neg_label = neg_label,
                    pos_label = pos_label,
                    mean_neg  = means[1],
                    mean_pos  = means[2],
                pos_is_larger = decreasing,
                    n_neg     = n_neg,
                    n_pos     = n_pos,
                    n_total   = n_neg + n_pos
                ),
                c("roc_info")
            ),
        optimal = optimal,
        all_results = all_results
    )

    class_add(res, c("roc_result_list"))

}
# =============================================================================
