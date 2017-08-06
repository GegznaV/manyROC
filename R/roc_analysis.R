# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# To Do:
#   [!!!] Remove dependency on mlr in parameters sections.
#
#   [!!!] DESCRIPTION MUST BE UPDATED
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Carry out the ROC analysis
#'
#' Do the ROC (receiver operating characteristic) analysis and calculate
#' vector of cut-off values and associated number of
#' true positives (TP),
#' false negatives (FN),
#' false positives (FP), and
#' true negatives (TN) as well as performance measures such as sensitivity,
#' specificity, etc.
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
#'        A flag indicating, if values of positive group are on avedage
#'        are expected to be larger than values of negative group.
#'        If \code{NULL}, this option is determined basing on data using
#'        group medians.
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
#' library(multiROC)
#' library(ggplot2)
#'
#' # Make some data
#'
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
#'
#' # --- Example 2 ---
#'
#' set.seed(1)
#' x2 = c(rnorm(50, mean = 14), rnorm(50, mean = 20))
#' gr2 = gl(2, 50, labels = c("Neg", "Pos"))
#'
#' (roc_rez<- roc_analysis(x2, gr2))
#'
#' optimal_cutoff2 <- roc_rez$optimal[1]
#'
#' qplot(x2, fill = gr2, color = gr2,
#'       geom = c("density", "rug"), alpha = I(0.3)) +
#'    geom_vline(xintercept = optimal_cutoff2)
#'
#'
#' # --- Example 3 ---
#'
#' set.seed(1)
#' x3 = c(rnorm(100, mean = 11), rnorm(100, mean = 14))
#' gr3 = gl(2, 100, labels = c("Neg", "Pos"))
#'
#' (roc_rez3 <- roc_analysis(x3, gr3))
#'
#' optimal_cutoff3 <- roc_rez3$optimal[1]
#'
#' qplot(x3, fill = gr3, color = gr3,
#'      geom = c("density", "rug"), alpha = I(0.3)) +
#'     geom_vline(xintercept = roc_rez3$optimal[1])
#'
#'

roc_analysis <- function(x,
                         gr,
                         pos_label  = levels(gr)[2],
                         pos_is_larger = NULL,
                         optimize_by = "bac",
                         results = "all", ...) {
    UseMethod("roc_analysis")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_analysis
#' @export
roc_analysis.default <- function(x,
                         gr,
                         pos_label  = levels(gr)[2],
                         pos_is_larger = NULL,
                         optimize_by = "bac",
                         results = "all", ...) {

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
    # Capture data to return later
    # data <- list(x = x, gr = gr)

    # Remove missing and infinite values
    finite_bool <- is.finite(x) & is.finite(gr)
    x  <-  x[finite_bool]
    gr <- gr[finite_bool]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Reorder levels to get predictabe results:
    ##  first -  label of negative group,
    ##  second - label of positive group.
    neg_label <- setdiff(levels(gr), pos_label)
    levels <- c(neg_label, pos_label)
    gr <- ordered(gr, levels = levels)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Automatically determine, which group has  smaller `x` values on average
    medians <- tapply(x, gr, median)
    dimnames(medians) <- NULL

    if (is.null(pos_is_larger)) {
        decreasing <- medians[1] < medians[2]  # median__neg_gr < median__pos_gr
    } else {
        decreasing <- pos_is_larger
    }

    # Determine, which group is above threshond, which belod.
    # Checks if positive group is prone to have larger values
    if (decreasing) {
        below <- neg_label
        above <- pos_label
    } else {
        below <- pos_label
        above <- neg_label
    }

    # ==========================================================================
    # # Do the ROC analysis
    x_order <- order(x, decreasing = decreasing)
    x_sorted <- x[x_order]

    # [!!!] Why is `rev()` called at all? Is it necessary?
    dupls <- rev(duplicated(rev(x_sorted)))

    ##  --- Original algorithm to determine cutoffs ---
    ## cutofs are values of vector x:
    ##
    # SIGN <- if (decreasing == TRUE) 1 else -1
    # cutoffs <- c(SIGN * Inf, x_sorted[!dupls])

    ##  --- Improved algorithm to determine cutoffs ---
    ##  cutoffs are middle values between two adjacent x values

    x_s <- x_sorted[!dupls]
    n <- length(x_s)
    if (decreasing == TRUE) {
        # cutoffs<-c(max(x_s), (x_s[1:(n - 1)] + diff(x_s)/2), min(x_s))
        cutoffs <- c(+Inf,     (x_s[1:(n - 1)] + diff(x_s)/2), -Inf)
    } else {
        # cutoffs<-c(min(x_s), (x_s[1:(n - 1)] - diff(x_s)/2), max(x_s))
        cutoffs <- c(-Inf,     (x_s[1:(n - 1)] - diff(x_s)/2), +Inf)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    n_neg <- sum(gr == neg_label)
    n_pos <- sum(gr == pos_label)

    # Following variables are vectors
    tp <- c(0, cumsum(gr[x_order] == pos_label)[!dupls])
    fp <- c(0, cumsum(gr[x_order] == neg_label)[!dupls])

    fn = n_pos - tp
    tn = n_neg - fp

    SE <- calculate_sensitivity(tp, fn)
    SP <- calculate_specificity(tn, fp)

    ppv <- calculate_ppv(tp, fp)
    npv <- calculate_npv(tn, fn)
    bac <- calculate_bac(SE, SP)
    youden <- calculate_youdens_j(SE, SP)

    # =========================================================================
    # The matrix of all numeric results

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
    # [!!!] class names must be reviewed, especially  "roc_results"
    all_results <- add_class_label(all_results, c("roc_results","roc_df"))

    # =========================================================================
    # One row of results, which are considered to be optimal

    if (!is.null(optimize_by)) {
        max_ind <- switch(tolower(optimize_by),
                          "bac"    = bac == max(bac),
                          "youden" = youden == max(youden),
                          # Calculations of kappa are relatively slow, thus if
                          # not needed, it is not computed for the whole vector:
                          "kappa"  = {
                              kappa =  calculate_kappa(tp, fn, fp, tn)
                              kappa == max(kappa)
                          }
        )

        # If several equally optimal values are present, the ties are resolved
        # by choosing the middle (rounded) index of available possibilities
        if (sum(max_ind) > 1)
            max_ind <- which(max_ind)[round(sum(max_ind)/2)]

        # Next, the kappa and AUC values are calculated.
        optimal <- c(all_results[max_ind, ],
                     kappa = calculate_kappa(tp[max_ind],
                                             fn[max_ind],
                                             fp[max_ind],
                                             tn[max_ind]),
                     auc = calculate_auc(SE, SP),
                     median_neg = medians[1],
                     median_pos = medians[2]
        )
        optimal <- t(as.matrix(optimal))
        attr(optimal, "optimized_by") <- optimize_by
        optimal <- add_class_label(optimal, c("roc_opt_result", "roc_df"))

    } else {
        # If optimize_by = NULL
        optimal <- "*** Not calculated ***"
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Output if results = "optimal"
    if (results == "optimal")
        return(optimal)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ==========================================================================
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Information about the analysis
    info <- data.frame(
        var_name    = "",
        n_total     = n_neg + n_pos,
        n_neg       = n_neg,
        n_pos       = n_pos,
        neg_label   = neg_label,
        pos_label   = pos_label,
        median_neg  = medians[1],
        median_pos  = medians[2],
        below  = below,
        cutoff = optimal[1],
        above  = above,
        # pos_is_larger = decreasing,

        stringsAsFactors = FALSE
    )

    info <- add_class_label(info, c("roc_info", "roc_df"))

      # ==========================================================================
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Output

    # otherwise:
    res <- list(info = info,
                optimal = optimal,
                all_results = all_results
                # , data = data
    )

    add_class_label(res, c("roc_result_list"))

}
# =============================================================================
#' @rdname roc_analysis
#' @export
roc_analysis.data.frame <- function(x,
                                    gr,
                                    pos_label  = levels(gr)[2],
                                    pos_is_larger = NULL,
                                    optimize_by = "bac",
                                    results = "all", ...) {
    assert_data_frame(x, types = "numeric", ncols = 1)
    roc_analysis(as.vector(x),
                 gr,
                 pos_label = pos_label,
                 pos_is_larger = pos_is_larger,
                 optimize_by = optimize_by,
                 results = results,
                 ...)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_analysis
#' @export
roc_analysis.matrix <- function(x,
                                gr,
                                pos_label  = levels(gr)[2],
                                pos_is_larger = NULL,
                                optimize_by = "bac",
                                results = "all", ...) {
    assert_matrix(x, ncols = 1)
    roc_analysis(as.vector(x),
                 gr,
                 pos_label = pos_label,
                 pos_is_larger = pos_is_larger,
                 optimize_by = optimize_by,
                 results = results,
                 ...)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~