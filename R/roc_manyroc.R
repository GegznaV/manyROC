# =============================================================================
# To do
# [!!!] the desctription needs revision
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Carry out the manyROC analysis
#'
#' Do the ROC analysis (\code{\link{roc_analysis}}) for each column of \code{x}.
#'
#'
#' @details
#' \enumerate{
#' \item Consider ordering of factor \code{gr} levels before the analysis, as the
#' first level will always be treated as negative and the last as positive.
#' \bold{E.g.}, if we have factor with 3 levels in this particular order
#' "A", "B", "C",  then "A" will always be negative, "C" always positive and
#' "B" positive, when compared to "A" and negative, when compared to "C".
#' The same principle applies if there are more than 3 levels.\cr
#' This is important determining what specificity and sensitivity, etc., means
#' in the context of group names: if positive is "A" then \emph{sensitivity}
#' will be related to group "A", but if "A" is negative, then \emph{specificity}
#' will be related to this group, and sensitivity to the other group.
#'
#' \item For spectroscopic data: compare spectra of each pair of indicated groups
#' at each wavelength.
#' }
#'
#' @param x A numeric matrix, a data frame, a \code{hyperSpec} object or another
#'           type of object, convertible to a numeric matrix.
#' @param gr Either a string (scalar, \code{character(1)}) or a \code{factor}
#'           variable (a vector) which defines groups in \code{x}.
#' @param gr_sep Group separator used to paste the names of groups. Default is
#'              \code{" vs. "}.
#'
#' @inheritParams roc_performance_measures
#'
#' @param optimize_by (\code{string(1)})\cr A string with the name of
#'                 classification performance measure to use. Currently
#'                 available options:
#' \itemize{
#'       \item \code{"bac"} - for balanced accuracy (mean of sensitivity and
#'        specificity);\cr
#'       \item \code{"kappa"} - for Cohens kappa;\cr
#'       \item \code{"youden"} - for Youden's index;
#' }
#'
#' @return Object of classes \code{manyroc_result} and \code{data.frame} with
#' columns:
#'  \itemize{
#'       \item \code{compared_groups} Names of compared groups (separated by
#'                   \code{gr_sep} with default value \code{" vs. "});
#'       \item \code{feature} names of numeric features used in analysis;
#'       \item \code{median_neg} median value of negatives group;
#'       \item \code{cutoff} for optimal threshold/cut-off values and
#'                   corresponding performance measures;
#'       \item \code{median_pos} median value positives group;
#'       \item \code{TP} number of true positives;
#'       \item \code{FN} number of false negatives;
#'       \item \code{FP} number of false positives;
#'       \item \code{TN} number of true negatives;
#'       \item \code{sens} sensitivity (true positive rate, recall);
#'       \item \code{spec} specificity (true negative rate);
#'       \item \code{PPV} positive predictive value (precision);
#'       \item \code{NPV} negative predictive value;
#'       \item \code{BAC} balanced accuracy;
#'       \item \code{Youden} Youden’s J index;
#'       \item \code{Kappa} Cohen's kappa;
#'       \item \code{AUC} area under the ROC curve;
#' }
#'
#'
#' @export
#'
#' @author Vilmantas Gegzna
#' @family \pkg{manyROC} functions.
#'
#' @examples
#'
#' library(manyROC)
#'
#'
#' # --- For numeric vectors objects ---
#'
#' data(PlantGrowth)
#' roc_manyroc(x = PlantGrowth$weight, gr = PlantGrowth$group)
#'
#'
#' # --- For dataframes objects ---
#'
#' data(CO2)
#' roc_manyroc(CO2[, c("conc", "uptake")], CO2$Type)
#'
#' data(OrchardSprays)
#' roc_manyroc(OrchardSprays$decrease, OrchardSprays$treatment)
#'
#' # --- For hyperSpec objects ---
#'
#' library(hyperSpec)
#' fluorescence
#'
#' roc_manyroc(fluorescence[ , , 500~502], fluorescence$gr)
#'
#' @name roc_manyroc

roc_manyroc <- function(x, gr = NULL, optimize_by = "bac",  ...) {
    UseMethod("roc_manyroc")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~ The default method, that does the job ~~~
#' @rdname roc_manyroc
#' @export
roc_manyroc.matrix <- function(x,
                               gr = NULL,
                               optimize_by = "bac",
                               ...,
                               gr_sep = " vs. ") {

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    assert_numeric(x)
    assert_factor(gr, min.levels = 2)
    if (nrow(x) != length(gr)) {
        stop("Number of cases in `x` and `gr` must agree." )
    }
    assert_choice(tolower(optimize_by), choices = c("bac", "kappa", "youden"))
    assert_string(gr_sep)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Initial preprocessing
    gr <- factor(gr, ordered = FALSE)
    rownames(x) <- NULL

    # Preparation: calculations
    n_wl  <- ncol(x)
    levs  <- levels(gr)
    cmb   <- t(combn(levels(gr), 2))
    n_cmb <- nrow(cmb)
    if (n_cmb == 2) n_cmb <- 1 # prevents from excessive calculations

    # Pre-allocate variables
    Compared <- paste0(cmb[, 1], gr_sep, cmb[, 2])
    grouppair_results <- vector("list", n_cmb)
    names(grouppair_results) <- Compared

    # For each pair of classes
    for (u in 1:n_cmb) {
        included_levels <- c(cmb[u, 1], cmb[u, 2])
        included_ind    <- gr %in% included_levels
        x_subset        <- x[included_ind, , drop = FALSE]
        included_gr     <- droplevels(gr[included_ind])

        # For each feature (column) in matrix `x`
        optimal <- apply(x_subset, 2,
            FUN = roc_analysis,
            gr = included_gr,
            pos_label = included_levels[2],
            optimize_by = optimize_by,
            results = "optimal")

        # Collect the results
        grouppair_results[[u]] <- t(optimal)
    }

    ## The names of each row of result produced by the `apply` function
    # in variable `optimal`.

    # result_names <- c("cutoff", "TP","FN","FP","TN",
    #                   "sens","spec","PPV","NPV",
    #                   "BAC","Youden", "Kappa","AUC",
    #                   "median_neg", "median_pos")

    result_names <- c(
        "cutoff", "tp","fn","fp","tn",
        "sens","spec","ppv","npv",
        "bac","youden", "kappa","auc",
        "median_neg", "median_pos"
    )

    # Clean the result
    OBJ <-
        grouppair_results  %>%
        purrr::map(
            ~.x %>%
                as.data.frame() %>%
                magrittr::set_colnames(result_names)  %>%
                tibble::rownames_to_column(var = "feature")
        ) %>%
        dplyr::bind_rows(.id = "compared_groups")  %>%
        dplyr::select(
            compared_groups, feature, median_neg, cutoff, median_pos,
            dplyr::everything()
        )

    # Output
    add_class_label(OBJ, "manyroc_result")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_manyroc
#' @export
roc_manyroc.numeric <- function(x, gr = NULL, optimize_by = "bac",  ...) {
    roc_manyroc(x = as.matrix(x), gr = gr, optimize_by = optimize_by, ...)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_manyroc
#' @export
roc_manyroc.data.frame <- function(x, gr = NULL, optimize_by = "bac",  ...) {
    roc_manyroc(x = as.matrix(x), gr = gr, optimize_by = optimize_by, ...)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_manyroc
#' @export
roc_manyroc.hyperSpec <- function(x, gr = NULL, optimize_by = "bac",  ...) {
    assert_class(x, "hyperSpec")
    roc_manyroc(x = x[[]], gr = gr, optimize_by = optimize_by, ...)
}
# =============================================================================

