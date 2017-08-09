#' Do manyROC anamysis with cross-validation for hyperSpec object
#'
#' [!!!] // No description yet //
#'
#' @param Spectra \code{hyperSpec} object, that contains colum \code{ID} for
#'                spectra blocking, columns with grouping variables as well
#'                as spectroscopic information.
#'
#' @param variables_to_analyze (\code{character}) \cr
#'                            A character vector with names of grouping
#'                            variables to use in analysis.
#'
#' @param k_folds (positive \code{integer})  \cr
#'                The number of folds in k-fold cross-validation.
#'
#' @param times (positive \code{integer})  \cr
#'              The number of repetitions in repeated k-fold cross-validation.
#'
#' @param seeds (\code{NULL} | \code{integer}) \cr
#'              Either a vector of integers of length \code{times} to set
#'              seed for each repetition of k-fold cross-validation.
#'              The seeds nust be at least of length 6 and meet other
#'              requirements for \code{"L'Ecuyer-CMRG"} random number
#'              generator.
#'
#' @return A list with results \cr
#'        [!!!] Description needs more specification.
#'
#' @export
#'
#' @examples
#'
#' library(manyROC)
#'
#' fluorescence$ID  <- 1:nrow(fluorescence)
#' sp_manyroc_with_cv_by_variable(fluorescence[,,500~501], c("gr", "class"),
#'                           k_folds = 3, times = 2)
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' \dontrun{
#' \donttest{
#'  sp_manyroc_with_cv_by_variable(fluorescence, c("gr")
#'  }}

sp_manyroc_with_cv_by_variable <-
    # -----------------------------------------------------------------------------
    function(Spectra,
             variables_to_analyze,
             k_folds = 3,
             times = 10,
             seeds = 2222222) {
    # -----------------------------------------------------------------------------
    if (!checkmate::test_named(variables_to_analyze))
        names(variables_to_analyze) <- variables_to_analyze
    # -----------------------------------------------------------------------------
    res_tmp <- parallelMap::parallelLapply(variables_to_analyze,
                                           purrr::safely(sp_manyroc_with_cv),
                                           Spectra = Spectra,
                                           k = k_folds,
                                           times = times,
                                           seeds = seeds)
    # -----------------------------------------------------------------------------
    not_error <- purrr::map_lgl(res_tmp, ~is.null(.x$error))
    t_tez <- purrr::transpose(res_tmp)

    res_not_err <- t_tez$result[not_error]
    names(res_not_err) <- purrr::map_chr(res_not_err, ~.x$variable)
    res_final <- purrr::transpose(res_not_err)


    res_final$cvo %<>%
        add_class_label("hide_it")

    res_final$results <-
        dplyr::bind_rows(res_final$results, .id = "grouping")

    res_final$ind_included_rows %<>%
        as.data.frame() %>%
        add_class_label("roc_df")

    res_final$n_included %<>%
        dplyr::bind_cols() %>%
        as.data.frame()

    res_final$variables_included <-
        res_final$variable %>%
        purrr::reduce(c)

    res_final$variables_errored <-
        variables_to_analyze[!not_error] %>%
        remove_names()

    res_final$variable <- NULL

    res_final$error_messages <-
        add_class_label(t_tez$error, "hide_it")
    # -------------------------------------------------------------------------
    res_final
}

# =============================================================================
remove_names <- function(x) {
    names(x) <- NULL
    x
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~