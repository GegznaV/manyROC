# # # ======================================================================
# # q025 <- purrr::partial(quantile, probs = 0.025, na.rm = TRUE)
# # q975 <- purrr::partial(quantile, probs = 0.975, na.rm = TRUE)
# #
# # _q025 <- purrr::partial(quantile, probs = 0.025, na.rm = TRUE)
# # _q975 <- purrr::partial(quantile, probs = 0.975, na.rm = TRUE)
# # _mean <- purrr::partial(mean,  na.rm = TRUE)
# #
# #
# # roc_rez  %>%
# #     group_by(compared_groups, set, .wavelength)  %>%
# #     summarize_at(
# #         vars(
# #             # sens,
# #             # spec,
# #             ppv,
# #             npv,
# #             # bac,
# #             # youden,
# #             # kappa,
# #             auc),
# #         funs("mean"))  %>%
# #
# #     gather(key = "Measure",
# #            value = "value",
# #            # sens,
# #            # spec,
# #            ppv,
# #            npv,
# #            # bac,
# #            # youden,
# #            # kappa,
# #            auc) %>%
# #
# #     ggplot(aes(.wavelength, value, color = Measure)) +
# #     geom_hline(yintercept = 0.75, lty = 2, color = "grey20") +
# #     geom_hline(yintercept = c(0, 0.5), lty = 2, color = "red4") +
# #     geom_point() +
# #     geom_line(size = 1) +
# #     facet_grid(compared_groups ~ set) +
# #     ylim(c(-0.15, 1))
# #
# # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # ggplotly( roc_rez  %>%
# #               group_by(compared_groups, set, .wavelength)  %>%
# #               summarize_at(
# #                   vars(median_neg, cutoff, median_pos),
# #                   funs(q025, mean, q975))  %>%
# #               as.data.frame()  %>%
# #
# #               gather(key = "Measures",
# #                      value = "value",
# #                      -compared_groups, -set, -.wavelength) %>%
# #               separate(col = "Measures",
# #                        into = c("Measure", "stat"),
# #                        sep = "_(?=[^np])") %>%
# #               spread(key = "stat", value = "value")  %>%
# #
# #               add_class_label("roc_df")  %>%
# #
# #               ggplot(aes(.wavelength, color = Measure, fill = Measure)) +
# #               # geom_hline(yintercept = 0.75, lty = 2, color = "grey20") +
# #               # geom_hline(yintercept = c(0, 0.5), lty = 2, color = "red4") +
# #               geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.3) +
# #               geom_line(aes(y = mean), size = 1) +
# #               facet_grid(compared_groups ~ set))
#
#
# # +
# #     ylim(c(-0.15, 1))
#
# # %>%
# # summarize_at(vars(sens, spec,  ppv,  npv,  bac, youden, kappa,  auc),
# #              funs(q025, mean, q975))
#
# =============================================================================


# =============================================================================
# NOTES for future developement of the paackage:
#
# [!!!] CVO objects can be different, if paralellization is done in different
# levels, and not in the top level as it is done now.
#
# =============================================================================


#' Do manyROC analysis with cross-validation for hyperSpec object for each variable
#'
#' [!!!] // No description yet //\cr\cr
#'  For reproducible results in parallel computing, set seed with
#' \code{parallel::\link[parallel]{clusterSetRNGStream}(iseed = x)}(where \code{x}
#' is your seed) (and not with \code{set.seed}) as package
#' \pkg{parallelMap} is used for paralellization.\cr
#'
#'
#'              [!!!] The seeds nust be at least of length 6 and meet other
#'              requirements for \code{"L'Ecuyer-CMRG"} random number
#'              generator.\cr\cr
#'              [!!!] At the moment Seeding section needs revision if
#'              it is necessary to use
#'              \code{"L'Ecuyer-CMRG"} (pseudo)random number generator as
#'              \code{\link[base]{set.seed}()} does not work with
#'              \pkg{parallelMap} package.
#'              Instead \code{parallel::clusterSetRNGStream(iseed = x)}
#'              (where \code{x} is your seed) should be used with
#'              \pkg{parallelMap} to get reproducible results.
#'
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
#'              The number of repetitions in repeated k-fold
#'              cross-validation.
#'
#' @param seeds (\code{NULL} | \code{integer}) \cr
#'              Either a vector of integers of length \code{times} to set
#'              seed for each repetition of k-fold cross-validation.
#'              For more about seeds see \code{\link[base]{set.seed}()}.
#'
#'              Each seed will be passed to \code{\link{cvo_create_folds}}.()
#'
#'
#' @inheritParams base::set.seed
#'
#' @return A list with results \cr
#'        [!!!] Description needs more specification.
#'
#' @export
#'
#' @seealso \code{\link{sp_manyroc_with_cv}}
#'
#' @examples
#'
#' library(manyROC)
#'
#' fluorescence$ID  <- 1:nrow(fluorescence)
#' sp_manyroc_with_cv_by_variable(
#'         fluorescence[,,500~501],
#'          c("gr", "class"),
#'          k_folds = 3,
#'          times = 2)
#'
sp_manyroc_with_cv_by_variable <-
    # -----------------------------------------------------------------------------
    function(Spectra,
             variables_to_analyze,
             k_folds = 3,
             times = 10,
             seeds = NULL, #2222222,
             kind  = NULL) {
    # -----------------------------------------------------------------------------
    if (!checkmate::test_named(variables_to_analyze))
        names(variables_to_analyze) <- variables_to_analyze
    # -----------------------------------------------------------------------------
    # Prepare for paralellization
    # [!!!] Check if these parallel export functions are really necessary
    # parallelMap::parallelLibrary("manyROC", "purrr",
    #                              level = "manyROC.grouping_variables",
    #                              show.info = FALSE)

    parallelMap::parallelExport("variables_to_analyze",
                                "Spectra", "k_folds", "times", "seeds", "kind",
                                 level = "manyROC.grouping_variables",
                                 show.info = FALSE)
    # -----------------------------------------------------------------------------
    res_tmp <- parallelMap::parallelLapply(variables_to_analyze,
                                           purrr::safely(sp_manyroc_with_cv),
                                           Spectra = Spectra,
                                           k = k_folds,
                                           times = times,
                                           seeds = seeds,
                                           kind = kind,

                                           level = "manyROC.grouping_variables")
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