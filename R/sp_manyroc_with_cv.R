# =============================================================================
# Spectra <- Spectra
# Var <- "CitoGr"


#' Do manyROC analysis with cross-validation for hyperSpec object
#'
#' [!!!] // No description yet //
#'
#' @param Var (\code{character(1)}\ \code{factor}) \cr
#'             \bold{Either} the name of variable in \code{Spectra} which
#'             contains the grouping variable
#'             \bold{or} a factor vector (or convertible to factor) with
#'             values for grouping.
#'
#' @param n_min \code{integer(1)} \cr
#'             minimum acceptable number of unique samples per group.
#'             Must be at least \code{k_folds} or bigger.
#'
#' @inheritParams sp_manyroc_with_cv_by_variable
#'
#' @details
#' Function \code{sp_manyroc_with_cv} will be \bold{renamed} in the future.
#'
#' @export
#'
#' @seealso \code{\link{sp_manyroc_with_cv_by_variable}}
#'
#' @examples
#' library(manyROC)
#'
#' fluorescence$ID  <- 1:nrow(fluorescence)
#' sp_manyroc_with_cv("gr", fluorescence[,,500~502], k_folds = 3, times = 2)
#'
sp_manyroc_with_cv <-
    function(Var,
             Spectra,
             k_folds = 5,
             times = 10,
             seeds = 2222222,
             kind = "L'Ecuyer-CMRG",
             # reikia įdėti seed generatoriaus pavadinimą
             n_min = k_folds
    ) {
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Remove rows with NA values and with groups which have too few samples in
        # that group

        # Convert to factor and drop unnecessary levels
        Spectra$ID  %<>% as.factor()

        var_values         <- get_var_values(Var, Spectra) %>% as.factor()

        too_few_in_gr      <- has_too_few_IDs(Spectra, Var, n_min = n_min)
        ind_too_few        <- var_values %in% too_few_in_gr
        ind_NA             <- is.na(var_values)
        ind_included_rows  <- !ind_NA & !ind_too_few
        Spectra            <- Spectra[ind_included_rows, ]

        # Drop unnecessary levels after subsetting
        eval_glue("Spectra$`{Var}` %<>% droplevels()")

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Make a cross-validation object
        cvo <- cvo_create_folds(Spectra,
                                block_by = "ID",
                                stratify_by = Var,
                                k = k_folds,
                                times = times,
                                seeds = seeds,
                                kind = kind)

        x  <- Spectra[[]]
        gr <- Spectra[[,Var, drop = TRUE]]

        roc_res <- roc_manyroc_cv(x = x,
                                  gr = gr,
                                  optimize_by = "bac",
                                  cvo = cvo)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        list(variable = Var,
             n_included = sum(ind_included_rows),
             ind_included_rows = add_class_label(ind_included_rows, "as_str"),
             # x  = add_class_label(x, "as_str"),
             # gr = add_class_label(gr, "as_str"),
             cvo = cvo,

             results = add_class_label(roc_res, "roc_df"))
    }

# =============================================================================
# Helpers
# =============================================================================
has_too_few_IDs <- function(OBJ,
                            Var = colnames(OBJ)[1],
                            ID = "ID",
                            n_min = 5,
                            na.rm = TRUE){

    gr_ <- count_spectra(OBJ, Var = Var, ID = ID, na.rm = na.rm)$n_ID
    names(gr_[gr_ < n_min])
}
# =============================================================================
count_spectra <- function(OBJ,
                          Var = NULL, # colnames(OBJ)[1],
                          ID = "ID",
                          na.rm = TRUE,
                          decimals   = 1,
                          include_na = TRUE,
                          na_level = "(Missing)") {

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Define necessary functions

    # Get counts and percentages
    n_unique <- function(x) {unique(x) %>% length()}

    get_percent <- function(x) {
        sprintf(fmt = glue::glue("%.{decimals}f%%"),
                100 * (x / sum(x, na.rm = na.rm)))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    hyperSpec::chk.hy(OBJ)
    if (!ID %in% hyperSpec::colnames(OBJ)) {
        stop("The dataset does not contain variable called `", ID, "`")
    }

    # For whole dataset
    if (is.null(Var)) { # new part of the function

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Extract necessary variables
        ID_  <- eval_glue("OBJ$`{ID}`")

        if (include_na == TRUE) {
            ID_  %<>% forcats::fct_explicit_na(na_level = "(Missing)")
        }

        n_ID      <- n_unique(ID_)
        n_spectra <- length(ID_)

        percent_ID      <- get_percent(n_ID)
        percent_spectra <- get_percent(n_spectra)


        # For subsets by values of `VAR`
    } else {# The original part of the function

        if (!Var %in% hyperSpec::colnames(OBJ)) {
            stop("The dataset does not contain variable called `", Var, "`")
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Extract necessary variables
        Var_ <- eval_glue("OBJ$`{Var}`")
        ID_  <- eval_glue("OBJ$`{ID}`")

        # Get counts and percentages
        n_unique <- function(x) {unique(x) %>% length()}

        if (include_na == TRUE) {
            ID_  %<>% forcats::fct_explicit_na(na_level = "(Missing)")
            Var_ %<>% forcats::fct_explicit_na(na_level = "(Missing)")
        }

        n_ID      <- tapply(ID_, Var_, n_unique)
        n_spectra <- tapply(ID_, Var_, length)

        percent_ID      <- get_percent(n_ID)
        percent_spectra <- get_percent(n_spectra)

    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Return results

    data.frame(n_ID, n_spectra, percent_ID, percent_spectra,
               check.names = FALSE)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
# =============================================================================
eval_glue <- function(..., envir = parent.frame(),
         .sep = "", .open = "{", .close = "}") {

    x2 <- glue::glue(..., .envir = envir, .open = .open, .close = .close)
    eval(parse(text = x2), envir = envir)
}
# =============================================================================