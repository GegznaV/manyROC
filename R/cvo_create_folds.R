# [!!!] Remove dependency on mlr in parameters sections


#' Create a cvo (cross-valitation object)
#'
#' [!!!] \bold{DESCRIPTION MUST BE UPDATED}\cr
#'
#' Create indices of folds with blocking and stratification (cvo object)
#' Create indices of folds with blocking and stratification for (repeated)
#' k-fold cross-validation. \cr
#' Function \code{cvo_create_folds} randomly divides observations into
#' folds that are used for (repeated) k-fold cross-validation. In these
#'  folds observations are:
#' \enumerate{
#'  \item \bold{blocked} by values in variable \code{block_by}
#'      (i.e. observations with the same "ID" or other kind of blocking factor
#'      are treated as one unit (a block) and are always in the same fold);
#'  \item \bold{stratified} by levels of factor variable \code{stratify_by}
#'       (the proportions of these grouped units of observations per each
#'       group (level) are kept approximately constant throughout all folds).
#'  }
#'
#' @name cvo_create_folds
#' @note If \code{folds} is such big, that some folds have no observations of
#'       a certain group (i.e., level in \code{stratify_by}), an error
#'       is returned. In that case smaller value of \code{folds} may be
#'       recommended.
#'
#' @param data A data frame, that contains variables which names are denoted
#'        by arguments \code{block_by} and by \code{stratify_by}.
#'
#' @param stratify_by A vector or a name of factor variable in \code{data},
#'                 which levels will be used for \emph{stratification}. E.g.,
#'                 a vector with medical groups.
#'
#' @param block_by A vector or a name of variable in \code{data}, that
#'                 contains identification codes/numbers (ID). These codes
#'                 will be used for blocking.
#'
#' @param folds,k (\code{integer})\cr A number of folds, default \code{folds = 5}.
#'
#' @param mode (\code{character})\cr Either \pkg{caret}-like or \pkg{mlr}-like
#'             cvo object. \bold{This option is not implemented yet!}
#' @param returnTrain (\code{logical} | \code{character}) \cr
#'                  If \code{TRUE}, returns indices of variables in
#'                  a training set (\pkg{caret} style).
#'                  If \code{FALSE}, returns indices of variables in
#'                  a test set (\pkg{caret} style).
#'                  If \code{"both"}, returns indices of
#'                  variables in both training and test sets (\pkg{mlr} style).
#'
#' @param times (\code{integer})\cr a number of repetitions for
#'              repeated cross-validation.
#' @param seeds (vector of integers | \code{NULL})\cr Seeds for random number
#'             generator for each repetition.\cr
#'             If \code{seeds = NULL} random seeds are generated.\cr
#'             If number of repetitions is
#'             greater than number of provided seeds, random seeds are
#'             generated and added to the provided ones. The first seed will
#'             be used to ensure reproducibility of the randomly generated
#'             seeds.\cr
#'
#'              (See \code{\link[base]{set.seed}} for more information about
#'              random number generation).
#'
#' @param kind (\code{character} | \code{NULL})\cr The kind of (pseudo)random
#'             number generator. Default is \code{"L'Ecuyer-CMRG"} as it
#'             provides the basis for the multiple streams used in package
#'             \pkg{parallel}.
#'             More information at \code{\link[base]{set.seed}}.
#'
#' @inheritParams mlr::makeResampleDesc
#'
#'
#' @return (\code{list}) A list of folds. In each fold there are indices
#'         observations. The structure of outputs is the similar to one
#'         created with either function \code{\link[caret]{createFolds}}
#'         from \pkg{caret} or function
#'         \code{\link[mlr]{makeResampleInstance}} in \pkg{mlr}.
#'
#' @export
#' @examples
#' library(multiROC)
#'
#' # [!!!] Load data
#' DataSet1 <-data.frame(ID = rep(1:20, each = 2),
#'                            gr = gl(4, 10, labels = LETTERS[1:4]),
#'                            .row = 1:40)
#'
#' # Explore data
#'      str(DataSet1)
#'
#'      table(DataSet1[,c("gr","ID")])
#'
#'      summary(DataSet1)
#'
#'
#' # Explore functions
#'      nFolds = 5
#'
#' # If variables of data frame are provided:
#'      Folds1_a <- cvo_create_folds(data = DataSet1,
#'                               stratify_by = "gr", block_by = "ID",
#'                                k = nFolds, returnTrain = FALSE)
#'      # str(Folds1_a)
#'      cvo_test_bs(Folds1_a, "gr", "ID", DataSet1)
#'
#' # If "free" variables are provided:
#'      Folds1_b <- cvo_create_folds(stratify_by = DataSet1$gr,
#'                                   block_by = DataSet1$ID,
#'                                   k = nFolds,
#'                                   returnTrain = FALSE)
#'      # str(Folds1_b)
#'      cvo_test_bs(Folds1_b, "gr", "ID", DataSet1)
#'
#' # Not blocked but stratified
#'      Folds1_c <- cvo_create_folds(stratify_by = DataSet1$gr,
#'                                   k = nFolds,
#'                                   returnTrain = FALSE)
#'      # str(Folds1_c)
#'      cvo_test_bs(Folds1_c, "gr", "ID", DataSet1)
#'
#' # Blocked but not stratified
#'      Folds1_d <- cvo_create_folds(block_by = DataSet1$ID,
#'                                   k = nFolds,
#'                                   returnTrain = FALSE)
#'      # str(Folds1_d)
#'      cvo_test_bs(Folds1_d, "gr", "ID", DataSet1)
#'
#'
#' @seealso Function \code{\link[caret]{createFolds}} from package
#'          \pkg{caret}. \cr
#'          Function \code{\link[mlr]{makeResampleInstance}} from package
#'          \pkg{mlr}. \cr
#' Test if folds are blocked and stratified \code{\link{cvo_test_bs}}
#' @author Vilmantas Gegzna

cvo_create_folds <- function(data = NULL,
                             stratify_by = NULL,
                             block_by = NULL,
                             folds = 5,
                             times = 1,
                             seeds = NULL,
                             kind = "L'Ecuyer-CMRG",
                             mode = c("caret", "mlr")[1],
                             returnTrain = c(TRUE, FALSE, "both")[1],
                             # predict: for compatibility with `mlr``
                             predict = c("test", "train", "both")[1],
                             k = folds

) {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    gr = stratify_by
    ID = block_by
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (k < 2) stop("Number of folds `k` must be at least 2.")
    nFolds <- k
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Choose seeds for random number generation -------------------------

    # If too few seeds are provided
    len_seeds <- length(seeds)

    if (!is.null(seeds) & (len_seeds < times) & (len_seeds > 1))
        warning("Number of provided `seeds` is not sufficient. \n",
                    "Random `seeds` will be added.\n")

    # If just one seed is provided
    if (len_seeds == 1 & (len_seeds < times))
        set.seed(seed = seeds, kind = kind)

    # Generate seeds, if needed
    if (is.null(seeds) | (len_seeds < times)) {
        seeds <- c(seeds,
                   sample(-9e7:9e7, times - len_seeds)
        )
    }

    # If too many seeds are provided
    seeds <- rep_len(seeds, times)

    # Force default values, if needed ===================================
    force(data)
    force(stratify_by)
    force(block_by)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (inherits(data, "hyperSpec")) data <- data$..

    # Parse input and prepare data ===========================================
    # if `data` is provided:
    ID <- getVarValues(ID, data)
    gr <- getVarValues(gr, data)

    # If either `ID` or `gr` is not provided:
    if (is.null(ID) & length(gr) > 1) {
        ID <- 1:length(gr) # create unique IDs, if not blocked
    }

    if (is.null(gr) & length(ID) > 1) {
        gr <- rep(0, length(ID)) # create one level of `gr`, if not stratified
    }

    if (is.null(gr) & is.null(ID)) {
        N_ <- nrow(data) %if_null_or_len0% length(data)

        ID <- 1:N_       # create unique IDs, if not blocked
        gr <- rep(0, N_) # create one level of `gr`, if not stratified
    }

    sample_size <- length(ID)
    rm(data)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(ID) != length(gr))
        stop("Lengths of vectors `stratify_by` and `block_by` must agree.")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # DF_all_ID <- data.frame(ID = ID, gr = gr)
    DF_all_ID <- data.frame(gr = gr,
                            ID = ID,
                            stringsAsFactors = FALSE)

    # get unique values only: for blocking

    DF_uni_ID <- unique(DF_all_ID)


    # Calculations  ==========================================================
    DF_uni_ID$Fold <- rep(NA, times = nrow(DF_uni_ID))
               nGr <- DF_uni_ID$gr %>% as.factor %>% nlevels # NA's are not included

    DFuniID_ByGr <- split(DF_uni_ID, DF_uni_ID$gr)
    n_ByGr       <- sapply(DFuniID_ByGr, nrow)     # unique IDs per class

    # If Number of observatuions in a group is to small
    if (any(n_ByGr < nFolds)) {
        bru("-")
        cat('Number of unique cases/blocks in each group:\n')
        print(n_ByGr)
        print(glue::glue('Number of folds = {nFolds}\n\n'))
        stop("Number of UNIQUE observations in one of the\n",
             "groups is smaller than number of folds.\n")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # For every repetition
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # fold_name_format <- paste0("Fold%0",
    #                            (log10(nFolds * times) %/% 1) + 1,
    #                            "g%s")


    fold_name_format <-
        glue::glue("Rep%0{digit_1}g_Fold%0{digit_2}g",
                   digit_1 = (log10(times) %/% 1)  + 1,
                   digit_2 = (log10(nFolds) %/% 1) + 1)

    for (i in 1:times) {
        set.seed(seeds[i], kind = kind)

        available_folds <- seq_len(nFolds)

        # available_folds = (1:nFolds) + nFolds * (i - 1)
        # REPETITION <- if (times == 1) "" else paste0("_Rep", i)

        # Assign numbers of fold to each row
        # Split to folds in a stratified way by group 'gr'
        for (gr_i in 1:nGr) {
            GrSize     <-  n_ByGr[gr_i]
            # modulus - how many times observations are divided
            #           proportionally to each fold.
            TimesEach  <-  GrSize %/% nFolds

            # reminder - number of observations, that cannot
            # be divided proportionally.
            nRem   <-  GrSize %%  nFolds

            # Separate permutations ensures more proportional distribution when
            # number of observations is small:

            # Create a list of proportionally distributed per fold
            Proportionals <-  rep(available_folds, times = TimesEach)
            # Permute the list of proportionally distributed
            Proportionals <-  sample(Proportionals, GrSize - nRem)
            # Permute reminders separately
            Reminders     <-  sample(available_folds, nRem)
            # Merge
            BelongsToFoldNr <- c(Proportionals, Reminders)
            DFuniID_ByGr[[gr_i]]$Fold <-
                sprintf(fold_name_format, i, BelongsToFoldNr)
        }

        # unsplit the dataframe: NA's removed
        # df_with_folds <- unsplit(DFuniID_ByGr, DF_uni_ID$gr[!is.na(DF_uni_ID$gr)])
        # df_with_folds <- do.call("rbind", DFuniID_ByGr)

        df_with_folds <- dplyr::bind_rows(DFuniID_ByGr)

        # data_i <- DF_all_ID %>%
        #     mutate(ORDER = seq_along(ID)) %>%
        #     merge(df_with_folds, by = "ID", sort = FALSE)  %>%
        #     arrange(ORDER)

        data_i <- DF_all_ID %>%
            dplyr::mutate(ORDER = seq_along(ID)) %>%
            dplyr::left_join(df_with_folds, by = c("ID", "gr"))  %>%
            dplyr::arrange(ORDER)

        if (!all(data_i$ID == ID)) {
            warning("Order of indices does not match order of input data. ",
                "This might be caused by NA values in the data."
                # , "Either IDs might be incorrectrly sorted inside function 'cvo_create_folds'"
            )
        }

        Ind_all <- 1:nrow(data_i)
        data_i$Test_ind <- Ind_all # Additional column with row numbers.
        # which are treated as indices for test
        # subset.

        DATA <- if (i == 1) data_i else dplyr::bind_rows(DATA, data_i)
    }

    Test_ind <- split(DATA$Test_ind,
                      factor(DATA$Fold, levels = sort(unique(DATA$Fold))
                      )
    )

    # Before `return` -------------------------------------------------------

        if (times > 1) {
            validation_type <- "Repeated k-fold"
            # cv_type <- "repeated cross-validation"
        } else if (times == 1) {
            "k-fold"
            validation_type <- "k-fold"
            # cv_type <- "cross-validation"
        }
    # Choose which indices (test/train) to remove

    switch(returnTrain %>% as.character() %>% toupper(),
           "TRUE" = {
               Train_ind  <- lapply(Test_ind, function(x) {setdiff(Ind_all, x)})
               ind_type   <- "Train"
               return_ind <- Train_ind
               class(return_ind) <- c("cvo_caret", "cvo")
           },
           "FALSE" = {
               ind_type   <- "Test"
               return_ind <- Test_ind
               class(return_ind) <- c("cvo_caret", "cvo")
           },
           "BOTH" = {
               ind_type <- "For `mlr`"
               desc <- list(
                   folds    = nFolds,
                   reps     = times,
                   id       = "repeated cross-validation",
                   iters    = nFolds * times,
                   # [!!!] Next libe should be pdated approriately:
                   predict  = predict, # c("train", "test", "both")
                   stratify = nGr > 1,
                   # [!!!] Next libe should be pdated approriately:
                   stratify.cols = NULL
                   )

               # addClasses(desc, stri_paste(method, "Desc"))
               desc %<>% add_class_label(c("RepCVDesc", "ResampleDesc"))

               # [!!!] The next line must be updated appropriately
               group_ <- factor()

               return_ind <- list(
                   desc = desc,
                   size = sample_size,
                   train.inds = lapply(Test_ind,
                                       function(x) {setdiff(Ind_all, x)}
                   ),
                   test.inds  = Test_ind,
                   group = group_
               )
               class(return_ind) <- c("ResampleInstance", "cvo_mlr", "cvo")
           }
    )

    # Add attributtes
    attr(return_ind, "info") <-
        data.frame(
            indices     = ind_type,
            stratified  = nGr > 1,
            blocked     = any(duplicated(ID)),
            cv_type     = validation_type, # type of cross-validation
            k           = k,
            repetitions = times,
            sample_size = sample_size,
            # cross_validation_type  = validation_type,

            stringsAsFactors = FALSE
        )

    attr(return_ind, "seeds") <- list(generator = kind, seeds = seeds)
    # -----------------------------------------------------------------------
    # Return
    return_ind
}
# [END]
