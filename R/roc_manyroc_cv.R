# =============================================================================
# FIXME: the desctription needs revision
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIXME in
#
#     roc_manyroc_cv(PlantGrowth$weight, PlantGrowth$group)
#     roc_manyroc_cv(PlantGrowth[,"weight", drop = FALSE], PlantGrowth$group)
#
# culumn `feature` should have named elements, not just all 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# x <- PlantGrowth$weight
# gr <- PlantGrowth$group
# sp_x <- sp
# x  <- sp_x[[]]
# gr <- sp_x$gr
# gr <- gl(3, 5, length = length(sp_x$gr), LETTERS[1:3])
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' Carry out the manyROC analysis with cross-validation
#'
#' Do the ROC analysis (\code{\link{roc_analysis}}) with cross-validation
#' for each column of \code{x} (see also: \code{\link{roc_manyroc}}).
#'
#'
#' @param cvo a cross-validation object (cvo), created with function
#'            \code{\link{cvo_create_folds}},
#'            \pkg{caret} \code{\link[caret]{createFolds}}
#'            or similar.
#'
#' @inheritParams roc_manyroc
#' @inheritParams cvo_create_folds
#' @inheritParams roc_performance_measures
#'
#'
#' @return A data frame with results. The object also inferits from
#'  class \code{manyroc_cv} (for displaying purposes).
#'
#'
#' @export
#'
#' @author Vilmantas Gegzna
#'
#' @examples
#'
#' library(manyROC)
#'
#'
#' # --- For numeric vectors objects ---
#'
#' data(PlantGrowth)
#'
#' set.seed(123456)
#'
#' roc_manyroc_cv(PlantGrowth$weight, PlantGrowth$group)
#'
#' roc_manyroc_cv(PlantGrowth$weight, gl(2, 1, 30))
roc_manyroc_cv <- function(x,
                           gr,
                           optimize_by = "bac",
                           cvo = cvo_create_folds(x, gr, seeds = seeds, kind = kind),
                           seeds = NA_real_,
                           kind = NULL) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- as.matrix(x)
  assert_numeric(x)

  assert_factor(gr, min.levels = 2)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Number of folds in total
  n_repetitions <- cvo_count_folds(cvo)

  # Prealocate variables
  rez_train <- vector("list", length = n_repetitions)
  names(rez_train) <- cvo_get_fold_names(cvo)
  rez_test <- rez_train

  for (i in 1:n_repetitions) {
    # Return indices of training subset:
    training_ind <- cvo_get_inds(cvo, fold = i, type = "train")

    x_train  <-  x[training_ind, ]
    gr_train <- gr[training_ind]

    x_test   <-  x[-training_ind, ]
    gr_test  <- gr[-training_ind]

    rez_train[[i]] <- roc_manyroc(
      x  = x_train,
      gr = gr_train,
      optimize_by = optimize_by
    )

    # FIXME: The function must fail when there are more than 2 classes.
    # An assertion / a test is needed.


    ## The order of names must be identical:
    #
    # tmp1 <- roc_extract_info(rez_train[[i]])
    # tmp2 <- split_by_feature(tmp1, levels = colnames(x_test))
    #
    # identical(tmp1$feature, names(tmp2))
    # identical(names(tmp2),  names(mat2list(x_test)))

    tmp1 <- roc_extract_info(rez_train[[i]])
    tmp2 <- split_by_feature(tmp1, levels = colnames(x_test))
    tmp3 <- purrr::map2(
      .x = tmp2,
      .y = mat2list(x_test),
      .f = roc_predict_performance_by_gr,
      gr_new = gr_test
    )

    rez_test[[i]] <- dplyr::bind_rows(tmp3, .id = "feature")

    # rez_test[[i]]  <- predict(roc_object, newdata = x_test, gr_test)
  }

  result <- dplyr::bind_rows(
    `training` = res2df(rez_train),
    `test`     = res2df(rez_test),
    .id = "set"
  )


  # output
  add_class_label(result, c("manyroc_cv_result", "roc_df"))


} # [END]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Hepler functions:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
res2df <- function(res) {
  res %>%
    dplyr::bind_rows(.id = "fold") %>%
    dplyr::arrange(feature) %>%
    add_class_label("manyroc_result")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
split_by_feature <- function(obj, levels) {
  # It is crucial to use forcats::as_factor as the levels of factors must
  # be sorted in order of first appearance of the value.
  # Othervise incorrect results will apear.
  factor_var <- factor(obj$feature, levels = levels)

  res <- split(obj, f = factor_var)
  purrr::map(res, add_class_label, c("roc_info", "roc_df"))
}
# if (is.null(levels)) {
#     factor_var <-
#         (obj$feature)  %>%
#         forcats::as_factor() %>%
#         forcats::fct_inorder()
# } else {
#     factor_var <- factor(obj$feature, levels = levels)
# }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mat2list <- function(x) {
  as.list(as.data.frame(x, stringsAsFactors = FALSE))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # ===========================================================================
# # @rdname roc_manyroc
# # @method print manyROC_cv
# # @export
# print.manyroc_cv_result <- function(x) {
#
#     bru("-")
#     cat("Summary:\n")
#     print(summary(x))
#
#     # cat("\nLength of $obj:" ,length(obj$obj), "\n\n")
#
#     cat("\n*** Summary of $obj[[1]]: ***\n")
#     print(x$obj[[1]])
#
#     cat("\n*** Summary of $cvo: ***\n")
#
#     info_cvo <- cvo_get_info(x$cvo)
#     info_cvo <- data.frame(colnames(info_cvo), t(info_cvo))
#     rownames(info_cvo) <- NULL
#     names(info_cvo)    <- c("<FIELD>", "<INFORMATION>")
#     print(info_cvo)
#     bru("-")
# }
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
