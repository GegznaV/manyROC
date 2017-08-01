context("cvo_get_info")

test_that("utility function vorks with `cvo_caret` object", {

    folds <- 10
    n_rows <- nrow(fluorescence)
    set.seed(123456, "L'E")
    cvo_caret <- cvo_create_folds(fluorescence, folds = folds)

    expect_is(cvo_get_info(cvo_caret), "data.frame")

    expect_is(cvo_get_seeds(cvo_caret), "list")
    expect_length(cvo_get_seeds(cvo_caret), 2)

    expect_is(cvo_get_sample_size(cvo_caret), "integer")
    expect_length(cvo_get_sample_size(cvo_caret), 1)
    expect_equal(cvo_get_sample_size(cvo_caret), n_rows)

    expect_equal(cvo_count_folds(cvo_caret), folds)

    expect_length(cvo_get_inds(cvo_caret, 1, "train"), n_rows - n_rows/folds)
    expect_length(cvo_get_inds(cvo_caret, 1, "test"),  n_rows/folds)


})

test_that("utility function vorks with `cvo_mlr` object", {

    folds <- 10
    n_rows <- nrow(fluorescence)
    set.seed(123456, "L'E")
    cvo_mlr <- cvo_create_folds(fluorescence,
                                folds = folds,
                                returnTrain = "both")
    # [!!!] option `returnTrain = "both"` is a temporary solution
    # and will be changed in the future

    expect_equal(cvo_count_folds(cvo_mlr), folds)

    expect_length(cvo_get_inds(cvo_mlr, 1, "train"), n_rows - n_rows/folds)
    expect_length(cvo_get_inds(cvo_mlr, 1, "test"),  n_rows/folds)

})
