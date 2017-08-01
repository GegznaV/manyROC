# library(testthat)

context("Function `cvo_create_folds()` ")

test_that("`data`, `stratify_by` and `block_by` parameters of `cvo_create_folds()` works", {
    # Load data
     # data("DataSet1", package = "spHelper")
     #
    DF1 <- data.frame(ID = rep(1:20, each = 2),
                      gr = gl(4, 10, labels = LETTERS[1:4]),
                      .row = 1:40)


    # If variable names and a data frame are provided:
     set.seed(123456, "L'Ecuyer-CMRG")
     Folds1_a <- cvo_create_folds(data = DF1,
                                  stratify_by = "gr",
                                  block_by    = "ID",
                                  returnTrain = FALSE)

    # If vectors are provided:
     set.seed(123456, "L'Ecuyer-CMRG")
     Folds1_b <- cvo_create_folds(stratify_by = DF1$gr,
                                  block_by    = DF1$ID,
                                  returnTrain = FALSE)

     # If mixed imput
     set.seed(123456, "L'Ecuyer-CMRG")
     Folds1_x <- cvo_create_folds(data = DF1,
                                  stratify_by = "gr",
                                  block_by    = DF1$ID,
                                  returnTrain = FALSE)

     set.seed(123456, "L'Ecuyer-CMRG")
     Folds1_y <- cvo_create_folds(data = DF1,
                                  stratify_by = DF1$gr,
                                  block_by    = "ID",
                                  returnTrain = FALSE)


     expect_identical(Folds1_a, Folds1_b)
     expect_identical(Folds1_a, Folds1_x)
     expect_identical(Folds1_a, Folds1_y)


     expect_is(Folds1_a, "cvo")
     expect_length(Folds1_a, 5)

     expect_is(Folds1_b, "cvo")
     expect_length(Folds1_b, 5)


     # -----------------------------------------------
     # If variable names and a data frame are provided:
     set.seed(123456, "L'Ecuyer-CMRG")
     Folds1_a1 <- cvo_create_folds(data = DF1,
                                   stratify_by = "gr",
                                   block_by    = "ID",
                                   returnTrain = TRUE)

     # If vectors are provided:
     set.seed(123456, "L'Ecuyer-CMRG")
     Folds1_b1 <- cvo_create_folds(stratify_by = DF1$gr,
                                   block_by    = DF1$ID,
                                   returnTrain = TRUE)

     # If mixed imput
     set.seed(123456, "L'Ecuyer-CMRG")
     Folds1_x1 <- cvo_create_folds(       data = DF1,
                                   stratify_by = "gr",
                                   block_by    = DF1$ID,
                                   returnTrain = TRUE)

     set.seed(123456, "L'Ecuyer-CMRG")
     Folds1_y1 <- cvo_create_folds(data = DF1,
                                   stratify_by = DF1$gr,
                                   block_by = "ID",
                                   returnTrain = TRUE)


     expect_identical(Folds1_a1, Folds1_b1)
     expect_identical(Folds1_a1, Folds1_x1)
     expect_identical(Folds1_a1, Folds1_y1)


     expect_is(Folds1_a1, "cvo")
     expect_length(Folds1_a1, 5)

     expect_is(Folds1_b1, "cvo")
     expect_length(Folds1_b1, 5)

})

test_that("Blocking in `cvo_create_folds()` works", {
    # Load data
    # data("DataSet1", package = "spHelper")
    DF1 <- data.frame(ID = rep(1:20, each = 2),
                      gr = gl(4, 10, labels = LETTERS[1:4]),
                      .row = 1:40)

    # If variable names and a data frame are provided:
    set.seed(123456, "L'Ecuyer-CMRG")
    Folds1_a <- cvo_create_folds(data = DF1,
                                 stratify_by = "gr",
                                 block_by = "ID",
                                 returnTrain = FALSE)

    # If vectors are provided:
     set.seed(123456, "L'Ecuyer-CMRG")
     Folds1_b <- cvo_create_folds(stratify_by = DF1$gr,
                                  block_by = DF1$ID,
                                  returnTrain = FALSE)

     # Not blocked but stratified
     Folds1_c <- cvo_create_folds(stratify_by = DF1$gr,
                                  returnTrain = FALSE)

     # Blocked but not stratified
     Folds1_d <- cvo_create_folds(block_by = DF1$ID, returnTrain = FALSE)


     expect_true(attributes(Folds1_a)$info$blocked)
     expect_true(attributes(Folds1_b)$info$blocked)
     expect_false(attributes(Folds1_c)$info$blocked)
     expect_true(attributes(Folds1_d)$info$blocked)


})

test_that("Stratification in `cvo_create_folds()` works", {
# Load data
    # data("DataSet1", package = "spHelper")
    DF1 <- data.frame(ID = rep(1:20, each = 2),
                      gr = gl(4, 10, labels = LETTERS[1:4]),
                      .row = 1:40)


# If variable names and a data frame are provided:
    set.seed(123456, "L'Ecuyer-CMRG")
    Folds1_a <- cvo_create_folds(data = DF1,
                                 stratify_by = "gr",
                                 block_by = "ID",
                                 returnTrain = FALSE)

    # If vectors are provided:
     set.seed(123456, "L'Ecuyer-CMRG")
     Folds1_b <- cvo_create_folds(stratify_by = DF1$gr,
                                  block_by = DF1$ID,
                                  returnTrain = FALSE)

     # Not blocked but stratified
     Folds1_c <- cvo_create_folds(stratify_by = DF1$gr,
                                  returnTrain = FALSE)

     # Blocked but not stratified
     Folds1_d <- cvo_create_folds(block_by = DF1$ID, returnTrain = FALSE)


     expect_true(attributes(Folds1_a)$info$stratified)
     expect_true(attributes(Folds1_b)$info$stratified)
     expect_true(attributes(Folds1_c)$info$stratified)
     expect_false(attributes(Folds1_d)$info$stratified)

})

test_that("`cvo_create_folds()` returns error if one of the groups is too small", {

    # Too little cases in each group
    expect_error(cvo_create_folds(stratify_by = letters[1:6], k = 5))
    # No error expected
    expect_length(cvo_create_folds(stratify_by = letters[rep(1:3,5)], k = 5),5)

})

test_that("parameter `k` in `cvo_create_folds()` works.", {

    # k is too small
    expect_error(cvo_create_folds(block_by = 1:25, k = 1))

    # No error expected
    expect_length(cvo_create_folds(block_by = 1:25, k = 2),2)
    expect_length(cvo_create_folds(block_by = 1:25, k = 5),5)
    expect_length(cvo_create_folds(block_by = 1:25, k = 10),10)
    expect_length(cvo_create_folds(block_by = 1:25, k = 25),25)
})

test_that("parameter `returnTrain` in `cvo_create_folds()` divides to training/test folds as expected", {

    # Test if divides to training/test folds as expected
    set.seed(123456, "L'Ecuyer-CMRG")
    TEST_2F  <- cvo_create_folds(block_by = 1:25, k = 2, returnTrain = FALSE)

    set.seed(123456, "L'Ecuyer-CMRG")
    TRAIN_2F <- cvo_create_folds(block_by = 1:25, k = 2, returnTrain = TRUE)

    expect_identical(TEST_2F[[1]], TRAIN_2F[[2]])

})

test_that("The size of training set is greater than the size of test set.", {

    N = 25
    # Training folds must be greater in size than test folds
    set.seed(123456, "L'Ecuyer-CMRG")
    TEST_3F  <- cvo_create_folds(block_by = 1:N, k = 3, returnTrain = FALSE)

    set.seed(123456, "L'Ecuyer-CMRG")
    TRAIN_3F <- cvo_create_folds(block_by = 1:N, k = 3, returnTrain = TRUE)

    expect_gt(length(TRAIN_3F[[1]]), length(TEST_3F[[1]]))
    expect_gt(length(TRAIN_3F[[2]]), length(TEST_3F[[2]]))
    expect_gt(length(TRAIN_3F[[3]]), length(TEST_3F[[3]]))
})

test_that("The size of training set in 3-fold CV is approximately 2/3 * N.", {

    N = 25

    set.seed(123456, "L'Ecuyer-CMRG")
    TRAIN_3F <- cvo_create_folds(block_by = 1:N, k = 3, returnTrain = TRUE)

    # Test if the size of training set is approximately 2/3*N
    expect_gte(length(TRAIN_3F[[1]]), 16)
    expect_gte(length(TRAIN_3F[[2]]), 16)
    expect_gte(length(TRAIN_3F[[3]]), 16)
})