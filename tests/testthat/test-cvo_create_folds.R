# library(testthat)

context("cvo_create_folds")

test_that("`data`, `stratify_by` and `block_by` parameters of `cvo_create_folds()` works", {
  # Load data

  DF1 <- data.frame(ID = rep(1:20, each = 2),
    gr = gl(4, 10, labels = LETTERS[1:4]),
    .row = 1:40)

  folds <- 5

  # If variable names and a data frame are provided:
  set.seed(123456, "L'Ecuyer-CMRG")
  Folds1_a <- cvo_create_folds(data = DF1,
    stratify_by = "gr",
    block_by    = "ID",
    folds = folds,
    returnTrain = FALSE, seeds = 123456)

  # If vectors are provided:
  set.seed(123456, "L'Ecuyer-CMRG")
  Folds1_b <- cvo_create_folds(stratify_by = DF1$gr,
    block_by    = DF1$ID,
    folds = folds,
    returnTrain = FALSE, seeds = 123456)

  # If mixed imput
  set.seed(123456, "L'Ecuyer-CMRG")
  Folds1_x <- cvo_create_folds(data = DF1,
    stratify_by = "gr",
    block_by    = DF1$ID,
    folds = folds,
    returnTrain = FALSE, seeds = 123456)

  set.seed(123456, "L'Ecuyer-CMRG")
  Folds1_y <- cvo_create_folds(data = DF1,
    stratify_by = DF1$gr,
    block_by    = "ID",
    folds = folds,
    returnTrain = FALSE, seeds = 123456)


  expect_identical(Folds1_a, Folds1_b)
  expect_identical(Folds1_a, Folds1_x)
  expect_identical(Folds1_a, Folds1_y)


  expect_is(Folds1_a, "cvo")
  expect_length(Folds1_a, folds)

  expect_is(Folds1_b, "cvo")
  expect_length(Folds1_b, folds)


  # -----------------------------------------------
  # If variable names and a data frame are provided:
  set.seed(123456, "L'Ecuyer-CMRG")
  Folds1_a1 <- cvo_create_folds(data = DF1,
    stratify_by = "gr",
    block_by    = "ID",
    folds = folds,
    returnTrain = TRUE, seeds = 123456)

  # If vectors are provided:
  set.seed(123456, "L'Ecuyer-CMRG")
  Folds1_b1 <- cvo_create_folds(stratify_by = DF1$gr,
    block_by    = DF1$ID,
    folds = folds,
    returnTrain = TRUE, seeds = 123456)

  # If mixed imput
  set.seed(123456, "L'Ecuyer-CMRG")
  Folds1_x1 <- cvo_create_folds(data = DF1,
    stratify_by = "gr",
    block_by    = DF1$ID,
    folds = folds,
    returnTrain = TRUE, seeds = 123456)

  set.seed(123456, "L'Ecuyer-CMRG")
  Folds1_y1 <- cvo_create_folds(data = DF1,
    stratify_by = DF1$gr,
    block_by = "ID",
    folds = folds,
    returnTrain = TRUE, seeds = 123456)


  expect_identical(Folds1_a1, Folds1_b1)
  expect_identical(Folds1_a1, Folds1_x1)
  expect_identical(Folds1_a1, Folds1_y1)


  expect_is(Folds1_a1, "cvo")
  expect_length(Folds1_a1, folds)

  expect_is(Folds1_b1, "cvo")
  expect_length(Folds1_b1, folds)

})

test_that("`cvo_create_folds()` returns error if one of the groups is too small", {

  # Too little cases in each group
  expect_error(capture_output(
    cvo_create_folds(stratify_by = letters[1:6], k = 5, seeds = 123456)
  ))
  # No error expected
  expect_length(cvo_create_folds(stratify_by = letters[rep(1:3, 5)],
    k = 5,
    seeds = 123456),
  5)

})

test_that("parameter `k` in `cvo_create_folds()` works.", {

  # k is too small
  expect_error(cvo_create_folds(block_by = 1:25, k = 1, seeds = 123456))

  # No error expected
  expect_length(cvo_create_folds(block_by = 1:25, k = 2, seeds = 123456), 2)
  expect_length(cvo_create_folds(block_by = 1:25, k = 5, seeds = 123456), 5)
  expect_length(cvo_create_folds(block_by = 1:25, k = 10, seeds = 123456), 10)
  expect_length(cvo_create_folds(block_by = 1:25, k = 25, seeds = 123456), 25)
})

test_that("parameter `returnTrain` in `cvo_create_folds()` divides to training/test folds as expected", {

  # Test if divides to training/test folds as expected
  set.seed(123456, "L'Ecuyer-CMRG")
  TEST_2F  <- cvo_create_folds(block_by = 1:25,
    k = 2,
    returnTrain = FALSE,
    seeds = 123456)

  set.seed(123456, "L'Ecuyer-CMRG")
  TRAIN_2F <-
    cvo_create_folds(
      block_by = 1:25,
      k = 2,
      returnTrain = TRUE,
      seeds = 123456
    )

  expect_identical(TEST_2F[[1]], TRAIN_2F[[2]])

})

test_that("The size of training set is greater than the size of test set.", {

  N <- 25
  # Training folds must be greater in size than test folds
  set.seed(123456, "L'Ecuyer-CMRG")
  TEST_3F  <- cvo_create_folds(block_by = 1:N, k = 3, returnTrain = FALSE, seeds = 123456)

  set.seed(123456, "L'Ecuyer-CMRG")
  TRAIN_3F <- cvo_create_folds(block_by = 1:N, k = 3, returnTrain = TRUE, seeds = 123456)

  expect_gt(length(TRAIN_3F[[1]]), length(TEST_3F[[1]]))
  expect_gt(length(TRAIN_3F[[2]]), length(TEST_3F[[2]]))
  expect_gt(length(TRAIN_3F[[3]]), length(TEST_3F[[3]]))
})

test_that("The size of training set in 3-fold CV is approximately 2/3 * N.", {

  N <- 25

  set.seed(123456, "L'Ecuyer-CMRG")
  TRAIN_3F <- cvo_create_folds(block_by = 1:N, k = 3, returnTrain = TRUE, seeds = 123456)

  # Test if the size of training set is approximately 2/3*N
  expect_gte(length(TRAIN_3F[[1]]), 16)
  expect_gte(length(TRAIN_3F[[2]]), 16)
  expect_gte(length(TRAIN_3F[[3]]), 16)
})


# =============================================================================
context("blocking in cvo_create_folds")

test_that("Blocking in `cvo_create_folds()` works", {
  # Load data

  DF1 <- data.frame(ID = rep(1:20, each = 2),
    gr = gl(4, 10, labels = LETTERS[1:4]),
    .row = 1:40)

  # If variable names and a data frame are provided:
  set.seed(123456, "L'Ecuyer-CMRG")
  Folds1_a <- cvo_create_folds(data = DF1,
    stratify_by = "gr",
    block_by = "ID",
    folds = 5,
    returnTrain = FALSE, seeds = 123456)

  # If vectors are provided:
  set.seed(123456, "L'Ecuyer-CMRG")
  Folds1_b <- cvo_create_folds(stratify_by = DF1$gr,
    block_by = DF1$ID,
    folds = 5,
    returnTrain = FALSE, seeds = 123456)

  # Not blocked but stratified
  Folds1_c <- cvo_create_folds(stratify_by = DF1$gr,
    folds = 5,
    returnTrain = FALSE, seeds = 123456)

  # Blocked but not stratified
  Folds1_d <- cvo_create_folds(block_by = DF1$ID,
    folds = 5,
    # seeds = 123456
    returnTrain = FALSE)


  expect_true(attributes(Folds1_a)$info$blocked)
  expect_true(attributes(Folds1_b)$info$blocked)
  expect_false(attributes(Folds1_c)$info$blocked)
  expect_true(attributes(Folds1_d)$info$blocked)


})

# =============================================================================
context("Stratification in cvo_create_folds")

test_that("Stratification in `cvo_create_folds()` works", {
  # Load data

  DF1 <- data.frame(ID = rep(1:20, each = 2),
    gr = gl(4, 10, labels = LETTERS[1:4]),
    .row = 1:40)


  # If variable names and a data frame are provided:
  set.seed(123456, "L'Ecuyer-CMRG")
  Folds1_a <- cvo_create_folds(data = DF1,
    stratify_by = "gr",
    block_by = "ID",
    returnTrain = FALSE, seeds = 123456)

  # If vectors are provided:
  set.seed(123456, "L'Ecuyer-CMRG")
  Folds1_b <- cvo_create_folds(stratify_by = DF1$gr,
    block_by = DF1$ID,
    returnTrain = FALSE, seeds = 123456)

  # Not blocked but stratified
  Folds1_c <- cvo_create_folds(stratify_by = DF1$gr,
    returnTrain = FALSE, seeds = 123456)

  # Blocked but not stratified
  Folds1_d <- cvo_create_folds(block_by = DF1$ID,
    returnTrain = FALSE, seeds = 123456)


  expect_true(attributes(Folds1_a)$info$stratified)
  expect_true(attributes(Folds1_b)$info$stratified)
  expect_true(attributes(Folds1_c)$info$stratified)
  expect_false(attributes(Folds1_d)$info$stratified)

})
