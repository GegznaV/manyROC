context("sp_manyroc_with_cv")
# [!!!] A test to check if calculations are correct is needed.

test_that("sp_manyroc_with_cv works", {
  k_folds <- 3
  times <- 2
  fluorescence$ID  <- 1:nrow(fluorescence)

  rez <- sp_manyroc_with_cv(
    "gr",
    fluorescence[, , 500 ~ 502],
    k_folds = k_folds,
    times = times
  )

  expect_length(rez, 5)
  expect_equal(rez$variable, "gr")
  expect_equal(rez$n_included, 150)
  expect_is(rez$ind_included_rows, "logical")
  expect_equal(attributes(rez$cvo)$info$k, k_folds)
  expect_equal(attributes(rez$cvo)$info$repetitions, times)
  expect_is(rez$results, "data.frame")

})
