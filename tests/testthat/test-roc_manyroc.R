context("roc_manyroc")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("works with vector input", {
  data(PlantGrowth)

  res <- roc_manyroc(PlantGrowth$weight,
    PlantGrowth$group)

  expect_s3_class(res, "data.frame")

  expect_equal(nrow(res), 3)
  expect_equal(ncol(res), 17)

  # Two factor levels:
  expect_s3_class(roc_manyroc(1:10, gl(2, 2, 10)), "data.frame")
  expect_s3_class(roc_manyroc(1:10, gl(2, 2, 10),
    optimize_by = "kappa"),
  "data.frame")
})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("works with data.frame input", {
  data(CO2)
  res <- roc_manyroc(CO2[, c("conc", "uptake")], CO2$Type)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(ncol(res), 17)
})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("works with hyperSpec input", {
  res <- roc_manyroc(fluorescence[, , 500 ~ 502],
    fluorescence$gr)
  expect_s3_class(res, "data.frame")
  expect_equal(ncol(res), 17)
})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("robust to incorrect inputinput", {
  x1 <-
    expect_error(roc_manyroc("a", "a"))
  expect_error(roc_manyroc(1, "a"))
  expect_error(roc_manyroc(1, 1))
  # Lengths differ:
  expect_error(roc_manyroc(1:10, gl(2, 2, 5)))
  # Too few factor levels:
  expect_error(roc_manyroc(1:10, gl(1, 2, 10)))
})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
