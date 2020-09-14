context("cvo_test_bs")

test_that("cvo_test_bs does not throw error", {

  DataSet1 <- data.frame(
    ID = rep(1:20, each = 2),
    gr = gl(4, 10, labels = LETTERS[1:4]),
    .row = 1:40
  )

  obj <- cvo_create_folds(
    data = DataSet1,
    stratify_by = "gr",
    block_by = "ID",
    returnTrain = FALSE
  )

  expect_is(
    capture.output(cvo_test_bs(obj,
      stratify_by = "gr",
      block_by = "ID",
      data = DataSet1)),
    "character"
  )

  # May throw error in future releases [!!!]
  expect_is(
    capture.output(cvo_test_bs(obj, "gr", "class", data = fluorescence)),
    "character"
  )

})


test_that("cvo_test_bs throw error", {

  DataSet1 <- data.frame(
    ID = rep(1:20, each = 2),
    gr = gl(4, 10, labels = LETTERS[1:4]),
    .row = 1:40
  )

  obj <- cvo_create_folds(
    data = DataSet1,
    stratify_by = "gr",
    block_by = "ID",
    returnTrain = FALSE
  )

  expect_error(cvo_test_bs(obj))
})
