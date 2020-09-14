context("roc_analysis")

test_that("roc_analysis() works", {
  set.seed(1)
  x_ <- rnorm(10)
  gr_ <- gl(n = 2, k = 5, length = 10, labels = c("H", "S"))
  res_list <- roc_analysis(x_, gr_)
  expect_s3_class(res_list, "list")

  res_list2 <- roc_analysis(x_, gr_, optimize_by = "kappa")
  expect_s3_class(res_list2, "list")

  res_list3 <- roc_analysis(x_, gr_, optimize_by = "youden")
  expect_s3_class(res_list3, "list")

  res_list4 <- roc_analysis(x_, gr_, pos_is_larger = TRUE)
  expect_s3_class(res_list4, "list")

  res_list5 <- roc_analysis(x_, gr_, optimize_by = NULL)
  expect_s3_class(res_list5, "list")

  res_matrix <- roc_analysis(x_, gr_, results = "optimal")
  expect_s3_class(res_matrix, "matrix")

  x_2 <- rep(c(1, 2), each = 50)
  gr_2 <- gl(n = 2, k = 1, length = 100, labels = c("H", "S"))
  res_list5 <- roc_analysis(x_2, gr_2)
  expect_s3_class(res_list5, "list")

  # Lengths of both inputs must agree
  expect_warning(expect_error(roc_analysis(x_2, gr_2[-1])))

})


test_that("roc_analysis() gives correct marginal results (1)", {
  set.seed(1)
  x_1  <- rep(1:2, times = 50)
  gr_  <- gl(n = 2, k = 1, length = 100, labels = c("A", "B"))
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  calculated_res1 <- roc_analysis(x_1, gr_)$optimal %>% as.data.frame()
  expected_res1   <- data.table::fread(data.table = FALSE,
    "cutoff tp fn fp tn sens spec ppv npv bac youden kappa auc median_neg median_pos
    1.5 50  0  0 50    1    1   1   1   1      1     1   1          1          2")
  expect_equivalent(calculated_res1, expected_res1)
})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("roc_analysis() gives correct marginal results (2)", {
  set.seed(1)
  x_2  <- rep(1:2, each = 50)
  gr_  <- gl(n = 2, k = 1, length = 100, labels = c("A", "B"))

  calculated_res2 <- roc_analysis(x_2, gr_)$optimal %>% as.data.frame()

  expected_res2 <- data.table::fread(data.table = FALSE,
    "cutoff tp fn fp tn sens spec ppv npv bac youden kappa auc median_neg median_pos
    0.5 25 25 25 25  0.5  0.5 0.5 0.5 0.5      0     0 0.5        1.5        1.5")
  expect_equivalent(calculated_res2, expected_res2)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("roc_analysis() gives correct marginal results (3)", {
  set.seed(1)
  x_34 <- rep(1,   each = 100)
  gr_  <- gl(n = 2, k = 1, length = 100, labels = c("A", "B"))

  calculated_res3 <-
    roc_analysis(x_34, gr_, pos_is_larger = TRUE)$optimal %>% as.data.frame()

  expected_res3 <- data.table::fread(data.table = FALSE,
    "cutoff tp fn fp tn sens spec ppv npv bac youden kappa auc median_neg median_pos
    Inf  0 50  0 50    0    1 NaN 0.5 0.5      0     0 0.5          1          1")
  expect_equivalent(calculated_res3, expected_res3)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("roc_analysis() gives correct marginal results (4)", {
  set.seed(1)
  x_34 <- rep(1,   each = 100)
  gr_  <- gl(n = 2, k = 1, length = 100, labels = c("A", "B"))
  calculated_res4 <-
    roc_analysis(x_34, gr_, pos_is_larger = FALSE)$optimal %>% as.data.frame()

  expected_res4 <- data.table::fread(data.table = FALSE,
    "cutoff tp fn fp tn sens spec ppv npv bac youden kappa auc median_neg median_pos
   -Inf  0 50  0 50    0    1 NaN 0.5 0.5      0     0 0.5          1          1")
  expect_equivalent(calculated_res4, expected_res4)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
})
