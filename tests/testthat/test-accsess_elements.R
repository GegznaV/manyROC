context("accsess_elements")

test_that("multiplication works", {
  set.seed(1)
  x_ <- rnorm(10)
  gr_ <- gl(n = 2, k = 5, length = 10, labels = c("H", "S"))
  res_list <- roc_analysis(x_, gr_)

  expect_identical(roc_get_all_results(res_list),
    res_list$all_results)

  expect_identical(roc_get(res_list, "ppv"),
    roc_get(res_list$all_results, "ppv"))



})
