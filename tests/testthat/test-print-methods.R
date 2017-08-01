context("print-methods")

test_that("print methos do not throw error", {
    set.seed(1)
    x_ <- rnorm(10)
    gr_ <- gl(n = 2, k = 5, length = 10)
    res_list <- roc_analysis(x_, gr_)

   expect_is(capture_output(print(res_list)), "character")

})
