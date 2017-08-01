context("print-methods")

test_that("print methos do not throw error", {
    set.seed(1)
    x_ <- rnorm(10)
    gr_ <- gl(n = 2, k = 5, length = 10)
    res_list <- roc_analysis(x_, gr_)

    out_1 <- capture_output(print(res_list))
    expect_is(out_1, "character")

    data(fluorescence)
    out_2 <- capture_output(roc_multiroc(fluorescence[ , , 500~502],
                                         fluorescence$gr))
    expect_is(out_2, "character")
})
