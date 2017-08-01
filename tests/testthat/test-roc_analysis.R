context("roc_analysis")

test_that("roc_analysis() works", {
    set.seed(1)
    x_ <- rnorm(10)
    gr_ <- gl(n = 2, k = 5, length = 10, labels = c("H","S"))
    res_list <- roc_analysis(x_, gr_)
    expect_s3_class(res_list, "list")

    res_list2 <- roc_analysis(x_, gr_, optimize_by = "kappa")
    expect_s3_class(res_list2, "list")

    res_list3 <- roc_analysis(x_, gr_, optimize_by = "youden")
    expect_s3_class(res_list3, "list")

    res_list4 <- roc_analysis(x_, gr_, pos_is_larger = TRUE)
    expect_s3_class(res_list4, "list")

    res_matrix <- roc_analysis(x_, gr_, results = "optimal")
    expect_s3_class(res_matrix, "matrix")


    x_2 <- rep(c(1,2), each = 50)
    gr_2 <- gl(n = 2, k = 1, length = 100, labels = c("H","S"))
    res_list5 <- roc_analysis(x_2, gr_2)
    expect_s3_class(res_list5, "list")
})
