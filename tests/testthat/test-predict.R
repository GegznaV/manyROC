context("predict")

test_that("roc_predict.roc_result_list works", {
    set.seed(1)
    x <- c(-1, -1, -1, -1, -2, 3, 4, 4, 4, 4)
    gr <- gl(n = 2, k = 5, length = 10, labels = c("H","S"))
    object <- roc_analysis(x, gr)

    expect_equal(roc_predict(object, x_new = -1), "H")
    expect_equal(roc_predict(object, x_new = 4),  "S")

    expect_true(is.vector(roc_predict(object, x_new = rnorm(20))))

    expect_length(roc_predict(object, x_new = rnorm(20)), 20)

})

test_that("roc_predict.roc_info works", {
    set.seed(1)
    x <- c(-1, -1, -1, -1, -2, 3, 4, 4, 4, 4)
    gr <- gl(n = 2, k = 5, length = 10, labels = c("H","S"))
    object <- roc_analysis(x, gr)

    expect_equal(roc_predict(object$info, x_new = -1), "H")
    expect_equal(roc_predict(object$info, x_new = 4),  "S")

})