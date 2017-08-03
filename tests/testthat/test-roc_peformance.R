context("roc_performance")

test_that("roc_performance works", {
    truth  <- gl(n = 2, k = 3, length = 20, labels = c("H","S"))
    prediction <- rev(truth)

    res_s <- roc_performance(truth, prediction, pos_label = "S")
    res_h <- roc_performance(truth, prediction, pos_label = "H")

    expect_equal(res_s$pos_label,  "S")
    expect_equal(res_h$pos_label,  "H")

    expect_equal(res_s$sens,  res_h$spec)
    expect_equal(res_s$kappa, res_h$kappa)

    expect_equal(round(res_s$kappa, 3), 0.394)

})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("roc_performance breaks", {
    truth  <- gl(n = 2, k = 3, length = 20, labels = c("H","S"))
    truth2 <- gl(n = 2, k = 3, length = 20, labels = c("d","S"))
    prediction <- rev(truth)

    res_s <- roc_performance(truth, prediction, pos_label = "S")
    res_h <- roc_performance(truth, prediction, pos_label = "H")


    expect_error(roc_performance(truth,
                                                prediction,
                                                pos_label = "d"))

    expect_error(roc_performance(truth, truth2))

    expect_error(
        expect_warning(
            roc_performance(truth, prediction[-2])
        )
    )

})
