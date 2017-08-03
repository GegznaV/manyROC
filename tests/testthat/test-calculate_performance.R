context("calculate_performance")

test_that("calculate_performance works", {
    truth  <- gl(n = 2, k = 3, length = 20, labels = c("H","S"))
    prediction <- rev(truth)

    res_s <- calculate_performance(truth, prediction, pos_label = "S")

    res_h <- calculate_performance(truth, prediction, pos_label = "H")

    expect_equivalent(attr(res_s, "labels")[1], "S") # $pos_label
    expect_equivalent(attr(res_s, "labels")[2], "H")

    expect_equal(as.data.frame(res_s)$sens,  as.data.frame(res_h)$spec)
    expect_equal(as.data.frame(res_s)$kappa, as.data.frame(res_h)$kappa)

    expect_equal(round(as.data.frame(res_s)$kappa, 3), 0.394)

})
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("calculate_performance breaks", {
    truth  <- gl(n = 2, k = 3, length = 20, labels = c("H","S"))
    truth2 <- gl(n = 2, k = 3, length = 20, labels = c("d","S"))
    prediction <- rev(truth)

    res_s <- calculate_performance(truth, prediction, pos_label = "S")
    res_h <- calculate_performance(truth, prediction, pos_label = "H")


    expect_error(calculate_performance(truth,
                                       prediction,
                                       pos_label = "d"))

    expect_error(calculate_performance(truth, truth2))

    expect_error(
        expect_warning(
            calculate_performance(truth, prediction[-2])
        )
    )

})
