context("calculate_two_class_perormance")

test_that("calculate_two_class_perormance works", {
    truth  <- gl(n = 2, k = 3, length = 20, labels = c("H","S"))
    truth2 <- gl(n = 2, k = 3, length = 20, labels = c("d","S"))
    prediction <- rev(truth)

    res_s <- calculate_two_class_perormance(truth, prediction, pos_label = "S")
    res_h <- calculate_two_class_perormance(truth, prediction, pos_label = "H")

    expect_equal(res_s$pos_label,  "S")
    expect_equal(res_h$pos_label,  "H")

    expect_equal(res_s$sens,  res_h$spec)
    expect_equal(res_s$kappa, res_h$kappa)

    expect_equal(round(res_s$kappa, 3), 0.394)

    expect_error(calculate_two_class_perormance(truth,
                                                prediction,
                                                pos_label = "d"))

    expect_error(calculate_two_class_perormance(truth, truth2))

    expect_error(
        expect_warning(
            calculate_two_class_perormance(truth, prediction[-2])
        )
    )

})
