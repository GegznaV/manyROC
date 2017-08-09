context("sp_manyroc_with_cv_by_variable")
# [!!!] A test to check if calculations are correct is needed.

test_that("sp_manyroc_with_cv_by_variable works", {
    fluorescence$ID  <- 1:nrow(fluorescence)
    k_folds = 3
    times = 2
    rez <- sp_manyroc_with_cv_by_variable(fluorescence[,,500~501],
                                          c("gr", "class"),
                                          k_folds = k_folds,
                                          times = times)
    expect_length(rez, 7)
    expect_equal(rez$variable, NULL)
    expect_equal(rez$n_included, data.frame(gr = 150, class = 150))
    expect_true(all(purrr::map_lgl(rez$ind_included_rows,
                                   ~inherits(.x, "logical"))
                    ))
    expect_equal(attributes(rez$cvo[[1]])$info$k, k_folds)
    expect_equal(attributes(rez$cvo[[1]])$info$repetitions, times)

    expect_is(rez$results, "data.frame")

    expect_equal(rez$variables_errored, character(0))

    expect_length(rez$variables_included, 2)

    expect_equal(rez$error_messages[[1]], NULL)
    expect_equal(rez$error_messages[[2]], NULL)

})
