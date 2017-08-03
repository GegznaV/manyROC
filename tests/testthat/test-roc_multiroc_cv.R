context("roc_multiroc_cv")

test_that("roc_multiroc_cv works", {
    data(PlantGrowth)
    r1 <- roc_multiroc_cv(PlantGrowth$weight,
                          PlantGrowth$group,
                          seeds = 1234567)

    r2 <- roc_multiroc_cv(PlantGrowth$weight, gl(2, 1, 30), seeds = 1234567)

    r3 <- roc_multiroc_cv(fluorescence[[,,500~502]],
                          fluorescence$class,
                          seeds = 1234567)



    expect_is(r1, "data.frame")
    expect_is(r2, "data.frame")
    expect_is(r3, "data.frame")
})
