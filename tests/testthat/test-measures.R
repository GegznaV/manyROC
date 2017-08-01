context("measures")

test_that("kappa is calculated correctly", {
    # -----------------------------------------------------------------------------
    # kappa equals to 0
         truth1 <- rep(1:2, times = 50)
    prediction1 <- rep(1:2, each = 50)

    k_0_a <- measure_kappa(truth1, prediction1)
    k_0_b <- measure_kappa(conf_mat = table(truth1, prediction1))

    expect_equal(k_0_a, k_0_b)
    expect_equal(k_0_a, 0)
    expect_equal(k_0_b, 0)
    # -----------------------------------------------------------------------------
    # kappa equals to 1
         truth2 <- rep(1:3, times = 50)

    k_1_a <- measure_kappa(truth2, truth2)
    k_1_b <- measure_kappa(conf_mat = table(truth2, truth2))

    expect_equal(k_1_a, k_1_b)
    expect_equal(k_1_a, 1)
    expect_equal(k_1_b, 1)
    # -----------------------------------------------------------------------------
    # kappa equals to -1
    truth3      <- rep(1:2,   times = 50)
    prediction3 <- rep(c(2,1), times = 50)

    k_neg1_a <- measure_kappa(truth3, prediction3)
    k_neg1_b <- measure_kappa(conf_mat = table(truth3, prediction3))

    expect_equal(k_neg1_a, k_neg1_b)
    expect_equal(k_neg1_a, -1)
    expect_equal(k_neg1_b, -1)
    # -----------------------------------------------------------------------------
    # kappa equals to -0.02
    truth4      <- rep(1:3, times = 50)
    prediction4 <- rep(3:1, each = 50)

    k_neg002_a <- measure_kappa(truth4, prediction4)
    k_neg002_b <- measure_kappa(conf_mat = table(truth4, prediction4))

    expect_equal(k_neg002_a, k_neg002_b)
    expect_equal(k_neg002_a, -0.02)
    expect_equal(k_neg002_b, -0.02)
    # -----------------------------------------------------------------------------
    # kappa equals to 0.4
    mat1 <- matrix(c(20,10,5,15), nrow = 2)
    k_04_b <- measure_kappa(conf_mat = mat1)
    expect_equal(k_04_b, 0.4)
    # kappa equals to 0.1304
    mat2 <- matrix(c(45,25,15,15), nrow = 2)
    k_013_b <- measure_kappa(conf_mat = mat2)
    expect_equal(round(k_013_b,4), 0.1304)
    # -----------------------------------------------------------------------------



})

# =============================================================================

test_that("kappa is robust to input errors", {
    # Different vector lengths
    truth10 <- rep(1:3, times = 50)
    prediction10 <- rep(1:2, each = 50)

    expect_error(measure_kappa(truth10, prediction10))
    # -----------------------------------------------------------------------------
    # Different number of factor levels
    truth11 <-      rep(1:3, length = 100)
    prediction11 <- rep(1:2, each = 50)

    expect_error(measure_kappa(truth11, prediction11))
    expect_error(measure_kappa(conf_mat = table(truth11, prediction11)))
    # -----------------------------------------------------------------------------

   # [!!!] missing values must not be accepted, but they are

})



# =============================================================================

test_that("wwkappa is calculated correctly", {
    # -----------------------------------------------------------------------------
    # wkappa equals to 0
    truth1 <- rep(1:2, times = 50)
    prediction1 <- rep(1:2, each = 50)

    k_0_a <- measure_wkappa(truth1, prediction1)
    k_0_b <- measure_wkappa(conf_mat = table(truth1, prediction1))

    expect_equal(k_0_a, k_0_b)
    expect_equal(k_0_a, 0)
    expect_equal(k_0_b, 0)
    # -----------------------------------------------------------------------------
    # wkappa equals to 1
    truth2 <- rep(1:3, times = 50)

    k_1_a <- measure_wkappa(truth2, truth2)
    k_1_b <- measure_wkappa(conf_mat = table(truth2, truth2))

    expect_equal(k_1_a, k_1_b)
    expect_equal(k_1_a, 1)
    expect_equal(k_1_b, 1)
    # -----------------------------------------------------------------------------
    # wkappa equals to -1
    truth3      <- rep(1:2,   times = 50)
    prediction3 <- rep(c(2,1), times = 50)

    k_neg1_a <- measure_wkappa(truth3, prediction3)
    k_neg1_b <- measure_wkappa(conf_mat = table(truth3, prediction3))

    expect_equal(k_neg1_a, k_neg1_b)
    expect_equal(k_neg1_a, -1)
    expect_equal(k_neg1_b, -1)
    # -----------------------------------------------------------------------------
    # wkappa equals to -0.02
    truth4      <- rep(1:3, times = 50)
    prediction4 <- rep(3:1, each = 50)

    k_neg002_a <- measure_wkappa(truth4, prediction4)
    k_neg002_b <- measure_wkappa(conf_mat = table(truth4, prediction4))

    expect_equal(k_neg002_a, k_neg002_b)
    expect_equal(k_neg002_a, -0.02)
    expect_equal(k_neg002_b, -0.02)
    # -----------------------------------------------------------------------------
    # wkappa equals to 0.4
    mat1 <- matrix(c(20,10,5,15), nrow = 2)
    k_04_b <- measure_wkappa(conf_mat = mat1)
    expect_equal(k_04_b, 0.4)
    # wkappa equals to 0.1304
    mat2 <- matrix(c(45,25,15,15), nrow = 2)
    k_013_b <- measure_wkappa(conf_mat = mat2)
    expect_equal(round(k_013_b,4), 0.1304)
    # -----------------------------------------------------------------------------

})

# =============================================================================

test_that("wkappa is robust to input errors", {
    # Different vector lengths
    truth10 <- rep(1:3, times = 50)
    prediction10 <- rep(1:2, each = 50)

    expect_error(measure_wkappa(truth10, prediction10))
    # -----------------------------------------------------------------------------
    # Different number of factor levels
    truth11 <-      rep(1:3, length = 100)
    prediction11 <- rep(1:2, each = 50)

    expect_error(measure_wkappa(truth11, prediction11))
    expect_error(measure_wkappa(conf_mat = table(truth11, prediction11)))
    # -----------------------------------------------------------------------------

    # [!!!] missing values must not be accepted, but they are

})
