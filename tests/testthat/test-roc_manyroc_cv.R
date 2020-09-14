context("roc_manyroc_cv")
# [!!!] more tests are needed that check if results are correct

test_that("roc_manyroc_cv works", {
  data(PlantGrowth)
  data(fluorescence)

  r1 <- roc_manyroc_cv(
    PlantGrowth$weight,
    PlantGrowth$group,
    seeds = 1234567
  )

  r2 <- roc_manyroc_cv(
    PlantGrowth$weight, gl(2, 1, 30),
    seeds = 1234567
  )

  r3 <- roc_manyroc_cv(
    fluorescence[[, , 500 ~ 502]],
    fluorescence$class,
    seeds = 1234567
  )

  expect_is(r1, "data.frame")
  expect_is(r2, "data.frame")
  expect_is(r3, "data.frame")
})




test_that("performance measure values are not constant per folds", {
  x <- read.table(text = "
a1_m a2_m a3_m a1_1 a2_1 a3_1
1  9.4  9.4  9.4  3.1  1.1  1.1
2  7.3  7.3  7.3  3.0  3.0  1.0
3  9.3  9.3  9.3  1.0  3.0  3.0
4  9.2  9.2  9.3  1.0  1.0  3.0
5  9.3  9.3  9.3  1.1  1.1  1.1
6  9.7  9.6  9.5  1.3  1.3  1.2
7  9.7  9.6  9.5  1.3  1.3  1.2
8  9.7  9.6  9.6  1.3  1.3  1.3
9  9.7  9.6  9.5  1.3  1.3  1.2
10 9.7  9.6  9.5  2.3  2.2  2.2
11 8.7  8.6  8.5  2.3  2.3  2.2
12 8.8  8.7  8.6  2.4  2.3  2.3
13 8.9  8.8  8.7  2.4  2.3  2.3
14 8.8  8.7  8.7  2.3  2.3  2.2
15 8.8  8.7  8.6  2.3  2.3  2.2
16 8.8  8.7  8.6  0.3  2.3  2.2
17 8.0  8.9  8.7  0.4  0.4  2.3
18 8.0  8.9  8.7  2.5  0.4  2.3
19 8.0  8.9  8.8  2.4  2.4  0.3
20 8.9  8.8  8.7  2.4  2.4  0.3 ")


  range_ <- function(x) diff(range(x))
  rezz <- roc_manyroc_cv(x, gl(2, 1, 20, labels = c("A", "B")), seeds = 1)

  # The values of performance measures ("youden", "kappa", "bac") must not be
  # constant per folds. Constant values equal to chance probabbility (e.g.,
  # kappa = 0) indicate error (incorrect cut-off and mixed positions of features
  # in test sets).
  expect_true(
    rezz %>%
      dplyr::group_by(set, compared_groups, feature)  %>%
      dplyr::filter(set == "test")  %>%
      dplyr::summarize_at(
        dplyr::vars("youden", "kappa", "bac"),
        list(range_)
      )  %>%
      dplyr::ungroup()  %>%
      dplyr::select("youden", "kappa", "bac")  %>%
      dplyr::mutate_all(list(~ . > 0))  %>%
      as.matrix()  %>%
      all()
  )

  expect_true(
    rezz %>%
      dplyr::group_by(set, compared_groups, feature)  %>%
      dplyr::filter(set == "training")  %>%
      dplyr::summarize_at(
        dplyr::vars("youden", "kappa", "bac"),
        list(range_)
      )  %>%
      dplyr::ungroup()  %>%
      dplyr::select("youden", "kappa", "bac")  %>%
      dplyr::mutate_all(list(~. > 0))  %>%
      as.matrix()  %>%
      all()
  )
})
