context("class_label_management")

test_that("class_add() and class_remove() work", {

  # --- test class_add() ---
  # Adds class
  new_obj <- list() %>% class_add(new_class = "n")
  expect_identical(class(new_obj), c("n", "list"))
  # Class name must be string or caracter vector
  expect_error(class_add(list(), new_class = 1))
  expect_error(class_add(list(), new_class = TRUE))

  # --- test class_remove() ---
  # Throws error if class does not exist or misspelled
  expect_error(class_remove(new_obj, drop_class = "non-existing-class"))
  # If class name is not character
  expect_error(class_remove(new_obj, drop_class = 1))
  expect_error(class_remove(new_obj, drop_class = FALSE))

  # Removes class
  updated_obj <- class_remove(new_obj, drop_class = "list")
  expect_identical(class(updated_obj),  "n")

})
