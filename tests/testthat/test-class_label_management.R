context("class_label_management")

test_that("add_class_label() and remove_class_label() work", {

  # --- test add_class_label() ---
  # Adds class
  new_obj <- list() %>% add_class_label(new_class = "n")
  expect_identical(class(new_obj), c("n", "list"))
  # Class name must be string or caracter vector
  expect_error(add_class_label(list(), new_class = 1))
  expect_error(add_class_label(list(), new_class = TRUE))

  # --- test remove_class_label() ---
  # Throws error if class does not exist or misspelled
  expect_error(remove_class_label(new_obj, drop_class = "non-existing-class"))
  # If class name is not character
  expect_error(remove_class_label(new_obj, drop_class = 1))
  expect_error(remove_class_label(new_obj, drop_class = FALSE))

  # Removes class
  updated_obj <- remove_class_label(new_obj, drop_class = "list")
  expect_identical(class(updated_obj),  "n")

})
