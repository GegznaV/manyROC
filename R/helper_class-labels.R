# =============================================================================
# Helper functions
# =============================================================================
#' Manage S3 class labels
#'
#' Add or remove S3 cpass labels.
#'
#' @param x An object to modify.
#' @param new_class (\code{character})\cr A name of a new class.
#'                   May be a vector of names.
#' @param old_class (\code{character})\cr A name of a class to remove.
#'                   May be a vector of names.
#' @export
#' @examples
#' class_add(list("ok"), "ok_list")
#'
class_add <- function(x, new_class) {
    assert_character(new_class)
    class(x) <- union(c(new_class, class(x)), class(x))
    x
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname class_add
#' @export
class_remove <- function(x, old_class) {
    assert_character(old_class)
    all_classes <- class(x)
    assert_subset(old_class, all_classes)

    class(x) <- setdiff(old_class, all_classes)
    x
}