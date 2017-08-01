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
#' @param drop_class (\code{character})\cr A name of a class to remove.
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
class_remove <- function(x, drop_class) {
    assert_character(drop_class)
    all_classes <- class(x)
    assert_subset(drop_class, all_classes)

    class(x) <- setdiff(all_classes, drop_class)
    x
}