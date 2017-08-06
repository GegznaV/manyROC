#' Get vector of variable values
#'
#' @param x Either a name (string) or a positive integer which indicates
#'          position of variable in \code{data} or a vector.
#' @param data A data frame (or sililar structure).
#'
#' @return \itemize{
#' \item If \code{x} is a vector of at least 2 elements,
#'                   this vector is returned.
#' \item If \code{x} is a string or a number (length 1),
#'                   then \code{data[[x]]} is returned.
#' \item If \code{x} is \code{NULL}, \code{NULL} is returned.
#' }
#'
#' @export
#'
#' @examples
#' library(multiROC)
#' dataset <- head(PlantGrowth)
#'
#' get_var_values(x = "group", data = dataset)
#'
#' get_var_values(x = c("group", "group"), data = dataset)
#'
#' get_var_values(x = NULL, data = dataset)
#'
#'
get_var_values <- function(x, data) {

    # Prepare data, if needed -------------------------------------------------
    if (inherits(data,"hyperSpec")) {
        data <- data$..
    }

    # If x is NULL ------------------------------------------------------------
    if (is.null(x)) {
        return(x)
    }

    # Is either a scalar string or positive integer number --------------------
    if (checkmate::test_string(x) || checkmate::test_number(x, lower = 1)) {
        data[[x]]
        # Is a vector -------------------------------------------------------------
    } else {
        checkmate::assert_vector(x, strict = TRUE)
        x
    }
    # -------------------------------------------------------------------------
}
