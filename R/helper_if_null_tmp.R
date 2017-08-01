# <environment: namespace:spMisc>
# [!!!] Will be removed in the future
`%if_null%` <- function(a, b) {if (!is.null(a)) a else b}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# <environment: namespace:spMisc>
# [!!!] Will be removed in the future
`%if_null_or_len0%` <- function(a, b) {
    if (isTRUE(!is.null(a) &
               length(a) > 0 &
               (if (length(a) == 1 & is.character(a)) {
                   nchar(a) > 0
               } else {
                   TRUE
               })))
        a
    else
        b
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
