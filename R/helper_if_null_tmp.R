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
# <environment: namespace:spMisc>
# [!!!] Will be removed in the future
bru <-
function(symbol = "=",
         n = 60,
         after  = (if (print) 1 else 0),
         before = 0,
         print  = TRUE)
{
    # Create sequences of symbols
    nlB   <- bru_n('\\n', before)
    lineC <- bru_n(symbol,n)
    nlA   <- bru_n('\\n', after)
    # Adjust the length
    lineC <- substr(lineC, 1, n)

    # Join all symbols
    lineC <- paste0(nlB, lineC, nlA)

    # Either print or return the result
    if (print)  cat(lineC) else return(lineC)
}

bru_n <- function(symbol, times) {
    paste0(rep(symbol, times),   collapse = "")
}
