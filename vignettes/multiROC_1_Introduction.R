## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(collapse = FALSE,
                      comment = "#>",
                      fig.align = 'center',
                      fig.width = 6)
optDEF <- knitr::opts_chunk$get()

## ----Load main package, message = FALSE, warning = FALSE-----------------
library(multiROC)

## ------------------------------------------------------------------------
# Make some data
set.seed(1)
(x_ <- rnorm(10))
(gr_ <- gl(n = 2, k = 5, length = 10, labels = c("H","S")))

# Explore the functions
roc_analysis(x_, gr_)

