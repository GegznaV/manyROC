# =============================================================================
head_tail <- function(x, top = 4, bottom = 4, sep = "...") {

  x <- dplyr::mutate_all(as.data.frame(x), as.character)
  h <- head(x, top)
  t <- tail(x, bottom)

  dots  <- rep(sep, ncol(x))
  space <- rep(" ", ncol(x))
  rbind(h, `...` = dots, t, `  ` = space)
}
# =============================================================================
