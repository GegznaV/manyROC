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
# head__tail <- function(x,
#                        top = 4,
#                        bottom = 4,
#                        from = 1,
#                        to = NULL,
#                        digits = 3,
#                        hlength = 4,
#                        tlength = 4,
#                        ellipsis = TRUE)
# {
#
#     # [!!!] This function based on psych::headTail, It must be rewritten in the future
#
#     if (is.data.frame(x) | is.matrix(x)) {
#         if (is.matrix(x))
#             x <- data.frame(unclass(x))
#         nvar <- dim(x)[2]
#         hlength <- top
#         tlength <- bottom
#         if (is.null(to)) to <- nvar
#         dots <- rep("...", nvar)
#         h <- data.frame(head(x[from:to], hlength))
#         t <- data.frame(tail(x[from:to], tlength))
#
#         for (i in 1:nvar) {
#             if (is.numeric(h[1, i])) {
#                 h[i] <- signif(h[i], digits)
#                 t[i] <- signif(t[i], digits)
#                 # h[i] <- round(h[i], digits)
#                 # t[i] <- round(t[i], digits)
#             } else {
#                 dots[i] <- NA
#             }
#         }
#         if (ellipsis) {
#             head.tail <- rbind(h, ... = dots, t)
#         } else {
#             head.tail <- rbind(h, t)
#         }
#     } else {
#         h <- head(x, hlength)
#         t <- tail(x, tlength)
#         if (ellipsis) {
#             head.tail <- rbind(h, "...       ...", t)
#         } else {
#             head.tail <- rbind(h, t)
#             head.tail <- as.matrix(head.tail)
#         }
#     }
#     return(head.tail)
# }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~