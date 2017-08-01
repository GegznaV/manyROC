# =============================================================================
#' @rdname cvo_get_info
#' @export
cvo_get_inds <- function(cvo,
                         fold,
                         type = c("asis", "train", "test")) {
    UseMethod("cvo_get_inds")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cvo_get_info
#' @export
cvo_get_inds.cvo_caret <- function(cvo,
                                 fold,
                                 type = c("asis", "train", "test")) {

    force(fold)

    type = match.arg(type)
    fold_ind <- cvo[[fold]]

    # Select `type` whics is in the `cvo`
    if (type == "asis") {
        type <- as.character(cvo_get_info(cvo)$indices)
    } else {
        # [!!!]
        # type <- fCap(type)
        type <- type
    }

    # Select indices
    if (tolower(cvo_get_info(cvo)$indices) == tolower(type)) {
        ind <- fold_ind
    } else {
        # indices complement to `type`
        all_ind <- seq_len(attr(cvo, "info")$sample_size)
        ind  <- which(!(all_ind %in% fold_ind))
        type <- paste("Complement to", cvo_get_info(cvo)$indices)
    }
    attr(ind, "fold") <- names(cvo)[fold]
    attr(ind, "indices") <- type
    ind
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cvo_get_info
#' @export
cvo_get_inds.cvo_mlr <- function(cvo,
                                 fold,
                                 type = c("asis", "train", "test")) {
    switch(type,
           "train" = {
               ind <- cvo$train.inds[[fold]]
               attr(ind, "fold") <- names(cvo$train.inds)[fold]
               attr(ind, "indices") <- "train"
           },
           "asis"  = ,
           "test"  = {
               ind <- cvo$test.inds[[fold]]
               attr(ind, "fold") <- names(cvo$test.inds)[fold]
               attr(ind, "indices") <- "test"
           })
    ind
}
# =============================================================================
