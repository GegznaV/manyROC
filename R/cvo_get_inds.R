# ====================================================================
#' @rdname cvo_get_inds
#' Function \code{cvo_get_inds} extracts indices of indicated
#' set (either training or test) from cross-validation object (\code{cvo})
#' created with function \code{cvo_create_folds} when fold of interest is
#' indicated as \code{fold} (it can be either a name of fold or an index
#' of fold). If \code{cvo} does not contain indices of chsen
#' \code{type}, it returns a \bold{complement} to those indices and this
#' information is indicated in attributes of returned object.
#'
#' @export
#'
#' @param cvo a cross-validation object (\code{cvo}) created with
#'            function \code{cvo_create_folds}.
#'
#' @param fold either a name or an index of fold of interest.
#'
#' @param type (string) one of options, indicating the kind of
#'             incices you are interested in:
#' \itemize{
#'      \item \code{"AsIs"} – indices as they are in the object,
#'      \item \code{"Train"} – indices of training set,
#'      \item \code{"Test"} – indices of test set.
#' }
#'
#' @examples
#'
#' cvo <- cvo_create_folds(fluorescence)
#' cvo
#'
#' cvo_get_info(cvo)
#'
#' cvo_get_seeds(cvo)
#'
#' cvo_get_sample_size(cvo)
#'
#' cvo_count_folds(cvo)
#'
#' cvo_get_inds(cvo, 1)
#'
#' cvo_get_inds(cvo, 1, "train")
#'
#' cvo_get_inds(cvo, 1, "test")
#'
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
    if (cvo_get_info(cvo)$indices == type) {
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
#' @rdname cvo_get_inds
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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~