#' Utility functions for cross vadidation (cvo) object
#'
#' Function \code{cvo_get_info} returns information about the
#' cross-validation object \code{cvo}.
#'
#' @export
#' @seealso \code{\link{cvo_create_folds}}
#'
#' @name cvo_get_info
cvo_get_info <- function(cvo) {
    attr(cvo, "info")
}
#====================================================================
#' @rdname cvo_get_info
#'  Function \code{cvo_get_inds} extracts indices of indicated
#' set (either training or test) from cross-validation object (\code{cvo})
#' created with function \code{cvo_create_folds} when fold of interest is
#'  indicated as \code{fold} (it can be either a name of fold or an index
#'  of fold). If \code{cvo} does not contain indices of chsen
#'  \code{type}, it returns a \bold{complement} to those indices and this
#'  information is indicated in attributes of returned object.
#'
#' @export
#'
#' @param cvo a cross-validation object (\code{cvo}) created with function \code{cvo_create_folds}.
#' @param fold either a name or an index of fold of interest.
#' @param type (string) one of options, indicating the kind of incices you are interested in: \cr
#' \code{"AsIs"} – indices as they are in the object,\cr
#' \code{"Train"} – indices of training set, \cr
#' \code{"Test"} – indices of test set.
#'
#' @examples
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' cvo <- cvo_create_folds(Spectra2)
#'
#' cvo_get_seeds(cvo)
#' cvo_get_info(cvo)
#' cvo_get_inds(cvo, 1)
#' cvo_get_inds(cvo, 1, "train")
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
#' @rdname cvo_get_info
#' @export
cvo_get_inds.default <- function(cvo,
                         fold,
                         type = c("asis", "train", "test")) {

    force(fold)

    type = match.arg(type)
    fold_ind <- cvo[[fold]]

    # Select `type` whics is in the `cvo`
    if (type == "asis") {
        type <- as.character(cvo_get_info(cvo)$indices)
    } else {
        type <- fCap(type)
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
#' @rdname cvo_get_info
#' @export
#' @description Function \code{cvo_get_sample_size} returns sample
#'  size of the object used to create the
#' cross-validation object \code{cvo}.

cvo_get_sample_size <- function(cvo) {
    attr(cvo, "info")$sample_size
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cvo_get_info
#' @export
#' @description Function \code{cvo_get_seeds} returns seeds of (pseudo)random number
#' generator used for each repetition of splitting to folds.
cvo_get_seeds <- function(cvo) {
    attr(cvo, "seeds")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cvo_get_info
#' @export
#' @description Function \code{cvo_count_folds} returns total number of folds in a
#'  \code{cvo} object.
cvo_count_folds <- function(cvo) {
    UseMethod("cvo_count_folds")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cvo_count_folds
#' @export
cvo_count_folds.cvo_caret <- function(cvo) {
    length(names(cvo))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname cvo_count_folds
#' @export
cvo_count_folds.cvo_mlr <- function(cvo) {
    cvo$desc$iters
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~