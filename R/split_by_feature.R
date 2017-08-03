
split_by_feature <- function(obj) {
    res <- split(obj, obj$feature)
    class_add(res, c("roc_info", "roc_df"))
}


mat2list <- function(x) {
    as.list(as.data.frame(x))
}
