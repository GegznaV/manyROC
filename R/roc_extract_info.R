#' [!!!] Extract the main information necessary for prediction
#'
#'
#'
#' @param obj object
#' @param ... pass to further methods
#' @export
roc_extract_info <- function(obj, ...) {
    UseMethod("roc_extract_info")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_extract_info
#' @method roc_extract_info multiroc_result
#' @export
roc_extract_info.multiroc_result <- function(obj, ...) {

    put_smaller_first <- function(pos_is_larger, pos, neg) {
        if (pos_is_larger)
            c(neg, pos)
        else
            c(pos, neg)
    }

    obj2 <- obj %>%
        tidyr::separate(
            compared_groups,
            into = c("neg_label", "pos_label"),
            sep = " vs\\. ",
            remove = FALSE
        ) %>%
        dplyr::mutate(pos_is_larger = median_neg < median_pos) %>%
        dplyr::select(feature,
                      compared_groups,
                      median_neg,
                      cutoff,
                      median_pos,
                      pos_is_larger,
                      neg_label,
                      pos_label)

      obj2 %$% # list(pos_is_larger, pos, neg)
        purrr::pmap(list(pos_is_larger, pos_label, neg_label),
                    put_smaller_first)  %>%
        purrr::reduce(rbind) %>%
        matrix(ncol = 2)  %>% # prevent failing when only one row is present
        magrittr::set_colnames(c("below", "above")) %>%
        tibble::as.tibble()  %>%
        dplyr::bind_cols(obj2, .)  %>%
        dplyr::select(feature,
                      compared_groups,
                      neg_label,
                      pos_label,
                      median_neg,
                      median_pos,
                      below,
                      cutoff,
                      above
        ) %>%
        add_class_label(c("multiroc_info", "roc_df"))
}
