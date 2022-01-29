featurise <- function(phlist) {
    feats <- tibble::tibble(
        phone = unlist(phlist)
    ) %>%
    dplyr::count(phone, name = "count") %>%
    dplyr::arrange(count)

    return(feats)
}
