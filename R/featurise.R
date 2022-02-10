data("ipa_symbols", envir = environment())

#' Add features to list of phones
#'
#' This function counts occurrences of phones and includes basic phonetic features.
#'
#' @param phlist A list of phones or the output of `phonetise()`.
#'
#' @return A tibble.
#' @export
featurise <- function(phlist) {
    feats <- tibble::tibble(
        phone = unlist(phlist)
    ) %>%
    dplyr::count(phone, name = "count") %>%
    dplyr::arrange(count) %>%
    dplyr::mutate(
        base = stringr::str_remove_all(
            phone, paste0(ipa_diacritics, collapse = "|")
        )
    ) %>%
    dplyr::left_join(y = ipa_symbols, by = c("base" = "IPA"))

    return(feats)
}
