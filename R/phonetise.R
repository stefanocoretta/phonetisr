#' Tokenise IPA strings
#'
#' `phonetise()` tokenises strings of IPA symbols (like phonetic transcriptions
#' of words) into individual "phones". The output is a tibble with two columns:
#' `ipa`, the IPA strings, and `ipa_token`, the tokenised IPA strings as a list
#' column.
#'
#' @param strings A charcter vector with a list of words in IPA.
#' @param multichar A character vector of one or more multi-character phones as
#'   strings.
#'
#' @return A tibble.
#' @export
phonetise <- function(strings, multichar) {
  multichar_len <- length(multichar)

  pua <- intToUtf8(
    Unicode::as.u_char_seq(Unicode::u_blocks("Private Use Area")[[1]])[[1]][1:multichar_len],
    multiple = T
  )

  names(pua) <- multichar

  strings_pua <- lapply(strings, function(.x) stringr::str_replace_all(.x, pua))

  strings_pua_token <- stringr::str_split(strings_pua, "")

  ipa_mc <- multichar
  names(ipa_mc) <- pua

  tibble::tibble(
    ipa = strings,
    ipa_token = lapply(strings_pua_token, function(.x) stringr::str_replace_all(.x, ipa_mc))
  )
}
