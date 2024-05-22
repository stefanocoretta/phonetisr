#' Search phones
#'
#' Given a vector of phonetised strings, find phones.
#'
#' @param phlist The output of `phonetise()`.
#' @param phonex A phonetic expression. Supported shorthands are `C` for
#'   consonant, `V` for vowel, and `#` for word boundary.
#'
#' @return A list.
#' @export
#'
#' @examples
#' ipa <- c("pʰãkʰ", "tʰum̥", "ɛkʰɯ", "pun")
#' ph <- c("pʰ", "tʰ", "kʰ", "ã", "m̥")
#' ipa_ph <- phonetise(ipa, multi = ph)
#' ph_search(ipa_ph, "#CV")
#'
#' # partial matches are also returned
#' ph_search(ipa_ph, "p")
#'
#' # use regular expressions
#' ph_search(ipa_ph, "pʰ?V")
ph_search <- function(phlist, phonex) {

  feats <- featurise(phlist)
  cons <- feats %>%
    dplyr::filter(type == "consonant") %>%
    dplyr::select(phone) %>%
    dplyr::pull()
  vows <- feats %>%
    dplyr::filter(type == "vowel") %>%
    dplyr::select(phone) %>%
    dplyr::pull()

  cons_regex <- paste0("(", stringr::str_flatten(cons, collapse = "|"), ")")
  vows_regex <- paste0("(", stringr::str_flatten(vows, collapse = "|"), ")")

  search_regex <- stringr::str_replace(phonex, "^#", "^") %>%
    stringr::str_replace("#$", "$") %>%
    stringr::str_replace_all("C", cons_regex) %>%
    stringr::str_replace_all("V", vows_regex)

  found <- sapply(
    phlist,
    function(ph) {
      stringr::str_extract_all(
        stringr::str_flatten(ph, collapse = ""),
        search_regex
      )
    }
  )

  return(found)

}
