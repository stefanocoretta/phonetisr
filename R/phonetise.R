#' Tokenise IPA strings
#'
#' `phonetise()` tokenises strings of IPA symbols (like phonetic transcriptions
#' of words) into individual "phones". The output is a tibble with two columns:
#' `ipa`, the IPA strings, and `ipa_token`, the tokenised IPA strings as a list
#' column.
#'
#' @param strings A character vector with a list of words in IPA.
#' @param multi A character vector of one or more multi-character phones as
#'   strings.
#' @param regex A string with a regular expression to match several
#'   multi-character phones.
#' @param split If set to `TRUE` (the default), the tokenised strings are split
#'   into phones (i.e. the output is a vector with one element per phone). If
#'   set to `FALSE`, the string is not split and the phones are separated with
#'   the character defined in `sep`.
#' @param sep A character to be used as the separator of the phones if `split = FALSE` (default is
#'   ` `, space).
#' @param sanitise Whether to remove all non-IPA characters (`TRUE` by default).
#' @param diacritics If set to `TRUE`, parses all valid diacritics as part of
#'   the previous character (`FALSE` by default).
#' @param affricates If set to `TRUE`, parses homorganic stop + fricative as affricates.
#' @param v_sequences If set to `TRUE`, collapses vowel sequences (`FALSE` by default).
#' @param prenasalised If set to `TRUE`, parses prenasalised consonants as such (`FALSE` by default).
#' @param all_multi If set to `TRUE`, `diacritics`, `affricates`, `v_sequences` and `prenasalised` are all set to `TRUE`.
#' @param sanitize Alias of `sanitise`.
#'
#' @return A list.
#'
#' @examples
#' ipa <- c("pʰãkʰ", "tʰum̥", "ɛkʰɯ")
#' ph <- c("pʰ", "tʰ", "kʰ", "ã", "m̥")
#'
#' phonetise(ipa, multi = ph)
#'
#' ph_2 <- ph[4:5]
#'
#' # Match any character followed by <ʰ> with ".ʰ".
#' phonetise(ipa, multi = ph_2, regex = ".ʰ")
#'
#' # Same result.
#' phonetise(ipa, regex = ".(\u0303|\u0325|\u02B0)")
#'
#' # Don't split strings and use "." as separator
#' phonetise(ipa, multi = ph, split = FALSE, sep = ".")
#' @export
phonetise <- function(strings, multi = NULL, regex = NULL, split = TRUE, sep = " ", sanitise = TRUE,
                      sanitize = sanitise, diacritics = FALSE, affricates = FALSE,
                      v_sequences = FALSE, prenasalised = FALSE, all_multi = FALSE) {
  if (sanitise | sanitize) {
    strings_no_ipa <- lapply(
      Unicode::as.u_char_seq(stringi::stri_trans_nfd(strings), ""),
      intToUtf8,
      multiple = TRUE
    ) %>%
      unlist() %>%
      unique() %>%
      get_no_ipa()

    no_ipa_repl <- c(rep("", length.out = length(strings_no_ipa)))
    names(no_ipa_repl) <- strings_no_ipa
    if (length(no_ipa_repl) > 0) {
      strings <- stringr::str_replace_all(strings, stringr::fixed(no_ipa_repl))
      cli::cli_alert_info(
        cli::col_blue("The following non-IPA character were found and removed: {strings_no_ipa}")
      )
      cli::cli_text("")
    }
  }

  if (all_multi) {
    diacritics <- TRUE
    affricates <- TRUE
    v_sequences <- TRUE
    prenasalised <- TRUE
  }

  # Prepare multicharacter list if specified ########################

  if (!is.null(multi)) {
    multi_len <- length(multi)
  } else {
    # Set to zero if no multichar is specified so that nothing is added to
    # multichar_len later
    multi_len <- 0
  }

  if (!is.null(regex)) {
    multi_rx <- stringr::str_extract_all(strings, regex) %>%
      unlist() %>%
      unique()

    multi_len <- multi_len + length(multi_rx)
    multi <- c(multi, multi_rx)
  }

  # Use default diacritic list for multichar symbols
  if (diacritics) {
    multi_dia <- stringr::str_extract_all(strings, diacritics_regex) %>%
      unlist() %>%
      unique()

    multi_len <- multi_len + length(multi_dia)
    multi <- c(multi, multi_dia)
  }

  # Use default affricates list
  if (affricates) {
    multi_aff <- stringr::str_extract_all(strings, affricates_regex) %>%
      unlist() %>%
      unique()

    multi_len <- multi_len + length(multi_aff)
    multi <- c(multi, multi_aff)
  }

  # Collapse vowel sequences
  if (v_sequences) {
    multi_vowels <- stringr::str_extract_all(strings, vowels_regex) %>%
      unlist() %>%
      unique()

    multi_len <- multi_len + length(multi_vowels)
    multi <- c(multi, multi_vowels)
  }

  # Collapse vowel sequences
  if (prenasalised) {
    multi_prenasal <- stringr::str_extract_all(strings, prenasal_regex) %>%
      unlist() %>%
      unique()

    multi_len <- multi_len + length(multi_prenasal)
    multi <- c(multi, multi_prenasal)
  }

  ####
  # Main tokeniser procedure ########################################
  ####

  if (!is.null(multi) & multi_len > 0) {
    pua <- intToUtf8(
      Unicode::as.u_char_seq(Unicode::u_blocks("Private Use Area")[[1]])[[1]][1:multi_len],
      multiple = TRUE
    )

    names(pua) <- multi

    strings_pua <- lapply(strings, function(.x) stringr::str_replace_all(.x, pua))

    strings_pua_token <- lapply(
      Unicode::as.u_char_seq(stringi::stri_trans_nfd(strings_pua), ""),
      intToUtf8,
      multiple = TRUE
    )

    ipa_mc <- multi
    names(ipa_mc) <- pua

    output <- lapply(strings_pua_token, function(.x) stringr::str_replace_all(.x, ipa_mc))

  } else {
    output <- lapply(
      Unicode::as.u_char_seq(stringi::stri_trans_nfd(strings), ""),
      intToUtf8,
      multiple = TRUE
    )
  }

  if (split) {
    return(output)
  } else {
    output <- lapply(output, paste, collapse = sep)
    return(output)
  }
}

#' @rdname phonetise
#' @examples
#' ipa <- c("pʰãkʰ", "tʰum̥", "ɛkʰɯ")
#' ph <- c("pʰ", "tʰ", "kʰ", "ã", "m̥")
#'
#' phonetise(ipa, multi = ph)
#'
#' @export
phonetize <- phonetise


#' Get non-IPA characters.
#'
#' Given a vector of characters, it returns those which are not part of the IPA.
#'
#' @param chars A vector of characters.
#'
#' @return A vector.
#'
#' @examples
#' get_no_ipa(c("a", "ʃ", ">"))
#'
#' @export
get_no_ipa <- function(chars) {
  chars[!(chars %in% ipa_chars)]
}
