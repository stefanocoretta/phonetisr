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
                      v_sequences = FALSE) {
  if (sanitise | sanitize) {
    strings_no_ipa <- lapply(
      Unicode::as.u_char_seq(stringi::stri_trans_nfd(strings), ""),
      intToUtf8,
      multiple = TRUE
    ) %>%
      unlist() %>%
      unique() %>%
      get_no_ipa() %>%
      # Escape special characters
      rex::rex()

    no_ipa_repl <- c(rep("", length.out = length(strings_no_ipa)))
    names(no_ipa_repl) <- strings_no_ipa
    if (length(no_ipa_repl) > 0) {
      strings <- stringr::str_replace_all(strings, no_ipa_repl)
    }
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

get_no_ipa <- function(chars) {
  chars[!(chars %in% ipa_chars)]
}

ipa_symbols_unicode <- c(
  '0061', '00E6', '0250', '0251', '0252', '0062', '0299', '0253', '0063',
  '00E7', '0255', '0064', '00F0', '0256', '0257', '0065', '0259', '025B',
  '0258', '025C', '025E', '0066', '0261', '0262', '0260', '029B', '0264',
  '0263', '0068', '0127', '029C', '0266', '0267', '0265', '0069', '026A',
  '0268', '006A', '029D', '025F', '0284', '006B', '006C', '029F', '026C',
  '026D', '026E', '028E', '006D', '0271', '006E', '0274', '0272', '0273',
  '014B', '006F', '00F8', '0153', '0276', '0254', '0275', '0070', '0278',
  '0071', '0072', '0280', '0279', '027A', '027B', '027D', '027E', '0281',
  '0073', '0282', '0283', '0074', '0288', '0075', '0289', '026F', '0270',
  '028A', '0076', '028B', '2C71', '028C', '0077', '028D', '0078', '0079',
  '028F', '007A', '0290', '0291', '0292', '0294', '0295', '02A1', '02A2',
  '01C0', '01C1', '01C2', '01C3', '0298', '03B2', '03B8', '03C7', '0334',
  '033C', '032A', '033B', '033A', '031F', '0320', '031D', '031E', '0318',
  '0319', '031C', '0339', '032C', '0325', '0330', '0324', '0329', '032F',
  '0303', '0308', '033D', '0306', '031A', '02DE', '02E1', '207F', '02B7',
  '02B2', '02E0', '02E4', '02B0', '02BC', '02D0', '02D1', '0361', '02C8',
  '02CC', '02E5', '02E6', '02E7', '02E8', '02E9', 'A71B', 'A71C', '2191',
  '2193', '2197', '2198', '0020', '002E', '007C', '2016', '203F', '030A',
  '0067', '030B', '0301', '0304', '0300', '030F', '0302', '030C', '1DC4',
  '1DC5', '1DC6', '1DC7', '1DC8', '1DC9', '035C', '203C', '1D91', '0348',
  '0349', '0353', '032E', '0347', '02C0', '02B1', '1D31'
)

ipa_chars <- intToUtf8(Unicode::as.u_char(ipa_symbols_unicode), multiple = TRUE)

ipa_diacritics_unicode <- c(
  '0334', '033C', '032A', '033B', '033A', '031F', '0320', '031D', '031E',
  '0318', '0319', '031C', '0339', '032C', '0325', '0330', '0324', '0329',
  '032F', '0303', '0308', '033D', '0306', '031A', '02DE', '02E1', '207F',
  '02B7', '02B2', '02E0', '02E4', '02B0', '02BC', '02D0', '02D1', '0361',
  '02C8', '02CC', '02E5', '02E6', '02E7', '02E8', '02E9', 'A71B', 'A71C',
  '2191', '2193', '2197', '2198', '203F', '030A', '030B', '0301', '0304',
  '0300', '030F', '0302', '030C', '1DC4', '1DC5', '1DC6', '1DC7', '1DC8',
  '1DC9', '035C', '0348', '0349', '0353', '032E', '0347', '02C0', '02B1',
  '1D31'
)

diacritics_regex <- ".(\u0334|\u033C|\u032A|\u033B|\u033A|\u031F|\u0320|\u031D|\u031E|\u0318|\u0319|\u031C|\u0339|\u032C|\u0325|\u0330|\u0324|\u0329|\u032F|\u0303|\u0308|\u033D|\u0306|\u031A|\u02DE|\u02E1|\u207F|\u02B7|\u02B2|\u02E0|\u02E4|\u02B0|\u02BC|\u02D0|\u02D1|\u0361|\u02E5|\u02E6|\u02E7|\u02E8|\u02E9|\uA71B|\uA71C|\u2191|\u2193|\u2197|\u2198|\u203F|\u030A|\u030B|\u0301|\u0304|\u0300|\u030F|\u0302|\u030C|\u1DC4|\u1DC5|\u1DC6|\u1DC7|\u1DC8|\u1DC9|\u035C|\u0348|\u0349|\u0353|\u032E|\u0347|\u02C0|\u02B1|\u1D31)+"

affricates <- c(
  "pf", "bv", "ts", "dz", "t\u0283", "d\u0292", "t\u0255", "t\u0291", "c\u00E7",
  "\u025F\u029D", "kx", "\u0261\u0263", "q\u03C7", "\u0262\u0281"
)

affricates_regex <- stringr::str_flatten(affricates, collapse = "|")

data("ipa_symbols", envir = environment())

vowels <- dplyr::filter(ipa_symbols, type == "vowel")

vowels_regex <- paste0("(", stringr::str_flatten(vowels$IPA, collapse = "|"), "){2,}")
