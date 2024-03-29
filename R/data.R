#' List of IPA symbols
#'
#' @format A data frame with 143 rows and 12 variables:
#' \describe{
#'   \item{IPA}{IPA symbol.}
#'   \item{unicode}{Unicode code.}
#'   \item{uni_name}{Unicode name.}
#'   \item{ipa_name}{IPA name.}
#'   \item{phon_type}{The phonetic type of the symbol.}
#'   \item{type}{General character type (`consonant`, `vowel`, `diacritic`).}
#'   \item{height_ipa}{Vowel openness.}
#'   \item{height}{Vowel height.}
#'   \item{backness}{Vowel backness.}
#'   \item{rounding}{Vowel rounding.}
#'   \item{voicing}{Consonant voicing.}
#'   \item{place}{Consonant place of articulation.}
#'   \item{manner}{Consonant manner of articulation.}
#'   \item{lateral}{Is the consonant lateral?}
#'   \item{sonorant}{Is the phone sonorant?}
#' }
"ipa_symbols"

#' Klingon Swadesh list
#'
#' The Swadesh list in Klingon.
#'
#' @format A data frame with 195 rows and 4 variables:
#' \describe{
#'   \item{id}{Swadesh list item number.}
#'   \item{gloss}{English gloss.}
#'   \item{translit}{Klingon transliteration.}
#'   \item{ipa}{IPA transcription.}
#' }
"kl_swadesh"
