# phonetisr v0.1.0

* Added vignette.

* Updated documentation of `featurise()`.



# phonetisr v0.0.5

## Fixed

* Corrected documentation of `phonetise()`.

* Fixed wrong parsing of <ç>.

* Fixed missing featurisation of <ç>.



# phonetisr v0.0.4

## Added

* `ph_search()` to search and extract phones with phonetic expressions.

## Fixed

* `featurise()` now correctly finds the base of non-space-modifying characters.




# phonetisr v0.0.3

## Added

* `ignore_stress` and `ignore_tone` in `phonetise()` ignore and remove stress and tone diacritics and letters. They are `TRUE` by default.

* `all_multi` argument in `phonetise()` sets `diacritics`, `affricates`, `v_sequences` and `prenasalised` to `TRUE`.

* `prenasalised` argument in `phonetise()` parses prenasalised consonants as single phones.

* Added the Unicode blocks Phonetic Extensions and Phonetic Extensions Supplement.

* `sanitise = TRUE` prints a message with info on removed non-IPA characters if any are found.

## Changed

* Improved base phone detection of `featurise()`.

* `ipa_symbols` now includes a column (`phon_type`) which differentiates base characters from secondary articulations, vowel/consonant articulation, stress, tone, and more.

* If `split = FALSE` in `phonetise()`, output a character vector rather than a list.




# phonetisr v0.0.2

## Breaking changes

* `default_multi` has been renamed to `diacritics`.

## Added

* `affricates` argument in `phonetise()` to parse homorganic "stop + fricative" clusters as affricates.

* `v_sequences` argument in `phonetise()` to collapse vowel sequences.

* `featurise()` to count phones and add features.


# phonetisr v0.0.1

## Changed

* `phonetise()` gained two new arguments: `split` and `sep`.




# phonetisr v0.0.0.9000

## Added

* `phonetise()` to tokenise phones from IPA transcriptions.

* `kl_swadesh.rda` with the Swadesh list of Klingon in transliteration and IPA transcription.

* `ipa_symbols.rda` with info on all the IPA symbols.
