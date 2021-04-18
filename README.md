
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phonetisr: A Very Naive IPA Tokeniser

<!-- badges: start -->
<!-- badges: end -->

This package is a (very naive) tokeniser of phonetic transcriptions in
the [International Phonetic
Alphabet](https://www.internationalphoneticassociation.org/content/ipa-chart)
(IPA).

With phonetisr, you can parse texts and word lists transcribed in IPA
and tokenise them into phones so that you can perform quantitative
analyses.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("stefanocoretta/phonetisr")
```

## Roadmap

-   [x] Scan for illegal (non-IPA) characters.
-   [ ] Provide a list of default multi-character phones.
-   [ ] Functions for data import/export.
-   [ ] Ignore diacritics.
