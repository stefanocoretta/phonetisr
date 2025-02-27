
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phonetisr: A Naive IPA Tokeniser

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.0.5.9000-blue.svg)](https://github.com/phonetisr)
[![](https://img.shields.io/badge/devel%20version-0.0.5.9000-orange.svg)](https://github.com/phonetisr)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This package is a (naive) tokeniser of phonetic transcriptions in the
[International Phonetic
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
remotes::install_github("stefanocoretta/phonetisr@v0.0.5")
```

## Usage

``` r
library(phonetisr)

# IPA strings to be tokenised
ipa <- c("pʰãkʰ", "tʰum̥", "ɛkʰɯ")

# List of character sequences to be considered single phones
ph <- c("pʰ", "tʰ", "kʰ", "ã", "m̥")

# Tokenise strings
phonetise(ipa, multi = ph)
#> [[1]]
#> [1] "pʰ" "ã"  "kʰ"
#> 
#> [[2]]
#> [1] "tʰ" "u"  "m̥" 
#> 
#> [[3]]
#> [1] "ɛ"  "kʰ" "ɯ"
```

## Roadmap

- [x] Scan for illegal (non-IPA) characters.
- [x] Provide a list of default multi-character phones.
- [ ] Functions for data import/export.
- [ ] Ignore diacritics.
