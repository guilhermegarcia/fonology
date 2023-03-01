# Fonology (BETA)

The package `fonology` contains functions to help bridge the gap between written data and phonological analysis. `tidyverse` is required.

## Main functions and data

- `getFeat()` and `getPhon()` to work with distinctive features
- `ipa_pt()` phonemically transcribes Portuguese words (real or not)
- `sonDisp()` calculates the sonority dispersion of a given demisyllable
- `gen_pt()` generates hypothetical words in Portuguese
- `biGram_pt()` calculates bigram probabilities for a given word
- `plotVowels()` generates vowel trapezoids
- `plotSon()` plots the sonority profile of a given word
- `psl` contains the [*Portuguese Stress Lexicon*](https://gdgarcia.ca/psl.html) 
- `pt_lex` contains a simplified version of `psl` 
- `stopwords_pt` contains stopwords in Portuguese

## Future steps

- implement `ipa_sp()` and `ipa_fr()` for Spanish and French

Visit [my website](https://gdgarcia.ca/fonology) for more information and demos.

## How to install the package

```
library(devtools)
install_github("guilhermegarcia/fonology", build_vignettes = TRUE)

library(Fonology)
vignette("Fonology")
```
