# Fonology (BETA)

The package `fonology` will contain functions to plot vowel trapezoids, find phonemes on the basis of a set of features (or vice-versa), as well as functions to explore the phonology of Portuguese.

## Current functions

- `getFeat(ph = c("..."), lg = "")`: returns the minimal matrix of distinctive features given a set of phonemes `ph` for a given language `lg`. So far, only three languages are supported: English, French, Portuguese and Spanish.
- `getPhon(ft = c("...")`: returns phonemes given a vector of features and their values. For example, `getPh(ft = c("+hi", "+syl"), lg = "Portuguese")` returns `"i" "u"`. So far, only three languages are supported: English, French, Portuguese and Spanish.
- `plotVowels()`: Plots the vowel trapezoid for a given language `lg` using `ggplot2`. The function also prints the LaTeX code to create the same trapezoid using the `vowel` package in LaTeX. Languages supported: `arabic french english dutch german hindi italian japanese korean mandarin portuguese spanish swahili russian talian thai vietnamese`.
- `biGram_pt()`: returns the log bigram probability for a possible string in Portuguese. The string must use broad phonemic transcription, but no syllabification or stress. Diacritics allows: `~` for `a` and `w` in nasal diphthongs. The reference used calculate probabilities is the [Portuguese Stress Lexicon](http://gdgarcia.ca/psl.html).
- `ipa_pt()`: given a possible Portuguese string `word`, the function returns the string transcribed, syllabified, and stressed (using **phonemic**, not phonetic, transcription). Stress assignment is based on three scenarios: a) if the word exists in the PSL, stress will be assigned as listed in the lexicon; b) if the word doesn't exist in the PSL, stress will be assigned based on the traditional stress assignment rules found in the literature; c) if the weight profile of the word is XLL or ...LH, stress will be assigned probabilistically following Garcia (2017) and subsequent work (see [here](https://gdgarcia.ca/research.html)). Secondary stress is also provided in conditions where it's possible.


## How to install the package

```
library(devtools)
install_github("guilhermegarcia/fonology")

library(Fonology)
```
