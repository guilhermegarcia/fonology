# Fonology 1.1.1

- Fixed vector handling in Spanish and French `ipa()` so multi-word inputs no longer fail on user IPA override checks
- Added regression tests for vectorized `ipa()` behavior in Spanish and French

# Fonology 1.1.0

- Added English support to `ipa()` via CMU-backed lookup with user IPA overrides and heuristic fallback for out-of-vocabulary forms
- Added `add_lex_en()` and English package data (`en_lex`, `en_ipa_lex`, `stopwords_en`)
- Updated documentation and README to reflect English support

# Fonology 0.9.8

- New function `ipa2typst()` to export Typst code for phonetic transcription using phonokit package

# Fonology 0.9.7

- Minor issues with Portuguese transcription fixed
- New function added (`cv()`) to return syllable shape of a given transcribed string

# Fonology 0.9.6

- Minor consistency issues fixed
- Sample tableaux added to accompany `nhg()` function
- Data file included based on features found in Hayes (2009)
  - This is _not_ the file used in functions involving distinctive features

# Fonology 0.9.5

This is a relatively large update.

- Added new function `maxent()` for Maximum Entropy grammars
- Added new function `nhg()` for Noisy Harmonic Grammar simulations (teaching)
- Updated `ipa()` with French transcription (beta)
- Improved README
- Minor bug fixes and cleanup
