# Fonology 1.2.0

## Lexical lookup for all five languages

- Added corpus-backed lexicons for Italian (`it_lex`, ~82K words) and Spanish
  (`sp_lex`, ~130K words), derived from the English Wiktionary via Wiktextract
  (kaikki.org, CC BY-SA); reproducible build scripts in `data-raw/`
- All five languages now use lexical lookup before the regex fallback, and all
  regex-derived forms are marked with `*` (new for Italian and Spanish)
- Renamed the user diacritized lexicons `it_lex` -> `it_lex_user` and
  `sp_lex` -> `sp_lex_user` (matching `pt_lex_user`); existing entries migrated

## Fallback accuracy overhauls (benchmarked against each language's lexicon)

- English: 8.6% -> 27% exact match (magic e, vowel+r, soft c/g, -s/-ed
  morphophonology, schwa reduction in unstressed syllables)
- Portuguese: 82% -> 95% exact (categorical coda s-voicing, posttonic glide
  formation, PSL-mined stressed mid-vowel lowering; new `posttonic_pt_vec()`)
- French: 48.6% -> 71% exact (ph/ch ordering fixes, -ment suffix family,
  verb-future schwa, nasal/glide protection, onset-cluster syllabification)
- Italian: 43.5% -> 70% exact (antepenult suffix classes, intervocalic
  gemination, z-affricate voicing, mined mid-vowel quality rules)
- Spanish: 67.5% -> 95% exact (yeismo, final tap, coda obstruent voicing,
  rising glides moved before stress assignment)

## Fixes

- Italian accent convention now follows standard orthography: grave = open-mid
  (è/ò -> ɛ/ɔ), acute/circumflex = close-mid (é/ó/ê/ô -> e/o); user lexicon
  entries auto-migrated
- Portuguese lookup normalization fixed (`mulher` -> mu.ˈʎɛr; tap replacement
  now global)
- Spanish `ll` digraph no longer collapsed before yeismo applies; overlapping
  tap-rule matches fixed; word-initial `z` -> dz no longer destroyed
- README now reports per-language lookup coverage and fallback accuracy in a
  summary table

# Fonology 1.1.2

- Added Lexique 4-backed French lookup (`fr_lex`) before the regex fallback, with `fr_ipa_lex` user overrides taking final priority
- Marked regex-derived Portuguese and French fallback forms with `*`, matching the English fallback convention
- Updated Portuguese `ipa()` to treat PSL-backed lexical matches as the unmarked lookup path
- Updated helper functions to ignore fallback markers when parsing phonological material
- Fixed vector handling in Spanish and French `ipa()` so multi-word inputs no longer fail on user IPA override checks
- Added regression tests for vectorized `ipa()` behavior in Spanish, French, and Portuguese

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
