# Fonology 1.3.0

## `getFeat()` and `getPhon()` audited and repaired

Both functions were rewritten around a shared inventory and lookup layer;
neither had test coverage before.

### Corrected results

- `/g/` was silently dropped from every language (the segment list spelled it
  `ɡ`, the data uses `g`), so e.g. `getFeat(c("k", "g"), "Italian")` returned
  the matrix for `/k/` alone and `getFeat("b", "Portuguese")` gained a spurious
  `-cor`
- `+long` behaved as `+lo`, and `+hitone`/`+hireg` as `+hi` (leftmost-first
  regex alternation); all 25 features are now reachable
- French `ɑ̃`, `ɔ̃` and `ɱ` were rejected by `getFeat()` but returned by
  `getPhon()`; the two functions now agree
- `[strid]` was `0` for every sibilant: `getFeat(c("s", "z", "ʃ", "ʒ"), "pt")`
  returned `0strid`, now `+strid +cor`
- Among equally small matrices, specified values are preferred over `0`

### Robustness

- Segments absent from the inventory or from `allFeatures` now raise an error
  naming them, instead of being dropped from the computation
- Fixed crashes on whole-inventory input, one-segment inventories and empty
  input; repeated or contradictory features now give a clear error
- Precomposed and decomposed IPA, and affricates with or without a tie bar,
  are both accepted

### Data

- `allFeatures` rebuilt (`data-raw/build_allFeatures.R`): 120 duplicate IPA keys
  removed (6,487 -> 6,367 rows), `strid` taken from upstream PanPhon, symbols
  normalised to NFD, `ɚ`/`ɝ` corrected to mid and unrounded
- Inventories now come from the `vowels_XX`/`consonants_XX` datasets
  (`data-raw/build_inventories.R`): new `vowels_en`/`consonants_en`, `n` added
  to Spanish, `x` to Portuguese, `ɚ`/`ɝ` to English, `ɱ` removed from French.
  Portuguese keeps oral vowels only — nasal vowels are treated as derived from
  vowel + nasal, so they are not valid input to these functions

### Breaking changes

- `getPhon()` returns phonemes in inventory order rather than merge-sort order;
  the sets are unchanged, but positional indexing may need updating
- `getFeat()` on an entire inventory returns a message instead of erroring

### Other

- Minimal-matrix search vectorised; `gtools` dropped from Imports
- New `tests/testthat/test-features.R`, including a property test verifying that
  `getFeat()` and `getPhon()` are inverses across all one- and two-phoneme sets
  in all five languages

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
