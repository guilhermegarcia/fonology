# Fonology 1.5.0

## Lexical corrections are separated from your own lexicon

Corrections used to live in a single per-language lexicon that was both the
user's private store and the list of fixes shipped with the package. The two
roles are now separate:

- **Shipped layer** (`pt_lex_fix`, `pt_ipa_fix`, and the Spanish, Italian,
  French and English equivalents): corrections curated by the maintainer and
  distributed with the package
- **Local layer** (`pt_lex_user`, `pt_ipa_lex`, ...): your own entries, added
  with `add_lex_*()`, stored under `tools::R_user_dir("Fonology")` and empty in
  a freshly installed package

The two are merged at transcription time, with your entries taking priority.

- **Fixes shipped in an update now reach everyone.** Previously a user's
  lexicon file *replaced* the shipped data rather than merging with it, so
  anyone who had ever called `add_lex_*()` stopped receiving the corrections
  added in later releases
- **New `promote_lex()`** (maintainer-facing) moves entries from the local
  layer into the shipped layer, so a correction can be tested locally, then
  shipped, after which the local layer can be reset to empty
- **`remove_lex_*()` now distinguishes two cases.** Removing a word you had
  overridden restores the shipped correction; removing a word you never touched
  suppresses the shipped correction for you alone. Calling it twice does both
  in turn
- `add_lex_*()` no longer writes into the package's own `data/` directory when
  run from a source checkout; the local layer is always the user data directory

## A startup message

`library(Fonology)` now prints a short banner with the package version, the
supported languages and a few example calls. Silence it with
`suppressPackageStartupMessages()` or `options(Fonology.quiet = TRUE)`.

## Lexicon corrections

- Four IPA-override entries stored orthography instead of IPA and were returned
  verbatim, so `ipa("féra", lg = "pt")` emitted the literal text `féras`. They
  have been removed: `bórda`, `féra` and `véra` in Portuguese, and `aquatique`
  in French, which stripped the syllable boundaries `fr_lex` already provides
  (`a.kwa.tik`)
- The intent behind them now lives in the diacritized store, where it belongs:
  `bordas` is `ˈbɔr.das`, `veras` is `ˈvɛ.ras` and `cleo` is `ˈklɛ.o`, all
  previously wrong (`ˈbor.das`, `ˈve.ras`, `ˈkle.o`)

- `add_lex_*()` now warns when an IPA-override value looks like orthography
  rather than IPA (the mistake that produced the four entries above), naming
  the offending value and pointing at the diacritized-form call that was
  probably intended. The entry is still stored: it is a warning, not an error

## Bug fixes

- Source checkouts were misdetected as installed packages once the vignettes
  had been built, because the test looked for a `Meta/` directory that vignette
  building creates in the source tree. The check now reads the `Built` field of
  `DESCRIPTION`
- Lexicon data is read from the package namespace rather than the attached
  search path, so the transcription functions also work when the package is
  used with `::` and never attached

# Fonology 1.4.0

## Console output rewritten with cli

Every message, warning and error in the package now goes through
[cli](https://cli.r-lib.org). Output is colour-coded and wrapped to the width of
the terminal, values are quoted, and paths are clickable in editors that support
it.

- Errors name the offending value instead of describing it in the abstract:
  `getFeat("i", "klingon")` now reports the language it received and lists the
  ones available, and a `words`/`ipa` length mismatch in `add_lex_*()` reports
  both lengths
- Errors raised inside internal helpers are attributed to the function the user
  actually called (`getFeat()`, `getPhon()`) rather than to the helper
- The `ipa_pt_test()`, `ipa_fr_test()`, `ipa_sp_test()`, `ipa_it_test()` and
  `ipa_en_test()` demos print an aligned word-to-transcription list under a
  section header, replacing the previous row of `=` separators
- `ipa2tipa()` and `ipa2typst()` print their output as a verbatim code block

### Compatibility

- Functions that returned `NA` after printing a message still return `NA`; only
  the appearance of the message changed
- cli messages are still R conditions of class `message`, so `suppressMessages()`
  and `testthat::expect_message()` behave as before

## Bug fixes

- Four messages in `plotnGrams()` and `nGramTbl()` told users to run
  `nGram_tbl()`, which does not exist; the function is `nGramTbl()`
- Fixed a typo in the `biGram_pt()` input check ("Input most be phonemic")

## Other

- `cli` added to Imports; `glue` removed (it was used in one place, and cli
  interpolates strings natively)
- `ipa2tipa()` now returns the TeX string invisibly; it previously returned
  `NULL`. `ipa_XX_test()` functions return their transcriptions invisibly

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
