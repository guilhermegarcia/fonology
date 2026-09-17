# Fonology 1.7.1

This release makes Portuguese transcription phonemic with respect to rhotics,
gives taps and trills distinct feature matrices, makes `cleanText()`'s handling
of hyphens explicit, rebuilds the Portuguese data from reproducible scripts,
and fixes a number of bugs in the Portuguese transcriber. The rhotic change is
breaking: broad transcriptions of Portuguese now write the strong rhotic as `r`
and the tap as `ɾ`.

## Portuguese rhotics are now phonemic (breaking)

Broad Portuguese transcription now writes the two contrastive rhotic phonemes:
the strong rhotic as `r` and the tap as `ɾ`. Up to 1.7.0 it wrote the strong
rhotic with one of its allophones, `x`, and the tap as `r`. This aligns
Portuguese with the package's Spanish transcriptions, which already used `r`
and `ɾ`, and with the standard phonemic analysis of the language (e.g. Massini-
Cagliari and Zampaulo 2024, Tables 1.1 and 4.1).

| | 1.7.0 | 1.7.1 broad | 1.7.1 narrow |
|---|---|---|---|
| *rato*, *carro*, *honra* | `ˈxa.to`, `ˈka.xo`, `ˈon.xa` | `ˈra.to`, `ˈka.ro`, `ˈon.ra` | `ˈxa.tʊ`, `ˈka.xʊ`, `ˈõn.xa` |
| *caro*, *prato*, *porta* | `ˈka.ro`, `ˈpra.to`, `ˈpɔr.ta` | `ˈka.ɾo`, `ˈpɾa.to`, `ˈpɔɾ.ta` | `ˈka.ɾʊ`, `ˈpɾa.tʊ`, `ˈpɔɾ.ta` |

- **The change leaves narrow output as it was**: `ipa(narrow = TRUE)` still
  realises the strong rhotic as [x] and the tap as [ɾ]. The [x] realisation moved from broad to
  narrow transcription, where allophones belong.
- **Verified against 1.7.0**, with the rhotic change applied on its own (before
  the bug fixes below), on 167,596 words (40,000 through the PSL lookup,
  127,596 through the regex fallback). Rewriting the new broad output back into
  the old convention reproduces the old output exactly for every word except
  four lookup entries covered by the data corrections below; narrow output is
  byte-identical apart from the same four. `wug_pt()` output, under a fixed
  seed, is identical after the symbol change (2,400 words).
- **Orthographic x is untouched.** The fallback emitted the strong rhotic as
  `x`, the same symbol an orthographic x takes when the grapheme rules miss it
  (e.g. *conexão*). The strong rhotic is now emitted as an internal placeholder
  and rewritten only at the end of the pipeline, so no orthographic x can be
  turned into a rhotic.
- Weight and stress are unaffected: `getWeight()`, `getStress()` and
  `countSyl()` treat `r`, `ɾ` and `x` alike.
- **Inventory.** `x` is removed from `consonants_pt`: [x] is an allophone of
  /r/, not a phoneme. The strong rhotic is a liquid, and sonority computations
  now treat it as one. `getFeat("x", lg = "pt")` reports `x` as outside the
  inventory, and the Portuguese fricatives are now described as `+strid`
  (every one of them is strident) rather than `-son, +cont`.
- `.is_pt_broad_ipa()` treats [x] as a surface symbol, alongside the other
  rhotic allophones it already listed.
- User IPA lexicons (`add_lex_pt()` with IPA) written for 1.7.0 bypass the
  transcriber and keep the old convention; they should be updated by hand.

## Distinctive features: taps and trills

- `allFeatures` gains two columns, `tap` and `trill`, following Hayes (2009),
  whose table ships as `features_Hayes_2009`. PanPhon has no feature separating
  taps from trills, so `r` and `ɾ` (and `ʀ`, `ʙ`, `ɽ`, `ɺ`) had identical
  matrices and neither `r` nor `ɾ` was a natural class on its own, in
  Portuguese or in Spanish. Now `getFeat("r", "pt")` is `+trill` and
  `getFeat("ɾ", "pt")` is `+tap`, and `getPhon()` accepts both features.
  Diacritic and length variants (`r̥`, `rː`, `ɾ̃`) inherit the value of their
  base. The columns are added last, so they never displace an existing
  description of the same size in `getFeat()`; the first 26 columns are
  unchanged, and every existing feature test passes except the two that
  assumed [x] was in the Portuguese inventory. Built by
  `data-raw/build_allFeatures.R`.

## `cleanText()`: hyphen handling is now explicit

`cleanText()` gains two arguments. **Default output is unchanged**: verified
identical on all 211,618 wordforms of SUBTLEX-BR and SUBTLEX-PT.

- `hyphen` controls what happens to hyphenated words: `"split"` (default, the
  historical behaviour) treats a hyphen as a token boundary; `"join"` removes
  it and returns one token; `"keep"` leaves it in place. Hyphenated words in
  Portuguese span verb+enclitic (`diz-me`), derivational prefixes
  (`anti-aborto`) and lexicalised compounds (`guarda-chuva`), which call for
  different treatment, and the last two cannot be told apart automatically.
  Note that `"join"` usually produces out-of-vocabulary forms, so `ipa()` falls
  back to its heuristic.
- `clitics` (default `FALSE`) removes a hyphenated Portuguese enclitic before
  hyphens are handled, so `cleanText("diz-me", clitics = TRUE)` is `"diz"`. Off
  by default because dropping the clitic removes a syllable from the prosodic
  word.

## Portuguese data

- **`pt_lex` is reproducible.** It is now built by `data-raw/build_pt_lex.R` as
  the distinct word/pronunciation pairs of `psl`, converted to IPA by the new
  internal `psl_to_ipa()`. No build script existed before; the conversion was
  reconstructed from the psl/pt_lex alignment and, in legacy mode, reproduces
  the 1.7.0 object row for row except for 16 rows, all corrected: 12 where a
  nasal diphthong was written `ãj` instead of `ãj̃` (inconsistent with `ãw̃`),
  and 4 for three PSL entries (*xênico*, *xona*, *xucro*) whose transcription
  contained the orthographic letter x in place of /ʃ/. The documented row count
  is corrected from 154,610 to 128,854.
- **`bigrams_pt` is reproducible**, built from `pt_lex` by
  `data-raw/build_bigrams_pt.R`. Differences from 1.7.0 are limited to the
  rhotic symbols and the bigrams touched by the `pt_lex` corrections.
- **`pt_freq`** is migrated by `data-raw/convert_pt_freq_rhotics.R`, which
  refuses to run twice. Its `x` was either the strong rhotic or an orthographic
  x copied through by the transcriber that produced the list (*máximo* →
  `ˈma.xi.mo`); the script matches each x against the orthography and rewrites
  only rhotics, leaving the 84 orthographic x's as they were. `apu`, `pu` and
  `u` are regenerated from the converted transcription.
- **`psl` gains IPA columns.** The 62 original columns keep the PSL notation;
  21 columns with the suffix `.ipa` are appended, one per segmental column. In
  the coda columns a nasal coda is written `N`, the nasal archiphoneme. Built by
  `data-raw/build_psl_ipa.R`. `data/psl.rda` grows from 7.6 to 10.9 MB.
- **`psl$stemPro` follows its definition**, `proU` minus the final phoneme, on
  all 154,610 rows. 6,587 entries carried syllable boundaries and stress, and
  449 oxytones ending in a nasal kept the whole of `proU`, although the final
  segment there is the nasal coda (`abiã`: `abia~` is now `abia`). One `proU`
  entry (*dresslerela*) carried a stress mark and is fixed.

## Bug fixes

### Portuguese transcription

- **Orthographic x no longer surfaces as `x`.** In the regex fallback, x was
  left untranscribed in several contexts: when two intervocalic x's were close
  (`araxixá`: the first match consumed the vowel the second needed), before a
  nasal vowel (`caxão`), after a consonant (`aljorxe`), and before consonants
  missing from the rule (`exfoliar`, `axl`, `exchange`). Contexts are now
  matched with lookarounds, nasal vowels count as vowels, x before any
  consonant letter is `s`, before `w` it is `ks`, and any x left over defaults
  to `ʃ`, the majority outcome in the PSL. Across the 3,439 PSL words spelled
  with x, the fallback no longer emits `x` for any of them. Words whose x is
  lexically `ks` and not covered by the PSL (`conexão`, `flexão`, `táxi`) get
  the default `ʃ`.
- **`ipa()` removes hyphenated enclitics.** It stripped punctuation before
  calling the clitic stripper, which needs the hyphen, so enclitics were never
  removed: `ipa("diz-me")` is now `ˈdis*`, not `ˈdiz.me*`.
- **Spelled `ãi`** is transcribed as the nasal diphthong `ãj̃`, like `ãe`
  (`cãibras`), and **`ñ`** as `ɲ` (`señor`).
- **`biGram_pt()` accepted no input containing `w`**: it was on the list of
  letters taken to indicate orthography, so any transcription with the glide
  /w/ (`ˈnãw̃`, `ˈkwa.dɾo`) returned `NA`.
- **Narrow transcription of e + nasal.** The rule dropped the nasal consonant
  and inserted `ɲ` in every context (`tempo` → `ˈtẽj̃ɲ.pʊ`), affecting 24,071
  of 170,000 words tested. The vowel is now nasalised with the nasal kept, as
  for the other vowels, and diphthongised only word-finally (`bem` → `ˈbẽj̃`,
  `homens` → `ˈo.mẽj̃s`, `tempo` → `ˈtẽm.pʊ`).
- **`transcribe_pt()`**, which is exported, was a separate scalar copy of the
  transcriber that had drifted from the one `ipa()` uses (it missed some nasal
  diphthongs and kept the old rhotics). It now delegates to that transcriber.
- Removed the unused internal functions `narrow_pt()` and `sec_stress_pt()`.

### `cleanText()` and clitic removal

- `cleanText()` had separate code paths for length-1 and longer input, with
  different policies for degenerate tokens: dropped at length 1, returned as
  `NA` otherwise. The paths are merged and such tokens are always dropped, so
  the result never contains `NA`. This also stops `nGramTbl()` from receiving
  the literal string `"NA"` as a token. This is the only change to the default
  output of `cleanText()`.
- Hyphens are now handled before punctuation stripping. Previously
  `[:punct:]` removal would have deleted them, which is why `"keep"` needs the
  new ordering to work at all.
- `strip_clitic_pt()` (internal): the alternation was ordered shortest-first,
  so `-los` matched as `-lo` and left a stranded consonant (`"vê-los"` became
  `"vês"`). Alternatives are now ordered longest-first and the pattern is
  anchored to the end of the token.
- `strip_clitic_pt()` now covers the full enclitic paradigm. The old list held
  twelve forms and matched 40% of enclitic occurrences in ~18h of transcribed
  speech per variety; the two largest gaps were `-se` (2,762 occurrences) and
  `-nos` (1,401), and `-se` is the most frequent clitic in the language. Added:
  the reflexive `-se`, the dative `-nos`/`-vos`/`-lhes`, the accusative
  allomorphs after a nasal diphthong (`-no`, `-na`, `-nos`, `-nas`) and the
  contracted dative+accusative forms (`-mo`, `-ma`, `-to`, `-ta`, `-lho`,
  `-lha`, and their plurals). Coverage is now complete on those data.
- `strip_clitic_pt()` dropped `-ão`, which is a verb ending rather than a
  clitic, matched nothing in either corpus, and invited confusion with the
  mesoclitic future (`far-lhe-ão`).
- `strip_clitic_pt()` removes a whole sequence of trailing enclitics in one
  pass, so `"parece-se-me"` returns `"parece"` rather than `"parece-se"`, and
  never returns an empty string: a token made only of clitics comes back
  unchanged. The package has no morphological analyser, so the function cannot
  tell a clitic from a compound whose final element is spelled like one; this
  is documented, and is why `cleanText()` applies it only on request (`ipa()`
  applies it, as it was always meant to; see above). Mesoclisis
  (`dar-se-á`) is explicitly out of scope, being vanishingly rare in the data.
- `strip_clitic_pt()` also carried a list of hyphenated prefixes (`anti-`,
  `ex-`, `pré-`, `além-`, …). That list was unreachable, because `cleanText()`
  replaced hyphens with spaces before calling it. It has been removed rather
  than switched on: applying it would delete real derivational material from
  962 of the 2,964 hyphenated forms in SUBTLEX-PT (`anti-aborto` → `aborto`,
  `além-mar` → `mar`) while affecting no enclitics at all, since that corpus
  lists none.
- Documented that `cleanText()` returns a token vector whose length need not
  match its input, and is therefore unsafe inside `dplyr::mutate()`.

# Fonology 1.7.0

## Audited Portuguese correction complement

- Added 154 audited Wiktionary-derived corrections behind the Portuguese PSL
  lookup, raising token lookup coverage from 25.5% to 28.0% while retaining
  broad/phonemic output and the existing regex fallback
- Added a reproducible correction audit, exhaustive broad-IPA tests, and
  component-level CC BY-SA 4.0 attribution for derived lexical data

# Fonology 1.6.1

## Namespace-only use

- Data-backed functions now work through `Fonology::` without first attaching
  the package, including transcription in all five languages, feature lookup,
  syllable weight and Portuguese bigram probabilities

# Fonology 1.6.0

## Consistent citation-form stress

- Spanish lexicon-backed transcriptions now receive primary stress when the
  source pronunciation omits it, matching the token-level convention already
  used by the Spanish and Portuguese fallback pipelines (for example,
  `como` → `ˈko.mo` and `las` → `ˈlas`)

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
