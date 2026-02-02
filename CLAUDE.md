# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Git Policy

- **Do NOT add Co-Authored-By lines** to commits or releases.

## Package Overview

Fonology is an R package for phonological analysis, providing tools for:
- Grapheme-to-phoneme conversion (IPA transcription) for Portuguese, French, and Spanish
- Distinctive feature analysis (phonemes ↔ features)
- Probabilistic grammars (MaxEnt, Noisy Harmonic Grammar)
- Sonority analysis and visualization
- Nonce word generation (Portuguese)

## Development Commands

```bash
# Generate documentation from roxygen2 comments
roxygen2::roxygenise()

# Run package checks (0 errors, 1 warning for non-ASCII in pt_lex, 0 notes expected)
devtools::check()

# Build the package
devtools::build()

# Load package for interactive development
devtools::load_all()
```

## Key Development Notes

- **README updates**: Edit `README.Rmd`, then run `rmarkdown::render("README.Rmd")` and delete the generated HTML. Do not edit `README.md` directly.
- **New exported functions**: Add `export(functionName)` to NAMESPACE via roxygen2 `@export` tag
- **Non-ASCII characters**: Package uses extensive IPA symbols. Use `toUTF.R` script to convert back to UTF if needed. The "UNISCAPE" addin handles ASCII conversion.
- **GitHub LFS**: Do NOT use LFS for `.rda` data files—it breaks `install_github()`. Keep data files in normal Git tracking.

## Architecture

### IPA Transcription Pipeline

The `ipa()` function in `R/ipa.R` is the main entry point, dispatching to language-specific pipelines:

```
ipa(word, lg) → ipa_pt_vec() / ipa_sp() / ipa_fr()
                     ↓
              transcribe_XX() → syllabify_XX() → stress_XX()
                     ↓ (Portuguese only, if narrow=TRUE)
              narrow_pt()
```

**Portuguese** (`R/IPA_pt.R`, `R/transcribe_pt.R`, `R/syllabify_pt.R`, `R/stress_pt.R`):
- Real words: Looked up in `pt_lex` dataset for stress assignment
- Nonce words: Uses probabilistic stress based on weight profiles and lexical statistics
- Narrow transcription adds: vowel reduction, palatalization, l-vocalization, epenthesis, secondary stress

**Spanish** (`R/IPA_sp.R`, `R/transcribe_sp.R`, `R/syllabify_sp.R`, `R/stress_sp.R`)

**French** (`R/IPA_fr.R`, `R/transcribe_fr.R`, `R/syllabify_fr.R`)

### Distinctive Features

`getFeat()` (`R/phonemes_to_features.R`) and `getPhon()` (`R/features_to_phonemes.R`) work with the `allFeatures` dataset (based on PanPhon). They compute minimal feature matrices for natural classes given a phonemic inventory.

### Probabilistic Grammars

- `maxent()` (`R/maxent.R`): Learns constraint weights from tableau data using L-BFGS-B optimization with optional Gaussian priors
- `nhg()` and `plotNhg()` (`R/nhg.R`): Noisy Harmonic Grammar simulation

### Key Datasets (in `data/`)

- `psl` / `pt_lex`: Portuguese Stress Lexicon (~154K non-verbs with phonological coding)
- `pt_freq`: Portuguese frequency corpus (~60K words)
- `allFeatures`: Comprehensive distinctive features table
- `bigrams_pt`: Bigram probabilities for Portuguese
- `vowels_XX` / `consonants_XX`: Phonemic inventories per language

## Function Naming Conventions

- `_pt`, `_sp`, `_fr` suffixes: Language-specific functions
- `_vec` suffix: Vectorized versions (accept multiple words)
- Functions without language suffix are language-independent (e.g., `getSyl()`, `getWeight()`, `countSyl()`)

---

## Change Log

All changes made during Claude Code sessions are documented here.

### 2026-02-01

**Added `ipa2typst()` in `R/ipa2typst.R`**
- New exported function that converts IPA Unicode strings to [phonokit](https://github.com/guilhermegarcia/phonokit) notation for Typst documents.
- Reverse-maps the full phonokit `ipa.typ` character set (170+ symbols: consonants, vowels, suprasegmentals, diacritics, affricates).
- Uses NFD normalization so precomposed characters (e.g., ã, õ) are decomposed before matching, ensuring combining diacritics are handled consistently.
- Automatically inserts spaces around any backslash-prefixed token, as required by phonokit's space-delimited parser.
- Mirrors the interface of `ipa2tipa()` with `pre`/`post` parameters for delimiters.

### 2026-01-20

**Fixed `getWeight_pt()` in `R/weight_pt_vec.R`**
- **Issue**: Weight profiles contained residual characters (e.g., "HɛrLH" instead of "HLH") because `\\w` regex only matches ASCII characters, not IPA symbols like ɛ, ɔ, ɾ, ʃ, ʒ.
- **Fix**: Replaced complex regex replacement approach with a simpler per-syllable check: if syllable ends in a vowel → L (light), otherwise → H (heavy). This correctly handles all IPA characters.

**Fixed scientific notation bug in `stress_pt_vec()` and `stress_sp.R`**
- **Issue**: Words at position ≥100,000 returned NA because `as.character(100000)` produces `"1e+05"` (scientific notation), but the stored name is `"100000"` - the lookup failed.
- **Fix**: Changed `as.character(sort(...))` to `format(sort(...), scientific = FALSE, trim = TRUE)` to preserve exact integer representation. Applied to both Portuguese and Spanish stress functions.

**Added NA handling to `getWeight_pt()`**
- Defensive fix: gracefully returns NA for invalid input rather than crashing, in case upstream functions produce unexpected NA values.

**Updated `.Rbuildignore`**
- Added `.claude`, `CLAUDE.md`, and `CITATION.cff` to suppress package check notes for non-standard top-level files.
