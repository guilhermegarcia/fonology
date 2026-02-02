#' Phonokit IPA translator
#'
#' Translates a phonemically transcribed sequence (IPA Unicode) into
#' \href{https://github.com/guilhermegarcia/phonokit}{phonokit} notation
#' for use in Typst documents.
#' Any token that requires a backslash command is automatically
#' surrounded by spaces, as required by phonokit's parser.
#' @param string The phonemically transcribed sequence from a function such as \code{ipa()}
#' @param pre Prefix for transcription. Defaults to \code{'#ipa("'}
#' @param post Suffix for transcription. Defaults to \code{'")'}
#' @return A string with phonokit notation (printed via \code{cat()} and returned invisibly)
#' @examples
#' "bonito" |> ipa(lg = "Portuguese") |> ipa2typst()
#' "chuva" |> ipa(lg = "Portuguese") |> ipa2typst()
#' @export

ipa2typst <- function(string, pre = '#ipa("', post = '")') {

  # ── Step 1: Normalize Unicode (NFD) ──
  # NFD decomposes precomposed characters (e.g., ã → a + combining tilde)
  # so that combining diacritics are always separate for matching.
  ipa <- stringi::stri_trans_nfd(string)

  # ── Step 2: Build the reverse mapping (Unicode → phonokit code) ──
  # Based on phonokit/ipa.typ mappings.
  # Order matters: multi-character sequences first (longest match).

  # Multi-char mappings (affricates with tie bar, nasalized vowels, etc.)
  multi_map <- list(
    # Affricates with combining tie bar (U+0361)
    "t\u0361\u0283" = "\\t tS",
    "d\u0361\u0292" = "\\t dZ",
    "t\u0361s"      = "\\t ts",
    "d\u0361z"      = "\\t dz",
    # Nasalized vowels (combining tilde U+0303)
    "a\u0303" = "\\~ a",
    "e\u0303" = "\\~ e",
    "i\u0303" = "\\~ i",
    "o\u0303" = "\\~ o",
    "u\u0303" = "\\~ u",
    "w\u0303" = "\\~ w",
    "j\u0303" = "\\~ j",
    "\u025b\u0303" = "\\~ E",
    "\u0254\u0303" = "\\~ O",
    "\u0251\u0303" = "\\~ A",
    "\u0153\u0303" = "\\~ \\oe",
    # Devoiced (combining ring below U+0325)
    "\u026a\u0325" = "\\r I"
  )

  # Single-char mappings (Unicode → phonokit code)
  single_map <- list(
    # Consonants - Plosives
    "p" = "p",
    "b" = "b",
    "t" = "t",
    "d" = "d",
    "\u0288" = "\\:t",     # ʈ
    "\u0256" = "\\:d",     # ɖ
    "c" = "c",
    "\u025f" = "\\barredj", # ɟ
    "k" = "k",
    "\u0261" = "g",        # ɡ (IPA g)
    "g" = "g",             # regular g
    "q" = "q",
    "\u0262" = "\\;G",     # ɢ
    "\u0294" = "P",        # ʔ
    # Consonants - Nasals
    "m" = "m",
    "\u0271" = "M",        # ɱ
    "n" = "n",
    "\u0273" = "\\:n",     # ɳ
    "\u0272" = "\\textltailn", # ɲ
    "\u014b" = "N",        # ŋ
    "\u0274" = "\\;N",     # ɴ
    # Consonants - Trills
    "\u0299" = "\\;B",     # ʙ
    "r" = "r",
    "\u0280" = "\\;R",     # ʀ
    # Consonants - Tap/Flap
    "\u027e" = "R",        # ɾ
    "\u027d" = "\\:r",     # ɽ
    # Consonants - Fricatives
    "f" = "f",
    "v" = "v",
    "\u0278" = "F",        # ɸ
    "\u03b2" = "B",        # β
    "\u03b8" = "T",        # θ
    "\u00f0" = "D",        # ð
    "s" = "s",
    "z" = "z",
    "\u0283" = "S",        # ʃ
    "\u0292" = "Z",        # ʒ
    "\u0282" = "\\:s",     # ʂ
    "\u0290" = "\\:z",     # ʐ
    "\u00e7" = "C",        # ç
    "\u029d" = "J",        # ʝ
    "x" = "x",
    "\u0263" = "G",        # ɣ
    "\u03c7" = "X",        # χ
    "\u0281" = "K",        # ʁ
    "\u0127" = "\\barredh", # ħ
    "\u0295" = "Q",        # ʕ
    "h" = "h",
    "\u0266" = "H",        # ɦ
    # Consonants - Lateral Fricatives
    "\u026c" = "\\textbeltl", # ɬ
    "\u026e" = "\\l3",     # ɮ
    # Consonants - Approximants
    "\u028b" = "V",        # ʋ
    "\u0279" = "\\*r",     # ɹ
    "j" = "j",
    "\u0270" = "\\mw",     # ɰ
    "\u027b" = "\\:R",     # ɻ
    # Consonants - Lateral Approximants
    "l" = "l",
    "\u026d" = "\\:l",     # ɭ
    "\u028e" = "L",        # ʎ
    "\u029f" = "\\;L",     # ʟ
    # Velarized l
    "\u026b" = "\\darkl",  # ɫ
    # Other consonants
    "\u0298" = "\\!o",     # ʘ (click)
    "\u01c2" = "\\doublebarpipe", # ǂ
    "\u01c1" = "||",       # ǁ
    "\u02a1" = "\\barredP", # ʡ
    "\u0253" = "\\!b",     # ɓ
    "\u0257" = "\\!d",     # ɗ
    "\u0284" = "\\!j",     # ʄ
    "\u0260" = "\\!g",     # ɠ
    "\u029b" = "\\!G",     # ʛ
    "\u028d" = "\\*w",     # ʍ
    "\u0267" = "\\texththeng", # ɧ
    "\u029c" = "\\;H",     # ʜ
    "\u02a2" = "\\barrevglotstop", # ʢ
    "\u027a" = "\\turnlonglegr", # ɺ
    "\u0265" = "4",        # ɥ
    # Vowels - Close
    "i" = "i",
    "\u026a" = "I",        # ɪ
    "y" = "y",
    "\u028f" = "Y",        # ʏ
    "\u0268" = "1",        # ɨ
    "\u0289" = "0",        # ʉ
    "\u026f" = "W",        # ɯ
    "u" = "u",
    "\u028a" = "U",        # ʊ
    # Vowels - Close-mid/Mid
    "e" = "e",
    "\u00f8" = "\\o",      # ø
    "\u0258" = "9",        # ɘ
    "\u0275" = "8",        # ɵ
    "\u0264" = "7",        # ɤ
    "o" = "o",
    # Vowels - Mid
    "\u0259" = "@",        # ə
    # Vowels - Open-mid
    "\u025b" = "E",        # ɛ
    "\u0153" = "\\oe",     # œ
    "\u025c" = "3",        # ɜ
    "\u025e" = "\\closeepsilon", # ɞ
    "\u028c" = "2",        # ʌ
    "\u0254" = "O",        # ɔ
    # Vowels - Near-open/Open
    "\u00e6" = "\\ae",     # æ
    "\u0276" = "\\OE",     # ɶ
    "a" = "a",
    "\u0250" = "5",        # ɐ
    "\u0251" = "A",        # ɑ
    "\u0252" = "6",        # ɒ
    "\u025a" = "\\schwar", # ɚ
    "\u025d" = "\\epsilonr", # ɝ
    # Suprasegmentals
    "\u02c8" = "'",        # ˈ primary stress
    "\u02cc" = ",",        # ˌ secondary stress
    "\u02d0" = ":",        # ː length
    "." = ".",             # syllable boundary
    # Backward diacritics (as independent characters)
    "\u02b0" = "\\h",      # ʰ aspiration
    "\u02b2" = "\\palatal", # ʲ palatalized
    "\u02b7" = "\\labial"  # ʷ labialized
  )

  # ── Step 3: Character-by-character conversion with longest match ──
  # Convert to character vector, preserving combining sequences
  chars <- strsplit(ipa, "")[[1]]
  n <- length(chars)
  output_tokens <- character(0)
  i <- 1

  while (i <= n) {
    matched <- FALSE

    # Try multi-char matches (up to 3 characters ahead)
    for (len in c(3, 2)) {
      if (i + len - 1 <= n) {
        candidate <- paste0(chars[i:(i + len - 1)], collapse = "")
        if (candidate %in% names(multi_map)) {
          output_tokens <- c(output_tokens, multi_map[[candidate]])
          i <- i + len
          matched <- TRUE
          break
        }
      }
    }

    if (!matched) {
      ch <- chars[i]
      if (ch %in% names(single_map)) {
        output_tokens <- c(output_tokens, single_map[[ch]])
      } else if (ch == " ") {
        output_tokens <- c(output_tokens, "\\s")
      } else {
        # Pass through unknown characters
        output_tokens <- c(output_tokens, ch)
      }
      i <- i + 1
    }
  }

  # ── Step 4: Build output with spacing around backslash tokens ──
  result <- ""
  for (k in seq_along(output_tokens)) {
    tok <- output_tokens[k]
    has_backslash <- grepl("\\\\", tok)

    if (has_backslash) {
      # Ensure space before backslash token
      if (nchar(result) > 0 && !endsWith(result, " ")) {
        result <- paste0(result, " ")
      }
      result <- paste0(result, tok)
      # Ensure space after backslash token
      if (k < length(output_tokens)) {
        result <- paste0(result, " ")
      }
    } else {
      result <- paste0(result, tok)
    }
  }

  # Clean up any double spaces
  result <- gsub("  +", " ", result)

  # Double backslashes for Typst (Typst requires \\ for a literal backslash)
  result <- gsub("\\", "\\\\", result, fixed = TRUE)

  # Add pre/post
  result <- paste0(pre, result, post)

  message("Done! Here's your phonokit code:")
  cat(result)
  return(invisible(result))
}
