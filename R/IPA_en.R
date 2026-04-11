#' IPA transcriber for English
#'
#' Given a string, the function returns its IPA transcription with stress and
#' syllabification. English support is primarily CMU-backed; user IPA overrides
#' take precedence, and out-of-vocabulary forms receive a best-effort fallback
#' transcription marked with \code{"*"}.
#' @param word A possible string in English in its orthographic form
#' @return The phonemic transcription for the string in question
#' @noRd

.en_cache <- new.env(parent = emptyenv())

.get_en_default_lex <- function() {
  if (!exists("default_lex", envir = .en_cache, inherits = FALSE)) {
    default_lex <- en_lex[en_lex$alt == 0L, c("word", "ipa_syll")]
    default_lex <- default_lex[!duplicated(default_lex$word), ]

    assign("default_lex", default_lex, envir = .en_cache)
  }

  get("default_lex", envir = .en_cache, inherits = FALSE)
}

.ipa_vowels_en <- c("e\u026a", "o\u028a", "a\u028a", "\u0254\u026a", "a\u026a", "i", "u", "\u025b", "\u026a", "\u0251", "\u028c", "\u00e6", "\u0259", "\u025a", "\u028a", "\u0254")
.ipa_multiseg_en <- c("t\u0283", "d\u0292", .ipa_vowels_en)

.legal_medial_onsets_ipa_en <- c(
  "p", "t", "k", "b", "d", "g", "f", "v", "\u03b8", "\u00f0", "\u0283", "t\u0283", "d\u0292",
  "h", "m", "n", "l", "\u0279", "w", "j", "z", "s",
  "p \u0279", "t \u0279", "k \u0279", "b \u0279", "d \u0279", "g \u0279", "f \u0279", "\u03b8 \u0279", "\u0283 \u0279",
  "p l", "k l", "b l", "g l", "f l", "s l",
  "s p l", "s k l", "s p \u0279", "s t \u0279", "s k \u0279"
)

.tokenize_ipa_en <- function(x) {
  tokens <- character()
  i <- 1L
  n <- nchar(x, type = "chars")

  while (i <= n) {
    rest <- substr(x, i, n)
    matched <- NULL

    for (seg in .ipa_multiseg_en) {
      if (startsWith(rest, seg)) {
        matched <- seg
        break
      }
    }

    if (is.null(matched)) {
      matched <- substr(x, i, i)
    }

    tokens <- c(tokens, matched)
    i <- i + nchar(matched, type = "chars")
  }

  tokens
}

.is_legal_medial_onset_ipa_en <- function(cluster) {
  if (length(cluster) == 0L) return(TRUE)
  paste(cluster, collapse = " ") %in% .legal_medial_onsets_ipa_en
}

.split_medial_cluster_ipa_en <- function(cluster) {
  if (length(cluster) == 0L) {
    return(list(coda = character(), onset = character()))
  }

  for (i in seq_len(length(cluster))) {
    onset <- cluster[i:length(cluster)]
    if (.is_legal_medial_onset_ipa_en(onset)) {
      return(list(
        coda = if (i == 1L) character() else cluster[seq_len(i - 1L)],
        onset = onset
      ))
    }
  }

  list(coda = cluster[-length(cluster)], onset = cluster[length(cluster)])
}

.syllabify_fallback_ipa_en <- function(x) {
  tokens <- .tokenize_ipa_en(x)
  vowels <- tokens %in% .ipa_vowels_en
  vowel_idx <- which(vowels)

  if (length(vowel_idx) <= 1L) {
    return(x)
  }

  syllables <- vector("list", length(vowel_idx))

  if (vowel_idx[1] > 1L) {
    syllables[[1]] <- c(syllables[[1]], tokens[seq_len(vowel_idx[1] - 1L)])
  }
  syllables[[1]] <- c(syllables[[1]], tokens[vowel_idx[1]])

  for (i in seq_len(length(vowel_idx) - 1L)) {
    left_vowel <- vowel_idx[i]
    right_vowel <- vowel_idx[i + 1L]

    if ((left_vowel + 1L) > (right_vowel - 1L)) {
      syllables[[i + 1L]] <- c(syllables[[i + 1L]], tokens[right_vowel])
      next
    }

    bridge <- tokens[seq.int(left_vowel + 1L, right_vowel - 1L)]
    split <- .split_medial_cluster_ipa_en(bridge)

    if (length(split$coda) > 0L) {
      syllables[[i]] <- c(syllables[[i]], split$coda)
    }
    if (length(split$onset) > 0L) {
      syllables[[i + 1L]] <- c(syllables[[i + 1L]], split$onset)
    }

    syllables[[i + 1L]] <- c(syllables[[i + 1L]], tokens[right_vowel])
  }

  if (vowel_idx[length(vowel_idx)] < length(tokens)) {
    trailing <- tokens[seq.int(vowel_idx[length(vowel_idx)] + 1L, length(tokens))]
    syllables[[length(syllables)]] <- c(syllables[[length(syllables)]], trailing)
  }

  vapply(syllables, paste0, collapse = "", character(1)) |>
    paste(collapse = ".")
}

.collapse_double_consonants_ipa_en <- function(x) {
  tokens <- .tokenize_ipa_en(x)

  if (length(tokens) <= 1L) {
    return(x)
  }

  out <- tokens[1]

  for (tok in tokens[-1]) {
    prev <- out[length(out)]
    is_double_consonant <- identical(tok, prev) && !(tok %in% .ipa_vowels_en)

    if (!is_double_consonant) {
      out <- c(out, tok)
    }
  }

  paste0(out, collapse = "")
}

.simple_vowel_map_en <- function(chars) {
  chars <- chars |>
    stringr::str_replace_all("ee", "i") |>
    stringr::str_replace_all("oo", "u") |>
    stringr::str_replace_all("ai", "e\u026a") |>
    stringr::str_replace_all("ay", "e\u026a") |>
    stringr::str_replace_all("oa", "o\u028a") |>
    stringr::str_replace_all("ow", "a\u028a") |>
    stringr::str_replace_all("ou", "a\u028a") |>
    stringr::str_replace_all("oi", "\u0254\u026a") |>
    stringr::str_replace_all("oy", "\u0254\u026a") |>
    stringr::str_replace_all("au", "\u0254") |>
    stringr::str_replace_all("ea", "i") |>
    stringr::str_replace_all("ie", "i") |>
    stringr::str_replace_all("e", "\u025b") |>
    stringr::str_replace_all("i", "\u026a") |>
    stringr::str_replace_all("o", "\u0251") |>
    stringr::str_replace_all("u", "\u028c") |>
    stringr::str_replace_all("a", "\u00e6")

  chars
}

.preprocess_fallback_en <- function(x) {
  consonant <- "[bcdfghjklmnpqrstvwxyz]"

  x <- x |>
    stringr::str_replace("e$", "") |>
    stringr::str_replace("^y(?=[aeiou])", "__YGLIDE__") |>
    stringr::str_replace("^sy(?=[^aeiou])", "s__YSHORT__") |>
    stringr::str_replace("^ty(?=[^aeiou])", "t__YLONG__") |>
    stringr::str_replace_all(paste0("(", consonant, ")y(?=", consonant, "{2,}|$)"), "\\1__YSHORT__") |>
    stringr::str_replace_all("y$", "i")

  x
}

.fallback_to_ipa_en <- function(word) {
  if (is.na(word) || !nzchar(stringr::str_trim(word))) {
    return(NA_character_)
  }

  x <- stringr::str_to_lower(word) |>
    stringr::str_replace_all("[^a-z']", "")

  if (!nzchar(x)) return(NA_character_)

  x <- .preprocess_fallback_en(x)

  replacements <- c(
    "tion" = "\u0283\u0259n",
    "ture" = "t\u0283\u028a\u0279",
    "ough" = "\u028cf",
    "eigh" = "e\u026a",
    "igh" = "a\u026a",
    "ph" = "f",
    "sh" = "\u0283",
    "ch" = "t\u0283",
    "th" = "\u03b8",
    "dh" = "\u00f0",
    "ng" = "\u014b",
    "ck" = "k",
    "qu" = "kw",
    "wh" = "w",
    "wr" = "r",
    "kn" = "n",
    "x" = "ks",
    "c" = "k",
    "q" = "k"
  )

  for (pattern in names(replacements)) {
    x <- stringr::str_replace_all(x, stringr::fixed(pattern), replacements[[pattern]])
  }

  x <- .simple_vowel_map_en(x)

  consonants <- c(
    "b" = "b", "d" = "d", "f" = "f", "g" = "g", "h" = "h",
    "j" = "d\u0292", "k" = "k", "l" = "l", "m" = "m", "n" = "n",
    "p" = "p", "r" = "\u0279", "s" = "s", "t" = "t", "v" = "v",
    "w" = "w", "z" = "z"
  )

  for (pattern in names(consonants)) {
    x <- stringr::str_replace_all(x, stringr::fixed(pattern), consonants[[pattern]])
  }

  x |>
    stringr::str_replace_all("__YGLIDE__", "j") |>
    stringr::str_replace_all("__YLONG__", "a\u026a") |>
    stringr::str_replace_all("__YSHORT__", "\u026a") |>
    stringr::str_replace_all("\\s+", "") |>
    .collapse_double_consonants_ipa_en() |>
    .syllabify_fallback_ipa_en() |>
    stringr::str_c("*")
}

ipa_en <- function(word = "hospital") {
  wd <- stringr::str_to_lower(word)
  has_digit <- stringr::str_detect(wd, "\\d")
  empty <- stringr::str_squish(wd) == ""
  wd <- wd |>
    stringr::str_remove_all("[:punct:]")

  wd[has_digit] <- NA_character_
  wd[empty] <- ""

  ipa_override <- !is.na(wd) & wd %in% names(en_ipa_lex)
  matches <- !is.na(wd) & wd %in% en_lex$word

  out <- rep(NA_character_, length(wd))

  if (any(matches)) {
    lex_default <- .get_en_default_lex()
    idx <- match(wd[matches], lex_default$word)
    out[matches] <- lex_default$ipa_syll[idx]
  }

  unmatched <- !is.na(wd) & !matches
  if (any(unmatched)) {
    out[unmatched] <- vapply(wd[unmatched], .fallback_to_ipa_en, character(1))
  }

  if (any(ipa_override)) {
    out[ipa_override] <- unname(en_ipa_lex[wd[ipa_override]])
  }

  out[empty] <- ""

  out
}
