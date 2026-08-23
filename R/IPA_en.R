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
    en_lex <- .get_pkg_data("en_lex")
    default_lex <- en_lex[en_lex$alt == 0L, c("word", "ipa_syll")]
    default_lex <- default_lex[!duplicated(default_lex$word), ]

    assign("default_lex", default_lex, envir = .en_cache)
  }

  get("default_lex", envir = .en_cache, inherits = FALSE)
}

.get_en_cmu_complement_lex <- function() {
  if (!exists("cmu_complement_lex", envir = .en_cache, inherits = FALSE)) {
    cmu_complement <- .get_pkg_data("lex_en_cmu_complement")

    complement_lex <- stats::setNames(
      as.character(cmu_complement$ipa),
      cmu_complement$word
    )

    assign("cmu_complement_lex", complement_lex, envir = .en_cache)
  }

  get("cmu_complement_lex", envir = .en_cache, inherits = FALSE)
}

.ipa_vowels_en <- c("e\u026a", "o\u028a", "a\u028a", "\u0254\u026a", "a\u026a", "i", "u", "\u025b", "\u026a", "\u0251", "\u028c", "\u00e6", "\u0259", "\u025a", "\u025d", "\u028a", "\u0254")
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

.parse_syllable_ipa_en <- function(syl) {
  tokens <- .tokenize_ipa_en(stringr::str_remove_all(syl, "[\u02c8\u02cc]"))
  vowel_idx <- which(tokens %in% .ipa_vowels_en)

  if (length(vowel_idx) == 0L) {
    return(list(
      onset = tokens,
      nucleus = character(),
      coda = character()
    ))
  }

  nucleus_pos <- vowel_idx[1]

  list(
    onset = if (nucleus_pos > 1L) tokens[seq_len(nucleus_pos - 1L)] else character(),
    nucleus = tokens[nucleus_pos],
    coda = if (nucleus_pos < length(tokens)) tokens[seq.int(nucleus_pos + 1L, length(tokens))] else character()
  )
}

.is_heavy_syllable_ipa_en <- function(syl) {
  parsed <- .parse_syllable_ipa_en(syl)

  if (length(parsed$nucleus) == 0L) {
    return(FALSE)
  }

  branching_nucleus <- parsed$nucleus %in% c("e\u026a", "o\u028a", "a\u028a", "\u0254\u026a", "a\u026a", "\u025d", "\u025a")

  branching_nucleus || length(parsed$coda) > 0L
}

.stress_index_from_suffix_en <- function(word, n_syl) {
  if (n_syl <= 0L) {
    return(NA_integer_)
  }

  if (stringr::str_detect(word, "((tion)|(sion)|(cian))$")) {
    return(if (n_syl >= 2L) n_syl - 1L else NA_integer_)
  }

  if (stringr::str_detect(word, "ic$")) {
    return(if (n_syl >= 2L) n_syl - 1L else NA_integer_)
  }

  if (stringr::str_detect(word, "ity$")) {
    return(if (n_syl >= 3L) n_syl - 2L else NA_integer_)
  }

  if (stringr::str_detect(word, "ify$")) {
    return(n_syl)
  }

  if (stringr::str_detect(word, "ian$") && !stringr::str_detect(word, "cian$")) {
    return(if (n_syl >= 2L) n_syl - 1L else NA_integer_)
  }

  NA_integer_
}

.assign_stress_fallback_ipa_en <- function(ipa_syll, word) {
  syllables <- stringr::str_split(ipa_syll, "\\.", simplify = TRUE)
  syllables <- syllables[syllables != ""]
  n_syl <- length(syllables)

  if (n_syl == 0L) {
    return(ipa_syll)
  }

  if (n_syl == 1L) {
    return(stringr::str_c("\u02c8", syllables))
  }

  stress_idx <- .stress_index_from_suffix_en(word, n_syl)

  if (is.na(stress_idx)) {
    if (n_syl == 2L) {
      stress_idx <- 1L
    } else if (.is_heavy_syllable_ipa_en(syllables[n_syl - 1L])) {
      stress_idx <- n_syl - 1L
    } else {
      stress_idx <- n_syl - 2L
    }
  }

  stress_idx <- max(1L, min(stress_idx, n_syl))
  syllables[stress_idx] <- stringr::str_c("\u02c8", syllables[stress_idx])

  stringr::str_c(syllables, collapse = ".")
}

.postprocess_stressed_ipa_en <- function(x) {
  syls <- stringr::str_split(x, stringr::fixed("."))[[1]]
  stressed <- stringr::str_detect(syls, "\u02c8")

  syls[!stressed] <- syls[!stressed] |>
    stringr::str_replace_all("\u025d", "\u025a") |>
    stringr::str_replace_all("[\u0251\u0254]\u0279$", "\u025a") |>
    stringr::str_replace_all("[\u028c\u00e6\u0251\u025b]", "\u0259")

  stringr::str_c(syls, collapse = ".")
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
    stringr::str_replace_all("([aeiou])rr(?=[aeiou])", "\\1r") |>
    stringr::str_replace_all("wor(?![aeiou])", "w__R_ER__") |>
    stringr::str_replace_all("oar(?![aeiou])", "__R_OR__") |>
    stringr::str_replace_all("eer(?![aeiou])", "__R_IR__") |>
    stringr::str_replace_all("ear(?=[bcdfghjklmnpqrstvwxz])", "__R_ER__") |>
    stringr::str_replace_all("ear(?![aeiou])", "__R_IR__") |>
    stringr::str_replace_all("air(?![aeiou])", "__R_EIR__") |>
    stringr::str_replace_all("oor(?![aeiou])", "__R_UR__") |>
    stringr::str_replace_all("our(?![aeiou])", "__R_OR__") |>
    stringr::str_replace_all("ar(?![aeiou])", "__R_AR__") |>
    stringr::str_replace_all("or(?![aeiou])", "__R_OR__") |>
    stringr::str_replace_all("[eiu]r(?![aeiou])", "__R_ER__") |>
    stringr::str_replace_all("ee", "__V_EE__") |>
    stringr::str_replace_all("oo(?=k)", "\u028a") |>
    stringr::str_replace_all("oo", "__V_OO__") |>
    stringr::str_replace_all("ow$", "__V_OA__") |>
    stringr::str_replace_all("ai", "__V_AI__") |>
    stringr::str_replace_all("ay", "__V_AI__") |>
    stringr::str_replace_all("oa", "__V_OA__") |>
    stringr::str_replace_all("ow", "__V_OW__") |>
    stringr::str_replace_all("ou", "__V_OW__") |>
    stringr::str_replace_all("oi", "__V_OI__") |>
    stringr::str_replace_all("oy", "__V_OI__") |>
    stringr::str_replace_all("au", "__V_AU__") |>
    stringr::str_replace_all("ea", "__V_EE__") |>
    stringr::str_replace_all("ie", "__V_EE__") |>
    stringr::str_replace_all("ey", "__V_AI__") |>
    stringr::str_replace_all("ei", "__V_EE__") |>
    stringr::str_replace_all("ew", "__V_OO__") |>
    stringr::str_replace_all("ue", "__V_OO__") |>
    stringr::str_replace_all("ui", "__V_OO__") |>
    stringr::str_replace_all("aw", "__V_AU__") |>
    stringr::str_replace_all("oe", "__V_OA__") |>
    stringr::str_replace_all("e", "\u025b") |>
    stringr::str_replace_all("i", "\u026a") |>
    stringr::str_replace_all("o", "\u0251") |>
    stringr::str_replace_all("u", "\u028c") |>
    stringr::str_replace_all("a", "\u00e6") |>
    stringr::str_replace_all("__V_EE__", "i") |>
    stringr::str_replace_all("__V_OO__", "u") |>
    stringr::str_replace_all("__V_IGH__", "a\u026a") |>
    stringr::str_replace_all("__V_AI__", "e\u026a") |>
    stringr::str_replace_all("__V_OA__", "o\u028a") |>
    stringr::str_replace_all("__V_OW__", "a\u028a") |>
    stringr::str_replace_all("__V_OI__", "\u0254\u026a") |>
    stringr::str_replace_all("__V_AU__", "\u0254") |>
    stringr::str_replace_all("__R_IR__", "\u026a\u0279") |>
    stringr::str_replace_all("__R_ER__", "\u025d") |>
    stringr::str_replace_all("__R_EIR__", "\u025b\u0279") |>
    stringr::str_replace_all("__R_AR__", "\u0251\u0279") |>
    stringr::str_replace_all("__R_OR__", "\u0254\u0279") |>
    stringr::str_replace_all("__R_UR__", "\u028a\u0279")

  chars
}

.preprocess_fallback_en <- function(x) {
  consonant <- "[bcdfghjklmnpqrstvwxyz]"

  x <- x |>
    stringr::str_replace("^kn", "n") |>
    stringr::str_replace("^wr", "r") |>
    stringr::str_replace("^gn", "n") |>
    stringr::str_replace("^ps", "s") |>
    stringr::str_replace("mb$", "m") |>
    stringr::str_replace("mn$", "m") |>
    stringr::str_replace("ture$", "t\u0283\u025a") |>
    stringr::str_replace("que$", "k") |>
    stringr::str_replace("are$", "\u025b\u0279") |>
    stringr::str_replace("ere$", "\u026a\u0279") |>
    stringr::str_replace("ire$", "a\u026a\u025a") |>
    stringr::str_replace("ore$", "\u0254\u0279") |>
    stringr::str_replace("ure$", "\u028a\u0279") |>
    stringr::str_replace("(sh|ch|ss|x|z)es$", "\\1__IZ__") |>
    stringr::str_replace("stle(s)?$", "s\u0259l\\1")

  long_map <- c(
    a = "__V_AI__", e = "__V_EE__", i = "__V_IGH__",
    o = "__V_OA__", u = "__V_OO__", y = "__V_IGH__"
  )

  for (v in names(long_map)) {
    long <- long_map[[v]]
    v <- paste0("(?<![aeiou])", v)
    x <- x |>
      stringr::str_replace(paste0(v, "([bdfgkpt])le(s)?$"), paste0(long, "\\1\u0259l\\2")) |>
      stringr::str_replace(paste0(v, "ces$"), paste0(long, "s__IZ__")) |>
      stringr::str_replace(paste0(v, "ced$"), paste0(long, "st")) |>
      stringr::str_replace(paste0(v, "ce$"), paste0(long, "s")) |>
      stringr::str_replace(paste0(v, "ges$"), paste0(long, "d\u0292__IZ__")) |>
      stringr::str_replace(paste0(v, "ged$"), paste0(long, "d\u0292d")) |>
      stringr::str_replace(paste0(v, "ge$"), paste0(long, "d\u0292")) |>
      stringr::str_replace(
        paste0(v, "([bdfjklmnpqstvz])e(s|d)?$"),
        paste0(long, "\\1\\2")
      )
  }

  x <- x |>
    stringr::str_replace("([bcdfghkmnprstz])le(s)?$", "\\1\u0259l\\2") |>
    stringr::str_replace("ies$", "iz") |>
    stringr::str_replace("ied$", "id") |>
    stringr::str_replace("([td])ed$", "\\1\u026ad") |>
    stringr::str_replace("([bcfghjklmnpqsvz])ed$", "\\1__ED__") |>
    stringr::str_replace("e$", "") |>
    stringr::str_replace("ify$", "if__YLONG__") |>
    stringr::str_replace("^y(?=[aeiou])", "__YGLIDE__") |>
    stringr::str_replace("^sy(?=[^aeiou])", "s__YSHORT__") |>
    stringr::str_replace("^ty(?=[^aeiou])", "t__YLONG__") |>
    stringr::str_replace_all(paste0("(", consonant, ")y(?=", consonant, ")"), "\\1__YSHORT__") |>
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

  word_clean <- stringr::str_remove_all(x, "'")

  x <- .preprocess_fallback_en(x)

  replacements <- c(
    "ssion" = "\u0283\u0259n",
    "([aeiour])sion" = "\\1\u0292\u0259n",
    "sion" = "\u0283\u0259n",
    "tion" = "\u0283\u0259n",
    "cian" = "\u0283\u0259n",
    "[ct]ial" = "\u0283\u0259l",
    "[ct]ious" = "\u0283\u0259s",
    "ous$" = "\u0259s",
    "ture" = "t\u0283\u025a",
    "aigh" = "e\u026a",
    "eigh" = "e\u026a",
    "augh" = "\u0254",
    "ough(?=t)" = "\u0254",
    "al(?=k)" = "\u0254",
    "igh" = "a\u026a",
    "ough" = "\u028cf",
    "^gh" = "g",
    "([aeiou])gh" = "\\1",
    "ph" = "f",
    "sch" = "sk",
    "sh" = "\u0283",
    "tch" = "t\u0283",
    "ch" = "t\u0283",
    "([aeiou])th(?=[aeiou])" = "\\1\u00f0",
    "th" = "\u03b8",
    "dh" = "\u00f0",
    "n(?=k|c(?![eiy]|__Y))" = "\u014b",
    "ng" = "\u014b",
    "ck" = "k",
    "qu" = "kw",
    "wh" = "w",
    "x" = "ks",
    "dg" = "d\u0292",
    "sc(?=[eiy]|__Y)" = "s",
    "c(?=[eiy]|__Y)" = "s",
    "c" = "k",
    "q" = "k",
    "g(?=[eiy]|__Y)" = "d\u0292"
  )

  for (pattern in names(replacements)) {
    x <- stringr::str_replace_all(x, pattern, replacements[[pattern]])
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
    stringr::str_replace_all("__IZ__", "\u026az") |>
    stringr::str_replace("([pkf\u03b8s\u0283])__ED__", "\\1t") |>
    stringr::str_replace_all("__ED__", "d") |>
    stringr::str_replace("([pkf\u03b8s\u0283])d$", "\\1t") |>
    stringr::str_replace("([bdgv\u00f0mnl\u014b\u0279\u0292])s$", "\\1z") |>
    stringr::str_replace_all("\\s+", "") |>
    .collapse_double_consonants_ipa_en() |>
    .syllabify_fallback_ipa_en() |>
    .assign_stress_fallback_ipa_en(word = word_clean) |>
    .postprocess_stressed_ipa_en() |>
    stringr::str_c("*")
}

ipa_en <- function(word = "hospital") {
  en_ipa_lex <- .lex("en_ipa_lex")
  en_lex <- .get_pkg_data("en_lex")

  wd <- stringr::str_to_lower(word)
  has_digit <- stringr::str_detect(wd, "\\d")
  empty <- stringr::str_squish(wd) == ""
  wd <- wd |>
    stringr::str_remove_all("[:punct:]")

  wd[has_digit] <- NA_character_
  wd[empty] <- ""

  ipa_override <- !is.na(wd) & wd %in% names(en_ipa_lex)
  complement_lex <- .get_en_cmu_complement_lex()
  complement_matches <- !is.na(wd) & wd %in% names(complement_lex)
  matches <- !is.na(wd) & wd %in% en_lex$word

  out <- rep(NA_character_, length(wd))

  if (any(complement_matches)) {
    out[complement_matches] <- unname(complement_lex[wd[complement_matches]])
  }

  cmu_matches <- matches & is.na(out)
  if (any(cmu_matches)) {
    lex_default <- .get_en_default_lex()
    idx <- match(wd[cmu_matches], lex_default$word)
    out[cmu_matches] <- lex_default$ipa_syll[idx]
  }

  unmatched <- !is.na(wd) & is.na(out)
  if (any(unmatched)) {
    out[unmatched] <- vapply(wd[unmatched], .fallback_to_ipa_en, character(1))
  }

  if (any(ipa_override)) {
    out[ipa_override] <- unname(en_ipa_lex[wd[ipa_override]])
  }

  out[empty] <- ""

  out
}
