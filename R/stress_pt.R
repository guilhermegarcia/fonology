#' Functions to assign stress to Portuguese words
#'
#' Assigns stress to a given string.
#' @param word The string of interest using IPA phonemic transcription and already syllabified
#' @return The stressed version of the string in question
#' @noRd

stress_pt <- function(word = "") {
  # Loanwords
  word <- word |>
    stringr::str_replace_all(
      pattern = "bur.ger$",
      replacement = "bur.GER"
    )

  # If word has a diacritic:
  if (stringr::str_detect(string = word, pattern = "[\u00e1\u00e9\u00ed\u00f3\u00fa\u00e0\u00e8\u00ec\u00f2\u00f9\u00ea\u00f4\u00e2\u00f4\u00ea]")) {
    word <- word |>
      stringr::str_replace_all(
        pattern = "(\\w*[\u00e1\u00e9\u00ed\u00f3\u00fa\u00e0\u00e8\u00ec\u00f2\u00f9\u00ea\u00f4\u00e2\u00f4\u00ea]\\w*)",
        replacement = "\u02c8\\1"
      ) |>
      stringr::str_replace(
        pattern = "[\u00e1\u00e0]",
        replacement = "a"
      ) |>
      stringr::str_replace(
        pattern = "[\u00e9\u00e8]",
        replacement = "\u025b"
      ) |>
      stringr::str_replace(
        pattern = "[\u00ed\u00ec]",
        replacement = "i"
      ) |>
      stringr::str_replace(
        pattern = "[\u00f3\u00f2]",
        replacement = "\u0254"
      ) |>
      stringr::str_replace(
        pattern = "[\u00fa\u00f9]",
        replacement = "u"
      ) |>
      stringr::str_replace(
        pattern = "\u00ea",
        replacement = "e"
      ) |>
      stringr::str_replace(
        pattern = "\u00f4",
        replacement = "o"
      ) |>
      stringr::str_replace(
        pattern = "\u00e2",
        replacement = "a"
      ) |>
      stringr::str_replace(
        pattern = "\u00f4",
        replacement = "o"
      ) |>
      stringr::str_replace(
        pattern = "\u00ea",
        replacement = "e"
      )

    # Final devoicing for z
    word <- word |>
      stringr::str_replace(
        pattern = "z$",
        replacement = "s"
      )

    word <- word |>
      stringr::str_replace(pattern = "\u02c8(?=.*\u02c8)", replacement = "") |>
      stringr::str_replace(pattern = "\u02c8(?=.*\u02c8)", replacement = "") |>
      stringr::str_replace(pattern = "\u02c8(?=.*\u02c8)", replacement = "") |>
      stringr::str_replace(pattern = "\u02c8(?=.*\u02c8)", replacement = "")

    # NOTE: Adjustments to loanwords ending in rs
    # If penult is light, antepenult:
    word <- word |>
      stringr::str_replace_all(
        pattern = "(\\w+)\\.(\\w+[aeiou])\\.(\\w+[rl]s)$",
        replacement = "\u02c8\\1.\\2.\\3"
      )

    # Else, penult:
    word <- word |>
      stringr::str_replace_all(
        pattern = "^(\\w+)\\.(\\w+[rl]s)$",
        replacement = "\u02c8\\1.\\2"
      )


    # Adjustments to vowel quality when coda is nasal (diacritics):
    word <- word |>
      stringr::str_replace(
        pattern = "\u025b([mn])",
        replacement = "e\\1"
      )

    word <- word |>
      stringr::str_replace(
        pattern = "\u0254([mn])",
        replacement = "o\\1"
      )
    word <- word |>
      stringr::str_replace_all(
        pattern = "\u02c8bur.GER$",
        replacement = "\u02c8bur.ger"
      )


    return(word)
  }

  # If word is monosyllabic:
  if (stringr::str_count(
    string = word,
    pattern = "\\."
  ) == 0) {
    word <- stringr::str_replace_all(
      string = word,
      pattern = "^(\\w*)$",
      replacement = "\u02c8\\1"
    )

    word <- word |>
      stringr::str_replace(
        pattern = "z$",
        replacement = "s"
      )
    return(word)
  }

  if (stringr::str_detect(string = word, pattern = "\\.\\w+[p|b|t|d|k|g|z|f|v|\u0283|m|n|l|\u027e|r|w|j|i|u|\u00e3|\u00f5|w\u0303]$") |
    stringr::str_detect(string = word, pattern = "\\.\\w+is$|\\.\\w+us$|\\.\\w+ws$|\\.\\w+js$") |
    stringr::str_detect(string = word, pattern = "[nlr]s$") |
    stringr::str_detect(string = word, pattern = "[wj]\u0303s$")) {
    # Stress is final if word ends in consonant other than s, diph, high vowel (Tupi), or high vowel + s
    #
    word <- stringr::str_replace_all(
      string = word,
      pattern = "\\.(\\w+[p|b|t|d|k|g|z|f|v|\u0283|m|n|l|r|\u027e|w|ws|j|js|i|u|is|us|\u00e3|\u00f5|w\u0303|o\u0303j\u0303s|a\u0303|w\u0303s]$)",
      replacement = ".\u02c8\\1"
    )

    # But change stress to penult if word ends in am:
    word <- word |>
      stringr::str_replace(
        pattern = "(\\w+\\.)\u02c8(\\w*am$)",
        replacement = "\u02c8\\1\\2"
      )

    # Also move stress to penult if word ends in gens:
    word <- word |>
      stringr::str_replace(
        pattern = "([:alpha:]+\\.)\u02c8(\u0292ens)$",
        replacement = "\u02c8\\1\\2"
      )

    word <- word |>
      stringr::str_replace(
        pattern = "([:alpha:]+\\.)\u02c8(\u0292em)$",
        replacement = "\u02c8\\1\\2"
      )

    word <- word |>
      stringr::str_replace(
        pattern = "z$",
        replacement = "s"
      )

    # Fix v height in lher]:
    word <- word |>
      stringr::str_replace(
        pattern = "\u028ee\u027e$",
        replacement = "\u028e\u025b\u027e"
      )

    word <- word |>
      stringr::str_replace(
        pattern = "\u028eer$",
        replacement = "\u028e\u025br"
      )

    # word <- word |>
    #   stringr::str_replace_all(
    #     pattern = "\u02c8bur.GER$",
    #     replacement = "\u02c8bur.ger"
    #   )
    #
    #
    return(word)
  } else if (stringr::str_detect(string = word, pattern = "\\w*[\u0254\u025b]\\w*\\.\\w*\\.\\w*$")) {
    # Stress is antepenultimate if vowel is open:
    word <- stringr::str_replace_all(
      string = word,
      pattern = "(\\w*[\u0254\u025b]\\w*)(\\.\\w*\\.\\w*$)",
      replacement = "\u02c8\\1\\2"
    )
    return(word)
  } else {
    # Else, penultimate stress:
    word <- stringr::str_replace_all(
      string = word,
      pattern = "(\\w+)(\\.\\w+)$",
      replacement = "\u02c8\\1\\2"
    )

    # Add adjustments to paroxytones:
    word <- word |>
      stringr::str_replace(
        pattern = "z$",
        replacement = "s"
      )

    word <- word |>
      stringr::str_replace(
        pattern = "a.o$",
        replacement = "aw"
      )

    word <- word |>
      stringr::str_replace(
        pattern = "a.os$",
        replacement = "aws"
      )
    word <- word |>
      stringr::str_replace_all(
        pattern = "\u02c8bur.GER$",
        replacement = "\u02c8bur.ger"
      )

    return(word)
  }
}

# 'Binary secondary stress function
#'
#' Assigns secondary stress to a given string.
#' @param word The string of interest using IPA phonemic transcription and already syllabified
#' @noRd
#' @return The stressed version of the string in question

sec_stress_pt <- function(word = "") {
  # Tokenize input and get stress position:
  mainStressPosition <- word |>
    # stress_pt() |>
    stringr::str_split(pattern = "\\.") |>
    unlist() |>
    stringr::str_detect("\u02c8") |>
    rev()

  mainStressPosition <- which(mainStressPosition == TRUE)

  # Number of syllables:
  nSyl <- word |>
    stringr::str_count(pattern = "\\.") + 1

  # Final stress if odd number of syllables:
  if (mainStressPosition == 1 & nSyl > 2) {
    # Assign secondary stress to every other syllable starting at 3rd syllable R-L
    split_word <- stringr::str_split(word, "\\.") |>
      unlist() |>
      rev()

    stressed_word <- c()

    for (i in seq(from = 1, to = nSyl)) {
      if (i %in% seq(from = 3, to = nSyl, by = 2)) {
        stressed_word[length(stressed_word) + 1] <- stringr::str_c("\u02cc", split_word[i])
      } else {
        stressed_word[length(stressed_word) + 1] <- split_word[i]
      }
    }

    stressed_word <- stressed_word |>
      rev() |>
      stringr::str_c(collapse = ".")

    return(stressed_word)
  } else

  # Penultimate stress
  if (mainStressPosition == 2 & nSyl > 3) {
    # Assign secondary stress to every other syllable starting at 3rd syllable R-L
    split_word <- stringr::str_split(word, "\\.") |>
      unlist() |>
      rev()

    stressed_word <- c()

    for (i in seq(from = 1, to = nSyl)) {
      if (i %in% seq(from = 4, to = nSyl, by = 2)) {
        stressed_word[length(stressed_word) + 1] <- stringr::str_c("\u02cc", split_word[i])
      } else {
        stressed_word[length(stressed_word) + 1] <- split_word[i]
      }
    }

    stressed_word <- stressed_word |>
      rev() |>
      stringr::str_c(collapse = ".")

    return(stressed_word)
  } else

  # Antepenultimate stress
  if (mainStressPosition == 3 & nSyl > 4) {
    # Assign secondary stress to every other syllable starting at 3rd syllable R-L
    split_word <- stringr::str_split(word, "\\.") |>
      unlist() |>
      rev()

    stressed_word <- c()

    for (i in seq(from = 1, to = nSyl)) {
      if (i %in% seq(from = 5, to = nSyl, by = 2)) {
        stressed_word[length(stressed_word) + 1] <- stringr::str_c("\u02cc", split_word[i])
      } else {
        stressed_word[length(stressed_word) + 1] <- split_word[i]
      }
    }

    stressed_word <- stressed_word |>
      rev() |>
      stringr::str_c(collapse = ".")

    return(stressed_word)
  } else {
    return(word)
  }
}

# 'Vectorized binary secondary stress function
#'
#' Assigns secondary stress to a given string.
#' @param word The string of interest using IPA phonemic transcription and already syllabified
#' @noRd
#' @return The stressed version of the string in question


sec_stress_pt_vec <- function(word = c("pa.\u02c8la.vra")) {
  # Part 1: adjacent foot
  unstressedFoot1 <- "((\\.|^)(?!\u02c8)\\w+\\.(?!\u02c8)(\\w+\\.\u02c8))" # FIRST STAGE: ADJACENT LEFT FOOT

  word <- word |>
    stringr::str_replace_all(
      pattern = unstressedFoot1,
      replacement = "\u02cc\\1"
    ) |>
    stringr::str_replace_all(
      pattern = "\u02cc\\.",
      replacement = ".\u02cc"
    )

  # Part 2: other feet
  unstressedFoot2 <- "(\\w+\\.\\w+\\.\u02cc)"

  word <- word |>
    stringr::str_replace_all(
      pattern = unstressedFoot2,
      replacement = "\u02cc\\1"
    ) |>
    stringr::str_replace_all(
      pattern = "\u02cc\u02cc",
      replacement = "\u02cc"
    ) |>
    stringr::str_replace_all(
      pattern = unstressedFoot2,
      replacement = "\u02cc\\1"
    ) |>
    stringr::str_replace_all(
      pattern = "\u02cc\u02cc",
      replacement = "\u02cc"
    ) |>
    stringr::str_replace_all(
      pattern = unstressedFoot2,
      replacement = "\u02cc\\1"
    ) |>
    stringr::str_replace_all(
      pattern = "\u02cc\u02cc",
      replacement = "\u02cc"
    ) |>
    stringr::str_replace_all(
      pattern = unstressedFoot2,
      replacement = "\u02cc\\1"
    ) |>
    stringr::str_replace_all(
      pattern = "\u02cc\u02cc",
      replacement = "\u02cc"
    ) |>
    stringr::str_replace_all(
      pattern = unstressedFoot2,
      replacement = "\u02cc\\1"
    ) |>
    stringr::str_replace_all(
      pattern = "\u02cc\u02cc",
      replacement = "\u02cc"
    ) |>
    stringr::str_replace_all(
      pattern = unstressedFoot2,
      replacement = "\u02cc\\1"
    ) |>
    stringr::str_replace_all(
      pattern = "\u02cc\u02cc",
      replacement = "\u02cc"
    ) |>
    stringr::str_replace_all(
      pattern = unstressedFoot2,
      replacement = "\u02cc\\1"
    ) |>
    stringr::str_replace_all(
      pattern = "\u02cc\u02cc",
      replacement = "\u02cc"
    ) |>
    stringr::str_replace_all(
      pattern = unstressedFoot2,
      replacement = "\u02cc\\1"
    ) |>
    stringr::str_replace_all(
      pattern = "\u02cc\u02cc",
      replacement = "\u02cc"
    ) |>
    stringr::str_replace_all(
      pattern = unstressedFoot2,
      replacement = "\u02cc\\1"
    ) |>
    stringr::str_replace_all(
      pattern = "\u02cc\u02cc",
      replacement = "\u02cc"
    ) |>
    stringr::str_replace_all(
      pattern = unstressedFoot2,
      replacement = "\u02cc\\1"
    ) |>
    stringr::str_replace_all(
      pattern = "\u02cc\u02cc",
      replacement = "\u02cc"
    ) |>
    stringr::str_replace_all(
      pattern = unstressedFoot2,
      replacement = "\u02cc\\1"
    ) |>
    stringr::str_replace_all(
      pattern = "\u02cc\u02cc",
      replacement = "\u02cc"
    ) |>
    stringr::str_replace_all(
      pattern = unstressedFoot2,
      replacement = "\u02cc\\1"
    ) |>
    stringr::str_replace_all(
      pattern = "\u02cc\u02cc",
      replacement = "\u02cc"
    ) |>
    stringr::str_replace_all(
      pattern = unstressedFoot2,
      replacement = "\u02cc\\1"
    ) |>
    stringr::str_replace_all(
      pattern = "\u02cc\u02cc",
      replacement = "\u02cc"
    ) |>
    stringr::str_replace_all(
      pattern = unstressedFoot2,
      replacement = "\u02cc\\1"
    ) |>
    stringr::str_replace_all(
      pattern = "\u02cc\u02cc",
      replacement = "\u02cc"
    ) |>
    stringr::str_replace_all(
      pattern = unstressedFoot2,
      replacement = "\u02cc\\1"
    ) |>
    stringr::str_replace_all(
      pattern = "\u02cc\u02cc",
      replacement = "\u02cc"
    )

  return(word)
}

# 'Paroxytone candidates for Portuguese
#'
#' Assigns penultimate or antepenultimate stress to certain words
#' @param word The string of interest using IPA phonemic transcription and already syllabified
#' @noRd
#' @return The stressed version of the string in question

pu_candidates <- function(word = "") {
  c1 <- stringr::str_replace(
    string = word,
    pattern = "(\\w+\\.\\w+$)",
    replacement = "\u02c8\\1"
  )

  c2 <- stringr::str_replace(
    string = word,
    pattern = "(\\w+\\.)(\\w+$)",
    replacement = "\\1\u02c8\\2"
  )

  candidates <- c(c1, c2)

  finalSegment <- stringr::str_sub(string = word, start = -1L, end = -1L)
  if (finalSegment %in% c("l", "m")) {
    winner <- sample(candidates, size = 1, prob = c(0.4, 0.6))
  } else if (finalSegment %in% c("s")) {
    winner <- sample(candidates, size = 1, prob = c(0.1, 0.9))
  } else {
    winner <- sample(candidates, size = 1, prob = c(0.02, 0.98))
  }

  # But change stress to penult if word ends in am:
  winner <- winner |>
    stringr::str_replace(
      pattern = "(\\w+\\.)\u02c8(\\w*am$)",
      replacement = "\u02c8\\1\\2"
    )

  return(winner)
}

# 'Proparoxytone candidates for Portuguese
#'
#' Assigns penultimate or antepenultimate stress to certain words
#' @param word The string of interest using IPA phonemic transcription and already syllabified
#' @noRd
#' @return The stressed version of the string in question

apu_candidates <- function(word = "") {
  c1 <- stringr::str_replace(
    string = word,
    pattern = "(\\w+\\.\\w+\\.\\w+$)",
    replacement = "\u02c8\\1"
  )

  c2 <- stringr::str_replace(
    string = word,
    pattern = "(\\w+\\.)(\\w+\\.\\w+$)",
    replacement = "\\1\u02c8\\2"
  )

  candidates <- c(c1, c2)
  winner <- sample(candidates, size = 1, prob = c(0.2, 0.8))

  return(winner)
}
