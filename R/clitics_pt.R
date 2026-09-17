#' Enclitic removal for Portuguese
#'
#' Given an orthographic word, the function removes hyphenated ENCLITICS from
#' the end and returns the host: "diz-me" -> "diz", "parece-se-me" -> "parece".
#' Input is expected to be a single whitespace-delimited token, since the
#' pattern is anchored to the end of the string.
#'
#' The package has no morphological analyser, so this is a regex over a closed
#' list, and it is worth being explicit about what that can and cannot do.
#'
#' It CAN: match the full clitic paradigm, including the allomorphs that
#' orthography distinguishes (\code{-lo} after \code{-r/-s/-z}, \code{-no}
#' after a nasal diphthong) and the contracted dative+accusative forms
#' (\code{-mo}, \code{-lho}). Alternatives are ordered longest-first, because
#' regex alternation is leftmost-first and \code{-lo} would otherwise match
#' inside \code{-los}, leaving a stranded consonant.
#'
#' It CANNOT: tell a clitic from a compound whose final element happens to be
#' spelled like one. Nothing here checks that the host is a verb, because
#' nothing here knows what a verb is. The risk is small in practice -- Portuguese
#' compounds do not usually end in a function word -- but it is real, and it is
#' the reason this runs only when \code{cleanText(clitics = TRUE)} asks for it.
#'
#' Mesoclisis (\code{dar-se-a}) is not handled: the clitic sits in the middle,
#' so the anchored pattern removes the tense ending instead. Measured on ~18h
#' of transcribed speech per variety, genuine mesoclitic forms are vanishingly
#' rare, and almost every token with two or more hyphens is either a compound
#' or a transcription artefact.
#'
#' @param word A character vector of orthographic words in Portuguese
#' @noRd
#' @return The vector with trailing hyphenated enclitics removed. A word that
#'   would be consumed entirely is returned unchanged.

strip_clitic_pt <- function(word = "") {
  enclitics <- c(
    # Accusative
    "o", "a", "os", "as",
    # Accusative allomorphs after -r, -s, -z
    "lo", "la", "los", "las",
    # Accusative allomorphs after a nasal diphthong
    "no", "na", "nos", "nas",
    # Dative and reflexive
    "me", "te", "lhe", "lhes", "nos", "vos", "se",
    # Contracted dative + accusative
    "mo", "ma", "mos", "mas",
    "to", "ta", "tos", "tas",
    "lho", "lha", "lhos", "lhas"
  )

  # "nos" is both a dative and an accusative allomorph; one entry is enough.
  enclitics <- unique(enclitics)

  # Regex alternation is leftmost-first, so a shorter alternative listed
  # earlier would win against a longer one: -los must be tried before -lo.
  enclitics <- enclitics[order(nchar(enclitics), decreasing = TRUE)]

  # The repeated group strips a sequence of enclitics in one pass, so
  # "parece-se-me" returns "parece" rather than "parece-se".
  enclitic_pattern <- stringr::str_c(
    "(?:-(?:",
    stringr::str_c(enclitics, collapse = "|"),
    "))+$"
  )

  out <- stringr::str_remove(word, pattern = enclitic_pattern)

  # Safety net for a token made only of clitics: without this, a string such as
  # "se-se" would be emptied. Morphologically blind code should degrade by
  # leaving the input alone, not by deleting it.
  out[!is.na(out) & out == ""] <- word[!is.na(out) & out == ""]

  out
}
