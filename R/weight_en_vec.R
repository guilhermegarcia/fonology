#' Syllable weight labeller for English
#'
#' Labels a given string in English according to its weight profile using Ls and Hs.
#' @param word The vector with strings of interest using IPA phonemic transcription, already syllabified and stressed
#' @return The sequence of Ls and Hs based on the word's weight profile given the phonology of English
#' @noRd

getWeight_en <- function(word = c("\u02c8h\u0251s.p\u026a.t\u0259l")) {
  .getWeight_by_vowels(
    word,
    c("e\u026a", "o\u028a", "a\u028a", "\u0254\u026a", "a\u026a", "i", "u",
      "\u025b", "\u026a", "\u0251", "\u028c", "\u00e6", "\u0259",
      "\u025a", "\u028a", "\u0254")
  )
}
