#' Stress assigner for Portuguese
#'
#' Assigns stress to a given string
#' @param word The string of interest using IPA phonemic transcription and already syllabified
#' @return The stressed version of the string in question
#' @examples
#' stress_pt(word = "kom.pu.ta.doɾ");
#' @export

stress_pt = function(word = ""){

  if(!require("pacman")){install.packages("pacman")}
  pacman::p_load(tidyverse)

  # Stress is final if word ends in consonant, diph OR high vowel (Tupi):
  word = str_replace_all(string = word,
                         pattern = "\\.(\\w+[pbtdkgszfvʃʒʎɲmnlɾwjiuãõw̃])$",
                         replacement = ".'\\1")

  word = str_replace_all(string = word,
                         pattern = "^(\\w*)$",
                         replacement = "'\\1")

  # Stress is antepenultimate if vowel is open:
  word = str_replace_all(string = word,
                         pattern = "(\\w*[ɔɛ]\\w*)(\\.\\w*\\.\\w*$)",
                         replacement = "'\\1\\2")

  # Else, penultimate stress:
  word = str_replace_all(string = word,
                         pattern = "(\\w+)(\\.\\w+)$",
                         replacement = "'\\1\\2")

  return(word)

}


pu_candidates = function(word = ""){

  c1 = str_replace(string = word,
                   pattern = "(\\w+\\.\\w+$)",
                   replacement = "'\\1")

  c2 = str_replace(string = word,
                   pattern = "(\\w+\\.)(\\w+$)",
                   replacement = "\\1'\\2")

  candidates = c(c1, c2)
  winner = sample(candidates, size = 1, prob = c(0.25, 0.75))

  return(winner)

}


apu_candidates = function(word = ""){

  c1 = str_replace(string = word,
                   pattern = "(\\w+\\.\\w+\\.\\w+$)",
                   replacement = "'\\1")

  c2 = str_replace(string = word,
                   pattern = "(\\w+\\.)(\\w+\\.\\w+$)",
                   replacement = "\\1'\\2")

  candidates = c(c1, c2)
  winner = sample(candidates, size = 1, prob = c(0.2, 0.8))

  return(winner)

}
