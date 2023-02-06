#' IPA transcriber for Portuguese
#'
#' Returns IPA phonemic transcription for a given string
#' @param word The string of interest
#' @return The IPA transcription of said string without syllabification or stress
#' @examples
#' transcribe_pt(word = "computador");
#' @export

transcribe_pt = function(word = ""){
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse)

  word = str_replace_all(word,
                         pattern = "(gu)([aeoéáóêô]n)",
                         replacement = "gw\\2")

    word = str_replace_all(word,
                      pattern = "gu([ei])",
                      replacement = "g\\1")


  word = str_replace_all(word,
                      pattern = "qu(en|a)",
                      replacement = "kw\\1")

  word = str_replace_all(word,
                      pattern = "qu([ieoéóôê])",
                      replacement = "k\\1")


  # Diacritics
  word = str_replace_all(word,
                         pattern = "á",
                         replacement = "a")


  word = str_replace_all(word,
                         pattern = "ê",
                         replacement = "e")

  word = str_replace_all(word,
                         pattern = "ê",
                         replacement = "e")

  word = str_replace_all(word,
                         pattern = "í",
                         replacement = "i")

  word = str_replace_all(word,
                         pattern = "ô",
                         replacement = "o")

  word = str_replace_all(word,
                         pattern = "ú",
                         replacement = "u")


  # Intervocalic S
  word = str_replace_all(word,
                      pattern = "([aeiouóéáêôã])s([aeiouóéáêôã])",
                      replacement = "\\1z\\2")

  # SS, XC, etc.
  word = str_replace_all(word,
                      pattern = "ss|ç|sç",
                      replacement = "s")

  # C before e,i
  word = str_replace_all(word,
                      pattern = "sc([eiéíê])",
                      replacement = "s\\1")

  word = str_replace_all(word,
                      pattern = "c([eiéíê])",
                      replacement = "s\\1")

  # C before a,o,u
  word = str_replace_all(word,
                      pattern = "c([aáãoóõôuú])",
                      replacement = "k\\1")

  # C before e,i
  word = str_replace_all(word,
                      pattern = "c([ei])",
                      replacement = "s\\1")

  # C before other consonant
  word = str_replace_all(word,
                         pattern = "c([pbtdkgszfvʃʒʎɲmnlr])",
                         replacement = "k\\1")

  # [R
  word = str_replace_all(word,
                      pattern = "^r|rr",
                      replacement = "x")

  # Low-mids:
  word = str_replace_all(word,
                      pattern = "é",
                      replacement = "ɛ")

  word = str_replace_all(word,
                      pattern = "ó",
                      replacement = "ɔ")


  # Subjunctive (future; 2nd group; irregulars)
  word = str_replace_all(word,
                      pattern = "(kiz|fiz|soub|tiv|troux|koub|detiv)er",
                      replacement = "\\1ɛr")

  # X
  word = str_replace_all(word,
                         pattern = "^x",
                         replacement = "ʃ")

  word = str_replace_all(word,
                      pattern = "([aeiou])x([aeiou])",
                      replacement = "\\1z\\2")

  word = str_replace_all(word,
                      pattern = "([aeiou])x([pbtdkgmn])",
                      replacement = "\\1s\\2")

  word = str_replace_all(word,
                      pattern = "xs",
                      replacement = "s")

  # Remove diacritics
  word = str_replace_all(word,
                      pattern = "ô",
                      replacement = "o")

  word = str_replace_all(word,
                      pattern = "ê",
                      replacement = "e")

  # lh + nh

  word = str_replace_all(word,
                      pattern = "lia",
                      replacement = "ʎa")

  word = str_replace_all(word,
                      pattern = "lhe",
                      replacement = "ʎɛ")

  word = str_replace_all(word,
                      pattern = "lh",
                      replacement = "ʎ")

  word = str_replace_all(word,
                      pattern = "nh",
                      replacement = "ɲ")

  # Palatal fricatives
  word = str_replace_all(word,
                      pattern = "ch",
                      replacement = "ʃ")

  word = str_replace_all(word,
                      pattern = "j([aeiou])",
                      replacement = "ʒ\\1")

  # Tap
  word = str_replace_all(word,
                      pattern = "r",
                      replacement = "ɾ")


  # Diphthongs
  word = str_replace_all(word,
                      pattern = "([aeiouɔɛ])i",
                      replacement = "\\1j")

  word = str_replace_all(word,
                      pattern = "([aeiouɔɛ])u",
                      replacement = "\\1w")

  word = str_replace_all(word,
                      pattern = "ão|am$",
                      replacement = "ãw̃")

  # Adjust x in diphthongs:
  word = str_replace_all(word,
                      pattern = "([aeiou])([wj])x",
                      replacement = "\\1\\2s")

  return(word)

}
