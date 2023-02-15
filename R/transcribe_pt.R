#' IPA transcriber for Portuguese
#'
#' Returns IPA phonemic transcription for a given string
#' @param word The string of interest
#' @return The IPA transcription of said string without syllabification or stress
#' @examples
#' transcribe_pt(word = "computador");

transcribe_pt = function(word = ""){
  if (!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)

  double_C = function(s = ""){

    doubleCs = "p{2,}|b{2,}|c{2,}|t{2,}|d{2,}|k{2,}|g{2,}|l{2,}|m{2,}|n{2,}|f{2,}|v{2,}"

    single_C = str_extract(s,
                           pattern = doubleCs) %>%
      str_sub(start = 1, end = 1)

    empty_s = str_replace_all(s,
                                 pattern = doubleCs,
                                 replacement = "#")

    final_s = empty_s %>%
      str_replace_all(pattern = "#",
                      replacement = single_C)

    return(final_s)


  }

  # Clean word of double Cs:
  word = double_C(word)




  # W, Y:
  word = str_replace_all(word,
                         pattern = "^w",
                         replacement = "v")

  word = str_replace_all(word,
                         pattern = "y",
                         replacement = "i")

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


  # n+lab:
  word = str_replace_all(word,
                         pattern = "n([bp])",
                         replacement = "m\\1")

  word = str_replace_all(word,
                         pattern = "m([tdszlr])",
                         replacement = "n\\1")

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

  word = str_replace(word,
                     pattern = "x$",
                     replacement = "ks")

  word = str_replace_all(word,
                         pattern = "^x",
                         replacement = "ʃ")

  word = str_replace_all(word,
                         pattern = "([pbtdkgszfvʃʒʎɲmnlr][a])x([a])",
                         replacement = "\\1ʃ\\2")

  word = str_replace_all(word,
                         pattern = "(^[a])x([a])",
                         replacement = "\\1ʃ\\2")


  word = str_replace_all(word,
                         pattern = "([pbtdkgszfvʃʒʎɲmnlr][i])x([o])",
                         replacement = "\\1ʃ\\2")

  word = str_replace_all(word,
                         pattern = "(^[i])x([o])",
                         replacement = "\\1ʃ\\2")

  word = str_replace_all(word,
                         pattern = "([pbtdkgszfvʃʒʎɲmnlr][o])x([u])",
                         replacement = "\\1ʃ\\2")

  word = str_replace_all(word,
                         pattern = "(^[o])x([u])",
                         replacement = "\\1ʃ\\2")

  word = str_replace_all(word,
                         pattern = "([pbtdkgszfvʃʒʎɲmnlr][u])x([a]])",
                         replacement = "\\1ʃ\\2")

  word = str_replace_all(word,
                         pattern = "(^[u])x([a]])",
                         replacement = "\\1ʃ\\2")


  word = str_replace_all(word,
                         pattern = "([pbtdkgszfvʃʒʎɲmnlr][aeiou])x([aeiou])",
                         replacement = "\\1z\\2")

  word = str_replace_all(word,
                         pattern = "(^[aeiou])x([aeiou])",
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


  # Diphthongs
  word = str_replace_all(word,
                         pattern = "([aeiouɔɛ])i",
                         replacement = "\\1j")

  word = str_replace_all(word,
                         pattern = "([aeiouɔɛ])u",
                         replacement = "\\1w")

  word = str_replace_all(word,
                         pattern = "ão$",
                         replacement = "ãw̃")

  word = str_replace_all(word,
                         pattern = "am$",
                         replacement = "ãw̃")

  word = str_replace_all(word,
                         pattern = "õe",
                         replacement = "õj̃")

  word = str_replace_all(word,
                         pattern = "ãe",
                         replacement = "ãj̃")

  # Adjust x in diphthongs:
  word = str_replace_all(word,
                         pattern = "([aeiou])([wj])x",
                         replacement = "\\1\\2ʃ")

  # Adjust x after n:
  word = str_replace_all(word,
                         pattern = "([aeiou])([n])x",
                         replacement = "\\1\\2ʃ")

  # [R
  word = str_replace_all(word,
                         pattern = "^r|rr",
                         replacement = "x")

  # s.r + n.r
  word = str_replace_all(word,
                         pattern = "([nmsz])r",
                         replacement = "\\1x")

  # Tap
  word = str_replace_all(word,
                         pattern = "r",
                         replacement = "ɾ")


  return(word)

}

