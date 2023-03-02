#' IPA transcriber for Portuguese
#'
#' Returns IPA phonemic transcription for a given string
#' @param word The string of interest
#' @return The IPA transcription of said string without syllabification or stress
#' @examples
#' transcribe_pt(word = "computador");
#' @importFrom magrittr %>%
#' @export

transcribe_pt = function(word = ""){

  word = stringr::str_to_lower(word)

  double_C = function(s = ""){

    doubleCs = "p{2,}|b{2,}|c{2,}|t{2,}|d{2,}|k{2,}|g{2,}|l{2,}|m{2,}|n{2,}|f{2,}|v{2,}"

    single_C = stringr::str_extract(s,
                                    pattern = doubleCs) %>%
      stringr::str_sub(start = 1, end = 1)

    empty_s = stringr::str_replace_all(s,
                                       pattern = doubleCs,
                                       replacement = "#")

    final_s = empty_s %>%
      stringr::str_replace_all(pattern = "#",
                               replacement = single_C)

    return(final_s)


  }

  # Clean word of double Cs:
  word = double_C(word)


  # Initial h
  word = stringr::str_remove(word,
                             pattern = "^h")

  # Odd diacritics:
  word = stringr::str_replace_all(word,
                                  pattern = "ü",
                                  replacement = "u")

  word = stringr::str_replace_all(word,
                                  pattern = "ä",
                                  replacement = "a")

  word = stringr::str_replace_all(word,
                                  pattern = "ò",
                                  replacement = "ó")

  word = stringr::str_replace_all(word,
                                  pattern = "è",
                                  replacement = "é")

  # W, Y:
  word = stringr::str_replace_all(word,
                                  pattern = "^w",
                                  replacement = "v")

  word = stringr::str_replace_all(word,
                                  pattern = "y",
                                  replacement = "i")

  # G:
  word = stringr::str_replace_all(word,
                                  pattern = "(gu)([aeoéáóêô]n)",
                                  replacement = "gw\\2")

  word = stringr::str_replace_all(word,
                                  pattern = "gu([ei])",
                                  replacement = "g\\1")

  word = stringr::str_replace_all(word,
                                  pattern = "g([ei])",
                                  replacement = "ʒ\\1")


  # Q:
  word = stringr::str_replace_all(word,
                                  pattern = "qu(en|a)",
                                  replacement = "kw\\1")

  word = stringr::str_replace_all(word,
                                  pattern = "qu([ieoéóôê])",
                                  replacement = "k\\1")


  # Diacritics
  word = stringr::str_replace_all(word,
                                  pattern = "á|â|à",
                                  replacement = "a")

  word = stringr::str_replace_all(word,
                                  pattern = "ê",
                                  replacement = "e")

  word = stringr::str_replace_all(word,
                                  pattern = "í",
                                  replacement = "i")

  word = stringr::str_replace_all(word,
                                  pattern = "ô",
                                  replacement = "o")

  word = stringr::str_replace_all(word,
                                  pattern = "ú",
                                  replacement = "u")


  # n+lab:
  word = stringr::str_replace_all(word,
                                  pattern = "n([bp])",
                                  replacement = "m\\1")

  word = stringr::str_replace_all(word,
                                  pattern = "m([tdszlr])",
                                  replacement = "n\\1")

  # Intervocalic S
  word = stringr::str_replace_all(word,
                                  pattern = "([aeiouóéáêôã])s([aeiouóéáêôã])",
                                  replacement = "\\1z\\2")

  # SS, XC, etc.
  word = stringr::str_replace_all(word,
                                  pattern = "ss|ç|sç",
                                  replacement = "s")

  # C before e,i
  word = stringr::str_replace_all(word,
                                  pattern = "sc([eiéíê])",
                                  replacement = "s\\1")

  word = stringr::str_replace_all(word,
                                  pattern = "c([eiéíê])",
                                  replacement = "s\\1")

  # C before a,o,u
  word = stringr::str_replace_all(word,
                                  pattern = "c([aáãoóõôuú])",
                                  replacement = "k\\1")

  # C before e,i
  word = stringr::str_replace_all(word,
                                  pattern = "c([ei])",
                                  replacement = "s\\1")

  # C before other consonant
  word = stringr::str_replace_all(word,
                                  pattern = "c([pbtdkgszfvʃʒʎɲmnlr])",
                                  replacement = "k\\1")

  # Low-mids:
  word = stringr::str_replace_all(word,
                                  pattern = "é",
                                  replacement = "ɛ")

  word = stringr::str_replace_all(word,
                                  pattern = "ó",
                                  replacement = "ɔ")


  # Subjunctive (future; 2nd group; irregulars)
  word = stringr::str_replace_all(word,
                                  pattern = "(kiz|fiz|soub|tiv|troux|koub|detiv)er",
                                  replacement = "\\1ɛr")

  # X

  word = stringr::str_replace(word,
                              pattern = "x$",
                              replacement = "ks")

  word = stringr::str_replace_all(word,
                                  pattern = "^x",
                                  replacement = "ʃ")

  word = stringr::str_replace_all(word,
                                  pattern = "([pbtdkgszfvʃʒʎɲmnlr][a])x([a])",
                                  replacement = "\\1ʃ\\2")

  word = stringr::str_replace_all(word,
                                  pattern = "(^[a])x([a])",
                                  replacement = "\\1ʃ\\2")


  word = stringr::str_replace_all(word,
                                  pattern = "([pbtdkgszfvʃʒʎɲmnlr][i])x([o])",
                                  replacement = "\\1ʃ\\2")

  word = stringr::str_replace_all(word,
                                  pattern = "(^[i])x([o])",
                                  replacement = "\\1ʃ\\2")

  word = stringr::str_replace_all(word,
                                  pattern = "([pbtdkgszfvʃʒʎɲmnlr][o])x([u])",
                                  replacement = "\\1ʃ\\2")

  word = stringr::str_replace_all(word,
                                  pattern = "(^[o])x([u])",
                                  replacement = "\\1ʃ\\2")

  word = stringr::str_replace_all(word,
                                  pattern = "([pbtdkgszfvʃʒʎɲmnlr][u])x([a]])",
                                  replacement = "\\1ʃ\\2")

  word = stringr::str_replace_all(word,
                                  pattern = "(^[u])x([a]])",
                                  replacement = "\\1ʃ\\2")


  word = stringr::str_replace_all(word,
                                  pattern = "([pbtdkgszfvʃʒʎɲmnlr][aeiou])x([aeiou])",
                                  replacement = "\\1z\\2")

  word = stringr::str_replace_all(word,
                                  pattern = "(^[aeiou])x([aeiou])",
                                  replacement = "\\1z\\2")


  word = stringr::str_replace_all(word,
                                  pattern = "([aeiou])x([pbtdkgmn])",
                                  replacement = "\\1s\\2")

  word = stringr::str_replace_all(word,
                                  pattern = "xs",
                                  replacement = "s")

  # Remove diacritics
  word = stringr::str_replace_all(word,
                                  pattern = "ô",
                                  replacement = "o")

  word = stringr::str_replace_all(word,
                                  pattern = "ê",
                                  replacement = "e")

  # lh + nh

  word = stringr::str_replace_all(word,
                                  pattern = "lia",
                                  replacement = "ʎa")

  word = stringr::str_replace_all(word,
                                  pattern = "lhe",
                                  replacement = "ʎɛ")

  word = stringr::str_replace_all(word,
                                  pattern = "lh",
                                  replacement = "ʎ")

  word = stringr::str_replace_all(word,
                                  pattern = "nh",
                                  replacement = "ɲ")

  # Palatal fricatives
  word = stringr::str_replace_all(word,
                                  pattern = "ch",
                                  replacement = "ʃ")

  word = stringr::str_replace_all(word,
                                  pattern = "j([aeiou])",
                                  replacement = "ʒ\\1")


  # Diphthongs
  word = stringr::str_replace_all(word,
                                  pattern = "([aeiouɔɛ])i",
                                  replacement = "\\1j")

  word = stringr::str_replace_all(word,
                                  pattern = "([aeiouɔɛ])u",
                                  replacement = "\\1w")

  word = stringr::str_replace_all(word,
                                  pattern = "ão$",
                                  replacement = "ãw̃")

  word = stringr::str_replace_all(word,
                                  pattern = "am$",
                                  replacement = "ãw̃")

  word = stringr::str_replace_all(word,
                                  pattern = "õe",
                                  replacement = "õj̃")

  word = stringr::str_replace_all(word,
                                  pattern = "ãe",
                                  replacement = "ãj̃")

  # Adjust x in diphthongs:
  word = stringr::str_replace_all(word,
                                  pattern = "([aeiou])([wj])x",
                                  replacement = "\\1\\2ʃ")

  # Adjust x after n:
  word = stringr::str_replace_all(word,
                                  pattern = "([aeiou])([n])x",
                                  replacement = "\\1\\2ʃ")

  # [R
  word = stringr::str_replace_all(word,
                                  pattern = "^r|rr",
                                  replacement = "x")

  # s.r + n.r
  word = stringr::str_replace_all(word,
                                  pattern = "([nmsz])r",
                                  replacement = "\\1x")

  # Tap
  word = stringr::str_replace_all(word,
                                  pattern = "r",
                                  replacement = "ɾ")


  return(word)

}

