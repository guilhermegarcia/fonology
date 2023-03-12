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
                                  pattern = "\u00fc",
                                  replacement = "u")

  word = stringr::str_replace_all(word,
                                  pattern = "\u00e4",
                                  replacement = "a")

  word = stringr::str_replace_all(word,
                                  pattern = "\u00f2",
                                  replacement = "\u00f3")

  word = stringr::str_replace_all(word,
                                  pattern = "\u00e8",
                                  replacement = "\u00e9")

  # W, Y:
  word = stringr::str_replace_all(word,
                                  pattern = "^w",
                                  replacement = "v")

  word = stringr::str_replace_all(word,
                                  pattern = "y",
                                  replacement = "i")

  # G:
  word = stringr::str_replace_all(word,
                                  pattern = "(gu)([aeo\u00e9\u00e1\u00f3\u00ea\u00f4]n)",
                                  replacement = "gw\\2")

  word = stringr::str_replace_all(word,
                                  pattern = "gu([ei])",
                                  replacement = "g\\1")

  word = stringr::str_replace_all(word,
                                  pattern = "g([ei])",
                                  replacement = "\u0292\\1")




  # Diacritics
  word = stringr::str_replace_all(word,
                                  pattern = "\u00e1|\u00e2|\u00e0",
                                  replacement = "a")

  word = stringr::str_replace_all(word,
                                  pattern = "\u00ea",
                                  replacement = "e")

  word = stringr::str_replace_all(word,
                                  pattern = "\u00ed",
                                  replacement = "i")

  word = stringr::str_replace_all(word,
                                  pattern = "\u00f4",
                                  replacement = "o")

  word = stringr::str_replace_all(word,
                                  pattern = "\u00fa",
                                  replacement = "u")

  # Q:
  word = stringr::str_replace_all(word,
                                  pattern = "qu(en|a|\u00e3)",
                                  replacement = "kw\\1")

  word = stringr::str_replace_all(word,
                                  pattern = "qu([i\u00edeo\u00e9\u00f3\u00f4\u00ea])",
                                  replacement = "k\\1")

  # n+lab:
  word = stringr::str_replace_all(word,
                                  pattern = "n([bp])",
                                  replacement = "m\\1")

  word = stringr::str_replace_all(word,
                                  pattern = "m([tdszlr])",
                                  replacement = "n\\1")

  # Intervocalic S
  word = stringr::str_replace_all(word,
                                  pattern = "([aeiou\u00f3\u00e9\u00e1\u00ea\u00f4\u00e3])s([aeiou\u00f3\u00e9\u00e1\u00ea\u00f4\u00e3])",
                                  replacement = "\\1z\\2")

  # SS, XC, etc.
  word = stringr::str_replace_all(word,
                                  pattern = "ss|\u00e7|s\u00e7",
                                  replacement = "s")

  # C before e,i
  word = stringr::str_replace_all(word,
                                  pattern = "sc([ei\u00e9\u00ed\u00ea])",
                                  replacement = "s\\1")

  word = stringr::str_replace_all(word,
                                  pattern = "c([ei\u00e9\u00ed\u00ea])",
                                  replacement = "s\\1")

  # C before a,o,u
  word = stringr::str_replace_all(word,
                                  pattern = "c([a\u00e1\u00e3o\u00f3\u00f5\u00f4u\u00fa])",
                                  replacement = "k\\1")

  # C before e,i
  word = stringr::str_replace_all(word,
                                  pattern = "c([ei])",
                                  replacement = "s\\1")

  # C before other consonant
  word = stringr::str_replace_all(word,
                                  pattern = "c([pbtdkgszfv\u0283\u0292\u028e\u0272mnlr])",
                                  replacement = "k\\1")

  # Low-mids:
  word = stringr::str_replace_all(word,
                                  pattern = "\u00e9",
                                  replacement = "\u025b")

  word = stringr::str_replace_all(word,
                                  pattern = "\u00f3",
                                  replacement = "\u0254")


  # Subjunctive (future; 2nd group; irregulars)
  word = stringr::str_replace_all(word,
                                  pattern = "(kiz|fiz|soub|tiv|troux|koub|detiv)er",
                                  replacement = "\\1\u025br")

  # X

  word = stringr::str_replace(word,
                              pattern = "x$",
                              replacement = "ks")

  word = stringr::str_replace_all(word,
                                  pattern = "^x",
                                  replacement = "\u0283")

  word = stringr::str_replace_all(word,
                                  pattern = "([pbtdkgszfv\u0283\u0292\u028e\u0272mnlr][a])x([a])",
                                  replacement = "\\1\u0283\\2")

  word = stringr::str_replace_all(word,
                                  pattern = "(^[a])x([a])",
                                  replacement = "\\1\u0283\\2")


  word = stringr::str_replace_all(word,
                                  pattern = "([pbtdkgszfv\u0283\u0292\u028e\u0272mnlr][i])x([o])",
                                  replacement = "\\1\u0283\\2")

  word = stringr::str_replace_all(word,
                                  pattern = "(^[i])x([o])",
                                  replacement = "\\1\u0283\\2")

  word = stringr::str_replace_all(word,
                                  pattern = "([pbtdkgszfv\u0283\u0292\u028e\u0272mnlr][o])x([u])",
                                  replacement = "\\1\u0283\\2")

  word = stringr::str_replace_all(word,
                                  pattern = "(^[o])x([u])",
                                  replacement = "\\1\u0283\\2")

  word = stringr::str_replace_all(word,
                                  pattern = "([pbtdkgszfv\u0283\u0292\u028e\u0272mnlr][u])x([a]])",
                                  replacement = "\\1\u0283\\2")

  word = stringr::str_replace_all(word,
                                  pattern = "(^[u])x([a]])",
                                  replacement = "\\1\u0283\\2")


  word = stringr::str_replace_all(word,
                                  pattern = "([pbtdkgszfv\u0283\u0292\u028e\u0272mnlr][aeiou])x([aeiou])",
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
                                  pattern = "\u00f4",
                                  replacement = "o")

  word = stringr::str_replace_all(word,
                                  pattern = "\u00ea",
                                  replacement = "e")

  # lh + nh

  word = stringr::str_replace_all(word,
                                  pattern = "lia",
                                  replacement = "\u028ea")

  word = stringr::str_replace_all(word,
                                  pattern = "lhe",
                                  replacement = "\u028e\u025b")

  word = stringr::str_replace_all(word,
                                  pattern = "lh",
                                  replacement = "\u028e")

  word = stringr::str_replace_all(word,
                                  pattern = "nh",
                                  replacement = "\u0272")

  # Palatal fricatives
  word = stringr::str_replace_all(word,
                                  pattern = "ch",
                                  replacement = "\u0283")

  word = stringr::str_replace_all(word,
                                  pattern = "j([aeiou])",
                                  replacement = "\u0292\\1")


  # Diphthongs
  word = stringr::str_replace_all(word,
                                  pattern = "([aeiou\u0254\u025b])i",
                                  replacement = "\\1j")

  word = stringr::str_replace_all(word,
                                  pattern = "([aeiou\u0254\u025b])u",
                                  replacement = "\\1w")

  word = stringr::str_replace_all(word,
                                  pattern = "\u00e3o$",
                                  replacement = "\u00e3w\u0303")

  word = stringr::str_replace_all(word,
                                  pattern = "am$",
                                  replacement = "\u00e3w\u0303")

  word = stringr::str_replace_all(word,
                                  pattern = "\u00f5e",
                                  replacement = "\u00f5j\u0303")

  word = stringr::str_replace_all(word,
                                  pattern = "\u00e3e",
                                  replacement = "\u00e3j\u0303")

  # Adjust x in diphthongs:
  word = stringr::str_replace_all(word,
                                  pattern = "([aeiou])([wj])x",
                                  replacement = "\\1\\2\u0283")

  # Adjust x after n:
  word = stringr::str_replace_all(word,
                                  pattern = "([aeiou])([n])x",
                                  replacement = "\\1\\2\u0283")

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
                                  replacement = "\u027e")


  return(word)

}

