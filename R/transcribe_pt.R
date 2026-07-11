#' IPA transcriber for Portuguese
#'
#' Returns IPA phonemic transcription for a given string.
#' @param word The string of interest
#' @return The IPA transcription of said string without syllabification or stress
#' @examples
#' transcribe_pt(word = "computador")
#' @export

transcribe_pt <- function(word = "") {
  word <- stringr::str_to_lower(word)

  # CC:
  word <- stringr::str_replace_all(word,
    pattern = "cc",
    replacement = "ks"
  )

  double_C <- function(s = "") {
    Cs <- c("bcdfghjklmnpqtvxywz")
    doubleCs <- Cs |>
      stringr::str_split("") |>
      unlist() |>
      stringr::str_c("{2,}") |>
      stringr::str_c(collapse = "|")

    single_C <- stringr::str_extract(s,
      pattern = doubleCs
    ) |>
      stringr::str_sub(start = 1, end = 1)

    empty_s <- stringr::str_replace_all(s,
      pattern = doubleCs,
      replacement = "#"
    )

    final_s <- empty_s |>
      stringr::str_replace_all(
        pattern = "#",
        replacement = single_C
      )

    return(final_s)
  }

  # Clean word of double Cs:
  word <- double_C(word)

  # Initial h
  word <- stringr::str_remove(word,
    pattern = "^h"
  )

  # Diaeresis marks a pronounced glide (freq\u00fcente, ag\u00fcia):
  word <- stringr::str_replace_all(word,
    pattern = "q\u00fc",
    replacement = "kw"
  )

  word <- stringr::str_replace_all(word,
    pattern = "g\u00fc",
    replacement = "gw"
  )

  # Odd diacritics:
  word <- stringr::str_replace_all(word,
    pattern = "\u00fc",
    replacement = "u"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00e4",
    replacement = "a"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00f2",
    replacement = "\u00f3"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00e8",
    replacement = "\u00e9"
  )

  # W, Y:
  word <- stringr::str_replace_all(word,
    pattern = "^w",
    replacement = "v"
  )

  word <- stringr::str_replace_all(word,
    pattern = "y",
    replacement = "i"
  )

  # G:
  word <- stringr::str_replace_all(
    string = word,
    pattern = "burger",
    replacement = "burGER"
  )

  word <- stringr::str_replace_all(word,
    pattern = "(gu)([ao\u00e1\u00f3\u00f4]n)",
    replacement = "gw\\2"
  )

  word <- stringr::str_replace_all(word,
    pattern = "gu([a\u00e1\u00e2\u00e3o\u00f3\u00f4])",
    replacement = "gw\\1"
  )

  word <- stringr::str_replace_all(word,
    pattern = "g([ei])",
    replacement = "\u0292\\1"
  )

  word <- stringr::str_replace_all(word,
    pattern = "(gu)([e\u00ea])",
    replacement = "g\\2"
  )

  word <- stringr::str_replace_all(word,
    pattern = "gu([ei])",
    replacement = "g\\1"
  )

  # Retrieve g:
  word <- stringr::str_replace_all(
    string = word,
    pattern = "GER",
    replacement = "ger"
  )




  # Diacritics
  # word = stringr::str_replace_all(word,
  #                                 pattern = "\u00e2",
  #                                 replacement = "a")
  #
  # word = stringr::str_replace_all(word,
  #                                 pattern = "\u00ea",
  #                                 replacement = "e")
  #
  # word = stringr::str_replace_all(word,
  #                                 pattern = "\u00ed",
  #                                 replacement = "i")
  #
  # word = stringr::str_replace_all(word,
  #                                 pattern = "\u00f4",
  #                                 replacement = "o")
  #
  # word = stringr::str_replace_all(word,
  #                                 pattern = "\u00fa",
  #                                 replacement = "u")

  # Q:
  word <- stringr::str_replace_all(word,
    pattern = "^que",
    replacement = "ke"
  )

  word <- stringr::str_replace_all(word,
    pattern = " que",
    replacement = " ke"
  )


  word <- stringr::str_replace_all(word,
    pattern = "qu(en|a|\u00e1|\u00e3)",
    replacement = "kw\\1"
  )

  word <- stringr::str_replace_all(word,
    pattern = "qu([ieo\u00e9\u00f3\u00f4\u00ea\u00ed])",
    replacement = "k\\1"
  )

  word <- stringr::str_replace_all(word,
    pattern = "q([bdfgklmnpqrstvxz\u0283\u0292])",
    replacement = "k\\1"
  )



  # n+lab:
  word <- stringr::str_replace_all(word,
    pattern = "n([bp])",
    replacement = "m\\1"
  )

  word <- stringr::str_replace_all(word,
    pattern = "m([tdszlr])",
    replacement = "n\\1"
  )

  # Intervocalic S
  word <- stringr::str_replace_all(word,
    pattern = "([aeiou\u00e1\u00e9\u00ed\u00f3\u00fa\u00e2\u00ea\u00f4\u00e3\u00f5])s([aeiou\u00e1\u00e9\u00ed\u00f3\u00fa\u00e2\u00ea\u00f4\u00e3\u00f5])",
    replacement = "\\1z\\2"
  )

  # SS, XC, etc.
  word <- stringr::str_replace_all(word,
    pattern = "ss|\u00e7|s\u00e7",
    replacement = "s"
  )

  # Posttonic -ceo(s) is categorically sjo(s) in the PSL (bombac\u00e1ceo);
  # J is a placeholder so the glide survives the j -> \u0292 rule below:
  word <- stringr::str_replace_all(word,
    pattern = "ceo(s?)$",
    replacement = "sJo\\1"
  )

  # C before e,i
  word <- stringr::str_replace_all(word,
    pattern = "sc([ei\u00e9\u00ed\u00ea])",
    replacement = "s\\1"
  )

  word <- stringr::str_replace_all(word,
    pattern = "c([ei\u00e9\u00ed\u00ea])",
    replacement = "s\\1"
  )

  # C before a,o,u
  word <- stringr::str_replace_all(word,
    pattern = "c([a\u00e2\u00e1\u00e3o\u00f3\u00f5\u00f4u\u00fa])",
    replacement = "k\\1"
  )

  # C before e,i
  word <- stringr::str_replace_all(word,
    pattern = "c([ei])",
    replacement = "s\\1"
  )

  # Word-final C (borrowings)
  word <- stringr::str_replace_all(word,
    pattern = "c$",
    replacement = "k"
  )

  # C before other consonant
  word <- stringr::str_replace_all(word,
    pattern = "c([pbtdkgszfv\u0283\u0292\u028e\u0272mnlr])",
    replacement = "k\\1"
  )

  # Low-mids:
  # word = stringr::str_replace_all(word,
  #                                 pattern = "\u00e9",
  #                                 replacement = "\u025b")
  #
  # word = stringr::str_replace_all(word,
  #                                 pattern = "\u00f3",
  #                                 replacement = "\u0254")


  # Subjunctive (future; 2nd group; irregulars)
  word <- stringr::str_replace_all(word,
    pattern = "(kiz|fiz|soub|tiv|troux|koub|detiv)er",
    replacement = "\\1\u025br"
  )

  # X

  word <- stringr::str_replace(word,
    pattern = "x$",
    replacement = "ks"
  )

  word <- stringr::str_replace_all(word,
    pattern = "^x",
    replacement = "\u0283"
  )

  # Initial ex + vowel is systematically voiced (exame, \u00eaxodo):
  word <- stringr::str_replace_all(word,
    pattern = "^([e\u00ea])x([aeiou\u00e1\u00e9\u00ed\u00f3\u00fa\u00e2\u00ea\u00f4])",
    replacement = "\\1z\\2"
  )

  # Remaining intervocalic x defaults to the majority outcome in the PSL:
  word <- stringr::str_replace_all(word,
    pattern = "([aeiou\u00e1\u00e9\u00ed\u00f3\u00fa\u00e2\u00ea\u00f4\u00e3\u00f5])x([aeiou\u00e1\u00e9\u00ed\u00f3\u00fa\u00e2\u00ea\u00f4])",
    replacement = "\\1\u0283\\2"
  )

  word <- stringr::str_replace_all(word,
    pattern = "([aeiou\u00e1\u00e9\u00ed\u00f3\u00fa\u00e2\u00ea\u00f4])x([pbtdkgmn])",
    replacement = "\\1s\\2"
  )

  word <- stringr::str_replace_all(word,
    pattern = "xs",
    replacement = "s"
  )

  # Coda s voices before voiced consonants (esloveno, desbordo):
  word <- stringr::str_replace_all(word,
    pattern = "s([bdglmnrvz\u0292])",
    replacement = "z\\1"
  )

  # Remove diacritics
  # word = stringr::str_replace_all(word,
  #                                 pattern = "\u00f4",
  #                                 replacement = "o")
  #
  # word = stringr::str_replace_all(word,
  #                                 pattern = "\u00ea",
  #                                 replacement = "e")

  # lh + nh

  word <- stringr::str_replace_all(word,
    pattern = "lher",
    replacement = "\u028e\u025br"
  )

  word <- stringr::str_replace_all(word,
    pattern = "lhor$",
    replacement = "\u028e\u0254r"
  )


  word <- stringr::str_replace_all(word,
    pattern = "lh",
    replacement = "\u028e"
  )

  word <- stringr::str_replace_all(word,
    pattern = "nh|\u00f1",
    replacement = "\u0272"
  )

  # Palatal fricatives
  word <- stringr::str_replace_all(word,
    pattern = "schw",
    replacement = "\u0283v"
  )

  # Palatal fricatives
  word <- stringr::str_replace_all(word,
    pattern = "ch|sch",
    replacement = "\u0283"
  )



  word <- stringr::str_replace_all(word,
    pattern = "j([aeiou\u00e1\u00e9\u00ed\u00f3\u00fa\u00e0\u00e8\u00ec\u00f2\u00f9])",
    replacement = "\u0292\\1"
  )

  # Diphthongs

  # ==================
  # i before n/m + vowel is a true diphthong (paineira), not a nasal:
  word <- stringr::str_replace_all(word,
    pattern = "([aeou\u0254\u025b])i([nm])([aeiou\u00e1\u00e9\u00ed\u00f3\u00fa\u00e2\u00ea\u00f4])",
    replacement = "\\1j\\2\\3"
  )

  word <- stringr::str_replace_all(word,
    pattern = "([aeou\u0254\u025b])i([^mn])",
    replacement = "\\1j\\2"
  )

  word <- stringr::str_replace_all(word,
    pattern = "([aeou\u0254\u025b])i([aeio])",
    replacement = "\\1j\\2"
  )

  word <- stringr::str_replace_all(word,
    pattern = "([aeou\u0254\u025b])i$",
    replacement = "\\1j"
  )

  # ==================
  word <- stringr::str_replace_all(word,
    pattern = "([aeio\u0254\u025b])u",
    replacement = "\\1w"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00e3o$",
    replacement = "\u00e3w\u0303"
  )

  # word = stringr::str_replace_all(word,
  #                                 pattern = "am$",
  #                                 replacement = "\u00e3w\u0303")

  word <- stringr::str_replace_all(word,
    pattern = "\u00f5e",
    replacement = "\u00f5j\u0303"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00e3e",
    replacement = "\u00e3j\u0303"
  )


  # Nasal diphthongs + plural:
  word <- stringr::str_replace_all(word,
    pattern = "\u00e3os",
    replacement = "a\u0303w\u0303s"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00f5es",
    replacement = "o\u0303j\u0303s"
  )



  # Fix diphthongs in case of diacritics:
  word <- stringr::str_replace_all(word,
    pattern = "\u00e1i",
    replacement = "\u00e1j"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00e1u",
    replacement = "\u00e1w"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00e9u",
    replacement = "\u00e9w"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00e9i",
    replacement = "\u00e9j"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00edi",
    replacement = "\u00edj"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00edu",
    replacement = "\u00edw"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00f3u",
    replacement = "\u00f3w"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00f3i",
    replacement = "\u00f3j"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00fai",
    replacement = "\u00faj"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00fau",
    replacement = "\u00faw"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00e2u",
    replacement = "\u00e2w"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00e2i",
    replacement = "\u00e2j"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00f4u",
    replacement = "\u00f4w"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00f4i",
    replacement = "\u00f4j"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00eau",
    replacement = "\u00eaw"
  )

  word <- stringr::str_replace_all(word,
    pattern = "\u00eai",
    replacement = "\u00eaj"
  )


  # Adjust x in diphthongs:
  word <- stringr::str_replace_all(word,
    pattern = "([aeiou])([wj])x",
    replacement = "\\1\\2\u0283"
  )

  # Adjust x after n:
  word <- stringr::str_replace_all(word,
    pattern = "([aeiou])([n])x",
    replacement = "\\1\\2\u0283"
  )

  # [R
  word <- stringr::str_replace_all(word,
    pattern = "^r|rr",
    replacement = "x"
  )

  # s.r + n.r
  word <- stringr::str_replace_all(word,
    pattern = "([nmslz])r",
    replacement = "\\1x"
  )

  # Tap
  # word <- stringr::str_replace_all(word,
  #   pattern = "r",
  #   replacement = "\u027e"
  # )

  # Resolve glide placeholder:
  word <- stringr::str_replace_all(word,
    pattern = "J",
    replacement = "j"
  )

  return(word)
}
