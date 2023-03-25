#' Phonemic transcriber for Spanish
#'
#' Phonemically transcribes a given word in Spanish. The function
#' works best with monomorphemic words. Polymorphemic words may also
#' work well, depending on how predictable stress is on the basis of
#' phonological and orthographic factors
#' @param word The string of interest in its orthographic form
#' @noRd
#' @return The transcribed word


transcribe_sp = function(word) {

  double_C = function(s = ""){

    doubleCs = "p{2,}|b{2,}|c{2,}|t{2,}|d{2,}|k{2,}|g{2,}|l{2,}|m{2,}|n{2,}|f{2,}|v{2,}"

    single_C = stringr::str_extract(s,
                                    pattern = doubleCs) |>
      stringr::str_sub(start = 1, end = 1)

    empty_s = stringr::str_replace_all(s,
                                       pattern = doubleCs,
                                       replacement = "#")

    final_s = empty_s |>
      stringr::str_replace_all(pattern = "#",
                               replacement = single_C)

    return(final_s)


  }

  # Clean word of double Cs:
  word = double_C(word)

  word = stringr::str_replace_all(word, "ch", "t\u0283")
  word = stringr::str_replace_all(word, "ll", "\u028e")
  word = stringr::str_replace_all(word, "x", "ks")
  word = stringr::str_replace_all(word, "sc([ei])", "s\\1")
  word = stringr::str_replace_all(word, "ss", "s")
  word = stringr::str_replace_all(word, "(e)j([aou])", "\\1\u029d\\2")
  word = stringr::str_replace_all(word, "j", "x")
  word = stringr::str_replace_all(word, "([^r])r([^r])", "\\1\u027e\\2")
  word = stringr::str_replace_all(word, "rr", "r")
  word = stringr::str_replace_all(word, "g([ei\u00e9\u00ed])", "x\\1")


  # qu and c:
  word = stringr::str_replace_all(word, "qu", "k")
  word = stringr::str_replace_all(word, "c(?=[ei])", "s")
  word = stringr::str_replace_all(word, "c(?![ei])", "k")

  # Word-initial h:
  word = stringr::str_replace_all(word, "^h", "")

  # Conjunction y:
  word = stringr::str_replace_all(word, "^y$", "i")

  # y:
  word = stringr::str_replace_all(word, "ay", "aj")
  word = stringr::str_replace_all(word, "y", "\u029d")

  # Specific sequences:
  word = stringr::str_replace_all(word, "zz", "ts")

  # Nasals:
  word = stringr::str_replace_all(word, "\u00f1", "\u0272")

  # Diphthongs/triphthongs:
  word = stringr::str_replace_all(word, "u([aeoi])", "w\\1")
  word = stringr::str_replace_all(word, "([aeou])[yi]", "\\1j")
  word = stringr::str_replace_all(word, "i([aeou])", "j\\1")
  word = stringr::str_replace_all(word, "([aeoi])u", "\\1w")
  word = stringr::str_replace_all(word, "[u]([aeo\u00e1\u00e9\u00f3])[iy]", "w\\1j")
  word = stringr::str_replace_all(word, "[i]([aeo\u00e1\u00e9\u00f3])[iy]", "j\\1j")
  word = stringr::str_replace_all(word, "[iy]([aeo\u00e1\u00e9\u00f3])[u]", "j\\1w")
  word = stringr::str_replace_all(word, "[u]([aeo\u00e1\u00e9\u00f3])[u]", "w\\1w")
  word = stringr::str_replace_all(word, "i\u00f3n$", "jon")

  word = stringr::str_replace_all(word, "z", "s")


  return(word)
}


