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

  word = stringr::str_replace_all(word, "diaria", "di.\u00e1.ri.a")


  word = stringr::str_replace_all(word, "ch", "t\u0283")
  word = stringr::str_replace_all(word, "ll", "\u028e")
  word = stringr::str_replace_all(word, "^x([ei\u00e9\u00ed])", "s\\1")#added by Nic
  word = stringr::str_replace_all(word, "x", "ks")
  word = stringr::str_replace_all(word, "sc([ei])", "s\\1")
  word = stringr::str_replace_all(word, "ss", "s")
  #word = stringr::str_replace_all(word, "(e)j([aou])", "\\1ʝ\\2")
  word = stringr::str_replace_all(word, "j", "x")
  word = stringr::str_replace_all(word, "([^r])r([^r])", "\\1\u027e\\2")
  word = stringr::str_replace_all(word, "rr", "r")
  word = stringr::str_replace_all(word, "g([ei\u00e9\u00ed])", "x\\1")


  # qu and c:
  word = stringr::str_replace_all(word, "qu", "k")
  word = stringr::str_replace_all(word, "c(?=[e\u00e9i\u00ed])", "s")
  word = stringr::str_replace_all(word, "c(?![e\u00e9i\u00ed])", "k")

  # Word-initial h:
  word = stringr::str_replace_all(word, "^h", "")

  # Conjunction y:
  word = stringr::str_replace_all(word, "^y$", "i")

  # y:
  word = stringr::str_replace_all(word, "([aeou])y", "\\1j")
  word = stringr::str_replace_all(word, "y", "\u029d")

  # Specific sequences:
  word = stringr::str_replace_all(word, "zz", "ts")

  # Nasals:
  word = stringr::str_replace_all(word, "\u00f1", "\u0272")

  # Nic
  word = stringr::str_replace_all(word, "v", "b")
  word = stringr::str_replace_all(word, "h", "")
  word = stringr::str_replace_all(word, "gu([ei])", "g\\1")
  word = stringr::str_replace_all(word, "g\u00fc([ei])", "gw\\1")
  word = stringr::str_replace_all(word, "p([snt])", "\\1")


  word = stringr::str_replace_all(word, "z", "s")



  return(word)
}


