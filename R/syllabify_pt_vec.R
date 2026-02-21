#' Vectorized syllabifier for Portuguese
#'
#' Returns syllabification for a given string.
#' @param word The strings of interest using IPA phonemic transcription
#' @return The syllabification for the strings in question
#' @noRd

syllabify_pt_vec <- function(word = "") {
  # Start with CV:
  word <- stringr::str_replace_all(
    string = word,
    pattern = "([aeiou\u025b\u0254\u00e1\u00e9\u00ed\u00f3\u00fa\u00e0\u00e8\u00ec\u00f2\u00f9\u00ea\u00f4\u00e2\u00f4\u00ea])",
    replacement = "\\1."
  )

  # Fix nasalization over syllable boundary:
  word <- stringr::str_replace(
    string = word,
    pattern = "a.\u0303",
    replacement = "\u00e3"
  )

  word <- stringr::str_replace(
    string = word,
    pattern = "o.\u0303",
    replacement = "\u00f5"
  )

  # Fix diphthongs:
  word <- stringr::str_replace_all(
    string = word,
    pattern = "([aeiou\u00e1\u00e9\u00ed\u00f3\u00fa\u00e0\u00e8\u00ec\u00f2\u00f9\u025b\u0254\u00e2\u00f4\u00ea])\\.([wj])",
    replacement = "\\1\\2."
  )

  # Fix onset clusters:
  # Sonorants (l, m, n, ɾ, r) and s never begin valid two-consonant onsets
  # in Portuguese, so always move them to the preceding coda.
  word <- stringr::str_replace_all(
    string = word,
    pattern = "\\.([lmn\u027ers])([pbtdkgsxzfv\u0283\u0292\u028e\u0272mnl])",
    replacement = "\\1.\\2"
  )
  # Stops (p, b, k, g) split before another obstruent or nasal, but NOT
  # before l — pl, bl, kl, gl are valid onsets in Portuguese.
  word <- stringr::str_replace_all(
    string = word,
    pattern = "\\.([kgpb])([pbtdkgsxzfv\u0283\u0292\u028e\u0272mn])",
    replacement = "\\1.\\2"
  )


  # Remove empty final syllables:
  word <- stringr::str_remove_all(
    string = word,
    pattern = "\\.$"
  )

  # Remove C-syllables word finally:
  word <- stringr::str_replace_all(
    string = word,
    pattern = "\\.([pbtdkgszfv\u0283\u0292\u028elmn\u027ers])$",
    replacement = "\\1"
  )

  word <- stringr::str_replace_all(
    string = word,
    pattern = "\\.([sznm])([lr])",
    replacement = "\\1.\\2"
  )

  # Remove h:
  word <- stringr::str_remove_all(
    string = word,
    pattern = "h"
  )

  # Overly complex onset clusters:
  word <- stringr::str_replace_all(
    string = word,
    pattern = "\\.s([tdpbkg][\u027erl])",
    replacement = "s.\\1"
  )

  # Fix sf sequences
  word <- stringr::str_replace_all(
    string = word,
    pattern = "n\\.sf",
    replacement = "ns.f"
  )

  # Fix kt sequences
  word <- stringr::str_replace_all(
    string = word,
    pattern = "([szlrnm\u014b])\\.([ptkbdgfvsz\u0283\u0292])([ptkbdgfvsz\u0283\u0292])",
    replacement = "\\1\\2.\\3"
  )


  # Remove word-final syllable boundary:
  word <- stringr::str_remove_all(
    string = word,
    pattern = "\\.$"
  )

  # Adjust complex nasal diphthongs (especially when followed by clitics)
  word <- word |>
    stringr::str_replace(
      pattern = "(j\u0303)([pbtdgkfvl\u028emnrsz\u027e\u0283\u0292x])",
      replacement = "\\1.\\2"
    )

  # Adjust complex nasal diphthongs word-finally:
  word <- word |>
    stringr::str_replace(
      pattern = "(w\u0303|j\u0303)\\.(s$)",
      replacement = "\\1\\2"
    )

  # Adjust gu.e with diacritics:
  # word = word |>
  #   stringr::str_replace(pattern = "(gu)")
  #

  # Onset maximization for [jw].[aeiou]
  word <- word |>
    stringr::str_replace_all(
      pattern = "([jw])\\.([aeiou\u00e1\u00e9\u00ed\u00f3\u00fa\u00e0\u00e8\u00ec\u00f2\u00f9\u00ea\u00f4\u00e2\u00f4\u00ea])",
      replacement = ".\\1\\2"
    )


  # Vowelless syllables word-finally:
  word <- word |>
    stringr::str_replace_all(
      pattern = "(\\w+)\\.([bdfgklmnp\u027erstvxz]+$)",
      replacement = "\\1\\2"
    )

  # NOTE: nasal-ending compound members
  word <- word |>
    stringr::str_replace_all(
      pattern = "w\u0303(?!s|$)",
      replacement = "w\u0303."
    )

  # NOTE: Fix syllabification for certain loanwords
  word <- word |>
    stringr::str_replace_all(
      pattern = "(\\w+)\\.t\u0292",
      replacement = "\\1t.\u0292"
    )

  word <- word |>
    stringr::str_replace_all(
      pattern = "(\\w+)\\.ts",
      replacement = "\\1t.s"
    )

  word <- word |>
    stringr::str_replace_all(
      pattern = "k\\.k",
      replacement = ".k"
    )



  # Two plosives word-finally:
  word <- word |>
    stringr::str_replace_all(
      pattern = "dt$|td$",
      replacement = "t"
    )

  return(word)
}
