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

  # Fix onset clusteres:
  word <- stringr::str_replace_all(
    string = word,
    pattern = "\\.([lmn\u027eskgpb])([pbtdkgsxzfv\u0283\u0292\u028e\u0272mn])",
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
    pattern = "\\.([pbtdkgszfv\u0283\u0292\u028elmn\u027es])$",
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
    pattern = "\\.s([tdpbkg][\u027el])",
    replacement = "s.\\1"
  )

  # Remove word-final syllable boundary:
  word <- stringr::str_remove_all(
    string = word,
    pattern = "\\.$"
  )

  # Adjust complex nasal diphthongs (especially when followed by clitics)
  word <- word |>
    stringr::str_replace(
      pattern = "(j\u0303)([pbtdgkfvl\u028emnsz\u027e\u0283\u0292x])",
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
      pattern = "(\\w+)\\.([bdfgklmnp\u027estvxz]+$)",
      replacement = "\\1\\2"
    )

  # NOTE: nasal-ending compound members
  word <- word |>
    stringr::str_replace_all(
      pattern = "w\u0303(?!s|$)",
      replacement = "w\u0303."
    )


  # Two plosives word-finally:
  word <- word |>
    stringr::str_replace_all(
      pattern = "dt$|td$",
      replacement = "t"
    )

  return(word)
}
