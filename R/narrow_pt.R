#' Narrow IPA transcriber for Portuguese
#'
#' Returns IPA phonemic transcription for a given string that is already phonemically transcribed.
#' @param word The string of interest is an output of \code{ipa()}
#' @noRd
#' @return The IPA transcription of said string with surface-level adjustments

narrow_pt <- function(word = "") {
  narrow <- word

  narrow <- stringr::str_replace(narrow,
    pattern = "am$",
    replacement = "\u00e3w\u0303"
  )

  # Vowel neutralization (word-final):
  narrow <- stringr::str_replace(narrow,
    pattern = "(\\.[^\u02c8]\\w*)os$",
    replacement = "\\1\u028as"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\\.[^\u02c8]\\w*)es$",
    replacement = "\\1\u026as"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\\.[^\u02c8]\\w*)as$",
    replacement = "\\1\u0250s"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\\.[^\u02c8]\\w*)o$",
    replacement = "\\1\u028a"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\\.[^\u02c8]\\w*)e$",
    replacement = "\\1\u026a"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\\.[^\u02c8]\\w*)a$",
    replacement = "\\1\u0250"
  )


  # Palatalization:
  narrow <- stringr::str_replace_all(narrow,
    pattern = "t([i\u026a])",
    replacement = "t\u0361\u0283\\1"
  )

  narrow <- stringr::str_replace_all(narrow,
    pattern = "d([i\u026a])",
    replacement = "d\u0361\u0292\\1"
  )

  # Vowel devoicing word-finally after voiceless affricate:
  narrow <- stringr::str_replace(narrow,
    pattern = "(t\u0361\u0283)\u026a$",
    replacement = "\\1\u026a\u0325"
  )

  # Clusters and epentheses:
  narrow <- stringr::str_replace(narrow,
    pattern = "^ps",
    replacement = "pi.s"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "k\\.(\u02c8*t)",
    replacement = ".ki\u0325.\\1"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "^sk",
    replacement = "is.k"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "p\\.(\u02c8*t)",
    replacement = ".pi\u0325.\\1"
  )

  # Nasal place assimilation:
  narrow <- stringr::str_replace(narrow,
    pattern = "n\\.([kg])",
    replacement = "\u014b.\\1"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "n\\.\u02c8([kg])",
    replacement = "\u014b.\u02c8\\1"
  )

  # Nasalization:
  narrow <- stringr::str_replace_all(narrow,
    pattern = "a([nm\u0272\u014b])",
    replacement = "\u00e3\\1"
  )

  narrow <- stringr::str_replace_all(narrow,
    pattern = "o([nm\u0272\u014b])",
    replacement = "\u00f5\\1"
  )

  narrow <- stringr::str_replace_all(narrow,
    pattern = "e([nm\u0272\u014b])",
    replacement = "e\u0303j\u0303\u0272"
  )

  narrow <- stringr::str_replace_all(narrow,
    pattern = "u([nm\u0272\u014b])",
    replacement = "u\u0303\\1"
  )

  narrow <- stringr::str_replace_all(narrow,
    pattern = "i([nm\u0272\u014b])",
    replacement = "i\u0303\\1"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\u02c8\\w+)a(\\.[mn\u0272])",
    replacement = "\\1\u00e3\\2"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\u02c8\\w+)e(\\.[mn\u0272])",
    replacement = "\\1e\u0303\\2"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\u02c8\\w+)i(\\.[mn\u0272])",
    replacement = "\\1i\u0303\\2"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\u02c8\\w+)o(\\.[mn\u0272])",
    replacement = "\\1\u00f5\\2"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\u02c8\\w+)u(\\.[mn\u0272])",
    replacement = "\\1u\u0303\\2"
  )



  # Lowering word-finally (before l-vocalization):
  narrow <- narrow |>
    stringr::str_replace(
      pattern = "(\u02c8\\w*)ol$",
      replacement = "\\1\u0254l"
    ) |>
    stringr::str_replace(
      pattern = "(\u02c8\\w*)el$",
      replacement = "\\1\u025bl"
    )


  # l-vocalization:
  narrow <- stringr::str_replace_all(narrow,
    pattern = "l($|\\.)",
    replacement = "w\\1"
  )

  # Voicing assimilation
  narrow <- stringr::str_replace_all(
    string = narrow,
    pattern = "s(\\.[mnl])",
    replacement = "z\\1"
  )

  # Diphthongization:
  # i.a -> ja, but diphthongization is blocked if onset is already CC
  # This shows that (a) max onset is 2, and (b) j is in onset position

  # When V.'V:
  # First, word-initial:
  narrow <- stringr::str_replace_all(narrow,
    pattern = "(^\\w?)i\\.\u02c8a",
    replacement = "\u02c8\\1ja"
  )

  # Word-internal:
  narrow <- stringr::str_replace_all(narrow,
    pattern = "(\\.\\w?)i\\.\u02c8a",
    replacement = "\u02c8\\1ja"
  )

  # When V.V:
  # First, word-initial:
  narrow <- stringr::str_replace_all(narrow,
    pattern = "(^\\w?)i\\.a",
    replacement = "\\1ja"
  )

  # Word-internal:
  narrow <- stringr::str_replace_all(narrow,
    pattern = "(\\.\\w?)i\\.a",
    replacement = "\\1ja"
  )

  # Epenthesis:
  narrow <- stringr::str_replace(narrow,
    pattern = "^s([ptkf])",
    replacement = "is.\\1"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "^s([bdgmnvlr])",
    replacement = "iz.\\1"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "([pbtdkg])\\.([nmpbtdkg])",
    replacement = ".\\1i.\\2"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "^\u02c8s([ptkf])",
    replacement = "is.\u02c8\\1"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "^\u02c8s([bdgmnvlr])",
    replacement = "iz.\u02c8\\1"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "([pbtdkg])\\.\u02c8([nmpbtdkg])",
    replacement = ".\\1i.\u02c8\\2"
  )

  narrow <- narrow |>
    sec_stress_pt()


  # Paragoge
  narrow <- stringr::str_replace(narrow,
    pattern = "t$",
    replacement = ".t\u0361\u0283\u026a"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "d$",
    replacement = ".d\u0361\u0292\u026a"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "([pbkgfv])$",
    replacement = ".\\1\u026a"
  )

  # Vowel devoicing word-finally after voiceless affricate:
  narrow <- stringr::str_replace(narrow,
    pattern = "(t\u0361\u0283)\u026a$",
    replacement = "\\1\u026a\u0325"
  )


  return(narrow)
}


#' Vectorized narrow IPA transcriber for Portuguese
#'
#' Returns IPA phonemic transcription for a given string that is already phonemically transcribed
#' @param word The string of interest is an output of ipa_pt()
#' @noRd
#' @return The IPA transcription of said string with surface-level adjustments

narrow_pt_vec <- function(word = "") {
  narrow <- word

  narrow <- stringr::str_replace(narrow,
    pattern = "am$",
    replacement = "\u00e3w\u0303"
  )

  # Vowel neutralization (word-final):
  narrow <- stringr::str_replace(narrow,
    pattern = "(\\.[^\u02c8]\\w*)os$",
    replacement = "\\1\u028as"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\\.[^\u02c8]\\w*)es$",
    replacement = "\\1\u026as"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\\.[^\u02c8]\\w*)as$",
    replacement = "\\1\u0250s"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\\.[^\u02c8]\\w*)o$",
    replacement = "\\1\u028a"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\\.[^\u02c8]\\w*)e$",
    replacement = "\\1\u026a"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\\.[^\u02c8]\\w*)a$",
    replacement = "\\1\u0250"
  )

  # Palatalization:
  narrow <- stringr::str_replace_all(narrow,
    pattern = "t([i\u026a])",
    replacement = "t\u0361\u0283\\1"
  )

  narrow <- stringr::str_replace_all(narrow,
    pattern = "d([i\u026a])",
    replacement = "d\u0361\u0292\\1"
  )

  # Vowel devoicing word-finally after voiceless affricate:
  narrow <- stringr::str_replace(narrow,
    pattern = "(t\u0361\u0283)\u026a$",
    replacement = "\\1\u026a\u0325"
  )

  # Clusters and epentheses:
  narrow <- stringr::str_replace(narrow,
    pattern = "^ps",
    replacement = "pi.s"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "k\\.(\u02c8*t)",
    replacement = ".ki\u0325.\\1"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "^sk",
    replacement = "is.k"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "p\\.(\u02c8*t)",
    replacement = ".pi\u0325.\\1"
  )

  # Nasal place assimilation:
  narrow <- stringr::str_replace(narrow,
    pattern = "n\\.([kg])",
    replacement = "\u014b.\\1"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "n\\.\u02c8([kg])",
    replacement = "\u014b.\u02c8\\1"
  )

  # Nasalization:
  narrow <- stringr::str_replace_all(narrow,
    pattern = "a([nm\u0272\u014b])",
    replacement = "\u00e3\\1"
  )

  narrow <- stringr::str_replace_all(narrow,
    pattern = "o([nm\u0272\u014b])",
    replacement = "\u00f5\\1"
  )

  narrow <- stringr::str_replace_all(narrow,
    pattern = "e([nm\u0272\u014b])",
    replacement = "e\u0303j\u0303\u0272"
  )

  narrow <- stringr::str_replace_all(narrow,
    pattern = "u([nm\u0272\u014b])",
    replacement = "u\u0303\\1"
  )

  narrow <- stringr::str_replace_all(narrow,
    pattern = "i([nm\u0272\u014b])",
    replacement = "i\u0303\\1"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\u02c8\\w+)a(\\.[mn\u0272])",
    replacement = "\\1\u00e3\\2"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\u02c8\\w+)e(\\.[mn\u0272])",
    replacement = "\\1e\u0303\\2"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\u02c8\\w+)i(\\.[mn\u0272])",
    replacement = "\\1i\u0303\\2"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\u02c8\\w+)o(\\.[mn\u0272])",
    replacement = "\\1\u00f5\\2"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "(\u02c8\\w+)u(\\.[mn\u0272])",
    replacement = "\\1u\u0303\\2"
  )



  # Lowering word-finally (before l-vocalization):
  narrow <- narrow |>
    stringr::str_replace(
      pattern = "(\u02c8\\w*)ol$",
      replacement = "\\1\u0254l"
    ) |>
    stringr::str_replace(
      pattern = "(\u02c8\\w*)el$",
      replacement = "\\1\u025bl"
    )


  # l-vocalization:
  narrow <- stringr::str_replace_all(narrow,
    pattern = "l($|\\.)",
    replacement = "w\\1"
  )

  # Voicing assimilation
  narrow <- stringr::str_replace_all(
    string = narrow,
    pattern = "s(\\.[mnl])",
    replacement = "z\\1"
  )

  # Diphthongization:
  # i.a -> ja, but diphthongization is blocked if onset is already CC
  # This shows that (a) max onset is 2, and (b) j is in onset position

  # When V.'V:
  # First, word-initial:
  narrow <- stringr::str_replace_all(narrow,
    pattern = "(^\\w?)i\\.\u02c8a",
    replacement = "\u02c8\\1ja"
  )

  # Word-internal:
  narrow <- stringr::str_replace_all(narrow,
    pattern = "(\\.\\w?)i\\.\u02c8a",
    replacement = "\u02c8\\1ja"
  )

  # When V.V:
  # First, word-initial:
  narrow <- stringr::str_replace_all(narrow,
    pattern = "(^\\w?)i\\.a",
    replacement = "\\1ja"
  )

  # Word-internal:
  narrow <- stringr::str_replace_all(narrow,
    pattern = "(\\.\\w?)i\\.a",
    replacement = "\\1ja"
  )

  # Epenthesis:
  narrow <- stringr::str_replace(narrow,
    pattern = "^s([ptkf])",
    replacement = "is.\\1"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "^s([bdgmnvlr])",
    replacement = "iz.\\1"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "([pbtdkg])\\.([nmpbtdkg])",
    replacement = ".\\1i.\\2"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "^\u02c8s([ptkf])",
    replacement = "is.\u02c8\\1"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "^\u02c8s([bdgmnvlr])",
    replacement = "iz.\u02c8\\1"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "([pbtdkg])\\.\u02c8([nmpbtdkg])",
    replacement = ".\\1i.\u02c8\\2"
  )

  narrow <- narrow |>
    sec_stress_pt_vec()

  # Paragoge
  narrow <- stringr::str_replace(narrow,
    pattern = "t$",
    replacement = ".t\u0361\u0283\u026a"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "d$",
    replacement = ".d\u0361\u0292\u026a"
  )

  narrow <- stringr::str_replace(narrow,
    pattern = "([pbkgfv])$",
    replacement = ".\\1\u026a"
  )

  # Vowel devoicing word-finally after voiceless affricate:
  narrow <- stringr::str_replace(narrow,
    pattern = "(t\u0361\u0283)\u026a$",
    replacement = "\\1\u026a\u0325"
  )


  # Adjust secondary stress:

  # "seMI" to "SEmi" prefix:
  narrow <- narrow |>
    stringr::str_replace_all(
      pattern = "^se\\.\u02ccmi",
      replacement = "\u02ccse.mi"
    )


  return(narrow)
}
