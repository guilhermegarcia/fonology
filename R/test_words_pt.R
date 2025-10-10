#' IPA tester for \code{ipa_pt()} and \code{ipa()} functions in Fonology package
#'
#' Returns a list of words. No arguments needed.
#' @return A list of words broadly and narrowly transcribed using \code{ipa_pt} and \code{ipa}
#' @examples
#' ipa_pt_test()
#' @export

ipa_pt_test <- function() {
  message("Broad and narrow transcriptions using ipa_pt():")


  testWords <- c(
    "strada",
    "cam\u00f3tipo",
    "paitrado",
    "m\u00eatilo",
    "frantidolanildo",
    "wagmo",
    "lispico",
    "fadist\u00e3o",
    "frinte",
    "catto",
    "dane",
    "mendes",
    "mendez",
    "merpis",
    "mylena",
    "parangaricutirrimirruaro"
  )


  for (i in 1:length(testWords)) {
    message(stringr::str_c(testWords[i], ":"))

    ipa_pt(testWords[i]) |>
      print()

    ipa_pt(testWords[i], narrow = T) |>
      print()

    message("========================")
  }

  message("Vectorized version using ipa(..., narrow = F):")
  print(ipa(testWords))

  message("Vectorized version using ipa(..., narrow = T):")
  print(ipa(testWords, narrow = T))
}
