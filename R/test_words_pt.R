#' IPA tester for \code{ipa_pt()} and \code{ipa()} functions in Fonology package
#'
#' Returns a list of words. No arguments needed.
#' @return A list of words broadly and narrowly transcribed using \code{ipa_pt} and \code{ipa}
#' @examples
#' ipa_pt_test()
#' @export

ipa_pt_test <- function() {
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

  broad <- ipa(testWords, narrow = FALSE)
  narrow <- ipa(testWords, narrow = TRUE)

  names(broad) <- testWords
  names(narrow) <- testWords

  cli::cli_h1("Broad and narrow transcriptions using {.fn ipa_pt}")

  cli::cli_h2("Broad")
  cli::cli_dl(broad)

  cli::cli_h2("Narrow")
  cli::cli_dl(narrow)

  invisible(list(broad = broad, narrow = narrow))
}
