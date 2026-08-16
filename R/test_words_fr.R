#' IPA tester for \code{ipa()} function in Fonology package
#'
#' Returns a list of words. No arguments needed.
#' @return A list of words broadly and narrowly transcribed using \code{ipa(lg = "French")}
#' @examples
#' ipa_fr_test()
#' @export

ipa_fr_test <- function() {
  cli::cli_h1("Transcriptions using {.code ipa(lg = \"French\")}")

  testWords <- c(
    "informatique", "combinaison",
    "chateaux", "table",
    "comportement", "chat",
    "fr\u00e9quemment", "parfois",
    "mettre", "tout",
    "o\u00f9", "r\u00e9p\u00e9t\u00e9",
    "tu", "couleur"
  )

  out <- vapply(testWords, ipa_fr, character(1), USE.NAMES = TRUE)

  cli::cli_dl(out)

  invisible(out)
}
