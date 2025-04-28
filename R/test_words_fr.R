#' IPA tester for \code{ipa()} function in Fonology package
#'
#' Returns a list of words. No arguments needed.
#' @return A list of words broadly and narrowly transcribed using \code{ipa(lg = "French")}
#' @examples
#' ipa_fr_test()
#' @export

ipa_fr_test <- function() {
  message("Transcriptions using ipa():")


  testWords <- c(
    "informatique", "combinaison",
    "chateaux", "table",
    "comportement", "chat",
    "fr\u00e9quemment", "parfois",
    "mettre", "tout",
    "o\u00f9", "r\u00e9p\u00e9t\u00e9",
    "tu", "couleur"
  )


  for (i in 1:length(testWords)) {
    message(stringr::str_c(testWords[i], ":"))

    ipa_fr(testWords[i]) |>
      print()

    message("========================")
  }
}
