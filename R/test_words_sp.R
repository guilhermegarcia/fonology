#' IPA tester for \code{ipa()} function in Fonology package
#'
#' Returns a list of words. No arguments needed.
#' @return A list of words broadly and narrowly transcribed using \code{ipa(lg = "Spanish")}
#' @examples
#' ipa_sp_test()
#' @export

ipa_sp_test <- function() {
  message("Transcriptions using ipa():")


  testWords <- c(
    "la", "algunos", "comunicarnos",
    "mejorado", "como", "nuevos",
    "en", "efectos", "nos",
    "vida", "tecnolog\u00eda", "m\u00e9dicos",
    "y", "con", "de",
    "acceder", "redes", "y",
    "en", "tiene"
  )


  for (i in 1:length(testWords)) {
    message(stringr::str_c(testWords[i], ":"))

    ipa_sp(testWords[i]) |>
      print()

    message("========================")
  }
}
