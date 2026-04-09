#' IPA tester for \code{ipa()} function in Fonology package
#'
#' Returns a list of English words. No arguments needed.
#' @return A list of words broadly transcribed using \code{ipa(lg = "English")}
#' @examples
#' ipa_en_test()
#' @export

ipa_en_test <- function() {
  message("Transcriptions using ipa():")

  testWords <- c(
    "hospital", "comfortably",
    "naive", "pretty",
    "international", "chuck",
    "history", "mistake",
    "aspect", "despite",
    "blick", "spling"
  )

  for (i in seq_along(testWords)) {
    message(stringr::str_c(testWords[i], ":"))

    ipa(testWords[i], lg = "en") |>
      print()

    message("========================")
  }
}
