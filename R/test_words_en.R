#' IPA tester for \code{ipa()} function in Fonology package
#'
#' Returns a list of English words. No arguments needed.
#' @return A list of words broadly transcribed using \code{ipa(lg = "English")}
#' @examples
#' ipa_en_test()
#' @export

ipa_en_test <- function() {
  cli::cli_h1("Transcriptions using {.code ipa(lg = \"English\")}")

  testWords <- c(
    "hospital", "comfortably",
    "naive", "pretty",
    "international", "chuck",
    "history", "mistake",
    "aspect", "despite",
    "blick", "spling"
  )

  out <- vapply(testWords, function(w) ipa(w, lg = "en"), character(1), USE.NAMES = TRUE)

  cli::cli_dl(out)

  invisible(out)
}
