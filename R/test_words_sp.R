#' IPA tester for \code{ipa()} function in Fonology package
#'
#' Returns a list of words. No arguments needed.
#' @return A list of words broadly and narrowly transcribed using \code{ipa(lg = "Spanish")}
#' @examples
#' ipa_sp_test()
#' @export

ipa_sp_test <- function() {
  cli::cli_h1("Transcriptions using {.code ipa(lg = \"Spanish\")}")

  testWords <- c(
    "la", "algunos", "comunicarnos",
    "mejorado", "como", "nuevos",
    "en", "efectos", "nos",
    "vida", "tecnolog\u00eda", "m\u00e9dicos",
    "y", "con", "de",
    "acceder", "redes", "y",
    "en", "tiene"
  )

  out <- vapply(testWords, ipa_sp, character(1), USE.NAMES = TRUE)

  cli::cli_dl(out)

  invisible(out)
}
