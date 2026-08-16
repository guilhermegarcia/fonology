#' IPA tester for \code{ipa()} function in Fonology package (Italian)
#'
#' Returns a list of Italian words transcribed using \code{ipa(lg = "Italian")}.
#' No arguments needed. Words are chosen to exercise the main phonological
#' patterns of Standard Italian: palatals, geminates, final stress, glides,
#' and intervocalic voicing.
#' @return A list of words transcribed using \code{ipa(lg = "Italian")}
#' @examples
#' ipa_it_test()
#' @export

ipa_it_test <- function() {
  cli::cli_h1("Transcriptions using {.code ipa(lg = \"Italian\")}")

  testWords <- c(
    "italiano", "bambino", "amore",
    "patto", "mamma", "pizza",
    "ciao", "chiesa", "scuola",
    "gnocchi", "figlio", "giorno",
    "cena", "accento", "acqua",
    "citt\u00e0", "caff\u00e8", "virt\u00f9",
    "rosa", "casa"
  )

  out <- vapply(testWords, ipa_it, character(1), USE.NAMES = TRUE)

  cli::cli_dl(out)

  invisible(out)
}
