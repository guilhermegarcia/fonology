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
  message("Transcriptions using ipa(lg = \"Italian\"):")

  testWords <- c(
    "italiano", "bambino", "amore",
    "patto", "mamma", "pizza",
    "ciao", "chiesa", "scuola",
    "gnocchi", "figlio", "giorno",
    "cena", "accento", "acqua",
    "citt\u00e0", "caff\u00e8", "virt\u00f9",
    "rosa", "casa"
  )

  for (i in seq_along(testWords)) {
    message(stringr::str_c(testWords[i], ":"))
    ipa_it(testWords[i]) |> print()
    message("========================")
  }
}
