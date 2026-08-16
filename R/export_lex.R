#' Export a user lexicon to a plain-text file
#'
#' Writes lexicon entries to a plain-text file for sharing.
#'
#' \strong{Diacritized-form mode} (\code{ipa = FALSE}, default): writes one
#' diacritized word per line. The file can be re-imported directly via
#' the corresponding \code{add_lex_XX()} function.
#'
#' \strong{IPA-override mode} (\code{ipa = TRUE}): writes IPA-override entries
#' as tab-separated \samp{plain_form<TAB>IPA} pairs, one per line. These can
#' be re-imported by reading the file and calling \code{add_lex_XX(words, ipa)}.
#'
#' @param lg Language: \code{"it"}, \code{"sp"}, \code{"pt"}, \code{"fr"}, or \code{"en"}.
#' @param file Path to the output file.
#' @param ipa Logical. If \code{FALSE} (default), export the diacritized-form
#'   lexicon. If \code{TRUE}, export the IPA-override lexicon.
#' @return Invisibly returns the character vector (diacritized-form mode) or
#'   named character vector (IPA-override mode) that was written.
#' @examples
#' \dontrun{
#' export_lex("it", "italian_entries.txt")
#' add_lex_it("italian_entries.txt")
#'
#' export_lex("pt", "pt_ipa_overrides.tsv", ipa = TRUE)
#' }
#' @export

export_lex <- function(lg, file, ipa = FALSE) {
  lg <- stringr::str_to_lower(lg)

  if (ipa) {
    lex_name <- switch(
      lg,
      "it" = "it_ipa_lex",
      "sp" = "sp_ipa_lex",
      "pt" = "pt_ipa_lex",
      "fr" = "fr_ipa_lex",
      "en" = "en_ipa_lex",
      cli::cli_abort("{.arg lg} must be one of {.val it}, {.val sp}, {.val pt}, {.val fr}, or {.val en}.")
    )

    lex <- .lex(lex_name)

    if (length(lex) == 0) {
      cli::cli_alert_info("IPA lexicon is empty \u2014 nothing to export.")
      return(invisible(lex))
    }

    lines <- paste(names(lex), unname(lex), sep = "\t")
    writeLines(lines, file)
    cli::cli_alert_success("{length(lex)} IPA-override entr{?y/ies} written to {.file {file}}.")
    return(invisible(lex))
  }

  lex_name <- switch(
    lg,
    "it" = "it_lex_user",
    "sp" = "sp_lex_user",
    "pt" = "pt_lex_user",
    "fr" = cli::cli_abort(c(
      "French only supports IPA-override mode.",
      "i" = "Use {.code export_lex(\"fr\", file, ipa = TRUE)}."
    )),
    "en" = cli::cli_abort(c(
      "English only supports IPA-override mode.",
      "i" = "Use {.code export_lex(\"en\", file, ipa = TRUE)}."
    )),
    cli::cli_abort("{.arg lg} must be one of {.val it}, {.val sp}, {.val pt}, {.val fr}, or {.val en}.")
  )

  lex <- .lex(lex_name)

  if (length(lex) == 0) {
    cli::cli_alert_info("Lexicon is empty \u2014 nothing to export.")
    return(invisible(character(0)))
  }

  writeLines(unname(lex), file)
  cli::cli_alert_success("{length(lex)} entr{?y/ies} written to {.file {file}}.")
  invisible(unname(lex))
}
