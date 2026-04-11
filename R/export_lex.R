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
      stop("lg must be one of: \"it\", \"sp\", \"pt\", \"fr\", \"en\"")
    )

    lex <- .get_user_lex(lex_name)

    if (length(lex) == 0) {
      message("IPA lexicon is empty - nothing to export.")
      return(invisible(lex))
    }

    lines <- paste(names(lex), unname(lex), sep = "\t")
    writeLines(lines, file)
    message(length(lex), " IPA-override entries written to ", file)
    return(invisible(lex))
  }

  lex_name <- switch(
    lg,
    "it" = "it_lex",
    "sp" = "sp_lex",
    "pt" = "pt_lex_user",
    "fr" = stop("French only supports IPA-override mode: use export_lex(\"fr\", file, ipa = TRUE)."),
    "en" = stop("English only supports IPA-override mode: use export_lex(\"en\", file, ipa = TRUE)."),
    stop("lg must be one of: \"it\", \"sp\", \"pt\", \"fr\", \"en\"")
  )

  lex <- .get_user_lex(lex_name)

  if (length(lex) == 0) {
    message("Lexicon is empty - nothing to export.")
    return(invisible(character(0)))
  }

  writeLines(unname(lex), file)
  message(length(lex), " entries written to ", file)
  invisible(unname(lex))
}
