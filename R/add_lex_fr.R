#' Add words to the French lexicon
#'
#' Adds one or more French words to the IPA-override lexicon used by
#' \code{ipa(lg = "French")}. Supply the plain orthographic form(s) in
#' \code{words} and the corresponding IPA string(s) in \code{ipa}. The IPA is
#' stored verbatim and returned directly---the transcription pipeline is
#' bypassed entirely.
#'
#' Entries added here are local to the current machine: they are written to
#' the user data directory returned by \code{tools::R_user_dir("Fonology")} and
#' survive package updates. They are kept separate from the corrections that
#' ship with the package, which they override; see \code{\link{promote_lex}}.
#'
#' @param words A character vector of French words in plain orthographic form.
#' @param ipa A character vector of IPA strings the same length as
#'   \code{words}.
#' @return Invisibly returns the updated IPA-override lexicon.
#' @seealso \code{\link{export_lex}}
#' @examples
#' \dontrun{
#' add_lex_fr("femme", ipa = "fam")
#' ipa("femme", lg = "fr")
#' }
#' @export

add_lex_fr <- function(words, ipa) {
  if (missing(ipa)) {
    cli::cli_abort(c(
      "French requires IPA-override mode.",
      "i" = "Supply both {.arg words} and {.arg ipa}, e.g. {.code add_lex_fr(\"femme\", ipa = \"fam\")}."
    ))
  }
  if (length(words) != length(ipa)) {
    cli::cli_abort(c(
      "{.arg words} and {.arg ipa} must have the same length.",
      "x" = "{.arg words} has length {length(words)}, but {.arg ipa} has length {length(ipa)}."
    ))
  }

  plain <- stringr::str_to_lower(words)
  keep  <- !duplicated(plain, fromLast = TRUE)
  plain <- plain[keep]
  ipa   <- ipa[keep]

  .check_ipa_override(plain, ipa, fn = "add_lex_fr")


  lex <- .get_user_lex("fr_ipa_lex")

  new_entries       <- ipa
  names(new_entries) <- plain
  lex <- c(lex[!names(lex) %in% plain], new_entries)

  .set_user_lex("fr_ipa_lex", lex)
  .save_user_lex("fr_ipa_lex")

  invisible(lex)
}
