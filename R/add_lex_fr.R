#' Add words to the French lexicon
#'
#' Adds one or more French words to the IPA-override lexicon used by
#' \code{ipa(lg = "French")}. Supply the plain orthographic form(s) in
#' \code{words} and the corresponding IPA string(s) in \code{ipa}. The IPA is
#' stored verbatim and returned directly---the transcription pipeline is
#' bypassed entirely.
#'
#' Changes made by this function are local to the current package
#' installation. When used from a source checkout loaded with
#' \code{devtools::load_all()}, the corresponding data file in the source tree
#' is updated and can be committed.
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
    stop("French requires IPA-override mode: supply both `words` and `ipa`.")
  }
  if (length(words) != length(ipa)) {
    stop("`words` and `ipa` must have the same length.")
  }

  plain <- stringr::str_to_lower(words)
  keep  <- !duplicated(plain, fromLast = TRUE)
  plain <- plain[keep]
  ipa   <- ipa[keep]

  lex <- .get_user_lex("fr_ipa_lex")

  new_entries       <- ipa
  names(new_entries) <- plain
  lex <- c(lex[!names(lex) %in% plain], new_entries)

  .set_user_lex("fr_ipa_lex", lex)
  .save_user_lex("fr_ipa_lex")

  invisible(lex)
}
