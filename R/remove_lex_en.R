#' Remove words from the English user lexicon
#'
#' Removes one or more words from the English IPA-override lexicon.
#'
#' @param words A character vector of words to remove.
#' @return Invisibly returns the updated IPA lexicon.
#' @seealso \code{\link{add_lex_en}}, \code{\link{export_lex}}
#' @noRd

remove_lex_en <- function(words) {
  plain <- stringr::str_to_lower(words)

  lex <- .get_user_lex("en_ipa_lex")
  lex <- lex[!names(lex) %in% plain]

  .set_user_lex("en_ipa_lex", lex)
  .save_user_lex("en_ipa_lex")

  invisible(lex)
}
