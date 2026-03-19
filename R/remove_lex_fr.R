#' Remove words from the French user lexicon
#'
#' Removes one or more words from the French IPA-override lexicon.
#'
#' @param words A character vector of words to remove (plain orthographic
#'   forms).
#' @return Invisibly returns the updated IPA-override lexicon.
#' @seealso \code{\link{add_lex_fr}}, \code{\link{export_lex}}
#' @noRd

remove_lex_fr <- function(words) {

  plain <- stringr::str_to_lower(words)

  lex <- get("fr_ipa_lex", envir = as.environment("package:Fonology"))
  lex <- lex[!names(lex) %in% plain]

  fr_ipa_lex <- lex
  save(fr_ipa_lex,
       file = file.path(find.package("Fonology"), "data", "fr_ipa_lex.rda"),
       compress = "xz")
  .lex_assign("fr_ipa_lex", lex)

  invisible(lex)
}
