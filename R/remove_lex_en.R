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

  lex <- get("en_ipa_lex", envir = as.environment("package:Fonology"))
  lex <- lex[!names(lex) %in% plain]

  en_ipa_lex <- lex
  save(en_ipa_lex,
       file = file.path(find.package("Fonology"), "data", "en_ipa_lex.rda"),
       compress = "xz")
  .lex_assign("en_ipa_lex", lex)

  invisible(lex)
}
