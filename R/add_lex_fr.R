#' Add words to the French lexicon
#'
#' Adds one or more French words to the IPA-override lexicon used by
#' \code{ipa(lg = "French")}. Supply the plain orthographic form(s) in
#' \code{words} and the corresponding IPA string(s) in \code{ipa}. The IPA is
#' stored verbatim and returned directly---the transcription pipeline is
#' bypassed entirely.
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

  lex <- get("fr_ipa_lex", envir = as.environment("package:Fonology"))

  new_entries       <- ipa
  names(new_entries) <- plain
  lex <- c(lex[!names(lex) %in% plain], new_entries)

  fr_ipa_lex <- lex
  save(fr_ipa_lex,
       file = file.path(find.package("Fonology"), "data", "fr_ipa_lex.rda"),
       compress = "xz")

  .lex_assign("fr_ipa_lex", lex)

  invisible(lex)
}
