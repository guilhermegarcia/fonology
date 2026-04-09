#' Add words to the English lexicon
#'
#' Adds one or more English words to the IPA-override lexicon used by
#' \code{ipa(lg = "English")}. Supply the plain orthographic form(s) in
#' \code{words} and the corresponding IPA string(s) in \code{ipa}. The IPA is
#' stored verbatim and returned directly---the CMU-backed lookup and fallback
#' heuristic are bypassed entirely.
#'
#' Changes made by this function are local to the current package
#' installation. When used from a source checkout loaded with
#' \code{devtools::load_all()}, the corresponding data file in the source tree
#' is updated and can be committed.
#'
#' @param words A character vector of English words in plain orthographic form.
#' @param ipa A character vector of IPA strings the same length as
#'   \code{words}.
#' @return Invisibly returns the updated IPA-override lexicon.
#' @seealso \code{\link{export_lex}}
#' @examples
#' \dontrun{
#' add_lex_en("naive", ipa = "na.\u02c8iv")
#' ipa("naive", lg = "en")
#' }
#' @export

add_lex_en <- function(words, ipa) {
  if (missing(ipa)) {
    stop("English requires IPA-override mode: supply both `words` and `ipa`.")
  }
  if (length(words) != length(ipa)) {
    stop("`words` and `ipa` must have the same length.")
  }

  plain <- stringr::str_to_lower(words)
  keep  <- !duplicated(plain, fromLast = TRUE)
  plain <- plain[keep]
  ipa   <- ipa[keep]

  lex <- get("en_ipa_lex", envir = as.environment("package:Fonology"))

  new_entries <- ipa
  names(new_entries) <- plain
  lex <- c(lex[!names(lex) %in% plain], new_entries)

  en_ipa_lex <- lex
  save(en_ipa_lex,
       file = file.path(find.package("Fonology"), "data", "en_ipa_lex.rda"),
       compress = "xz")

  .lex_assign("en_ipa_lex", lex)

  invisible(lex)
}
