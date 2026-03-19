#' Remove words from the Spanish user lexicon
#'
#' Removes one or more words from the Spanish user lexicon (both the
#' diacritized-form store and the IPA-override store). Plain orthographic
#' forms are accepted; diacritized forms are also accepted and the plain key
#' is derived automatically via diacritic stripping.
#'
#' @param words A character vector of words to remove (plain or diacritized
#'   orthographic forms).
#' @return Invisibly returns a list with the updated lexicons:
#'   \code{diacritized} (\code{sp_lex}) and \code{ipa} (\code{sp_ipa_lex}).
#' @seealso \code{\link{add_lex_sp}}, \code{\link{export_lex}}
#' @noRd

remove_lex_sp <- function(words) {

  plain <- stringi::stri_trans_nfd(words) |>
    stringr::str_remove_all("\\p{M}")

  ## ---- Diacritized-form lexicon -------------------------------------------
  lex_d <- get("sp_lex", envir = as.environment("package:Fonology"))
  lex_d <- lex_d[!names(lex_d) %in% plain]

  sp_lex <- lex_d
  save(sp_lex,
       file = file.path(find.package("Fonology"), "data", "sp_lex.rda"),
       compress = "xz")
  .lex_assign("sp_lex", lex_d)

  ## ---- IPA-override lexicon -----------------------------------------------
  lex_i <- get("sp_ipa_lex", envir = as.environment("package:Fonology"))
  lex_i <- lex_i[!names(lex_i) %in% plain]

  sp_ipa_lex <- lex_i
  save(sp_ipa_lex,
       file = file.path(find.package("Fonology"), "data", "sp_ipa_lex.rda"),
       compress = "xz")
  .lex_assign("sp_ipa_lex", lex_i)

  invisible(list(diacritized = lex_d, ipa = lex_i))
}
