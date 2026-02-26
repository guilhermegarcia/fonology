#' Remove words from the Italian user lexicon
#'
#' Removes one or more words from the Italian user lexicon (both the
#' diacritized-form store and the IPA-override store). Plain orthographic
#' forms are accepted; diacritized forms are also accepted and the plain key
#' is derived automatically via diacritic stripping.
#'
#' @param words A character vector of words to remove (plain or diacritized
#'   orthographic forms).
#' @return Invisibly returns a list with the updated lexicons:
#'   \code{diacritized} (\code{it_lex}) and \code{ipa} (\code{it_ipa_lex}).
#' @seealso \code{\link{add_lex_it}}, \code{\link{export_lex}}
#' @noRd

remove_lex_it <- function(words) {

  plain <- stringi::stri_trans_nfd(words) |>
    stringr::str_remove_all("\\p{M}")

  ## ---- Diacritized-form lexicon -------------------------------------------
  lex_d <- get("it_lex", envir = as.environment("package:Fonology"))
  lex_d <- lex_d[!names(lex_d) %in% plain]

  it_lex <- lex_d
  save(it_lex,
       file = file.path(find.package("Fonology"), "data", "it_lex.rda"),
       compress = "xz")
  tryCatch(
    assign("it_lex", lex_d, envir = as.environment("package:Fonology")),
    error = function(e) invisible(NULL)
  )

  ## ---- IPA-override lexicon -----------------------------------------------
  lex_i <- get("it_ipa_lex", envir = as.environment("package:Fonology"))
  lex_i <- lex_i[!names(lex_i) %in% plain]

  it_ipa_lex <- lex_i
  save(it_ipa_lex,
       file = file.path(find.package("Fonology"), "data", "it_ipa_lex.rda"),
       compress = "xz")
  tryCatch(
    assign("it_ipa_lex", lex_i, envir = as.environment("package:Fonology")),
    error = function(e) invisible(NULL)
  )

  invisible(list(diacritized = lex_d, ipa = lex_i))
}
