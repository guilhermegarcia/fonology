#' Add words to the Spanish lexicon
#'
#' Adds one or more Spanish words (supplied with stress diacritics) to the
#' \code{sp_lex} lexicon used by \code{ipa(lg = "Spanish")}. The plain
#' orthographic key is derived automatically by stripping all combining
#' diacritics, so only the diacritized form needs to be provided.
#'
#' You can also pass a path to a plain-text file (one diacritized word per line,
#' as produced by \code{export_lex("sp", file)}) and the words will be read
#' from that file automatically.
#'
#' @param words A character vector of Spanish words with stress diacritics,
#'   e.g. \code{c("t\u00e9rmino", "im\u00e1gen")}. Alternatively, a path to a
#'   plain-text file with one diacritized word per line.
#' @return Invisibly returns the updated \code{sp_lex} named character vector.
#' @seealso \code{\link{export_lex}}
#' @examples
#' \dontrun{
#' add_lex_sp(c("t\u00e9rmino", "im\u00e1gen"))
#' ipa("termino", lg = "sp")
#' # Import from a shared file:
#' add_lex_sp("spanish_entries.txt")
#' }
#' @export

add_lex_sp <- function(words) {
  if (length(words) == 1 && file.exists(words)) {
    words <- readLines(words, warn = FALSE)
    words <- words[nzchar(words)]
  }

  plain <- stringi::stri_trans_nfd(words) |>
    stringr::str_remove_all("\\p{M}")

  keep <- !duplicated(plain, fromLast = TRUE)
  words <- words[keep]
  plain <- plain[keep]

  lex <- get("sp_lex", envir = as.environment("package:Fonology"))

  new_entries <- words
  names(new_entries) <- plain
  lex <- c(lex[!names(lex) %in% plain], new_entries)

  sp_lex <- lex
  save(sp_lex, file = file.path(find.package("Fonology"), "data", "sp_lex.rda"),
       compress = "xz")

  tryCatch(
    assign("sp_lex", lex, envir = as.environment("package:Fonology")),
    error = function(e) invisible(NULL)
  )

  invisible(lex)
}
