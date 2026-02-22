#' Add words to the Portuguese user lexicon
#'
#' Adds one or more Portuguese words (supplied with stress diacritics) to the
#' \code{pt_lex_user} lexicon used by \code{ipa(lg = "Portuguese")}. Entries
#' here take priority over the default \code{pt_lex} stress lexicon. The plain
#' orthographic key is derived automatically by stripping all combining
#' diacritics, so only the diacritized form needs to be provided.
#'
#' You can also pass a path to a plain-text file (one diacritized word per line,
#' as produced by \code{export_lex("pt", file)}) and the words will be read
#' from that file automatically.
#'
#' @param words A character vector of Portuguese words with stress diacritics,
#'   e.g. \code{c("f\u00f4lego", "r\u00fabrica")}. Alternatively, a path to a
#'   plain-text file with one diacritized word per line.
#' @return Invisibly returns the updated \code{pt_lex_user} named character vector.
#' @seealso \code{\link{export_lex}}
#' @examples
#' \dontrun{
#' add_lex_pt(c("f\u00f4lego", "r\u00fabrica"))
#' ipa("folego", lg = "pt")
#' # Import from a shared file:
#' add_lex_pt("portuguese_entries.txt")
#' }
#' @export

add_lex_pt <- function(words) {
  if (length(words) == 1 && file.exists(words)) {
    words <- readLines(words, warn = FALSE)
    words <- words[nzchar(words)]
  }

  plain <- stringi::stri_trans_nfd(words) |>
    stringr::str_remove_all("\\p{M}")

  keep <- !duplicated(plain, fromLast = TRUE)
  words <- words[keep]
  plain <- plain[keep]

  lex <- get("pt_lex_user", envir = as.environment("package:Fonology"))

  new_entries <- words
  names(new_entries) <- plain
  lex <- c(lex[!names(lex) %in% plain], new_entries)

  pt_lex_user <- lex
  save(pt_lex_user,
       file = file.path(find.package("Fonology"), "data", "pt_lex_user.rda"),
       compress = "xz")

  tryCatch(
    assign("pt_lex_user", lex, envir = as.environment("package:Fonology")),
    error = function(e) invisible(NULL)
  )

  invisible(lex)
}
