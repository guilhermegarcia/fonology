#' Export a user lexicon to a plain-text file
#'
#' Writes the diacritized entries of a language lexicon to a plain-text file
#' (one word per line). The file can be shared and re-imported directly via
#' the corresponding \code{add_lex_XX()} function.
#'
#' @param lg Language: \code{"it"}, \code{"sp"}, or \code{"pt"}.
#' @param file Path to the output file (e.g. \code{"my_it_lex.txt"}).
#' @return Invisibly returns the character vector that was written.
#' @examples
#' \dontrun{
#' export_lex("it", "italian_entries.txt")
#' # share the file, then the recipient runs:
#' add_lex_it("italian_entries.txt")
#' }
#' @export

export_lex <- function(lg, file) {
  lex_name <- switch(
    stringr::str_to_lower(lg),
    "it" = "it_lex",
    "sp" = "sp_lex",
    "pt" = "pt_lex_user",
    stop("lg must be one of: \"it\", \"sp\", \"pt\"")
  )

  lex <- get(lex_name, envir = as.environment("package:Fonology"))

  if (length(lex) == 0) {
    message("Lexicon is empty - nothing to export.")
    return(invisible(character(0)))
  }

  writeLines(unname(lex), file)
  message(length(lex), " entries written to ", file)
  invisible(unname(lex))
}
