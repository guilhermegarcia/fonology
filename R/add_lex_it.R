#' Add words to the Italian lexicon
#'
#' Adds one or more Italian words (supplied with diacritics that encode stress
#' position and/or mid-vowel quality) to the \code{it_lex} lexicon used by
#' \code{ipa(lg = "Italian")}. The plain orthographic key is derived
#' automatically by stripping all combining diacritics, so only the diacritized
#' form needs to be provided.
#'
#' You can also pass a path to a plain-text file (one diacritized word per line,
#' as produced by \code{export_lex("it", file)}) and the words will be read
#' from that file automatically.
#'
#' Diacritic conventions for Italian:
#' \describe{
#'   \item{\code{é}}{Stressed open-mid /ɛ/ (e.g. \code{"café"} → stress + open e)}
#'   \item{\code{ê}}{Stressed close-mid /e/ (e.g. \code{"chêsa"} → stress + close e)}
#'   \item{\code{è}}{Stressed open-mid /ɛ/ on final syllable (Italian orthographic convention)}
#'   \item{\code{ó}}{Stressed open-mid /ɔ/ (e.g. \code{"nótto"} → stress + open o)}
#'   \item{\code{ô}}{Stressed close-mid /o/ (e.g. \code{"vôce"} → stress + close o)}
#'   \item{\code{ò}}{Stressed open-mid /ɔ/ on final syllable}
#'   \item{\code{à}, \code{ì}, \code{ù}}{Stress-only markers (no quality distinction)}
#' }
#'
#' @param words A character vector of Italian words with diacritics, e.g.
#'   \code{c("chiédere", "livéllo", "città")}. Alternatively, a path to a
#'   plain-text file with one diacritized word per line.
#' @return Invisibly returns the updated \code{it_lex} named character vector.
#' @seealso \code{\link{export_lex}}
#' @examples
#' \dontrun{
#' add_lex_it(c("chiédere", "livéllo"))
#' ipa("chiedere", lg = "it")   # now uses lexicon entry
#' # Import from a shared file:
#' add_lex_it("italian_entries.txt")
#' }
#' @export

add_lex_it <- function(words) {
  # If a single file path is given, read words from that file
  if (length(words) == 1 && file.exists(words)) {
    words <- readLines(words, warn = FALSE)
    words <- words[nzchar(words)]
  }

  # Derive plain key by stripping combining diacritics via NFD decomposition
  plain <- stringi::stri_trans_nfd(words) |>
    stringr::str_remove_all("\\p{M}")  # remove all combining marks

  # Deduplicate input (keep last occurrence of each plain key)
  keep <- !duplicated(plain, fromLast = TRUE)
  words <- words[keep]
  plain <- plain[keep]

  # Retrieve current lexicon. Package data lives in the package environment
  # (on the search path), not directly in the namespace.
  lex <- get("it_lex", envir = as.environment("package:Fonology"))

  # Upsert: remove existing entries for these keys, then add new ones
  new_entries <- words
  names(new_entries) <- plain
  lex <- c(lex[!names(lex) %in% plain], new_entries)

  # Persist to the package data directory (works for both installed packages
  # and development via devtools::load_all()).
  it_lex <- lex
  save(it_lex, file = file.path(find.package("Fonology"), "data", "it_lex.rda"),
       compress = "xz")

  # Update in-memory copy for the current session (may silently fail in some
  # check environments where the package environment binding is locked).
  tryCatch(
    assign("it_lex", lex, envir = as.environment("package:Fonology")),
    error = function(e) invisible(NULL)
  )

  invisible(lex)
}
