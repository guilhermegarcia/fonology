#' Package startup message
#'
#' Printed when the package is attached with `library(Fonology)`. The message
#' is built with cli (so it is styled, width-aware, and degrades gracefully in
#' non-UTF-8 terminals) but emitted with [packageStartupMessage()], which is
#' what makes `suppressPackageStartupMessages()` work and what R CMD check
#' requires of `.onAttach()`.
#'
#' Users can silence it permanently with `options(Fonology.quiet = TRUE)` in
#' their `.Rprofile`.
#' @noRd

.fonology_startup_lines <- function() {
  version <- utils::packageVersion("Fonology")

  langs <- c(
    "Portuguese" = "pt",
    "Spanish" = "sp",
    "French" = "fr",
    "Italian" = "it",
    "English" = "en"
  )

  # Entries persisted by add_lex_*(). Only the on-disk user files are counted,
  # so a fresh installation (which has none) stays quiet; a corrupt or missing
  # file must never break attach.
  n_user <- tryCatch(
    sum(vapply(
      .user_lex_names,
      function(n) length(.load_user_lex_file(n)),
      integer(1)
    )),
    error = function(e) 0L
  )

  # The only emoji is in the rule. It is a single-codepoint, emoji-presentation
  # character: glyphs that need a U+FE0F variation selector render one column
  # narrower in many terminals and misalign the rule.
  cli::cli_fmt({
    cli::cli_rule(left = "\U0001f3a7 {.strong Welcome to Fonology} {version}")
    cli::cli_text("")
    cli::cli_text(
      "{.emph Phonological analysis in R} \u00b7 ",
      "{length(langs)} language{?s}: {.val {langs}}"
    )
    cli::cli_text("")
    cli::cli_bullets(c(
      "*" = "Transcribe: {.code ipa(\"fonologia\", lg = \"pt\")}",
      "*" = "Features: {.code getFeat(c(\"i\", \"u\"), lg = \"pt\")}",
      "*" = "Cite: {.run citation(\"Fonology\")}",
      "*" = "Learn more: {.url gdgarcia.ca/fonology}"
    ))
    if (n_user > 0L) {
      cli::cli_text("")
      cli::cli_alert_info("{n_user} user lexicon entr{?y/ies} loaded.")
    }
  })
}

.onAttach <- function(libname, pkgname) {
  if (isTRUE(getOption("Fonology.quiet", FALSE))) {
    return(invisible(NULL))
  }

  lines <- tryCatch(
    .fonology_startup_lines(),
    error = function(e) NULL
  )

  if (!is.null(lines)) {
    packageStartupMessage(paste(lines, collapse = "\n"))
  }

  invisible(NULL)
}
