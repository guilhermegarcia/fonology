#' IPA transcriber for Italian
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' @param word A possible string in Italian in its orthographic form
#' @return The phonemic transcription for the string in question
#' @noRd

ipa_it <- function(word = "italiano") {
  wd <- stringr::str_to_lower(word) |>
    stringr::str_remove_all("[:punct:]")

  # Set digit-containing words to NA (vectorized; does not affect other entries)
  wd[stringr::str_detect(wd, "\\d")] <- NA

  if (any(stringr::str_detect(stats::na.omit(wd), "-"))) {
    message("Input must be monomorphemic. Stress assignment may be incorrect.")
  }

  # Save plain keys before any substitution
  wd_plain <- wd
  ipa_override <- !is.na(wd) & wd %in% names(it_ipa_lex)

  # Diacritized-form lookup: replace plain form so stress_it()'s diacritic rule
  # places stress and vowel quality correctly.
  matches <- !is.na(wd) & wd %in% names(it_lex)
  if (any(matches)) wd[matches] <- it_lex[wd[matches]]

  # Run pipeline on non-NA entries only (stress_it uses names-based tracking
  # that does not tolerate NA elements in the input vector)
  not_na <- !is.na(wd)
  if (any(not_na)) {
    wd[not_na] <- wd[not_na] |>
      transcribe_it() |>
      syllabify_it() |>
      stress_it()
  }

  # Apply IPA overrides last (highest priority; bypass pipeline result)
  if (any(ipa_override)) {
    wd[ipa_override] <- unname(it_ipa_lex[wd_plain[ipa_override]])
  }

  return(wd)
}
