#' IPA transcriber for French
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' @param word A possible string in French in its orthographic form
#' @return The phonemic transcription for the string in question
#' @noRd

ipa_fr <- function(word = "comportamento") {
  fr_ipa_lex <- .get_user_lex("fr_ipa_lex")

  wd <- stringr::str_to_lower(word) |>
    stringr::str_remove_all("[:punct:]")

  if (any(stringr::str_detect(wd, pattern = "\\d"))) {
    message("Input contains a number and will be ignored.")
  }

  wd[stringr::str_detect(wd, pattern = "\\d")] <- NA

  if (any(stringr::str_detect(stats::na.omit(wd), pattern = "-"))) {
    message("Input must be monomorphemic. Stress assignment may be incorrect.")
  }

  wd_plain <- wd
  ipa_override <- !is.na(wd) & wd %in% names(fr_ipa_lex)

  not_na <- !is.na(wd)
  if (any(not_na)) {
    wd[not_na] <- wd[not_na] |>
      transcribe_fr() |>
      syllabify_fr()
  }

  if (any(ipa_override)) {
    wd[ipa_override] <- unname(fr_ipa_lex[wd_plain[ipa_override]])
  }

  return(wd)
}
