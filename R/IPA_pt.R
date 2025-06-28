#' IPA transcriber for Portuguese
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' Stress is assigned on the basis of the Portuguese Stress Lexicon for existing words, or lexical regularities
#' and probabilistic distributions in certain cases for hypothetical words.
#' @param word A possible string in Portuguese in its orthographic form
#' @param narrow Boolean. Whether a narrow transcription is desired (defaults to \code{FALSE}).
#' @return The phonemic transcription for the string in question
#' @examples
#' ipa_pt(word = "palado")
#' @export

ipa_pt <- function(word = "palavra", narrow = FALSE) {
  wd <- stringr::str_to_lower(word)

  if (stringr::str_detect(wd, pattern = "\\d")) {
    message("Input contains a number and will be ignored.")
    return(NA)
  }

  if (stringr::str_detect(wd, pattern = "-")) {
    message("Input must be monomorphemic. The function will remove any clitics it detects.")
    wd <- strip_clitic_pt(wd)
  }

  wd <- wd |>
    stringr::str_remove_all("[:punct:]")

  if (wd %in% pt_lex$word) {
    broadLex <- pt_lex |>
      dplyr::filter(word == wd) |>
      dplyr::slice(1) |>
      dplyr::pull(pro) |>
      stringr::str_replace(pattern = "\'", replacement = "\u02c8") |>
      stringr::str_replace(pattern = "\u027e", replacement = "r")


    if (narrow == F) {
      broadLex <- broadLex |>
        # Fix v height in lher]:
        stringr::str_replace(
          pattern = "\u028ee\u027e$",
          replacement = "\u028e\u025b\u027e"
        )

      return(broadLex)
    } else if (narrow == T) {
      broadLex <- broadLex |>
        narrow_pt()

      return(broadLex)
    }

    # If word is novel:

    # Broad transcription:
  } else {
    # if word has a diacritic and narrow is T:
    if (stringr::str_detect(
      string = wd,
      pattern = "[\u00e1\u00e9\u00ed\u00f3\u00fa\u00e0\u00e8\u00ec\u00f2\u00f9\u00e2\u00ea\u00f4]"
    ) & narrow == T) {
      wd <- wd |>
        transcribe_pt() |>
        syllabify_pt() |>
        stress_pt() |>
        narrow_pt()

      return(wd)
    } else if (stringr::str_detect(
      string = wd,
      pattern = "[\u00e1\u00e9\u00ed\u00f3\u00fa\u00e0\u00e8\u00ec\u00f2\u00f9\u00e2\u00ea\u00f4]"
    ) & narrow == F) {
      wd <- wd |>
        transcribe_pt() |>
        syllabify_pt() |>
        stress_pt()

      return(wd)
    }



    # If not:
    wd <- wd |>
      transcribe_pt() |>
      syllabify_pt()

    # Feed probabilistic patterns in lexicon (but only if word doesn't end in high V):
    weight <- getWeight_pt(wd)

    if (weight %in% c("HLL", "LLL") & stringr::str_detect(wd, pattern = "[^ius]$")) {
      wd <- wd |>
        apu_candidates() |>
        dact_pt()
    } else if (weight %in% c("LLH", "LH", "HH", "LHH") & stringr::str_detect(wd, pattern = "[^ius]$")) {
      wd <- wd |>
        pu_candidates() |>
        spond_pt() |>
        stringr::str_replace(
          pattern = "z$",
          replacement = "s"
        )
      # If stress is final and weight = (X)LH, e,o -> E,O
    } else {
      wd <- wd |>
        stress_pt() |>
        stringr::str_replace(
          pattern = "z$",
          replacement = "s"
        )
    }

    # Check for narrow transcription:
    if (narrow == T) {
      wd <- wd |>
        narrow_pt()
      return(wd)
    } else if (narrow == F) {
      wd <- wd |>
        stringr::str_replace(
          pattern = "(\u02c8\\w*)ol$",
          replacement = "\\1\u0254l"
        ) |>
        stringr::str_replace(
          pattern = "(\u02c8\\w*)el$",
          replacement = "\\1\u025bl"
        ) |>
        stringr::str_replace_all(
          pattern = "\u027e",
          replacement = "r"
        )

      return(wd)
    }
  }
}
