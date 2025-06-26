#' Syllable constituent extractor
#'
#' Returns onset, nucleus, or coda for a given syllable.
#' @param syl The syllable of interest using IPA phonemic transcription
#' @param const The constituent of interest (defaults to nucleus)
#' @param glides_as_onsets Whether glides are to be interpreted as onsets (defaults to \code{FALSE})
#' @param glides_as_codas Whether glides are to be interpreted as codas (defaults to \code{FALSE})
#' @return The constituent of interest or \code{NA} if said constituent doesn't exist for the syllable in question
#' @examples
#' syllable(syl = "kom", const = "nucleus")
#' @export

syllable <- function(syl = "", const = "nucleus", glides_as_onsets = FALSE, glides_as_codas = FALSE) {
  # Lower case:
  syl <- stringr::str_to_lower(syl)
  syl <- stringr::str_remove_all(syl, pattern = "\u02c8|\u02cc")

  # If rhymes, count glides as codas:
  if (stringr::str_to_lower(const) == "rhyme") {
    glides_as_codas <- FALSE
    glides_as_onsets <- FALSE
  }

  # Inventory:
  nuclei <- c(
    "a", "e", "i", "o", "u",
    "\u00f5", "\u00e3",
    "y", "\u0268", "\u0289", "\u026f",
    "\u026a", "\u028f", "\u028a",
    "\u00f8", "\u0258", "\u0275", "\u0264",
    "\u025b", "\u0153", "\u025c", "\u025e", "\u028c", "\u0254",
    "\u00e6", "\u0250", "\u0276", "\u0251", "\u0252",
    "\u025a", "\u025d"
  )

  nasalization <- "\u0303"
  nasalization_glides <- "\u001a"

  nuclei <- c(nuclei, stringr::str_c(nuclei, nasalization))

  glides <- c("j", "w", "\u0265")

  glides <- c(
    glides,
    stringr::str_c(glides, nasalization_glides),
    stringr::str_c(glides, nasalization)
  )

  onGlide <- tidyr::crossing(glides, nuclei) |>
    dplyr::mutate(onGlide = stringr::str_c(glides, nuclei)) |>
    dplyr::pull(onGlide) |>
    stringr::str_c(collapse = "|")

  offGlide <- tidyr::crossing(nuclei, glides) |>
    dplyr::mutate(offGlide = stringr::str_c(nuclei, glides)) |>
    dplyr::pull(offGlide) |>
    stringr::str_c(collapse = "|")

  # Define default patterns for each constituent (glides are always nuclear)
  forOnsets <- stringr::str_c("^[^", nuclei |> stringr::str_c(collapse = ""), glides |> stringr::str_c(collapse = ""), "]+")
  forNuclei <- stringr::str_c("[", nuclei |> stringr::str_c(collapse = ""), glides |> stringr::str_c(collapse = ""), "]+")
  forCodas <- stringr::str_c("[^", nuclei |> stringr::str_c(collapse = ""), glides |> stringr::str_c(collapse = ""), "]+$")


  # Check assumptions re. glides:
  # T F
  if (glides_as_onsets & !glides_as_codas) {
    forOnsets <- forOnsets |> stringr::str_remove_all(pattern = glides |> stringr::str_c(collapse = "|"))
    forNuclei <- offGlide |>
      stringr::str_c(collapse = "|") |>
      stringr::str_c(nuclei |> stringr::str_c(collapse = "|"), sep = "|")
  }

  # F T
  if (!glides_as_onsets & glides_as_codas) {
    forOnsets <- forOnsets
    forNuclei <- onGlide |>
      stringr::str_c(collapse = "|") |>
      stringr::str_c(nuclei |> stringr::str_c(collapse = "|"), sep = "|")
    forCodas <- forCodas |> stringr::str_remove_all(pattern = glides |> stringr::str_c(collapse = "|"))
  }

  # T T
  if (glides_as_onsets & glides_as_codas) {
    forOnsets <- forOnsets |> stringr::str_remove_all(pattern = glides |> stringr::str_c(collapse = "|"))
    forNuclei <- nuclei |> stringr::str_c(collapse = "|")
    forCodas <- forCodas |> stringr::str_remove_all(pattern = glides |> stringr::str_c(collapse = "|"))
  }

  if (stringr::str_to_lower(const) == "onset") {
    output <- stringr::str_extract(syl, pattern = forOnsets)

    return(output)
  } else if (stringr::str_to_lower(const) == "nucleus") {
    output <- stringr::str_extract(syl, pattern = forNuclei)

    return(output)
  } else if (stringr::str_to_lower(const) == "coda") {
    output <- stringr::str_extract(syl, pattern = forCodas)

    return(output)
  } else if (stringr::str_to_lower(const) == "rhyme") {
    N <- stringr::str_extract(syl, pattern = forNuclei |> stringr::str_c(collapse = "|"))
    N[is.na(N)] <- ""
    C <- stringr::str_extract(syl, pattern = forCodas)
    C[is.na(C)] <- ""

    output <- stringr::str_c(N, C)

    return(output)
  } else {
    message("Constituents must be \'onset\', \'nucleus\', or \'coda\'.")
    return(NA)
  }
}
