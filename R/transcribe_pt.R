#' IPA transcriber for Portuguese
#'
#' Returns IPA phonemic transcription for a given string.
#' @param word The string of interest
#' @return The IPA transcription of said string without syllabification or stress.
#'   Rhotics are phonemic: the strong rhotic is written \code{r} and the tap
#'   \enc{ɾ}{(U+027E)}, as in \code{ipa()}.
#' @examples
#' transcribe_pt(word = "computador")
#' @export

transcribe_pt <- function(word = "") {
  # This used to be a separate, scalar copy of transcribe_pt_vec() that had
  # drifted from it (it missed some nasal diphthongs and never adopted the
  # phonemic rhotics). It now delegates to the transcriber ipa() uses.
  word |>
    stringr::str_to_lower() |>
    transcribe_pt_vec() |>
    phonemic_rhotics_pt()
}
