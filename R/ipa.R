#' IPA transcriber
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' @param word A possible string in its orthographic form
#' @param narrow Boolean. Whether a narrow transcription is desired (default is FALSE)
#' Narrow transcription is only available for Portuguese inputs and will be ignored for other languages
#' @param lg Language. Currently, only Portuguese is supported
#' @return The phonemic transcription for the string in question
#' @examples
#' ipa(word = "palado");
#' @export

ipa = function(word, lg = "Portuguese", narrow = FALSE){

  if(stringr::str_to_lower(lg) %in% c("pt", "portuguese")){

    output = ipa_pt_vec(word, narrow = narrow) |>
      stringr::str_replace_all("\\.\\s+", " ")

    return(output)
  }

  else if(stringr::str_to_lower(lg) %in% c("sp", "spanish")){
    output = ipa_sp(word) |>
      stringr::str_replace_all("\\.\\s+", " ")

    return(output)
  }

  else {
    message("Only Portuguese and Spanish are currently supported.")
    return(NA)
  }
}


