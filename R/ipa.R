#' IPA transcriber
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' @param word A possible string in its orthographic form
#' @param narrow Boolean. Whether a narrow transcription is desired (default is FALSE).
#' Narrow transcription is only available for Portuguese inputs
#' @param lg Language. Currently, only Portuguese is supported
#' @return The phonemic transcription for the string in question
#' @examples
#' ipa(word = "palado");
#' @export

ipa = function(word, lg = "Portuguese", narrow = FALSE){

  if(stringr::str_to_lower(lg) %in% c("pt", "portuguese")){

    output = ipa_pt_vec(word, narrow = narrow)

    return(output)
  }
  else {
    message("Only Portuguese is currently supported.")
    return(NA)
  }
}


