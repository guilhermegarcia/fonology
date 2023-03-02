#' Spondaic lowering helper function
#'
#' Given a string, the function tests if the string is LH and has penult stress,
#' in which case the penultimate vowel height is checked and lowered if needed.
#' @param word A possible string in Portuguese in its phonemic form
#' @return The transcription with spondaic lowering if applicable
#' @examples
#' spond_pt(word = "ˈpe.tel");
#' @export

spond_pt = function(word = ""){

  wordWeight = getWeight_pt(word)
  wordStress = getStress(word)

  if(wordStress == "Penult" &
     stringr::str_detect(wordWeight, pattern = "LH$")){
    # Apply spondaic lowering
    # E
    word = stringr::str_replace(word,
                       pattern = "(^ˈ\\w*)e(\\.\\w*$)",
                       replacement = "\\1ɛ\\2")

    word = stringr::str_replace(word,
                       pattern = "(\\.ˈ\\w*)e(\\.\\w*$)",
                       replacement = "\\1ɛ\\2")

    # O
    word = stringr::str_replace(word,
                       pattern = "(^ˈ\\w*)o(\\.\\w*$)",
                       replacement = "\\1ɔ\\2")

    word = stringr::str_replace(word,
                       pattern = "(\\.ˈ\\w*)o(\\.\\w*$)",
                       replacement = "\\1ɔ\\2")

    return(word)

  } else {
    return(word)
  }
}
