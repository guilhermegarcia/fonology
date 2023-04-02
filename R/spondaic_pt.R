#' Spondaic lowering helper function
#'
#' Given a string, the function tests if the string is LH and has penult stress,
#' in which case the penultimate vowel height is checked and lowered if needed.
#' @param word A possible string in Portuguese in its phonemic form
#' @return The transcription with spondaic lowering if applicable
#' @noRd

spond_pt = function(word = ""){

  wordWeight = getWeight_pt(word)
  wordStress = getStress(word)

  if(stringr::str_to_lower(wordStress) == "penult" &
     stringr::str_detect(wordWeight, pattern = "LH$")){
    # Apply spondaic lowering
    # E
    word = stringr::str_replace(word,
                                pattern = "(^\u02c8\\w*)e(\\.\\w*$)",
                                replacement = "\\1\u025b\\2")

    word = stringr::str_replace(word,
                                pattern = "(\\.\u02c8\\w*)e(\\.\\w*$)",
                                replacement = "\\1\u025b\\2")

    # O
    word = stringr::str_replace(word,
                                pattern = "(^\u02c8\\w*)o(\\.\\w*$)",
                                replacement = "\\1\u0254\\2")

    word = stringr::str_replace(word,
                                pattern = "(\\.\u02c8\\w*)o(\\.\\w*$)",
                                replacement = "\\1\u0254\\2")

    return(word)

  } else {
    return(word)
  }
}
