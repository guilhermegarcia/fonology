#' Spondaic lowering helper function
#'
#' Given a string, the function tests if the string is LH and has penult stress,
#' in which case the penultimate vowel height is checked and lowered if needed.
#' @param word A possible string in Portuguese in its phonemic form
#' @return The transcription with spondaic lowering if applicable
#' @examples
#' ipa_pt(word = "ˈpe.tel");
#' @export



spond_pt = function(word = ""){

  wordWeight = weight_pt(word)
  wordStress = which_stress_pt(word)

  if(wordStress == "Penult" &
     str_detect(wordWeight, pattern = "LH$")){
    # Apply spondaic lowering
    # E
    word = str_replace(word,
                       pattern = "(^ˈ\\w*)e(\\.\\w*$)",
                       replacement = "\\1ɛ\\2")

    word = str_replace(word,
                       pattern = "(\\.ˈ\\w*)e(\\.\\w*$)",
                       replacement = "\\1ɛ\\2")

    # O
    word = str_replace(word,
                       pattern = "(^ˈ\\w*)o(\\.\\w*$)",
                       replacement = "\\1ɔ\\2")

    word = str_replace(word,
                       pattern = "(\\.ˈ\\w*)o(\\.\\w*$)",
                       replacement = "\\1ɔ\\2")

    return(word)

  } else {
    return(word)
  }
}


dact_pt = function(word = ""){

  word = str_replace(word,
                     pattern = "(ˈ\\w*)e(\\.\\w+\\.\\w+$)",
                     replacement = "\\1ɛ\\2")

  return(word)
}
