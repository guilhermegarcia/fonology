#' Functions to assign stress to Portuguese words
#'
#' Assigns stress to a given string.
#' @param word The string of interest using IPA phonemic transcription and already syllabified
#' @return The stressed version of the string in question
#' @examples
#' stress_pt(word = "kom.pu.ta.doɾ");
#' @importFrom magrittr %>%
#' @export

stress_pt = function(word = ""){

  # If word is monosyllabic:
  if(stringr::str_count(string = word,
                        pattern = "\\.") == 0){
    word = stringr::str_replace_all(string = word,
                                    pattern = "^(\\w*)$",
                                    replacement = "ˈ\\1")
    return(word)
  }

  if(stringr::str_detect(string = word, pattern = "\\.\\w+[pbtdkgszfvʃʒʎɲmnlɾwjiuãõw̃]$")){
    # Stress is final if word ends in consonant, diph OR high vowel (Tupi):
    word = stringr::str_replace_all(string = word,
                                    pattern = "\\.(\\w+[pbtdkgszfvʃʒʎɲmnlɾwjiuãõw̃])$",
                                    replacement = ".ˈ\\1")


    return(word)

  } else if(stringr::str_detect(string = word, pattern = "\\w*[ɔɛ]\\w*\\.\\w*\\.\\w*$")) {

    # Stress is antepenultimate if vowel is open:
    word = stringr::str_replace_all(string = word,
                                    pattern = "(\\w*[ɔɛ]\\w*)(\\.\\w*\\.\\w*$)",
                                    replacement = "ˈ\\1\\2")
    return(word)
  } else {

    # Else, penultimate stress:
    word = stringr::str_replace_all(string = word,
                                    pattern = "(\\w+)(\\.\\w+)$",
                                    replacement = "ˈ\\1\\2")

    return(word)

  }


}

# Secondary stress function
sec_stress_pt = function(word = ""){

  # Tokenize input and get stress position:
  mainStressPosition = word %>%
    # stress_pt() %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist() %>%
    stringr::str_detect("ˈ") %>%
    rev() %>%
    which(isTRUE(.))

  # Number of syllables:
  nSyl = word %>%
    stringr::str_count(pattern = "\\.") + 1

  # Final stress if odd number of syllables:
  if(mainStressPosition == 1 & nSyl > 2){
    # Assign secondary stress to every other syllable starting at 3rd syllable R-L
    split_word = stringr::str_split(word, "\\.") %>% unlist() %>% rev()

    stressed_word = c()

    for(i in seq(from = 1, to = nSyl)){
      if(i %in% seq(from = 3, to = nSyl, by = 2)){
        stressed_word[length(stressed_word) + 1] = stringr::str_c("ˌ", split_word[i])
      } else {
        stressed_word[length(stressed_word) + 1] = split_word[i]
      }
    }

    stressed_word %>%
      rev() %>%
      stringr::str_c(collapse = ".") %>%
      return()
  } else

    # Penultimate stress
    if(mainStressPosition == 2 & nSyl > 3){
      # Assign secondary stress to every other syllable starting at 3rd syllable R-L
      split_word = stringr::str_split(word, "\\.") %>% unlist() %>% rev()

      stressed_word = c()

      for(i in seq(from = 1, to = nSyl)){
        if(i %in% seq(from = 4, to = nSyl, by = 2)){
          stressed_word[length(stressed_word) + 1] = stringr::str_c("ˌ", split_word[i])
        } else {
          stressed_word[length(stressed_word) + 1] = split_word[i]
        }
      }

      stressed_word %>%
        rev() %>%
        stringr::str_c(collapse = ".") %>%
        return()
    } else

      # Antepenultimate stress
      if(mainStressPosition == 3 & nSyl > 4){
        # Assign secondary stress to every other syllable starting at 3rd syllable R-L
        split_word = stringr::str_split(word, "\\.") %>% unlist() %>% rev()

        stressed_word = c()

        for(i in seq(from = 1, to = nSyl)){
          if(i %in% seq(from = 5, to = nSyl, by = 2)){
            stressed_word[length(stressed_word) + 1] = stringr::str_c("ˌ", split_word[i])
          } else {
            stressed_word[length(stressed_word) + 1] = split_word[i]
          }
        }

        stressed_word %>%
          rev() %>%
          stringr::str_c(collapse = ".") %>%
          return()
      } else {
        return(word)
      }


}

pu_candidates = function(word = ""){

  c1 = stringr::str_replace(string = word,
                            pattern = "(\\w+\\.\\w+$)",
                            replacement = "ˈ\\1")

  c2 = stringr::str_replace(string = word,
                            pattern = "(\\w+\\.)(\\w+$)",
                            replacement = "\\1ˈ\\2")

  candidates = c(c1, c2)

  finalSegment = stringr::str_sub(string = word, start = -1L, end = -1L)
  if(finalSegment %in% c("l", "m")){
    winner = sample(candidates, size = 1, prob = c(0.4, 0.6))
  } else if(finalSegment %in% c("s")){
    winner = sample(candidates, size = 1, prob = c(0.1, 0.9))
  } else {
    winner = sample(candidates, size = 1, prob = c(0.02, 0.98))
  }

  return(winner)

}

apu_candidates = function(word = ""){

  c1 = stringr::str_replace(string = word,
                            pattern = "(\\w+\\.\\w+\\.\\w+$)",
                            replacement = "ˈ\\1")

  c2 = stringr::str_replace(string = word,
                            pattern = "(\\w+\\.)(\\w+\\.\\w+$)",
                            replacement = "\\1ˈ\\2")

  candidates = c(c1, c2)
  winner = sample(candidates, size = 1, prob = c(0.2, 0.8))

  return(winner)

}
