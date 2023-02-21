#' Functions to assign stress to Portuguese words
#'
#' Assigns stress to a given string.
#' @param word The string of interest using IPA phonemic transcription and already syllabified
#' @return The stressed version of the string in question
#' @examples
#' stress_pt(word = "kom.pu.ta.doɾ");
#' @export


helper_pt = list(

  getWeight_pt = function(word = ""){

    if(!require("pacman", quietly = T)){install.packages("pacman")}
    pacman::p_load(tidyverse)

    word = str_to_lower(word)

    potentialPl = str_detect(word, "s$")
    sgWd = str_remove_all(string = word, pattern = "s$")




    # Remove stress
    word = str_remove_all(string = word,
                          pattern = "ˈ|'")


    # Light syllables
    word = str_replace_all(string = word,
                           pattern = "[\\w*]{0,3}[ãõaeiouɛɔ]$",
                           replacement = "L")

    word = str_replace_all(string = word,
                           pattern = "[\\w*]{0,3}[ãõaeiouɛɔ]\\.",
                           replacement = "L.")


    # Heavy syllables
    word = str_replace_all(string = word,
                           pattern = "\\w+[jwlmnɾspbtdkgɾzfvʃʒʎɲ]",
                           replacement = "H")

    # Remove syllabification
    word = str_remove_all(string = word,
                          pattern = "\\.")

    # Fix nasal diphthongs
    word = str_replace_all(string = word,
                           pattern = "H̃",
                           replacement = "H")

    # Pick only trisyllabic window
    word = str_sub(string = word,
                   start = -3L, end = -1L)

    # H -> L if s] = plural
    if(potentialPl & sgWd %in% pt_lex$pro){
      word = str_replace(string = word,
                         pattern = "H$",
                         replacement = "L")
    }


    return(word)

  }

  dePlu_pt = function(word = ""){
    if(!require("pacman", quietly = T)){install.packages("pacman")}
    pacman::p_load(tidyverse)

    word = str_to_lower(word)
    if(str_detect(word, pattern = "s$")){
      temp = str_remove(word, pattern = "s$")
      if(temp %in% pt_lex$word){
        return(temp)
      } else {
        return(word)
      }
    } else {
      return(word)
    }

  }


  getSyl = function(word = "", pos = 1, syl = "\\."){
    if(!require("pacman", quietly = T)){install.packages("pacman")}
    pacman::p_load(tidyverse)

    syllables = word %>%
      str_split(pattern = syl) %>%
      unlist() %>%
      rev() %>%
      str_remove_all(pattern = "['ˈˌ]")

    if(pos > length(syllables)){
      return(NA)
    }

    return(syllables[pos])

  }


  stress_pt = function(word = ""){

    if(!require("pacman", quietly = T)){install.packages("pacman")}
    pacman::p_load(tidyverse)

    # If word is monosyllabic:
    if(str_count(string = word,
                 pattern = "\\.") == 0){
      word = str_replace_all(string = word,
                             pattern = "^(\\w*)$",
                             replacement = "ˈ\\1")
      return(word)
    }

    if(str_detect(string = word, pattern = "\\.\\w+[pbtdkgszfvʃʒʎɲmnlɾwjiuãõw̃]$")){
      # Stress is final if word ends in consonant, diph OR high vowel (Tupi):
      word = str_replace_all(string = word,
                             pattern = "\\.(\\w+[pbtdkgszfvʃʒʎɲmnlɾwjiuãõw̃])$",
                             replacement = ".ˈ\\1")


      return(word)

    } else if(str_detect(string = word, pattern = "\\w*[ɔɛ]\\w*\\.\\w*\\.\\w*$")) {

      # Stress is antepenultimate if vowel is open:
      word = str_replace_all(string = word,
                             pattern = "(\\w*[ɔɛ]\\w*)(\\.\\w*\\.\\w*$)",
                             replacement = "ˈ\\1\\2")
      return(word)
    } else {

      # Else, penultimate stress:
      word = str_replace_all(string = word,
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
      str_split(pattern = "\\.") %>%
      unlist() %>%
      str_detect("ˈ") %>%
      rev() %>%
      which(isTRUE(.))

    # Number of syllables:
    nSyl = word %>%
      str_count(pattern = "\\.") + 1

    # Final stress if odd number of syllables:
    if(mainStressPosition == 1 & nSyl > 2){
      # Assign secondary stress to every other syllable starting at 3rd syllable R-L
      split_word = str_split(word, "\\.") %>% unlist() %>% rev()

      stressed_word = c()

      for(i in seq(from = 1, to = nSyl)){
        if(i %in% seq(from = 3, to = nSyl, by = 2)){
          stressed_word[length(stressed_word) + 1] = str_c("ˌ", split_word[i])
        } else {
          stressed_word[length(stressed_word) + 1] = split_word[i]
        }
      }

      stressed_word %>%
        rev() %>%
        str_c(collapse = ".") %>%
        return()
    } else

      # Penultimate stress
      if(mainStressPosition == 2 & nSyl > 3){
        # Assign secondary stress to every other syllable starting at 3rd syllable R-L
        split_word = str_split(word, "\\.") %>% unlist() %>% rev()

        stressed_word = c()

        for(i in seq(from = 1, to = nSyl)){
          if(i %in% seq(from = 4, to = nSyl, by = 2)){
            stressed_word[length(stressed_word) + 1] = str_c("ˌ", split_word[i])
          } else {
            stressed_word[length(stressed_word) + 1] = split_word[i]
          }
        }

        stressed_word %>%
          rev() %>%
          str_c(collapse = ".") %>%
          return()
      } else

        # Antepenultimate stress
        if(mainStressPosition == 3 & nSyl > 4){
          # Assign secondary stress to every other syllable starting at 3rd syllable R-L
          split_word = str_split(word, "\\.") %>% unlist() %>% rev()

          stressed_word = c()

          for(i in seq(from = 1, to = nSyl)){
            if(i %in% seq(from = 5, to = nSyl, by = 2)){
              stressed_word[length(stressed_word) + 1] = str_c("ˌ", split_word[i])
            } else {
              stressed_word[length(stressed_word) + 1] = split_word[i]
            }
          }

          stressed_word %>%
            rev() %>%
            str_c(collapse = ".") %>%
            return()
        } else {
          return(word)
        }


  }




  pu_candidates = function(word = ""){

    c1 = str_replace(string = word,
                     pattern = "(\\w+\\.\\w+$)",
                     replacement = "ˈ\\1")

    c2 = str_replace(string = word,
                     pattern = "(\\w+\\.)(\\w+$)",
                     replacement = "\\1ˈ\\2")

    candidates = c(c1, c2)

    finalSegment = str_sub(string = word, start = -1L, end = -1L)
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

    c1 = str_replace(string = word,
                     pattern = "(\\w+\\.\\w+\\.\\w+$)",
                     replacement = "ˈ\\1")

    c2 = str_replace(string = word,
                     pattern = "(\\w+\\.)(\\w+\\.\\w+$)",
                     replacement = "\\1ˈ\\2")

    candidates = c(c1, c2)
    winner = sample(candidates, size = 1, prob = c(0.2, 0.8))

    return(winner)

  }

  getStress = function(word = "", stress = "ˈ"){
    if (!require("pacman", quietly = T)) install.packages("pacman")
    pacman::p_load(tidyverse)

    word = str_split(string = word,
                     pattern = "\\.") %>%
      unlist() %>%
      str_detect(stress)

    if(word[length(word)] == TRUE){
      return("Final")
    } else if(word[length(word)-1] == TRUE){
      return("Penult")
    } else if(word[length(word)-2] == TRUE){
      return("Antepenult")
    } else if(word[length(word)-3] == TRUE){
      return("Preantepenult")
    } else {
      return("Not a possible stress in Portuguese.")
    }

  }



  spond_pt = function(word = ""){

    wordWeight = getWeight_pt(word)
    wordStress = getStress(word)

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




)
