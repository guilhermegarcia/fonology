#' IPA transcriber for Portuguese
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' Stress is assigned on the basis of the Portuguese Stress Lexicon for existing words, or lexical regularities
#' and probabilistic distributions in certain cases for hypothetical words
#' @param word A possible string in Portuguese in its orthographic form
#' @return The phonemic transcription for the string in question
#' @examples
#' ipa_pt(word = "palado");
#' @export

ipa_pt = function(word = ""){

  if(!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)

  wd = str_to_lower(word)

  # Check if word is in PSL:
  # If yes, pick pro column
  if(wd %in% pt_lex$word){

    pt_lex %>%
      filter(word == wd) %>%
      slice(1) %>%
      pull(pro) %>%
      str_replace(pattern = "'", replacement = "ˈ") %>%
      sec_stress_pt() %>%
      return()

    # If not, run functions:

  } else {

    wd = wd %>%
      transcribe_pt() %>%
      syllabify_pt()

    # Feed probabilistic patterns in lexicon:
    weight = weight_pt(wd)

    if(weight %in% c("HLL", "LLL")){
      wd %>% apu_candidates() %>%
        sec_stress_pt() %>%
        # str_c("/", ., "/") %>%
      return()

    } else if(weight %in% c("LLH", "LH")){
      wd %>% pu_candidates() %>%
        sec_stress_pt() %>%
        # str_c("/", ., "/") %>%
        return()

    } else {
      wd %>%
        stress_pt() %>%
        sec_stress_pt() %>%
        # str_c("/", ., "/") %>%
        return()
    }
  }

}
