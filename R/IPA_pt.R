#' IPA transcriber for Portuguese
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' Stress is assigned on the basis of the Portuguese Stress Lexicon for existing words, or lexical regularities
#' and probabilistic distributions in certain cases for hypothetical words
#' @param word A possible string in Portuguese in its orthographic form
#' @param narrow Boolean. Whether a narrow trasncription is desired (default is FALSE).
#' @return The phonemic transcription for the string in question
#' @examples
#' ipa_pt(word = "palado");
#' @export

ipa_pt = function(word = "", narrow = F){

  if(!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)

  wd = str_to_lower(word)

  if(str_detect(wd, pattern = "-")){
    message("Input must be monomorphemic. The function will remove any clitics it detects.")
    wd = strip_clitic_pt(wd)
  }


  potentialPl = str_detect(wd, "s$")
  sgWd = str_remove(string = wd, pattern = "s$")

  # If singular form exists:
  if(potentialPl & sgWd %in% pt_lex$word){
    broadLex = pt_lex %>%
      filter(word == sgWd) %>%
      slice(1) %>%
      pull(pro) %>%
      str_replace(pattern = "'", replacement = "ˈ")

    if(narrow == F){
      broadLex %>%
        str_c("s") %>%
        return()
    } else if(narrow == T){
      broadLex %>%
        narrow_pt() %>%
        str_c("s") %>%
        return()
    }

    # If word exists:
  } else if(wd %in% pt_lex$word){
    broadLex = pt_lex %>%
      filter(word == wd) %>%
      slice(1) %>%
      pull(pro) %>%
      str_replace(pattern = "'", replacement = "ˈ")

    if(narrow == F){
      broadLex %>%
        return()

    } else if(narrow == T){
      broadLex %>%
        narrow_pt() %>%
        return()
    }

    # If word is novel:
  } else {

    # Broad transcription:
    wd = wd %>%
      transcribe_pt() %>%
      syllabify_pt()

    # Feed probabilistic patterns in lexicon (but only if word doesn't end in high V):
    weight = getWeight_pt(wd)

    if(weight %in% c("HLL", "LLL") & str_detect(wd, pattern = "[^iu]$")){
      wd = wd %>%
        apu_candidates() %>%
        dact_pt()

    } else if(weight %in% c("LLH", "LH", "HH", "LHH")){
      wd =  wd %>%
        pu_candidates() %>%
        spond_pt()
      # If stress is final and weight = (X)LH, e,o -> E,O

    } else {
      wd = wd %>%
        stress_pt()
    }

    # Check for narrow transcription:
    if(narrow == T){
      wd %>%
        narrow_pt() %>%
        return()
    } else if (narrow == F){
      wd %>%
        return()
    }


  }

}
