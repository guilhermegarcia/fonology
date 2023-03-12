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
#' @importFrom magrittr %>%
#' @export

ipa_pt = function(word = "", narrow = F){

  wd = stringr::str_to_lower(word)

  if(stringr::str_detect(wd, pattern = "\\d")){
    message("Input contains a number and will be ignored.")
    return(NA)
  }

  if(stringr::str_detect(wd, pattern = "-")){
    message("Input must be monomorphemic. The function will remove any clitics it detects.")
    wd = strip_clitic_pt(wd)
  }

  wd = wd %>%
    stringr::str_remove_all("[:punct:]")

  # If there's a stress diacritic:
  if(!is.na(diacritic_pt(wd))){
    position = diacritic_pt(wd)
    stressRegEx = stringr::str_c("((\\w+\\.){", position, "})$")
    output_with_diacritic = wd %>%
      transcribe_pt() %>%
      syllabify_pt() %>%
      stringr::str_c(".") %>%
      stringr::str_replace(pattern = stressRegEx,
                           replacement = "\u02c8\\1") %>%
      stringr::str_remove(pattern = "\\.$")

    return(output_with_diacritic)
  }

  potentialPl = stringr::str_detect(wd, "s$")
  sgWd = stringr::str_remove(string = wd, pattern = "s$")

  # If singular form exists:
  if(potentialPl & sgWd %in% pt_lex$word){
    broadLex = pt_lex %>%
      dplyr::filter(word == sgWd) %>%
      dplyr::slice(1) %>%
      dplyr::pull(pro) %>%
      stringr::str_replace(pattern = "\'", replacement = "\u02c8")

    if(narrow == F){
      broadLex %>%
        stringr::str_c("s") %>%
        return()
    } else if(narrow == T){
      broadLex %>%
        narrow_pt() %>%
        stringr::str_c("s") %>%
        return()
    }

    # If word exists:
  } else if(wd %in% pt_lex$word){
    broadLex = pt_lex %>%
      dplyr::filter(word == wd) %>%
      dplyr::slice(1) %>%
      dplyr::pull(pro) %>%
      stringr::str_replace(pattern = "\'", replacement = "\u02c8")

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

    if(weight %in% c("HLL", "LLL") & stringr::str_detect(wd, pattern = "[^iu]$")){
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
        stringr::str_replace(pattern = "(\u02c8\\w*)ol$",
                             replacement = "\\1\u0254l") %>%
        stringr::str_replace(pattern = "(\u02c8\\w*)el$",
                             replacement = "\\1\u025bl") %>%
        return()
    }


  }

}
