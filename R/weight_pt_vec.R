#' Vectorized syllable weight labeller
#'
#' Labels a given string in Portuguese according to its weight profile using Ls and Hs.
#' Note that, although simple plural -s is taken into account, the function assumes the string provided is monomorphemic.
#' For a simplified but vectorized version of the function, use getWeight_pt_simple().
#' @param word The vector with strings of interest using IPA phonemic transcription, already syllabified and stressed
#' @return The sequence of Ls and Hs based on the word's weight profile given the phonology of Portuguese
#' @examples
#' getWeight_pt_simple(word = c("kom.pu.ta.ˈdoɾ", "ka.ˈloɾ.pe"));
#' @export

getWeight_pt_simple = function(word = c("kom.pu.ta.ˈdoɾ")){
  if(!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)

  syl_list = word %>%
    str_split("\\.")

  syl_list = lapply(syl_list, function(x) str_replace_all(x, pattern = "\\w+[jwlmnɾspbtdkgɾzfvʃʒʎɲ]",
                                                          replacement = "H"))

  syl_list = lapply(syl_list, function(x) str_replace_all(x, pattern = "[\\w*]{0,3}[ãõaeiouɛɔ]$",
                                                          replacement = "L"))

  syl_list = lapply(syl_list, function(x) str_replace_all(x, pattern = "H̃",
                                                          replacement = "H"))

  # Select only trisyllabic window:
  profile = lapply(syl_list, function(x) rev(rev(x)[1:3]))

  # Remove NAs:
  profile = lapply(profile, function(x) x[!is.na(x)])

  # Collapse weight:
  profile = lapply(profile, function(x) str_c(x, collapse = "")) %>%
    unlist()

  profile[profile %in% ""] = NA

  return(profile)
}
