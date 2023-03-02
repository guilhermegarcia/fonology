#' Stress labeller
#'
#' Labels a given stressed string as follows: Final, Penult, Antepenult
#' @param word The string of interest using IPA phonemic transcription, already syllabified and stressed
#' @param stress The symbol used to mark stress
#' @return The primary stress position
#' @examples
#' getStress(word = "kom.pu.ta.ˈdoɾ", stress = "ˈ");
#' @importFrom magrittr %>%
#' @export

getStress = function(word = c("kom.pu.ta.ˈdoɾ"), stress = "ˈ"){

  syl_list = word %>%
    stringr::str_split("\\.")

  indices = lapply(syl_list, function(x) which(stringr::str_detect(rev(x), pattern = stress))) %>% unlist()

  indices = stringr::str_replace_all(indices, pattern = "1", replacement = "final")
  indices = stringr::str_replace_all(indices, pattern = "2", replacement = "penult")
  indices = stringr::str_replace_all(indices, pattern = "3", replacement = "antepenult")
  indices = stringr::str_replace_all(indices, pattern = "4", replacement = "pre-antepenult")
  indices = stringr::str_replace_all(indices, pattern = "[56789]", replacement = "ungrammatical")

  return(indices)
}
