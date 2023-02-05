which_stress_pt = function(word = ""){

  word = str_split(string = word,
                   pattern = "\\.") %>%
    unlist() %>%
    str_detect("'")

  if(word[length(word)] == TRUE){
    return("Final")
  } else if(word[length(word)-1] == TRUE){
    return("Penult")
  } else if(word[length(word)-2] == TRUE){
    return("Antepenult")
  } else {
    return("Not a possible stress in Portuguese.")
  }

}

