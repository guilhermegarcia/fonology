weight_pt = function(word = ""){

  # Remove stress
  word = str_remove_all(string = word,
                 pattern = "'")


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
  return(word)

}
