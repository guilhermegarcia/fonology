syllabify_pt = function(word = ""){

  source("R/transcribe_pt.R")

  # Start with CV:
  word = str_replace_all(string = word,
                      pattern = "([aeiouɛɔ])",
                      replacement = "\\1.")

  # Fix diphthongs:
  word = str_replace_all(string = word,
                      pattern = "([aeiouɛɔ])\\.([wj])",
                      replacement = "\\1\\2.")

  # Fix onset clusteres:
  word = str_replace_all(string = word,
                      pattern = "\\.([lmnɾs])([pbtdkgsɾzfvʃʒʎɲmn])",
                      replacement = "\\1.\\2")

  # Remove empty final syllables:
  word = str_remove_all(string = word,
                     pattern = "\\.$")

  # Remove C-syllables word finally:
  word = str_replace_all(string = word,
                      pattern = "\\.([pbtdkgszfvʃʒʎlmnɾs])$",
                      replacement = "\\1")

  return(word)

}



