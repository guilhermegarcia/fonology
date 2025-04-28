#' Syllabifier for French
#'
#' Syllabifies a given word in French
#' @param word The string of interest using IPA phonemic transcription
#' @noRd
#' @return The syllabified version of the string in question

syllabify_fr <- function(word) {
  # Gestion des voyelles nasales
  word <- stringr::str_replace_all(word, pattern = "\u0251\u0303", replacement = "A")
  word <- stringr::str_replace_all(word, pattern = "\u025b\u0303", replacement = "E")
  word <- stringr::str_replace_all(word, pattern = "\u0254\u0303", replacement = "O")
  word <- stringr::str_replace_all(word, pattern = "\u0153\u0303", replacement = "U")

  # Separation apres chaque voyelles
  word <- stringr::str_replace_all(word, pattern = "([iyue\u00f8o\u025b\u0153\u0254a\u0251\u0259AEOU])", replacement = "\\1-")

  # Effacement des separateurs en fin de mots
  word <- stringr::str_replace_all(word, pattern = "-$|-(?= )", replacement = "")

  # Gestion des majuscules
  word <- stringr::str_replace_all(word, pattern = "A", replacement = "\u0251\u0303")
  word <- stringr::str_replace_all(word, pattern = "E", replacement = "\u025b\u0303")
  word <- stringr::str_replace_all(word, pattern = "O", replacement = "\u0254\u0303")
  word <- stringr::str_replace_all(word, pattern = "U", replacement = "\u0153\u0303")

  # Gestion des consonnes seules en fin de mots
  word <- stringr::str_replace_all(word, pattern = "-(n|\u0281|k|l|s|d|b|t|v)(s|t|b|\u0281|k|d|p|f|l|\u0281|v|n)", replacement = "\\1-\\2")
  word <- stringr::str_replace_all(word, pattern = "-([pbtdkgsz\u0283\u0292fvl\u0281mn\u0272jw]$)", replacement = "\\1")
  word <- stringr::str_replace_all(word, pattern = "-([pbtdkgsz\u0283\u0292fvl\u0281mn\u0272jw](?= ))", replacement = "\\1")

  # Transfert de tout les trait d'union en point
  word <- stringr::str_replace_all(word, pattern = "-", replacement = ".")

  # TODO: Post-syllabification adjustments to improve accuracy
  word <- stringr::str_replace_all(word, pattern = "\\.\u0281([bdfgklmnpstvz])", replacement = "\u0281.\\1")
  word <- stringr::str_replace_all(word, pattern = "o(\\w+)", replacement = "\u0254\\1")

  # Need to fix schwa in words like mettre; should be easy


  return(word)
}
