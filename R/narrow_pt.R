#' Narrow IPA transcriber for Portuguese
#'
#' Returns IPA phonemic transcription for a given string that is already phonemically transcribed
#' @param word The string of interest is an output of ipa_pt()
#' @return The IPA transcription of said string with surface-level adjustments
#' @examples
#' narrow_pt(word = "ˈpsi.ko");

narrow_pt = function(word = ""){
  if (!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)

  narrow = word



  # Vowel neutralization (word-final):
  narrow = str_replace(narrow,
                       pattern = "(\\.[^ˈ]\\w*)o$",
                       replacement = "\\1ʊ")

  narrow = str_replace(narrow,
                       pattern = "(\\.[^ˈ]\\w*)e$",
                       replacement = "\\1ɪ")

  narrow = str_replace(narrow,
                       pattern = "(\\.[^ˈ]\\w*)a$",
                       replacement = "\\1ɐ")

  # Palatalization:
  narrow = str_replace_all(narrow,
                           pattern = "t([iɪ])",
                           replacement = "t͡ʃ\\1")

  narrow = str_replace_all(narrow,
                           pattern = "d([iɪ])",
                           replacement = "d͡ʒ\\1")

  # Vowel devoicing word-finally after voiceless affricate:
  narrow = str_replace(narrow,
                       pattern = "(t͡ʃ)ɪ$",
                       replacement = "\\1ɪ̥")

  # Clusters and epentheses:
  narrow = str_replace(narrow,
                       pattern = "^ps",
                       replacement = "pi.s")

  narrow = str_replace(narrow,
                       pattern = "k\\.(ˈ*t)",
                       replacement = ".ki̥.\\1")

  narrow = str_replace(narrow,
                       pattern = "^sk",
                       replacement = "is.k")

  narrow = str_replace(narrow,
                       pattern = "p\\.(ˈ*t)",
                       replacement = ".pi̥.\\1")


  # Nasalization:
  narrow = str_replace_all(narrow,
                           pattern = "a([nmɲ])",
                           replacement = "ã\\1")

  narrow = str_replace_all(narrow,
                           pattern = "o([nmɲ])",
                           replacement = "õ\\1")

  narrow = str_replace_all(narrow,
                           pattern = "ẽj̃([nmɲ])",
                           replacement = "õ\\1")

  narrow = str_replace_all(narrow,
                           pattern = "i([nmɲ])",
                           replacement = "ĩ\\1")

  # l-vocalization:
  narrow = str_replace_all(narrow,
                           pattern = "l($|\\.)",
                           replacement = "w\\1")

  # Voicing assimilation
  word = str_replace_all(string = word,
                         pattern = "s(\\.[mnl])",
                         replacement = "z\\1")

  # Diphthongization:
  # i.a -> ja, but diphthongization is blocked if onset is already CC
  # This shows that (a) max onset is 2, and (b) j is in onset position

  # When V.'V:
  # First, word-initial:
  narrow = str_replace_all(narrow,
                           pattern = "(^\\w?)i\\.ˈa",
                           replacement = "ˈ\\1ja")

  # Word-internal:
  narrow = str_replace_all(narrow,
                           pattern = "(\\.\\w?)i\\.ˈa",
                           replacement = "ˈ\\1ja")

  # When V.V:
  # First, word-initial:
  narrow = str_replace_all(narrow,
                           pattern = "(^\\w?)i\\.a",
                           replacement = "\\1ja")

  # Word-internal:
  narrow = str_replace_all(narrow,
                           pattern = "(\\.\\w?)i\\.a",
                           replacement = "\\1ja")

  # Epenthesis:
  narrow = str_replace(narrow,
                       pattern = "^s([ptkf])",
                       replacement = "is.\\1")

  narrow = str_replace(narrow,
                       pattern = "^s([bdgmnvlr])",
                       replacement = "iz.\\1")

  narrow = str_replace(narrow,
                       pattern = "([pbtdkg])\\.([nmpbtdkg])",
                       replacement = ".\\1i.\\2")

  narrow = str_replace(narrow,
                       pattern = "^ˈs([ptkf])",
                       replacement = "is.ˈ\\1")

  narrow = str_replace(narrow,
                       pattern = "^ˈs([bdgmnvlr])",
                       replacement = "iz.ˈ\\1")

  narrow = str_replace(narrow,
                       pattern = "([pbtdkg])\\.ˈ([nmpbtdkg])",
                       replacement = ".\\1i.ˈ\\2")

  narrow %>%
    sec_stress_pt() %>%
    return()

}
