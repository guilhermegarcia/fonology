#' Diacritic detector in Portuguese orthography
#'
#' This is a helper function that detects whether a stress diacritic is present in the input,
#' which allows for stress assignment to be more accurate in so-called exceptional stress words
#' in the language (e.g., all proparoxytones are orthographically accented). A stress diacritic
#' is an acute accent, a grave accent, or a circumflex accent. The nasal diacritic are not included
#' as it is already accounted for by ipa_pt(). Grave accents are included to accommodate potential
#' typos or OCR errors for digitized inputs.
#' @param word A possible string in Portuguese in its orthographic form
#' @return The position of a stress diacritic (if present) counting from the
#' right edge of the word. The function returns NA otherwise. If multiple stress diacritics are provided,
#' or if a diacritic is outside the stress window, an NA is returned.
#' @examples
#' ipa_pt(word = "pálado");
#' @importFrom magrittr %>%
#' @export

diacritic_pt = function(word){

  if(!stringr::str_detect(word, pattern = "[\u00e8\u00f2\u00e0\u00ec\u00f9\u00e1\u00e2\u00f3\u00f4\u00ea\u00e9\u00ed\u00fa]")){
    return(NA)
  }

  syllabify_ort_pt = function(word = ""){

    # Start with CV:
    word = stringr::str_replace_all(string = word,
                                    pattern = "([aeiou\u025b\u0254\u00e8\u00f2\u00e0\u00ec\u00f9\u00e1\u00e2\u00e3\u00f3\u00f4\u00f5\u00ea\u00e9\u00ed\u00fa])",
                                    replacement = "\\1.")

    # Fix diphthongs:
    word = stringr::str_replace_all(string = word,
                                    pattern = "([aeiou\u025b\u0254])\\.([wj])",
                                    replacement = "\\1\\2.")

    # Fix onset clusteres:
    word = stringr::str_replace_all(string = word,
                                    pattern = "\\.([lmn\u027eskgpb])([pbtdkgsxzfv\u0283\u0292\u028e\u0272mn])",
                                    replacement = "\\1.\\2")


    # Remove empty final syllables:
    word = stringr::str_remove_all(string = word,
                                   pattern = "\\.$")

    # Remove C-syllables word finally:
    word = stringr::str_replace_all(string = word,
                                    pattern = "\\.([pbtdkgszfv\u0283\u0292\u028elmn\u027es])$",
                                    replacement = "\\1")

    word = stringr::str_replace_all(string = word,
                                    pattern = "\\.([sznm])([lr])",
                                    replacement = "\\1.\\2")

    # Overly complex onset clusters:
    word = stringr::str_replace_all(string = word,
                                    pattern = "\\.s([tdpbkg][\u027el])",
                                    replacement = "s.\\1")

    # qu
    word = stringr::str_replace_all(string = word,
                                    pattern = "(qu)\\.([aeiou\u00e8\u00f2\u00e0\u00ec\u00f9\u00e1\u00e9\u00ed\u00f3\u00fa\u00ea\u00f4])",
                                    replacement = "\\1\\2")

    # gu
    word = stringr::str_replace_all(string = word,
                                    pattern = "(gu)\\.([aeiou\u00e8\u00f2\u00e0\u00ec\u00f9\u00e1\u00e9\u00ed\u00f3\u00fa\u00ea\u00f4])",
                                    replacement = "\\1\\2")

    # L+N onsets:
    word = stringr::str_replace_all(string = word,
                                    pattern = "\\.([rl])([mn])",
                                    replacement = "\\1.\\2")

    # ss
    word = stringr::str_replace_all(string = word,
                                    pattern = "s.s",
                                    replacement = ".s")

    # rr
    word = stringr::str_replace_all(string = word,
                                    pattern = "r.r",
                                    replacement = ".r")


    return(word)

  }

  # Locate diacritic:
  split_word = syllabify_ort_pt(word) %>%
    stringr::str_split("\\.") %>% unlist() %>%
    rev()

  diacritics = split_word %>% stringr::str_detect(pattern = "[\u00e1\u00e2\u00f3\u00f4\u00ea\u00e9\u00ed\u00fa]")

  diacritics_result = which(diacritics == TRUE)

  if(length(diacritics_result) > 1){
    message("A given word cannot have more than one stress diacritic in Portuguese.")
    return(NA)
  } else if(diacritics_result > 3){
    message("Diacritic provided is outside stress window in the language.")
    return(NA)
  }

  return(diacritics_result)

}
