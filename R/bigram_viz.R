#' Phonotactic bigrams
#'
#' Given a phonemically transcribed text, the function returns the phonotactic bigrams in the text in descending order of frequency.
#' You must make sure that your input (your text) is phonemically transcribed first. Otherwise, the bigrams will
#' be based on orthography. You might want to use the cleanText() function to remove syllable boundaries and stress marks.
#' This function uses the ngram function from the ngram package.
#'
#' @param text A possible string in its phonemic form without syllabification or stress. The only diacritic that should be used is the tilde for nasals, e.g., ã.
#' @return A tibble with phonotactic bigrams
#' @examples
#' biGram_tbl(text = "")
#' @importFrom magrittr %>%
#' @export


biGram_tbl = function(text = ""){

  if(stringr::str_c(text, collapse = " ") %in% c("", NA)){
    return(NA)
  }

  textClean = text %>%
    cleanText() %>%
    stringr::str_c("^", ., "$")

  textClean = textClean %>%
    stringr::str_c(collapse = " ") %>%
    stringr::str_split("") %>%
    unlist() %>%
    stringr::str_c(collapse = " ", sep  = " ")

  ng = ngram::ngram(str = textClean, n = 2)

  ng_tbl = ng %>%
    ngram::get.phrasetable() %>%
    tibble::as_tibble() %>%
    dplyr::rename(nGrams = ngrams) %>%
    dplyr::mutate(nGrams = stringr::str_remove_all(nGrams, "\\s$")) %>%
    tidyr::separate_wider_delim(cols = nGrams, delim = " ", names = c("n1", "n2")) %>%
    dplyr::mutate(nGrams = stringr::str_c(n1, n2)) %>%
    dplyr::select(nGrams, n1, n2, freq, prop) %>%
    dplyr::filter(!nGrams %in% c("^^", "$$", "^$", "$^")) %>%
    dplyr::arrange(dplyr::desc(prop)) %>%
    dplyr::mutate(n1 = stringr::str_replace_all(string = n1,
                                                pattern = "[\\^\\$]",
                                                replacement = "#"),
                  n2 = stringr::str_replace_all(string = n2,
                                                pattern = "[\\$\\^]",
                                                replacement = "#")) %>%
    dplyr::mutate(nGrams = stringr::str_replace_all(string = nGrams,
                                                    pattern = "[\\^\\$]",
                                                    replacement = "#"))

  return(ng_tbl)

}

#' Visualizing bigrams
#'
#' Given a tibble with phonotactic bigrams (result of biGram_tbl()), the function plots the n most common bigrams
#'
#' @param bigrams A tibble in the exact format used in biGram_tbl()
#' @param type The type of plot desired: heatmap or lollipop (default)
#' @return A ggplot2 plot with the most common bigrams
#' @examples
#' plot_biGrams(bigrams = "")
#' @importFrom magrittr %>%
#' @export

plot_biGrams = function(bigrams, type = "lollipop"){

  if(!sum(names(bigrams) == c("nGrams", "n1", "n2", "freq", "prop")) == 5){
    message("Your input is invalid. You must first run biGram_tbl() to create an appropriate input.")
    return(NA)
  }

  if(nrow(bigrams) < 10){
    message("Text has too few bigrams for plotting.")
    return(NA)
  }

  bigrams_lolli = bigrams %>%
    # dplyr::mutate(n1 = stringr::str_replace_all(string = n1,
    #                                             pattern = "\\^",
    #                                             replacement = "#"),
    #               n2 = stringr::str_replace_all(string = n2,
    #                                             pattern = "\\$",
    #                                             replacement = "#")) %>%
    dplyr::slice(1:10)


  lollipop = ggplot2::ggplot(data = bigrams_lolli,
                             ggplot2::aes(x = forcats::fct_reorder(nGrams, prop, .desc = F),
                                          y = prop,
                                          label = stringr::str_remove(string = nGrams, pattern = "-"))) +
    ggplot2::geom_point(size = 4) +
    ggplot2::geom_segment(ggplot2::aes(x = nGrams, xend = nGrams, y = 0, yend = prop),
                          color = "gray") +
    ggplot2::geom_label(fill = "#CCE8FF") +
    ggplot2::coord_flip() +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank()) +
    ggplot2::labs(x = NULL,
                  y = "Proportion") +
    ggplot2::scale_y_continuous(labels = scales::percent_format())



  heat = ggplot2::ggplot(data = bigrams,
                         ggplot2::aes(x = n2, y = n1, fill = prop)) +
    ggplot2::geom_tile(color = "white", lwd = .5) +
    # ggplot2::geom_text(ggplot2::aes(label = round(prop, 2)),
    #                    color = "white", size = 4) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_fill_gradient(low = "#CCE8FF",
                                 high = "#08306B") +
    ggplot2::labs(x = "2", y = "1") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::coord_fixed()

  if(stringr::str_to_lower(type) == "lollipop"){
    return(lollipop)
  } else if(stringr::str_to_lower(type) == "heat"){
    return(heat)
  } else {
    message("The only two types of plots available are 'lollipop' and 'heat'.")
    return(NA)
  }

}




