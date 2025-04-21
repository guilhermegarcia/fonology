#' Phonotactic ngrams
#'
#' Given a phonemically transcribed text, the function returns the phonotactic ngrams in the text in descending order of frequency.
#' You must make sure that your input (your text) is phonemically transcribed first. Otherwise, the ngrams will
#' be based on orthography. You might want to use the \code{cleanText()} function to remove syllable boundaries and stress marks.
#' This function uses the ngram function from the ngram package.
#'
#' @param text A possible string in its phonemic form without syllabification or stress. The only diacritic that should be used is the tilde for nasals, e.g., ã.
#' @param n ngram of choice (\code{1}, \code{2}, or \code{3}). Defaults to \code{2}
#' @return A tibble with phonotactic ngrams
#' @examples
#' nGramTbl(text = "", n = 2)
#' @export


nGramTbl <- function(text = "", n = 2) {
  if (stringr::str_c(text, collapse = " ") %in% c("", NA)) {
    return(NA)
  }

  combs2 <- c("^^", "^$", "$^", "$$")

  combs3 <- c("^^^", "^^$", "^$^", "^$$", "$^^", "$^$", "$$^", "$$$")

  textClean <- text |>
    cleanText()

  textClean <- stringr::str_c("^", textClean, "$")

  textClean <- textClean |>
    stringr::str_c(collapse = " ") |>
    stringr::str_split("") |>
    unlist() |>
    stringr::str_c(collapse = " ", sep = " ")

  # Unigram
  #
  if (n == 1) {
    ng <- ngram::ngram(str = textClean, n = n)

    ng_tbl <- ng |>
      ngram::get.phrasetable() |>
      tibble::as_tibble(.name_repair = "minimal") |>
      dplyr::rename(nGrams = ngrams) |>
      dplyr::mutate(nGrams = stringr::str_remove_all(nGrams, "\\s$")) |>
      tidyr::separate_wider_delim(cols = nGrams, delim = " ", names = c("n1")) |>
      dplyr::mutate(nGrams = stringr::str_c(n1)) |>
      dplyr::select(nGrams, n1, freq, prop) |>
      dplyr::filter(!nGrams %in% c("^", "$")) |>
      dplyr::arrange(dplyr::desc(prop)) |>
      dplyr::mutate(n1 = stringr::str_replace_all(
        string = n1,
        pattern = "[\\^\\$]",
        replacement = "#"
      )) |>
      dplyr::mutate(nGrams = stringr::str_replace_all(
        string = nGrams,
        pattern = "[\\^\\$]",
        replacement = "#"
      ))

    return(ng_tbl)
  }

  # Bigram
  else if (n == 2) {
    ng <- ngram::ngram(str = textClean, n = n)

    ng_tbl <- ng |>
      ngram::get.phrasetable() |>
      tibble::as_tibble(.name_repair = "minimal") |>
      dplyr::rename(nGrams = ngrams) |>
      dplyr::mutate(nGrams = stringr::str_remove_all(nGrams, "\\s$")) |>
      tidyr::separate_wider_delim(cols = nGrams, delim = " ", names = c("n1", "n2")) |>
      dplyr::mutate(nGrams = stringr::str_c(n1, n2)) |>
      dplyr::select(nGrams, n1, n2, freq, prop) |>
      dplyr::filter(!nGrams %in% combs2) |>
      dplyr::arrange(dplyr::desc(prop)) |>
      dplyr::mutate(
        n1 = stringr::str_replace_all(
          string = n1,
          pattern = "[\\^\\$]",
          replacement = "#"
        ),
        n2 = stringr::str_replace_all(
          string = n2,
          pattern = "[\\^\\$]",
          replacement = "#"
        )
      ) |>
      dplyr::mutate(nGrams = stringr::str_replace_all(
        string = nGrams,
        pattern = "[\\^\\$]",
        replacement = "#"
      ))

    return(ng_tbl)
  }



  # Trigram
  else if (n == 3) {
    ng <- ngram::ngram(str = textClean, n = n)

    ng_tbl <- ng |>
      ngram::get.phrasetable() |>
      tibble::as_tibble(.name_repair = "minimal") |>
      dplyr::rename(nGrams = ngrams) |>
      dplyr::mutate(nGrams = stringr::str_remove_all(nGrams, "\\s$")) |>
      tidyr::separate_wider_delim(cols = nGrams, delim = " ", names = c("n1", "n2", "n3")) |>
      dplyr::mutate(nGrams = stringr::str_c(n1, n2, n3)) |>
      dplyr::select(nGrams, n1, n2, n3, freq, prop) |>
      dplyr::filter(!nGrams %in% combs3) |>
      dplyr::arrange(dplyr::desc(prop)) |>
      dplyr::mutate(
        n1 = stringr::str_replace_all(
          string = n1,
          pattern = "[\\^\\$]",
          replacement = "#"
        ),
        n2 = stringr::str_replace_all(
          string = n2,
          pattern = "[\\^\\$]",
          replacement = "#"
        ),
        n3 = stringr::str_replace_all(
          string = n3,
          pattern = "[\\^\\$]",
          replacement = "#"
        )
      ) |>
      dplyr::mutate(nGrams = stringr::str_replace_all(
        string = nGrams,
        pattern = "[\\^\\$]",
        replacement = "#"
      )) |>
      dplyr::filter(!stringr::str_detect(string = nGrams, pattern = "#{2,}"))

    return(ng_tbl)
  } else {
    message("n must be 1, 2, or 3.")
    return(NA)
  }
}

#' Visualizing nGrams
#'
#' Given a tibble with phonotactic ngrams (result of nGram_tbl(..., n = 2)), the function plots the n most common ngrams
#'
#' @param ngrams A tibble in the exact format used in biGram_tbl()
#' @param type The type of plot desired: heatmap or lollipop (default)
#' @param n The number of ngrams to plot
#' @return A ggplot2 plot with the most common ngrams
#' @examples
#' plotnGrams(ngrams = "")
#' @export

plotnGrams <- function(ngrams, type = "lollipop", n = 5) {
  nCols <- length(names(ngrams))

  if (nCols == 4) {
    if (!(sum(names(ngrams) == c("nGrams", "n1", "freq", "prop"))) == 4) {
      message("Your input is invalid. You must first run nGram_tbl(..., n = {1,2,3}) to create an appropriate input.")
      return(NA)
    }
  } else if (nCols == 5) {
    if (!(sum(names(ngrams) == c("nGrams", "n1", "n2", "freq", "prop"))) == 5) {
      message("Your input is invalid. You must first run nGram_tbl(..., n = {1,2,3}) to create an appropriate input.")
      return(NA)
    }
  } else if (nCols == 6) {
    if (!(sum(names(ngrams) == c("nGrams", "n1", "n2", "n3", "freq", "prop"))) == 6) {
      message("Your input is invalid. You must first run nGram_tbl(..., n = {1,2,3}) to create an appropriate input.")
      return(NA)
    }
  } else {
    message("Your input is invalid. You must first run nGram_tbl(..., n = {1,2,3}) to create an appropriate input.")
    return(NA)
  }

  # Requirement for heat:
  if (type == "heat" & nCols != 5) {
    message("Your input is invalid. You must first run nGram_tbl(..., n = 2) to create an appropriate input for a heat map.")
    return(NA)
  }



  if (nrow(ngrams) < n) {
    message("Text has fewer ngrams than the desired n.")
    return(NA)
  }

  ngrams <- ngrams |>
    dplyr::slice(1:n)


  lollipop <- ggplot2::ggplot(
    data = ngrams,
    ggplot2::aes(
      x = forcats::fct_reorder(nGrams, prop, .desc = F),
      y = prop,
      label = stringr::str_remove(string = nGrams, pattern = "-")
    )
  ) +
    ggplot2::geom_point(size = 4) +
    ggplot2::geom_segment(ggplot2::aes(x = nGrams, xend = nGrams, y = 0, yend = prop),
      color = "gray"
    ) +
    ggplot2::geom_label(fill = "#CCE8FF") +
    ggplot2::coord_flip() +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Proportion"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format())



  heat <- ggplot2::ggplot(
    data = ngrams,
    ggplot2::aes(x = n2, y = n1, fill = prop)
  ) +
    ggplot2::geom_tile(color = "white", lwd = .5) +
    # ggplot2::geom_text(ggplot2::aes(label = round(prop, 2)),
    #                    color = "white", size = 4) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_fill_gradient(
      low = "#CCE8FF",
      high = "#08306B"
    ) +
    ggplot2::labs(x = "2", y = "1") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::coord_fixed()

  if (stringr::str_to_lower(type) == "lollipop") {
    return(lollipop)
  } else if (stringr::str_to_lower(type) == "heat") {
    return(heat)
  } else {
    message("The only two types of plots available are 'lollipop' and 'heat'.")
    return(NA)
  }
}
