#' Vowel plot generator
#'
#' Generates vowel trapezoid using \code{ggplot2} as well as LaTeX code using vowel package.
#' @param lg The language whose vowel inventory will be plotted
#' @param tex Whether a \code{tex} file is desired. By default, \code{tex = FALSE}
#' @return The vowel inventory desired
#' @examples
#' \dontrun{
#' plotVowels(lg = "portuguese")
#' plotVowels(lg = "english")
#' }
#' @export

plotVowels <- function(lg = "English", tex = F) {
  # Tex file:
  texOutput <- "\\documentclass[12pt, letterpaper]{article}
\\usepackage{vowel}
\\usepackage{tipa}
\\begin{document}
{\\LARGE
\\begin{center}
\\begin{vowel}
 \\putcvowel[l]{\\textipa{i}}{1}
 \\putcvowel[r]{\\textipa{y}}{1}
 \\putcvowel[l]{\\textipa{e}}{2}
 \\putcvowel[r]{\\textipa{\\o}}{2}
 \\putcvowel[l]{\\textipa{E}}{3}
 \\putcvowel[r]{\\textipa{\\oe}}{3}
 \\putcvowel[l]{\\textipa{a}}{4}
 \\putcvowel[r]{\\textscoelig}{4}
 \\putcvowel[l]{\\textipa{A}}{5}
 \\putcvowel[r]{\\textipa{6}}{5}
 \\putcvowel[l]{\\textipa{2}}{6}
 \\putcvowel[r]{\\textipa{O}}{6}
 \\putcvowel[l]{\\textipa{7}}{7}
 \\putcvowel[r]{\\textipa{o}}{7}
 \\putcvowel[l]{\\textipa{W}}{8}
 \\putcvowel[r]{\\textipa{u}}{8}
 \\putcvowel[l]{\\textipa{1}}{9}
 \\putcvowel[r]{\\textipa{0}}{9}
 \\putcvowel[l]{\\textipa{9}}{10}
 \\putcvowel[r]{\\textipa{8}}{10}
 \\putcvowel{\\textipa{@}}{11}
 \\putcvowel[l]{\\textipa{3}}{12}
 \\putcvowel[r]{\\textcloserevepsilon}{12}
 \\putcvowel{\\textipa{I}\\ \\textipa{ }}{13}
 \\putcvowel{\\textipa{ }\\ \\textipa{Y}}{13}
 \\putcvowel{\\textipa{U}}{14}
 \\putcvowel{\\textipa{5}}{15}
 \\putcvowel{\\textipa{\\ae}}{16}
\\end{vowel}
\\end{center}
}
\\end{document}"


  # Tex tibble:
  latex <- tibble::tibble(
    lines = stringr::str_split(texOutput, "\\n")[[1]],
    vowel = c(
      rep("pre", 7),
      c(
        "i", "y", "e", "\u00f8",
        "\u025b", "\u0153", "a", "\u0276",
        "\u0251", "\u0252", "\u028c", "\u0254",
        "\u0264", "o", "\u026f", "u",
        "\u0268", "\u0289", "\u0258", "\u0275",
        "\u0259", "\u025c", "\u025e", "\u026a",
        "\u028f", "\u028a", "\u0250", "\u00e6"
      ),
      rep("post", 4)
    )
  )

  # Tibble:
  V <- tibble::tibble(vowel = character(), x = numeric(), y = numeric())

  V <- V |>
    tibble::add_row(
      vowel = "a",
      x = 3.4,
      y = -0.9
    ) |>
    tibble::add_row(
      vowel = "\u025b",
      x = 1.7,
      y = 3.1
    ) |>
    tibble::add_row(
      vowel = "\u0254",
      x = 11.7,
      y = 3.1
    ) |>
    tibble::add_row(
      vowel = "e",
      x = 0,
      y = 7.1
    ) |>
    tibble::add_row(
      vowel = "o",
      x = 11.7,
      y = 7.1
    ) |>
    tibble::add_row(
      vowel = "i",
      x = -1.5,
      y = 11.1
    ) |>
    tibble::add_row(
      vowel = "u",
      x = 11.7,
      y = 11.1
    ) |>
    tibble::add_row(
      vowel = "\u0276",
      x = 4.8,
      y = -0.9
    ) |>
    tibble::add_row(
      vowel = "\u0251",
      x = 10.2,
      y = -0.9
    ) |>
    tibble::add_row(
      vowel = "\u0252",
      x = 11.7,
      y = -0.9
    ) |>
    tibble::add_row(
      vowel = "\u00e6",
      x = 2.5,
      y = 1
    ) |>
    tibble::add_row(
      vowel = "\u0250",
      x = 6.65,
      y = 1.3
    ) |>
    tibble::add_row(
      vowel = "\u0153",
      x = 3.1,
      y = 3.1
    ) |>
    tibble::add_row(
      vowel = "\u025c",
      x = 5.8,
      y = 3.1
    ) |>
    tibble::add_row(
      vowel = "\u025e",
      x = 7,
      y = 3.1
    ) |>
    tibble::add_row(
      vowel = "\u028c",
      x = 10.2,
      y = 3.1
    ) |>
    tibble::add_row(
      vowel = "\u00f8",
      x = 1.35,
      y = 7.1
    ) |>
    tibble::add_row(
      vowel = "\u0258",
      x = 5,
      y = 7.1
    ) |>
    tibble::add_row(
      vowel = "\u0275",
      x = 6.4,
      y = 7.1
    ) |>
    tibble::add_row(
      vowel = "\u0259",
      x = 6,
      y = 5.1
    ) |>
    tibble::add_row(
      vowel = "\u0264",
      x = 10.2,
      y = 7.1
    ) |>
    tibble::add_row(
      vowel = "\u026a",
      x = 1.4,
      y = 9.25
    ) |>
    tibble::add_row(
      vowel = "\u028f",
      x = 2.4,
      y = 9.25
    ) |>
    tibble::add_row(
      vowel = "y",
      x = -0.28,
      y = 11.1
    ) |>
    tibble::add_row(
      vowel = "\u0268",
      x = 4.3,
      y = 11.1
    ) |>
    tibble::add_row(
      vowel = "\u0289",
      x = 5.8,
      y = 11.1
    ) |>
    tibble::add_row(
      vowel = "\u026f",
      x = 10.1,
      y = 11.1
    ) |>
    tibble::add_row(
      vowel = "\u028a",
      x = 9,
      y = 9.25
    )

  V <- V |>
    dplyr::mutate(choice = dplyr::row_number())

  # Language inventories:

  arabic <- c("a", "i", "u")
  french <- c("a", "e", "\u00f8", "\u0251", "i", "y", "o", "u", "\u025b", "\u0254", "\u0259", "\u0153")
  english <- c("a", "e", "\u0251", "i", "o", "u", "\u025b", "\u0254", "\u0259", "\u026a", "\u028a", "\u00e6", "\u028c")
  dutch <- c("a", "e", "i", "o", "u", "\u025b", "\u0254", "\u0259", "\u026a", "\u028f", "\u0250", "y", "\u00f8")
  german <- c("a", "e", "\u0153", "i", "o", "u", "\u025b", "\u0254", "\u0259", "\u026a", "\u028a", "\u028f", "\u0250", "y", "\u00f8")
  hindi <- c("\u0251", "e", "i", "o", "u", "\u025b", "\u0254", "\u026a", "\u028a", "\u0259")
  italian <- c("a", "e", "i", "o", "u", "\u025b", "\u0254")
  japanese <- c("a", "e", "i", "o", "\u026f")
  korean <- c("\u0250", "e", "i", "o", "\u026f", "u", "\u028c")
  mandarin <- c("i", "y", "u", "\u0264", "a", "\u0259")
  portuguese <- c("a", "e", "i", "o", "u", "\u025b", "\u0254")
  spanish <- c("a", "e", "i", "o", "u")
  swahili <- c("a", "i", "u", "\u025b", "\u0254")
  russian <- c("a", "e", "i", "o", "u", "\u0268")
  talian <- c("a", "e", "i", "o", "u", "\u025b", "\u0254")
  thai <- c("a", "e", "i", "o", "u", "\u025b", "\u0254", "\u026f", "\u0264")
  vietnamese <- c("a", "e", "i", "o", "u", "\u025b", "\u0254", "\u026f", "\u0264")



  if (lg == "all") {
    fig <- ggplot2::ggplot(data = V, ggplot2::aes(x = x, y = y, label = vowel)) +
      ggplot2::labs(
        x = NULL,
        y = NULL
      ) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.ticks = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        line = ggplot2::element_blank()
      ) +
      ggplot2::geom_segment(ggplot2::aes(x = 7, y = -1, xend = 5, yend = 11), color = "gray") + # center vertical
      ggplot2::geom_segment(ggplot2::aes(x = 3, y = 3, xend = 5.3, yend = 3), color = "gray") + # mid-low left
      ggplot2::geom_segment(ggplot2::aes(x = 7, y = 3, xend = 10, yend = 3), color = "gray") + # mid-low right
      ggplot2::geom_segment(ggplot2::aes(x = 1.35, y = 7, xend = 5, yend = 7), color = "gray") + # mid-hi left
      ggplot2::geom_segment(ggplot2::aes(x = 6.4, y = 7, xend = 10, yend = 7), color = "gray") + # mid-hi right
      ggplot2::geom_segment(ggplot2::aes(x = 4.7, y = -1, xend = 10, yend = -1), color = "gray40") + # bottom
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 11, xend = 4, yend = 11), color = "gray40") + # top left
      ggplot2::geom_segment(ggplot2::aes(x = 6, y = 11, xend = 10, yend = 11), color = "gray40") + # top right
      ggplot2::geom_label(
        size = 10, color = "black", label.size = 0, fill = "white",
        # family = "Charis SIL",
        label.r = ggplot2::unit(1, "lines"),
        label.padding = ggplot2::unit(0.5, "lines")
      ) +
      ggplot2::geom_segment(ggplot2::aes(x = 4, y = -1, xend = -1, yend = 11), color = "gray40") + # left
      ggplot2::geom_segment(ggplot2::aes(x = 11, y = 11, xend = 11, yend = -1), color = "gray40") + # right
      ggplot2::geom_point(ggplot2::aes(x = 4, y = -1), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 11, y = -1), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 2.33, y = 3), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 6.35, y = 3), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 11, y = 3), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 0.67, y = 7), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 5.68, y = 7), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 11, y = 7), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = -1, y = 11), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 5, y = 11), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 11, y = 11), size = 5, color = "gray40")


    texOutput <- latex |>
      # filter(vowel %in% c(targetLanguage, "pre", "post")) |>
      dplyr::pull(lines) |>
      stringr::str_c(collapse = "\n")

    print(stringr::str_c("LaTeX code: ", texOutput))

    return(fig)
  } else {
    targetLanguage <- eval(parse(text = stringr::str_to_lower(lg)))

    fig <- ggplot2::ggplot(data = V |>
      dplyr::filter(vowel %in% targetLanguage) |>
      droplevels(), ggplot2::aes(x = x, y = y, label = vowel)) +
      ggplot2::labs(
        x = NULL,
        y = NULL
      ) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.ticks = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        line = ggplot2::element_blank()
      ) +
      ggplot2::geom_segment(ggplot2::aes(x = 7, y = -1, xend = 5, yend = 11), color = "gray") + # center vertical
      ggplot2::geom_segment(ggplot2::aes(x = 3, y = 3, xend = 5.3, yend = 3), color = "gray") + # mid-low left
      ggplot2::geom_segment(ggplot2::aes(x = 7, y = 3, xend = 10, yend = 3), color = "gray") + # mid-low right
      ggplot2::geom_segment(ggplot2::aes(x = 1.35, y = 7, xend = 5, yend = 7), color = "gray") + # mid-hi left
      ggplot2::geom_segment(ggplot2::aes(x = 6.4, y = 7, xend = 10, yend = 7), color = "gray") + # mid-hi right
      ggplot2::geom_segment(ggplot2::aes(x = 4.7, y = -1, xend = 10, yend = -1), color = "gray40") + # bottom
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 11, xend = 4, yend = 11), color = "gray40") + # top left
      ggplot2::geom_segment(ggplot2::aes(x = 6, y = 11, xend = 10, yend = 11), color = "gray40") + # top right
      ggplot2::geom_label(
        size = 10, color = "black", label.size = 0, fill = "white",
        # family = "Charis SIL",
        label.r = ggplot2::unit(1, "lines"),
        label.padding = ggplot2::unit(0.5, "lines")
      ) +
      ggplot2::geom_segment(ggplot2::aes(x = 4, y = -1, xend = -1, yend = 11), color = "gray40") + # left
      ggplot2::geom_segment(ggplot2::aes(x = 11, y = 11, xend = 11, yend = -1), color = "gray40") + # right
      ggplot2::geom_point(ggplot2::aes(x = 4, y = -1), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 11, y = -1), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 2.33, y = 3), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 6.35, y = 3), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 11, y = 3), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 0.67, y = 7), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 5.68, y = 7), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 11, y = 7), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = -1, y = 11), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 5, y = 11), size = 5, color = "gray40") +
      ggplot2::geom_point(ggplot2::aes(x = 11, y = 11), size = 5, color = "gray40")

    # ADJUST TEX:
    texOutput <- latex |>
      dplyr::filter(vowel %in% c(targetLanguage, "pre", "post")) |>
      dplyr::pull(lines) |>
      stringr::str_c(collapse = "\n")

    if (tex == T) {
      readr::write_lines(texOutput, file = "vowels.tex")
      print("\'vowels.tex\' exported to your current working directory.")
    }
    return(fig)
  }
}
