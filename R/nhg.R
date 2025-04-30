#' Noisy Harmonic Grammar simulation
#'
#' Given an object with one or more tableaux and an object with the weights for all the constraints in question, the function returns the probabilities for each candidate by adding random noise from a Gaussian distribution to each constraint weight.
#' @param tableau The data object (data frame or tibble) containing an \code{input} column
#' and an \code{output} column, in addition to all relevant constraints, their respective
#' violations
#' @param weights A vector with the weights for the constraints in \code{tableau}
#' @param input_col The name of the input column in \code{tableau} (default: \code{input})
#' @param output_col The name of the output column in \code{tableau} (default: \code{output})
#' @param n_sim The number of simulations to generate the probabilities for all candidates
#' @param noise_sd The standard deviation of the Gaussian distribution from which noise will be added to constraint weights
#' @param seed For reproducibility
#' @examples
#' tableaux <- tibble::tibble(
#'   input = rep(c("/input_A/", "/input_B/"), each = 3),
#'   output = rep(c("[output_1]", "[output_2]", "[output_3]"), times = 2),
#'   C1 = c(-1, 0, 0, -2, 0, -1),
#'   C2 = c(0, -2, -1, 0, -1, -1),
#'   C3 = c(0, 0, -1, 0, 0, -1)
#' )
#'
#' weights <- c(15, 8, 8)
#'
#' nhg(tableau = tableaux, weights, n_sim = 100, noise_sd = 1)
#' @importFrom rlang `:=`
#' @importFrom glue glue
#' @export

nhg <- function(tableau,
                weights,
                input_col = "input",
                output_col = "output",
                n_sim = 100,
                noise_sd = 1,
                seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Identify constraint columns
  constraint_cols <- setdiff(names(tableau), c(input_col, output_col))

  # Check that weights and constraints match length-wise:
  if (length(weights) != length(constraint_cols)) {
    stop(glue::glue("Length of weights ({length(weights)}) does not match number of constraints columns ({length(constraint_cols)}): {paste(constraint_cols, collapse = ', ')}"))
  }

  # Ensure numeric constraint violations
  tableau <- tableau |>
    dplyr::mutate(dplyr::across(dplyr::all_of(constraint_cols), as.numeric))

  # Get all possible candidates
  all_candidates <- tableau |>
    dplyr::distinct(!!dplyr::sym(output_col)) |>
    dplyr::pull()
  # Simulate winner selection for each input
  sim_results <- vector("list", n_sim)

  for (i in seq_len(n_sim)) {
    # Add Gaussian noise to weights
    noisy_weights <- weights + rnorm(length(weights), mean = 0, sd = noise_sd)

    tableau_copy <- tableau |>
      dplyr::mutate(harmony = as.vector(as.matrix(dplyr::select(tableau, dplyr::all_of(constraint_cols))) %*% noisy_weights))

    # For each input, select candidate with best (highest) harmony
    winners <- tableau_copy |>
      dplyr::group_by(dplyr::across(dplyr::all_of(input_col))) |>
      dplyr::filter(harmony == max(harmony)) |>
      dplyr::ungroup() |>
      dplyr::pull(dplyr::all_of(output_col))

    sim_results[[i]] <- winners
  }


  # Combine results and calculate probs
  winner_df <- tibble::tibble(winner = unlist(sim_results)) |>
    dplyr::count(winner, sort = FALSE) |>
    dplyr::right_join(tibble::tibble(winner = all_candidates), by = "winner") |>
    dplyr::mutate(
      n = tidyr::replace_na(n, 0), prob = n / sum(n),
      sd = noise_sd,
      n_sim = n_sim
    ) |>
    dplyr::mutate(winner = factor(winner, levels = all_candidates)) |>
    dplyr::arrange(winner) |>
    dplyr::rename(!!output_col := winner)

  # Join probs back to tableau
  output <- dplyr::right_join(tableau, winner_df, by = output_col) |>
    dplyr::mutate(cand = letters[dplyr::row_number()], .by = input) |>
    dplyr::select(input, cand, output:n_sim)


  return(output)
}


#' Plotting probabilities as a function of sigma in harmonic grammar simulations
#'
#' Given an object with one or more tableaux and an object with the weights for all the constraints in question, the function creates a figure using \code{ggplot2} showing how candidate probabilities change as a function of the standard deviation of the Gaussian distribution from which the noise is derived. Sigmas 1:5 are used to illustrate the effect of different standard deviation values.
#' @param tableau The data object (data frame or tibble) containing an \code{input} column
#' and an \code{output} column, in addition to all relevant constraints, their respective
#' violations
#' @param weights A vector with the weights for the constraints in \code{tableau}
#' @examples
#' tableaux <- tibble::tibble(
#'   input = rep(c("/input_A/", "/input_B/"), each = 3),
#'   output = rep(c("[output_1]", "[output_2]", "[output_3]"), times = 2),
#'   C1 = c(-1, 0, 0, -2, 0, -1),
#'   C2 = c(0, -2, -1, 0, -1, -1),
#'   C3 = c(0, 0, -1, 0, 0, -1)
#' )
#'
#' weights <- c(15, 8, 8)
#'
#' plotNhg(tableau = tableaux, weights)
#' @export



plotNhg <- function(tableau, weights) {
  noise_values <- 1:5

  simulations <- purrr::map_dfr(
    noise_values,
    ~ nhg(
      tableau = tableau,
      weights = weights,
      noise_sd = .x,
      n_sim = 100,
      seed = 123
    )
  )

  # NOTE: Define range to control spacing on x-axis
  x_range <- max(simulations$sd) - min(simulations$sd)

  # NOTE: Figure:
  figure <- ggplot2::ggplot(
    data = simulations,
    ggplot2::aes(x = sd |> forcats::as_factor(), y = prob)
  ) +
    ggplot2::geom_line(ggplot2::aes(group = output)) +
    ggplot2::geom_label(ggplot2::aes(label = cand)) +
    ggplot2::geom_label(
      data = simulations |> dplyr::filter(sd == 5),
      fill = "gray90",
      ggplot2::aes(label = prob |> round(2)),
      position = ggplot2::position_nudge(x = x_range * 0.1)
    ) +
    ggplot2::theme_classic() +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(),
    ) +
    ggplot2::coord_cartesian(xlim = c(
      min(simulations$sd),
      max(simulations$sd) * 1.05
    )) +
    ggplot2::labs(
      x = "Standard deviation of noise in simulations",
      y = "Probability of candidate"
    )

  return(figure)
}
