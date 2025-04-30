#' MaxEnt Grammar
#'
#' Given an object with one or more tableaux, the function returns the
#' weights for the set of constraints.
#' @param tableau The data object (data frame or tibble) containing an \code{input} column
#' and an \code{output} column, in addition to all relevant constraints, their respective
#' violations, and a column \code{obs} containing the number of observations for each output
#' @param obs_col Name of the column containing the number of observations for each output (default: \code{obs})
#' @param mu The desired mean for the Gaussian prior
#' @param sigma The desired standard deviation for the Gaussian prior
#' @param upper_bound The maximum value allowed for constraint weights (default: 10)
#' @param temperature Controls "sharpness" of candidate probabilities. Default behavior is \code{temperature = 1}. Lower values make distributions sharper (favor one candidate strongly);
#' higher values make distributions flatter (probabilities more even)
#' @param use_gradient Whether the function should use a gradient function (default: TRUE) to
#' optimize its search for constraint weights
#' @return A list with the following components:
#' \itemize{
#'   \item \code{predictions} — a tibble containing the original tableau(s) with added columns: \code{harmony} (score for each candidate), \code{max_h} (maximum harmony within each input), \code{exp_h}, \code{Z} (partition function within each input), \code{obs_prob} (observed probabilities), \code{pred_prob} (predicted probabilities), and \code{error} (difference between observed and predicted probabilities).
#'   \item \code{weights} — a named numeric vector of the learned constraint weights.
#'   \item \code{log_likelihood} — the log-likelihood of the model fit.
#'   \item \code{log_likelihood_norm} — the normalized log-likelihood (log-likelihood divided by number of rows).
#'   \item \code{bic} — the Bayesian Information Criterion (BIC) value for the fitted model.
#' }
#' @examples
#' maxent_data <- tibble::tibble(
#'   input = rep(c("pad", "tab", "bid", "dog", "pok"), each = 2),
#'   output = c("pad", "pat", "tab", "tap", "bid", "bit", "dog", "dok", "pog", "pok"),
#'   ident_vce = c(0, 1, 0, 1, 0, 1, 0, 1, 1, 0),
#'   no_vce_final = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
#'   obs = c(5, 15, 10, 20, 12, 18, 12, 17, 4, 8)
#' )
#' maxent(tableau = maxent_data)
#' @importFrom stats optim rnorm
#' @export

maxent <- function(tableau,
                   obs_col = "obs",
                   mu = NULL,
                   sigma = NULL,
                   upper_bound = 10,
                   temperature = 1,
                   use_gradient = TRUE) {
  # Read data
  df <- tableau
  df[is.na(df)] <- 0
  names(df) <- stringr::str_to_lower(names(df))

  constraint_cols <- setdiff(names(df), c("input", "output", obs_col))
  eps <- 1e-10

  df <- df |> dplyr::mutate(dplyr::across(dplyr::all_of(constraint_cols), as.numeric))

  # Negative log-likelihood
  nll <- function(weights, data) {
    violations <- as.matrix(data[, constraint_cols])
    harmony <- violations %*% weights

    data <- data |>
      dplyr::mutate(harmony = as.vector(harmony)) |>
      dplyr::group_by(input) |>
      dplyr::mutate(
        max_h = max(harmony),
        exp_h = exp(-(harmony - max_h) / temperature),
        Z = sum(exp_h),
        pred_prob = exp_h / Z
      ) |>
      dplyr::ungroup()

    neg_log_lik <- -sum(log(data$pred_prob + eps) * dplyr::pull(data, dplyr::all_of(obs_col)))

    if (!is.null(mu) && !is.null(sigma)) {
      prior_penalty <- sum((weights - mu)^2) / (2 * sigma^2)
      neg_log_lik <- neg_log_lik + prior_penalty
    }

    return(neg_log_lik)
  }

  # Gradient function
  gradient_function <- function(weights, ...) {
    data <- list(...)$data
    violations <- as.matrix(data[, constraint_cols])
    harmony <- violations %*% weights

    data$harmony <- as.vector(harmony)
    max_h <- stats::ave(data$harmony, data$input, FUN = max)
    data$exp_h <- exp(-(data$harmony - max_h) / temperature)
    data$Z <- stats::ave(data$exp_h, data$input, FUN = sum)
    data$pred_prob <- data$exp_h / data$Z

    obs_viols <- colSums(violations * dplyr::pull(data, dplyr::all_of(obs_col)))
    input_totals <- stats::ave(dplyr::pull(data, dplyr::all_of(obs_col)), data$input, FUN = sum)
    exp_viols <- colSums(violations * (data$pred_prob * input_totals))

    grad <- obs_viols - exp_viols

    if (!is.null(mu) && !is.null(sigma)) {
      grad <- grad - (weights - mu) / sigma^2
    }

    grad[weights == 0 & grad < 0] <- 0
    return(grad)
  }

  init_weights <- rep(1, length(constraint_cols))

  # Run optimization
  fit <- if (use_gradient) {
    optim(
      par = init_weights,
      fn = nll,
      gr = gradient_function,
      method = "L-BFGS-B",
      lower = rep(0, length(constraint_cols)),
      upper = rep(upper_bound, length(constraint_cols)),
      data = df
    )
  } else {
    optim(
      par = init_weights,
      fn = nll,
      method = "L-BFGS-B",
      lower = rep(0, length(constraint_cols)),
      data = df
    )
  }

  final_weights <- fit$par
  names(final_weights) <- constraint_cols

  # Compute predictions
  violations <- as.matrix(df[, constraint_cols])
  harmony <- violations %*% final_weights

  df <- df |>
    dplyr::mutate(harmony = as.vector(harmony)) |>
    dplyr::group_by(input) |>
    dplyr::mutate(
      max_h = max(harmony),
      exp_h = exp(-(harmony - max_h) / temperature),
      Z = sum(exp_h),
      obs_prob = obs / sum(obs),
      pred_prob = exp_h / Z,
      error = obs_prob - pred_prob
    ) |>
    dplyr::ungroup()

  log_likelihood <- sum(log(df$pred_prob + eps) * dplyr::pull(df, dplyr::all_of(obs_col)))
  log_likelihood_norm <- log_likelihood / nrow(df)
  k <- length(constraint_cols)
  n <- nrow(df)
  bic <- k * log(n) - 2 * (-log_likelihood)

  return(list(
    predictions = df,
    weights = final_weights,
    log_likelihood = log_likelihood,
    log_likelihood_norm = log_likelihood_norm,
    bic = bic
  ))
}
