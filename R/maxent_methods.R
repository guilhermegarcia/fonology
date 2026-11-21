#' Methods for fitted MaxEnt grammars
#'
#' \code{\link{maxent}} returns an object of class \code{maxent_fit}. It is a
#' list, so its components are read with \code{$} as before
#' (\code{fit$weights}, \code{fit$bic}, ...). The class adds the methods below.
#'
#' \itemize{
#'   \item \code{print()} shows a short summary: the weights (largest first),
#'   the log-likelihood and the BIC. Use \code{print(unclass(fit))} to see
#'   every component in full.
#'   \item \code{coef()} returns the constraint weights.
#'   \item \code{predict()} returns the predicted probability of each candidate
#'   in \code{newdata}, normalised within each input. Without \code{newdata}
#'   it returns the fitted tableau, \code{fit$predictions}.
#'   \item \code{logLik()} returns the log-likelihood, with \code{df} = number
#'   of constraints and \code{nobs} = total number of observations. This is
#'   what makes \code{\link[stats]{AIC}} and \code{\link[stats]{BIC}} work,
#'   including \code{BIC(fit1, fit2)}, which returns a comparison table.
#'   \code{BIC(fit)} equals \code{fit$bic}.
#'   \item \code{nobs()} returns the total number of observations, the sum of
#'   the observation column.
#' }
#'
#' @param x,object An object of class \code{maxent_fit}, returned by
#'   \code{\link{maxent}}.
#' @param digits Number of decimal places for the weights in \code{print()}.
#' @param newdata A data frame or tibble with an \code{input} column and one
#'   column per constraint in the model. Column names are matched ignoring
#'   case, as in \code{maxent()}. Missing violations (\code{NA}) count as 0, as
#'   in \code{maxent()}. Other columns (\code{output}, \code{obs}, ...) are kept
#'   and returned unchanged. A numeric column the model has no weight for
#'   (other than the observation column) gives a warning, since it looks like
#'   a constraint the model cannot use. If \code{NULL} (the default), the
#'   fitted tableau is returned.
#' @param temperature Temperature for the predicted probabilities. If
#'   \code{NULL} (the default), the temperature the model was fitted with.
#' @param ... Not used.
#'
#' @return
#' \itemize{
#'   \item \code{print()}: \code{x}, invisibly.
#'   \item \code{coef()}: a named numeric vector.
#'   \item \code{predict()}: \code{newdata} with two added columns,
#'   \code{harmony} and \code{pred_prob}.
#'   \item \code{logLik()}: an object of class \code{logLik}.
#'   \item \code{nobs()}: a number.
#' }
#'
#' @examples
#' maxent_data <- tibble::tibble(
#'   input = rep(c("pad", "tab", "bid", "dog", "pok"), each = 2),
#'   output = c("pad", "pat", "tab", "tap", "bid", "bit", "dog", "dok", "pog", "pok"),
#'   ident_vce = c(0, 1, 0, 1, 0, 1, 0, 1, 1, 0),
#'   no_vce_final = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
#'   obs = c(5, 15, 10, 20, 12, 18, 12, 17, 4, 8)
#' )
#' fit <- maxent(maxent_data)
#' fit
#' coef(fit)
#' logLik(fit)
#' nobs(fit)
#' AIC(fit)
#'
#' new_data <- tibble::tibble(
#'   input = rep(c("bag", "tob"), each = 2),
#'   output = c("bag", "bak", "tob", "top"),
#'   ident_vce = c(0, 1, 0, 1),
#'   no_vce_final = c(1, 0, 1, 0)
#' )
#' predict(fit, new_data)
#'
#' fit_small <- maxent_data |>
#'   dplyr::select(-no_vce_final) |>
#'   maxent()
#' BIC(fit, fit_small)
#'
#' @name maxent_fit
NULL

#' @rdname maxent_fit
#' @export
print.maxent_fit <- function(x, digits = 3, ...) {
  w <- x$weights
  p <- x$predictions
  k <- length(w)
  n_cand <- nrow(p)
  n_input <- length(unique(p$input))

  cli::cat_line("MaxEnt grammar (maxent_fit)")
  cli::cat_line(sprintf(
    "%d constraint%s, %d candidate%s, %d input%s, n = %s",
    k, if (k == 1) "" else "s",
    n_cand, if (n_cand == 1) "" else "s",
    n_input, if (n_input == 1) "" else "s",
    format(.maxent_nobs(x))
  ))

  # Optional settings, shown only when they differ from the defaults.
  temperature <- attr(x, "temperature", exact = TRUE)
  if (!is.null(temperature) && !isTRUE(all.equal(temperature, 1))) {
    cli::cat_line("Temperature: ", format(temperature))
  }
  mu <- attr(x, "mu", exact = TRUE)
  sigma <- attr(x, "sigma", exact = TRUE)
  if (!is.null(mu) && !is.null(sigma)) {
    cli::cat_line(
      "Gaussian prior: mu = ", paste(format(mu), collapse = ", "),
      ", sigma = ", paste(format(sigma), collapse = ", ")
    )
  }

  cli::cat_line()
  cli::cat_line("Weights:")
  if (k == 0) {
    cli::cat_line("  (none)")
  } else {
    w_sorted <- sort(w, decreasing = TRUE)
    labels <- format(names(w_sorted))
    values <- formatC(w_sorted, format = "f", digits = digits)
    cli::cat_line(paste0("  ", labels, "  ", format(values, justify = "right")))
  }

  cli::cat_line()
  cli::cat_line(sprintf(
    "logLik = %.3f (df = %d)   logLik/n = %.4f   BIC = %.3f",
    x$log_likelihood, k, x$log_likelihood_norm, x$bic
  ))

  convergence <- attr(x, "convergence", exact = TRUE)
  if (!is.null(convergence) && convergence != 0) {
    msg <- attr(x, "optim_message", exact = TRUE)
    cli::cat_line(
      "Warning: optim() did not converge (code ", convergence, ")",
      if (!is.null(msg) && nzchar(msg)) paste0(": ", msg) else "", "."
    )
  }

  invisible(x)
}

#' @rdname maxent_fit
#' @importFrom stats coef
#' @export
coef.maxent_fit <- function(object, ...) {
  object$weights
}

#' @rdname maxent_fit
#' @importFrom stats logLik
#' @export
logLik.maxent_fit <- function(object, ...) {
  structure(
    object$log_likelihood,
    df = length(object$weights),
    nobs = .maxent_nobs(object),
    class = "logLik"
  )
}

#' @rdname maxent_fit
#' @importFrom stats nobs
#' @export
nobs.maxent_fit <- function(object, ...) {
  .maxent_nobs(object)
}

#' @rdname maxent_fit
#' @importFrom stats predict
#' @export
predict.maxent_fit <- function(object, newdata = NULL, temperature = NULL, ...) {
  if (is.null(newdata)) {
    return(object$predictions)
  }
  if (!is.data.frame(newdata)) {
    cli::cli_abort(c(
      "{.arg newdata} must be a data frame or tibble.",
      "x" = "It is {.cls {class(newdata)}}."
    ))
  }
  if (is.null(temperature)) temperature <- attr(object, "temperature", exact = TRUE)
  if (is.null(temperature)) temperature <- 1
  if (!is.numeric(temperature) || length(temperature) != 1 ||
      is.na(temperature) || temperature <= 0) {
    cli::cli_abort("{.arg temperature} must be a single positive number.")
  }

  w <- object$weights
  needed <- c("input", names(w))
  lower_names <- tolower(names(newdata))

  missing_cols <- setdiff(needed, lower_names)
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "{.arg newdata} lacks {length(missing_cols)} column{?s} the model needs.",
      "x" = "Missing: {.val {missing_cols}}.",
      "i" = "It needs {.val input} and one column per constraint."
    ))
  }
  dup <- needed[vapply(needed, function(col) sum(lower_names == col) > 1, logical(1))]
  if (length(dup) > 0) {
    cli::cli_abort(c(
      "{.arg newdata} has more than one column matching {.val {dup}}.",
      "i" = "Column names are matched ignoring case."
    ))
  }

  # Numeric columns the model has no weight for look like constraints added
  # to the new tableau. They cannot be used, so say so instead of silently
  # dropping them. The observation column and the columns maxent() and
  # predict() themselves add are not constraints and are skipped.
  not_constraints <- c(
    "input", "output", attr(object, "obs_col", exact = TRUE), "obs",
    "harmony", "max_h", "min_h", "exp_h", "z", "obs_prob", "pred_prob", "error"
  )
  numeric_cols <- names(newdata)[vapply(newdata, is.numeric, logical(1))]
  unknown <- numeric_cols[!tolower(numeric_cols) %in% c(names(w), not_constraints)]
  if (length(unknown) > 0) {
    hint <- if (length(unknown) == 1) {
      "It is ignored. To use it, refit {.fn maxent} with it as a constraint."
    } else {
      "They are ignored. To use them, refit {.fn maxent} with them as constraints."
    }
    cli::cli_warn(c(
      "{.arg newdata} has {length(unknown)} numeric column{?s} the model has no weight for: {.val {unknown}}.",
      "i" = hint
    ))
  }

  cols <- match(names(w), lower_names)
  violations <- as.data.frame(newdata)[, cols, drop = FALSE]
  non_numeric <- names(w)[!vapply(violations, function(v) is.numeric(v) || is.logical(v), logical(1))]
  if (length(non_numeric) > 0) {
    cli::cli_abort(c(
      "Constraint column{?s} {.val {non_numeric}} in {.arg newdata} must be numeric.",
      "x" = "Violations are counts."
    ))
  }
  violations <- as.matrix(violations) * 1 # logical to numeric
  violations[is.na(violations)] <- 0 # as in maxent()

  input <- newdata[[match("input", lower_names)]]
  harmony <- as.vector(violations %*% w)

  if (length(harmony) == 0) {
    pred_prob <- numeric(0)
  } else {
    # Same computation as maxent(): shifted by the smallest harmony per
    # input, so exp() cannot overflow (see .softmax_shifted() in maxent.R).
    pred_prob <- stats::ave(harmony, input, FUN = function(h) .softmax_shifted(h, temperature))
  }

  newdata$harmony <- harmony
  newdata$pred_prob <- pred_prob
  newdata
}

# Total observations. Fits made before the class existed, or objects built by
# hand, may lack the attribute; fall back to the observation column.
.maxent_nobs <- function(object) {
  n <- attr(object, "n", exact = TRUE)
  if (!is.null(n)) {
    return(n)
  }
  obs_col <- attr(object, "obs_col", exact = TRUE)
  if (is.null(obs_col)) obs_col <- "obs"
  sum(object$predictions[[obs_col]])
}
