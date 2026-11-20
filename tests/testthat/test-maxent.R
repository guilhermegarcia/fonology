test_that("maxent gradient path matches direct optimization", {
  tableau <- tibble::tribble(
    ~input, ~output, ~onset, ~no_coda, ~no_complex_onset, ~no_complex_coda, ~obs,
    "input", "CV", 0, 0, 0, 0, 8209,
    "input", "CVC", 0, 1, 0, 0, 5034,
    "input", "V", 1, 0, 0, 0, 2122,
    "input", "VC", 1, 1, 0, 0, 1617,
    "input", "CCV", 0, 0, 1, 0, 1369,
    "input", "CVCC", 0, 1, 0, 1, 82,
    "input", "VCC", 1, 1, 0, 1, 28,
    "input", "CCVCC", 0, 1, 1, 1, 37
  )

  fit_grad <- maxent(tableau)
  fit_nograd <- maxent(tableau, use_gradient = FALSE)

  expect_equal(fit_grad$weights, fit_nograd$weights, tolerance = 1e-5)
  expect_gt(unname(fit_grad$weights["no_coda"]), 0)
})

devoicing_tableau <- function() {
  tibble::tibble(
    input = rep(c("pad", "tab", "bid", "dog", "pok"), each = 2),
    output = c("pad", "pat", "tab", "tap", "bid", "bit", "dog", "dok", "pog", "pok"),
    ident_vce = c(0, 1, 0, 1, 0, 1, 0, 1, 1, 0),
    no_vce_final = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    obs = c(5, 15, 10, 20, 12, 18, 12, 17, 4, 8)
  )
}

test_that("maxent BIC is k*log(n) - 2*logLik", {
  fit <- maxent(devoicing_tableau())
  k <- length(fit$weights)
  n <- nrow(fit$predictions)

  expect_equal(fit$bic, k * log(n) - 2 * fit$log_likelihood)
  # logLik < 0, so a correct BIC is positive here (it was negative before 1.9.2)
  expect_gt(fit$bic, 0)
})

test_that("maxent BIC prefers the better-fitting grammar", {
  tab <- devoicing_tableau()
  full <- maxent(tab)
  small <- maxent(tab[, c("input", "output", "ident_vce", "obs")])

  # The full grammar fits significantly better (LR test p ~ .001), so it
  # must have the lower BIC. The pre-1.9.2 formula picked the smaller one.
  lr <- 2 * (full$log_likelihood - small$log_likelihood)
  expect_lt(stats::pchisq(lr, df = 1, lower.tail = FALSE), 0.01)
  expect_lt(full$bic, small$bic)
})

test_that("maxent honours obs_col throughout, in any case", {
  tab <- devoicing_tableau()
  ref <- maxent(tab)

  tab_freq <- dplyr::rename(tab, freq = obs)
  fit_freq <- maxent(tab_freq, obs_col = "freq")
  expect_equal(fit_freq$weights, ref$weights)
  expect_equal(fit_freq$predictions$obs_prob, ref$predictions$obs_prob)
  expect_equal(fit_freq$bic, ref$bic)

  # Column names are lowercased internally; obs_col must follow suit,
  # otherwise "Freq" would be fitted as a constraint.
  tab_upper <- dplyr::rename(tab, Freq = obs)
  fit_upper <- maxent(tab_upper, obs_col = "Freq")
  expect_equal(fit_upper$weights, ref$weights)
})

test_that("maxent errors clearly when obs_col is missing", {
  expect_error(maxent(devoicing_tableau(), obs_col = "count"), "no observation column")
})
