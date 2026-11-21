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

test_that("maxent BIC is k*log(n) - 2*logLik, with n = total observations", {
  tab <- devoicing_tableau()
  fit <- maxent(tab)
  k <- length(fit$weights)
  n <- sum(tab$obs) # tokens, not tableau rows

  expect_equal(n, 121)
  expect_equal(fit$bic, k * log(n) - 2 * fit$log_likelihood)
  expect_equal(fit$log_likelihood_norm, fit$log_likelihood / n)
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

# Up to 1.9.2, maxent() shifted harmonies by the per-input MAXIMUM before
# exp(), so every exponent was >= 0 and a harmony gap above ~709 overflowed to
# Inf: optim() aborted with "L-BFGS-B needs finite values of 'fn'". With obs
# 10/0 it failed from 381 violations on, because optim() probes weights above
# the starting value of 1. The shift is now the per-input minimum.
gap_tableau <- function(v, obs) {
  tibble::tibble(input = "x", output = c("a", "b"), c1 = c(0, v), obs = obs)
}

test_that("maxent fits tableaux with very large harmony gaps", {
  for (v in c(381, 700, 710, 1000, 5000)) {
    for (obs in list(c(10, 0), c(9, 1), c(5, 5))) {
      label <- paste0("v = ", v, ", obs = ", paste(obs, collapse = "/"))
      fit <- expect_no_error(maxent(gap_tableau(v, obs)), message = label)
      expect_true(all(is.finite(fit$predictions$pred_prob)), info = label)
      expect_true(is.finite(fit$log_likelihood), info = label)
      expect_equal(sum(fit$predictions$pred_prob), 1, info = label)
    }
  }
})

test_that("large harmony gaps give the expected probabilities", {
  # The violating candidate is never observed: it should get ~0.
  fit <- maxent(gap_tableau(1000, c(10, 0)))
  expect_equal(fit$predictions$pred_prob, c(1, 0), tolerance = 1e-8)

  # Both candidates equally frequent: the weight goes to 0 and both get 0.5.
  fit <- maxent(gap_tableau(1000, c(5, 5)))
  expect_equal(unname(fit$weights), 0)
  expect_equal(fit$predictions$pred_prob, c(0.5, 0.5))
})

test_that("exp_h and Z are the textbook eH = e^-H and Z = sum(eH)", {
  # Hayes & Moore-Cantwell, (16): no shift in the reported columns
  fit <- maxent(devoicing_tableau())
  p <- fit$predictions

  expect_equal(
    names(p),
    c("input", "output", "ident_vce", "no_vce_final", "obs",
      "harmony", "exp_h", "Z", "obs_prob", "pred_prob", "error")
  )
  expect_equal(p$exp_h, exp(-p$harmony))
  expect_equal(p$Z, stats::ave(p$exp_h, p$input, FUN = sum))
  expect_equal(p$pred_prob, p$exp_h / p$Z)

  # /pad/: H = 0.639 / 0.054, eH = 0.528 / 0.947, Z = 1.475, p = 0.358 / 0.642
  expect_equal(p$exp_h[1:2], c(0.528, 0.947), tolerance = 1e-3)
  expect_equal(p$Z[1:2], c(1.475, 1.475), tolerance = 1e-3)
})

test_that("with a temperature, exp_h is e^(-H/T)", {
  fit <- maxent(devoicing_tableau(), temperature = 2)
  p <- fit$predictions
  expect_equal(p$exp_h, exp(-p$harmony / 2))
  expect_equal(p$pred_prob, p$exp_h / p$Z)
})

test_that("pred_prob stays correct when eH and Z underflow to 0", {
  # Both candidates have H = 1000: e^-1000 is 0 in double precision, so the
  # reported eH and Z are 0 (as in a spreadsheet), but p is still 0.5 each.
  tab <- tibble::tibble(input = "x", output = c("a", "b"), c1 = c(1000, 1000), obs = c(5, 5))
  fit <- maxent(tab)
  p <- fit$predictions
  expect_equal(p$exp_h, c(0, 0))
  expect_equal(p$Z, c(0, 0))
  expect_equal(p$pred_prob, c(0.5, 0.5))
  expect_true(is.finite(fit$log_likelihood))
})

test_that("the overflow fix leaves the fit unchanged (pinned 1.9.2 values)", {
  fit <- maxent(devoicing_tableau())
  expect_equal(unname(fit$weights), c(0.05410682, 0.63904035), tolerance = 1e-6)
  expect_equal(fit$log_likelihood, -78.72152, tolerance = 1e-6)
  expect_equal(fit$bic, 167.0346, tolerance = 1e-6)
})
