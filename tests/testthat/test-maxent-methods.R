# Tests for the maxent_fit class (R/maxent_methods.R) and the classed return
# value of maxent().

devoicing <- function() {
  tibble::tibble(
    input = rep(c("pad", "tab", "bid", "dog", "pok"), each = 2),
    output = c("pad", "pat", "tab", "tap", "bid", "bit", "dog", "dok", "pog", "pok"),
    ident_vce = c(0, 1, 0, 1, 0, 1, 0, 1, 1, 0),
    no_vce_final = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    obs = c(5, 15, 10, 20, 12, 18, 12, 17, 4, 8)
  )
}

small_tab <- function() devoicing()[, c("input", "output", "ident_vce", "obs")]

new_inputs <- function() {
  tibble::tibble(
    input = rep(c("bag", "tob"), each = 2),
    output = c("bag", "bak", "tob", "top"),
    ident_vce = c(0, 1, 0, 1),
    no_vce_final = c(1, 0, 1, 0)
  )
}

# ---------------------------------------------------------------------------
# The returned object
# ---------------------------------------------------------------------------

test_that("maxent() returns a maxent_fit that is still the same list", {
  fit <- maxent(devoicing())

  expect_s3_class(fit, "maxent_fit")
  expect_true(is.list(fit))
  expect_identical(
    names(fit),
    c("predictions", "weights", "log_likelihood", "log_likelihood_norm", "bic")
  )
  expect_s3_class(fit$predictions, "tbl_df")
  expect_type(fit$weights, "double")
  expect_named(fit$weights, c("ident_vce", "no_vce_final"))
  expect_length(fit$log_likelihood, 1)
  expect_length(fit$bic, 1)
})

test_that("maxent() stores n, temperature, obs_col and the optim status", {
  fit <- maxent(devoicing())
  expect_equal(attr(fit, "n"), 121)
  expect_equal(attr(fit, "temperature"), 1)
  expect_equal(attr(fit, "obs_col"), "obs")
  expect_null(attr(fit, "mu"))
  expect_null(attr(fit, "sigma"))
  expect_equal(attr(fit, "convergence"), 0L)
  expect_type(attr(fit, "optim_message"), "character")
})

test_that("attributes follow obs_col (lowercased), temperature and the prior", {
  tab <- dplyr::rename(devoicing(), Freq = obs)
  fit <- maxent(tab, obs_col = "Freq", temperature = 2, mu = 0, sigma = 1)
  expect_equal(attr(fit, "obs_col"), "freq")
  expect_equal(attr(fit, "n"), 121)
  expect_equal(attr(fit, "temperature"), 2)
  expect_equal(attr(fit, "mu"), 0)
  expect_equal(attr(fit, "sigma"), 1)
})

test_that("the class does not change any value maxent() computes", {
  fit <- maxent(devoicing())
  plain <- unclass(fit)
  expect_equal(plain$bic, 167.0346, tolerance = 1e-6)
  expect_equal(plain$log_likelihood_norm, -0.6505911, tolerance = 1e-6)
  expect_equal(plain$bic, 2 * log(121) - 2 * plain$log_likelihood)
})

test_that("$<- keeps the class; [ drops it, as for any S3 list", {
  fit <- maxent(devoicing())
  fit$bic <- 0
  expect_s3_class(fit, "maxent_fit")
  expect_false(inherits(fit["weights"], "maxent_fit"))
})

test_that("saveRDS/readRDS round-trips the class and attributes", {
  fit <- maxent(devoicing())
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(fit, path)
  back <- readRDS(path)
  expect_identical(back, fit)
  expect_equal(BIC(back), fit$bic)
})

# ---------------------------------------------------------------------------
# print()
# ---------------------------------------------------------------------------

test_that("print() returns the object invisibly", {
  fit <- maxent(devoicing())
  capture.output(out <- withVisible(print(fit)))
  expect_false(out$visible)
  expect_identical(out$value, fit)
})

test_that("print() shows a summary, weights sorted largest first", {
  fit <- maxent(devoicing())
  out <- capture.output(print(fit))

  expect_equal(out[1], "MaxEnt grammar (maxent_fit)")
  expect_equal(out[2], "2 constraints, 10 candidates, 5 inputs, n = 121")
  expect_true("Weights:" %in% out)

  w_lines <- grep("^  (ident_vce|no_vce_final)", out, value = TRUE)
  expect_length(w_lines, 2)
  expect_match(w_lines[1], "no_vce_final")
  expect_match(w_lines[2], "ident_vce")
  expect_match(w_lines[1], "0\\.639$")
  expect_match(w_lines[2], "0\\.054$")

  expect_true(any(grepl(
    "logLik = -78.722 (df = 2)   logLik/n = -0.6506   BIC = 167.035",
    out,
    fixed = TRUE
  )))
  # Not the old dump of every element
  expect_false(any(grepl("$predictions", out, fixed = TRUE)))
  expect_lt(length(out), 12)
})

test_that("print() goes to stdout, so capture.output() sees it", {
  fit <- maxent(devoicing())
  expect_gt(length(capture.output(print(fit))), 0)
  expect_silent(capture.output(print(fit)))
})

test_that("print() respects digits", {
  fit <- maxent(devoicing())
  out <- capture.output(print(fit, digits = 5))
  expect_true(any(grepl("0\\.63904$", out)))
})

test_that("print() uses the singular for one constraint and one input", {
  tab <- tibble::tibble(input = "a", output = c("x", "y"), c1 = c(0, 1), obs = c(7, 3))
  out <- capture.output(print(maxent(tab)))
  expect_equal(out[2], "1 constraint, 2 candidates, 1 input, n = 10")
})

test_that("print() mentions temperature and prior only when set", {
  default <- capture.output(print(maxent(devoicing())))
  expect_false(any(grepl("Temperature", default)))
  expect_false(any(grepl("prior", default)))

  set <- capture.output(print(maxent(devoicing(), temperature = 2, mu = 0, sigma = 1)))
  expect_true("Temperature: 2" %in% set)
  expect_true("Gaussian prior: mu = 0, sigma = 1" %in% set)
})

test_that("print() warns in its output when optim() did not converge", {
  fit <- maxent(devoicing())
  expect_false(any(grepl("converge", capture.output(print(fit)))))

  attr(fit, "convergence") <- 1L
  attr(fit, "optim_message") <- "NEW_X"
  out <- capture.output(print(fit))
  expect_true("Warning: optim() did not converge (code 1): NEW_X." %in% out)
})

test_that("print(unclass(fit)) still shows every component", {
  out <- capture.output(print(unclass(maxent(devoicing()))))
  for (el in c("$predictions", "$weights", "$log_likelihood", "$log_likelihood_norm", "$bic")) {
    expect_true(el %in% out, info = el)
  }
})

# ---------------------------------------------------------------------------
# coef(), logLik(), nobs(), AIC(), BIC()
# ---------------------------------------------------------------------------

test_that("coef() returns the named weights", {
  fit <- maxent(devoicing())
  expect_identical(coef(fit), fit$weights)
})

test_that("logLik() carries df = constraints and nobs = observations", {
  fit <- maxent(devoicing())
  ll <- logLik(fit)
  expect_s3_class(ll, "logLik")
  expect_equal(as.numeric(ll), fit$log_likelihood)
  expect_equal(attr(ll, "df"), 2L)
  expect_equal(attr(ll, "nobs"), 121)
})

test_that("nobs() is the total number of observations, not tableau rows", {
  expect_equal(nobs(maxent(devoicing())), 121)
  tab <- dplyr::rename(devoicing(), freq = obs)
  expect_equal(nobs(maxent(tab, obs_col = "freq")), 121)
})

test_that("BIC() equals the package's own $bic, in every setting", {
  fit <- maxent(devoicing())
  expect_equal(BIC(fit), fit$bic)

  fit_freq <- maxent(dplyr::rename(devoicing(), Freq = obs), obs_col = "Freq")
  expect_equal(BIC(fit_freq), fit_freq$bic)

  fit_temp <- maxent(devoicing(), temperature = 0.5)
  expect_equal(BIC(fit_temp), fit_temp$bic)

  fit_prior <- maxent(devoicing(), mu = 0, sigma = 0.5)
  expect_equal(BIC(fit_prior), fit_prior$bic)
})

test_that("AIC() is 2k - 2 logLik", {
  fit <- maxent(devoicing())
  expect_equal(AIC(fit), 2 * 2 - 2 * fit$log_likelihood)
  expect_equal(AIC(fit), 161.443, tolerance = 1e-5)
})

test_that("BIC() and AIC() compare several grammars in one table", {
  full <- maxent(devoicing())
  small <- maxent(small_tab())

  b <- BIC(full, small)
  expect_s3_class(b, "data.frame")
  expect_equal(rownames(b), c("full", "small"))
  expect_equal(b$df, c(2, 1))
  expect_equal(b$BIC, c(full$bic, small$bic))
  expect_lt(b$BIC[1], b$BIC[2]) # the full grammar wins

  a <- AIC(full, small)
  expect_equal(a$AIC, c(AIC(full), AIC(small)))
})

test_that("objects without the attributes still give nobs() and BIC()", {
  # e.g. a list rebuilt by hand, then classed
  fit <- maxent(devoicing())
  bare <- structure(unclass(fit)[names(fit)], class = "maxent_fit")
  expect_null(attr(bare, "n", exact = TRUE))
  expect_equal(nobs(bare), 121)
  expect_equal(BIC(bare), fit$bic)
  expect_true("2 constraints, 10 candidates, 5 inputs, n = 121" %in% capture.output(print(bare)))
})

# ---------------------------------------------------------------------------
# predict()
# ---------------------------------------------------------------------------

test_that("predict() without newdata returns the fitted tableau", {
  fit <- maxent(devoicing())
  expect_identical(predict(fit), fit$predictions)
})

test_that("predict() on the training tableau reproduces the fitted probabilities", {
  fit <- maxent(devoicing())
  p <- predict(fit, devoicing())
  expect_equal(p$pred_prob, fit$predictions$pred_prob)
  expect_equal(p$harmony, fit$predictions$harmony)
})

test_that("predict() uses the model's temperature", {
  fit <- maxent(devoicing(), temperature = 2)
  expect_equal(predict(fit, devoicing())$pred_prob, fit$predictions$pred_prob)
})

test_that("predict() lets the temperature be overridden", {
  fit <- maxent(devoicing())
  p1 <- predict(fit, new_inputs())$pred_prob
  p_hot <- predict(fit, new_inputs(), temperature = 100)$pred_prob
  p_cold <- predict(fit, new_inputs(), temperature = 0.01)$pred_prob
  # high temperature flattens towards 0.5, low sharpens towards 0/1
  expect_true(all(abs(p_hot - 0.5) < abs(p1 - 0.5)))
  expect_true(all(abs(p_cold - 0.5) > abs(p1 - 0.5)))
})

test_that("predict() normalises within each input", {
  fit <- maxent(devoicing())
  p <- predict(fit, new_inputs())
  sums <- as.vector(tapply(p$pred_prob, p$input, sum))
  expect_equal(sums, c(1, 1))
  expect_equal(p$pred_prob, c(0.358, 0.642, 0.358, 0.642), tolerance = 1e-3)
})

test_that("predict() matches the softmax computed by hand", {
  fit <- maxent(devoicing())
  nd <- new_inputs()
  w <- coef(fit)
  h <- as.vector(as.matrix(nd[, names(w)]) %*% w)
  by_hand <- exp(-h) / stats::ave(exp(-h), nd$input, FUN = sum)
  p <- predict(fit, nd)
  expect_equal(p$harmony, h)
  expect_equal(p$pred_prob, by_hand)
})

test_that("predict() keeps row order when inputs are interleaved", {
  fit <- maxent(devoicing())
  nd <- new_inputs()[c(1, 3, 2, 4), ]
  p <- predict(fit, nd)
  expect_equal(p$output, c("bag", "tob", "bak", "top"))
  expect_equal(p$pred_prob, predict(fit, new_inputs())$pred_prob[c(1, 3, 2, 4)])
})

test_that("predict() keeps the input's class and its other columns", {
  fit <- maxent(devoicing())

  p_tbl <- predict(fit, new_inputs())
  expect_s3_class(p_tbl, "tbl_df")
  expect_equal(names(p_tbl), c(names(new_inputs()), "harmony", "pred_prob"))

  p_df <- predict(fit, as.data.frame(new_inputs()))
  expect_identical(class(p_df), "data.frame")
  expect_equal(p_df$pred_prob, p_tbl$pred_prob)

  nd <- new_inputs()
  nd$note <- letters[1:4]
  nd$obs <- 1:4
  p <- predict(fit, nd)
  expect_equal(p$note, letters[1:4])
  expect_equal(p$obs, 1:4)
})

test_that("predict() works on grouped tibbles", {
  fit <- maxent(devoicing())
  nd <- dplyr::group_by(new_inputs(), input)
  p <- predict(fit, nd)
  expect_equal(p$pred_prob, predict(fit, new_inputs())$pred_prob)
})

test_that("predict() matches column names ignoring case, as maxent() does", {
  fit <- maxent(devoicing())
  nd <- new_inputs()
  names(nd) <- c("Input", "Output", "IDENT_VCE", "No_Vce_Final")
  p <- predict(fit, nd)
  expect_equal(p$pred_prob, predict(fit, new_inputs())$pred_prob)
  expect_true("IDENT_VCE" %in% names(p)) # the user's names are kept
})

test_that("predict() treats NA violations as 0, as maxent() does", {
  fit <- maxent(devoicing())
  nd <- new_inputs()
  nd$ident_vce[c(1, 3)] <- NA
  expect_equal(predict(fit, nd)$pred_prob, predict(fit, new_inputs())$pred_prob)
})

test_that("predict() accepts logical and integer violation columns", {
  fit <- maxent(devoicing())
  nd <- new_inputs()
  nd$ident_vce <- as.logical(nd$ident_vce)
  nd$no_vce_final <- as.integer(nd$no_vce_final)
  expect_equal(predict(fit, nd)$pred_prob, predict(fit, new_inputs())$pred_prob)
})

test_that("predict() warns about, and ignores, constraints the model lacks", {
  fit <- maxent(devoicing())
  nd <- new_inputs()
  nd$max_io <- c(1, 0, 1, 0)
  expect_warning(p <- predict(fit, nd), "1 numeric column the model has no weight for")
  expect_equal(p$pred_prob, predict(fit, new_inputs())$pred_prob)
  expect_warning(predict(fit, nd), "It is ignored")

  nd$dep_io <- 0
  expect_warning(predict(fit, nd), "2 numeric columns")
  expect_warning(predict(fit, nd), "They are ignored")
})

test_that("predict() does not warn about observation or output columns", {
  fit <- maxent(devoicing())
  # the training tableau: obs is the observation column
  expect_no_warning(predict(fit, devoicing()))
  # a fitted tableau: harmony, exp_h, Z, obs_prob, pred_prob, error
  expect_no_warning(predict(fit, fit$predictions))
  # a custom observation column, in any case
  fit_freq <- maxent(dplyr::rename(devoicing(), Freq = obs), obs_col = "Freq")
  expect_no_warning(predict(fit_freq, dplyr::rename(devoicing(), Freq = obs)))
  # non-numeric extra columns are never constraints
  nd <- new_inputs()
  nd$note <- "x"
  expect_no_warning(predict(fit, nd))
})

test_that("predict() overwrites stale harmony / pred_prob columns", {
  fit <- maxent(devoicing())
  stale <- predict(fit, new_inputs())
  stale$pred_prob <- 0
  p <- predict(fit, stale)
  expect_equal(p$pred_prob, predict(fit, new_inputs())$pred_prob)
  expect_equal(sum(names(p) == "pred_prob"), 1)
})

test_that("predict() handles a single candidate and a single input", {
  fit <- maxent(devoicing())
  p <- predict(fit, new_inputs()[1, ])
  expect_equal(p$pred_prob, 1)
})

test_that("predict() handles zero rows", {
  fit <- maxent(devoicing())
  p <- predict(fit, new_inputs()[0, ])
  expect_equal(nrow(p), 0)
  expect_true(all(c("harmony", "pred_prob") %in% names(p)))
})

test_that("predict() does not overflow with large harmony differences", {
  fit <- maxent(devoicing())
  nd <- tibble::tibble(
    input = "x", output = c("a", "b"),
    ident_vce = c(0, 0), no_vce_final = c(0, 5000)
  )
  p <- predict(fit, nd)$pred_prob
  expect_true(all(is.finite(p)))
  expect_equal(p, c(1, 0))
})

test_that("predict() errors clearly on bad input", {
  fit <- maxent(devoicing())

  expect_error(predict(fit, as.matrix(new_inputs())), "must be a data frame")
  expect_error(predict(fit, list(a = 1)), "must be a data frame")

  expect_error(
    predict(fit, new_inputs()[, c("input", "output", "ident_vce")]),
    "lacks 1 column the model needs"
  )
  expect_error(
    predict(fit, new_inputs()[, c("output", "ident_vce")]),
    "lacks 2 columns"
  )

  nd <- new_inputs()
  nd$ident_vce <- c("0", "1", "0", "1")
  expect_error(predict(fit, nd), "must be numeric")

  nd <- new_inputs()
  nd$Ident_Vce <- nd$ident_vce
  expect_error(predict(fit, nd), "more than one column")

  expect_error(predict(fit, new_inputs(), temperature = 0), "single positive number")
  expect_error(predict(fit, new_inputs(), temperature = c(1, 2)), "single positive number")
  expect_error(predict(fit, new_inputs(), temperature = "1"), "single positive number")
  expect_error(predict(fit, new_inputs(), temperature = NA_real_), "single positive number")
})

test_that("attribute lookups are exact (no partial match of n to names)", {
  fit <- maxent(devoicing())
  bare <- structure(unclass(fit)[names(fit)], class = "maxent_fit")
  # attr(bare, "n") would partially match "names"; the methods must not
  expect_type(nobs(bare), "double")
  expect_type(attr(logLik(bare), "nobs"), "double")
})

test_that("predict() errors blame predict(), not an internal", {
  fit <- maxent(devoicing())
  err <- tryCatch(predict(fit, 1), error = function(e) e)
  expect_s3_class(err, "rlang_error")
  expect_match(deparse(conditionCall(err))[1], "predict")
})

# ---------------------------------------------------------------------------
# The documented examples run
# ---------------------------------------------------------------------------

test_that("the ?maxent workflow runs end to end", {
  fit <- maxent(devoicing())
  fit_small <- maxent(small_tab())
  expect_no_error(capture.output(print(fit)))
  expect_no_error(predict(fit, new_inputs()))
  expect_no_error(BIC(fit, fit_small))
  expect_no_error(AIC(fit, fit_small))
})
