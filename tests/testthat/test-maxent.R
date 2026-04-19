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
