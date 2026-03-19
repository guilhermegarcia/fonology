#' Safely assign a value to a package-level binding
#'
#' Handles locked bindings (installed packages) and unlocked bindings
#' (\code{devtools::load_all()}) transparently.
#' @param name Character. The variable name to update.
#' @param value The new value.
#' @noRd

.lex_assign <- function(name, value) {
  pkg <- as.environment("package:Fonology")
  locked <- bindingIsLocked(name, pkg)
  if (locked) unlockBinding(name, pkg)
  assign(name, value, envir = pkg)
  if (locked) lockBinding(name, pkg)
  invisible(value)
}
