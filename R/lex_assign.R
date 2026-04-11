#' Package-private mutable lexicon state
#'
#' Stores user-editable lexica outside the package namespace so add/remove
#' operations do not need to unlock bindings at runtime.
#' @noRd

.fonology_state <- new.env(parent = emptyenv())

.user_lex_names <- c(
  "en_ipa_lex",
  "fr_ipa_lex",
  "it_ipa_lex",
  "it_lex",
  "pt_ipa_lex",
  "pt_lex_user",
  "sp_ipa_lex",
  "sp_lex"
)

.get_pkg_data <- function(name) {
  get(name, envir = as.environment("package:Fonology"))
}

.init_user_lex_state <- function() {
  if (isTRUE(.fonology_state$initialized)) {
    return(invisible(NULL))
  }

  for (name in .user_lex_names) {
    assign(name, .get_pkg_data(name), envir = .fonology_state)
  }

  .fonology_state$initialized <- TRUE
  invisible(NULL)
}

.get_user_lex <- function(name) {
  .init_user_lex_state()
  get(name, envir = .fonology_state, inherits = FALSE)
}

.set_user_lex <- function(name, value) {
  .init_user_lex_state()
  assign(name, value, envir = .fonology_state)
  invisible(value)
}

.save_user_lex <- function(name) {
  .init_user_lex_state()
  save(
    list = name,
    envir = .fonology_state,
    file = file.path(find.package("Fonology"), "data", paste0(name, ".rda")),
    compress = "xz"
  )

  invisible(get(name, envir = .fonology_state, inherits = FALSE))
}
