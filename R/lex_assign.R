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
  "it_lex_user",
  "pt_ipa_lex",
  "pt_lex_user",
  "sp_ipa_lex",
  "sp_lex_user"
)

.get_pkg_data <- function(name) {
  get(name, envir = as.environment("package:Fonology"))
}

# Installed packages lazy-load their data from Rdata.rdb, so writing .rda
# files into the installation's data/ directory has no effect (and may not
# even be permitted). User edits are therefore persisted under
# tools::R_user_dir(). A source checkout loaded with devtools::load_all()
# (detected by the absence of the Meta/ directory that installed packages
# have) keeps the original behavior of writing to the package's own data/
# directory, so that entries can be committed.
.is_source_checkout <- function() {
  !dir.exists(file.path(find.package("Fonology"), "Meta"))
}

.user_lex_path <- function(name) {
  if (.is_source_checkout()) {
    file.path(find.package("Fonology"), "data", paste0(name, ".rda"))
  } else {
    file.path(
      tools::R_user_dir("Fonology", which = "data"),
      paste0(name, ".rda")
    )
  }
}

.load_user_lex_file <- function(name) {
  path <- .user_lex_path(name)

  if (!file.exists(path)) {
    return(NULL)
  }

  e <- new.env(parent = emptyenv())
  load(path, envir = e)

  if (!exists(name, envir = e, inherits = FALSE)) {
    return(NULL)
  }

  get(name, envir = e, inherits = FALSE)
}

.init_user_lex_state <- function() {
  if (isTRUE(.fonology_state$initialized)) {
    return(invisible(NULL))
  }

  for (name in .user_lex_names) {
    value <- .load_user_lex_file(name)
    if (is.null(value)) value <- .get_pkg_data(name)
    assign(name, value, envir = .fonology_state)
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

  path <- .user_lex_path(name)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  save(
    list = name,
    envir = .fonology_state,
    file = path,
    compress = "xz"
  )

  invisible(get(name, envir = .fonology_state, inherits = FALSE))
}
