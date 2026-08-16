#' Package-private lexicon state (two layers)
#'
#' Lexical corrections live in two separate layers:
#'
#' \itemize{
#'   \item the \strong{shipped} layer (\code{*_fix}), curated by the package
#'     maintainer and distributed with the package. Never written at runtime.
#'   \item the \strong{local} layer (\code{*_lex_user}, \code{*_ipa_lex}),
#'     owned by the user, empty in a freshly installed package, and persisted
#'     under \code{tools::R_user_dir()}.
#' }
#'
#' \code{.lex()} returns the two merged, with the local layer winning. This is
#' what the transcription pipelines consult. \code{.get_user_lex()} returns the
#' local layer alone and is what \code{add_lex_*()} / \code{remove_lex_*()}
#' operate on, so resetting the local layer can never remove a shipped fix.
#'
#' A local entry whose value is \code{NA} is a tombstone: it suppresses a
#' shipped entry of the same name. That is how \code{remove_lex_*()} can undo
#' a correction the package ships.
#' @noRd

.fonology_state <- new.env(parent = emptyenv())

# Local, user-writable layers.
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

# Shipped counterpart of each local layer.
.fix_lex_names <- c(
  en_ipa_lex  = "en_ipa_fix",
  fr_ipa_lex  = "fr_ipa_fix",
  it_ipa_lex  = "it_ipa_fix",
  it_lex_user = "it_lex_fix",
  pt_ipa_lex  = "pt_ipa_fix",
  pt_lex_user = "pt_lex_fix",
  sp_ipa_lex  = "sp_ipa_fix",
  sp_lex_user = "sp_lex_fix"
)

# Local layers belonging to each language, used by promote_lex().
.lex_names_by_lg <- list(
  pt = c("pt_lex_user", "pt_ipa_lex"),
  sp = c("sp_lex_user", "sp_ipa_lex"),
  it = c("it_lex_user", "it_ipa_lex"),
  fr = "fr_ipa_lex",
  en = "en_ipa_lex"
)

# Datasets are lazy-loaded, so they live in the namespace's lazydata
# environment. Reading them from there (rather than from the attached
# "package:Fonology" environment) means the pipelines also work when the
# package is used with :: and never attached.
.get_pkg_data <- function(name) {
  ns <- asNamespace("Fonology")

  ld <- tryCatch(.getNamespaceInfo(ns, "lazydata"), error = function(e) NULL)
  if (!is.null(ld) && exists(name, envir = ld, inherits = FALSE)) {
    return(get(name, envir = ld, inherits = FALSE))
  }

  if (exists(name, envir = ns, inherits = FALSE)) {
    return(get(name, envir = ns, inherits = FALSE))
  }

  get(name, envir = as.environment("package:Fonology"))
}

# Only promote_lex() cares, since it writes the shipped layer back into data/.
#
# An installed DESCRIPTION carries a "Built:" field; a source one never does.
# The previous test (absence of a Meta/ directory) was unreliable: building the
# vignettes creates Meta/vignette.rds inside the source tree, after which a
# checkout looked installed.
.is_source_checkout <- function() {
  desc <- system.file("DESCRIPTION", package = "Fonology")

  if (!nzchar(desc)) return(FALSE)

  built <- tryCatch(
    read.dcf(desc, fields = "Built")[1, 1],
    error = function(e) NA_character_
  )

  is.na(built)
}

# Root of the package as loaded: the source tree under devtools::load_all(),
# the installation otherwise.
.pkg_dir <- function() {
  desc <- system.file("DESCRIPTION", package = "Fonology")

  if (nzchar(desc)) dirname(desc) else find.package("Fonology")
}

# The local layer always persists to the user's data directory, including in a
# source checkout: dev-time add_lex_*() calls must not silently modify tracked
# data files. promote_lex() is the deliberate path from local to shipped.
.user_lex_path <- function(name) {
  file.path(
    tools::R_user_dir("Fonology", which = "data"),
    paste0(name, ".rda")
  )
}

.fix_lex_path <- function(name) {
  file.path(.pkg_dir(), "data", paste0(name, ".rda"))
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

    fix_name <- .fix_lex_names[[name]]
    assign(fix_name, .get_pkg_data(fix_name), envir = .fonology_state)
  }

  .fonology_state$initialized <- TRUE
  invisible(NULL)
}

# The local layer alone: what add_lex_*() and remove_lex_*() edit.
.get_user_lex <- function(name) {
  .init_user_lex_state()
  get(name, envir = .fonology_state, inherits = FALSE)
}

# The shipped layer alone.
.get_fix_lex <- function(name) {
  .init_user_lex_state()
  get(.fix_lex_names[[name]], envir = .fonology_state, inherits = FALSE)
}

# Shipped and local merged, local winning; NA entries are tombstones and drop
# out. This is the effective lexicon the pipelines see.
.lex <- function(name) {
  fix <- .get_fix_lex(name)
  usr <- .get_user_lex(name)

  out <- c(fix[!names(fix) %in% names(usr)], usr)
  out[!is.na(out)]
}

# Removes keys from a local layer.
#
# Removing a word the user had overridden locally means "undo my override", so
# the shipped correction is allowed to resurface. Removing a word the user
# never touched means "I do not want this correction at all", which cannot be
# expressed by deletion (the shipped entry would come straight back), so an NA
# tombstone is stored instead; .lex() filters those out. Calling
# remove_lex_*() twice therefore first reverts to the shipped form, then
# suppresses it.
.drop_lex <- function(lex, keys, name) {
  overridden <- intersect(keys, names(lex))
  lex <- lex[!names(lex) %in% keys]

  shipped <- setdiff(intersect(keys, names(.get_fix_lex(name))), overridden)

  if (length(shipped) > 0) {
    tombstones <- rep(NA_character_, length(shipped))
    names(tombstones) <- shipped
    lex <- c(lex, tombstones)
  }

  lex
}

# Sanity check for the IPA-override mode of add_lex_*().
#
# That mode returns its value verbatim, so a mistyped call (passing a
# diacritized orthographic form where IPA is expected) silently produces
# orthography as "transcription". Acute, grave, circumflex and diaeresis
# vowels never occur in the package's IPA output -- vowel quality is written
# with IPA symbols, and nasality with a tilde -- so their presence in a value
# is a reliable sign the argument was meant for diacritized-form mode.
#
# A warning, not an error: an unusual override is still the user's call.
.orthographic_vowels <- paste0(
  "[\u00e1\u00e0\u00e2\u00e4\u00e9\u00e8\u00ea\u00eb\u00ed\u00ec\u00ee\u00ef",
  "\u00f3\u00f2\u00f4\u00f6\u00fa\u00f9\u00fb\u00fc]"
)

.check_ipa_override <- function(words, ipa, fn, call = rlang::caller_env()) {
  vals <- stringi::stri_trans_nfc(ipa)
  keys <- stringi::stri_trans_nfc(words)

  bad <- stringr::str_detect(vals, .orthographic_vowels)

  if (!any(bad)) {
    return(invisible(NULL))
  }

  n <- sum(bad)
  bullets <- c(
    "Suspicious IPA-override entr{cli::qty(n)}{?y/ies} in {.fn {fn}}.",
    "x" = "{cli::qty(n)}Value{?s} {.val {unname(vals[bad])}} look{?s/} like orthography, not IPA."
  )

  key_bad <- bad & stringr::str_detect(keys, .orthographic_vowels)

  if (any(key_bad)) {
    bullets <- c(bullets, "x" = paste(
      "{cli::qty(sum(key_bad))}Key{?s} {.val {unname(keys[key_bad])}}",
      "carr{?ies/y} diacritics; IPA-override keys are plain orthographic forms."
    ))
  }

  bullets <- c(bullets, "i" = paste(
    "To mark stress or vowel quality instead, pass the diacritized form{cli::qty(n)}{?s}",
    "without {.arg ipa}: {.code {fn}({deparse(unname(vals[bad]))})}."
  ))

  cli::cli_warn(bullets, call = call)

  invisible(NULL)
}

.set_user_lex <- function(name, value) {
  .init_user_lex_state()
  assign(name, value, envir = .fonology_state)
  invisible(value)
}

.set_fix_lex <- function(name, value) {
  .init_user_lex_state()
  assign(.fix_lex_names[[name]], value, envir = .fonology_state)
  invisible(value)
}

# Writes the shipped layer back into the source tree's data/ directory. Only
# meaningful in a source checkout, which promote_lex() enforces.
.save_fix_lex <- function(name) {
  .init_user_lex_state()

  fix_name <- .fix_lex_names[[name]]

  save(
    list = fix_name,
    envir = .fonology_state,
    file = .fix_lex_path(fix_name),
    compress = "xz"
  )

  invisible(get(fix_name, envir = .fonology_state, inherits = FALSE))
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
