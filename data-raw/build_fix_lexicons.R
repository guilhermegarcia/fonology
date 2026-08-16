# Builds the shipped lexicon layer (data/*_fix.rda) and resets the local
# layer (data/*_lex_user.rda, data/*_ipa_lex.rda) to empty.
#
# Background. Until v1.4.0 there was a single lexicon per language, which was
# simultaneously the user's private store and the package's shipped list of
# corrections: add_lex_*() wrote into the package's own data/ directory when
# run from a source checkout, and those files were committed. That conflated
# two roles. A user file *replaced* the shipped data rather than merging with
# it, so any user who had ever called add_lex_*() stopped receiving corrections
# shipped in later releases.
#
# From v1.5.0 the two layers are separate:
#
#   *_fix        shipped, maintainer-curated, committed here.
#   *_lex_user   local, written only under tools::R_user_dir(), empty on
#   *_ipa_lex    a fresh installation.
#
# .lex() merges them with the local layer winning. promote_lex() moves entries
# from local to shipped.
#
# This script is idempotent: run it again and the shipped files keep whatever
# is currently in data/*_fix.rda, plus anything still sitting in the old
# single-layer files.

pairs <- c(
  en_ipa_lex  = "en_ipa_fix",
  fr_ipa_lex  = "fr_ipa_fix",
  it_ipa_lex  = "it_ipa_fix",
  it_lex_user = "it_lex_fix",
  pt_ipa_lex  = "pt_ipa_fix",
  pt_lex_user = "pt_lex_fix",
  sp_ipa_lex  = "sp_ipa_fix",
  sp_lex_user = "sp_lex_fix"
)

# Malformed legacy entries, dropped on migration.
#
# All four were added through the IPA-override mode of add_lex_*(), which
# returns its value verbatim and bypasses the pipeline. Three of them store
# orthography rather than IPA, so ipa("féra", lg = "pt") emitted the literal
# text "féras". The intent behind them (signalling an open mid vowel) belongs
# in the diacritized store, where equivalent entries already exist for the
# singulars; the plurals were added there instead. The French entry stripped
# the syllable boundaries that fr_lex already provides for the same word
# (a.kwa.tik), so the override made the output worse.
drop_entries <- list(
  pt_ipa_lex = c("bórda", "féra", "véra"),
  fr_ipa_lex = "aquatique"
)

read_rda <- function(path, object) {
  if (!file.exists(path)) return(NULL)
  e <- new.env(parent = emptyenv())
  load(path, envir = e)
  if (!exists(object, envir = e, inherits = FALSE)) return(NULL)
  get(object, envir = e, inherits = FALSE)
}

empty_lex <- setNames(character(0), character(0))

for (user_name in names(pairs)) {
  fix_name <- pairs[[user_name]]

  old <- read_rda(file.path("data", paste0(user_name, ".rda")), user_name)
  cur <- read_rda(file.path("data", paste0(fix_name, ".rda")), fix_name)

  if (is.null(old)) old <- empty_lex
  if (is.null(cur)) cur <- empty_lex

  # Entries already in the shipped file win over the legacy ones only if the
  # legacy file does not define them; a re-run must not resurrect stale values.
  merged <- c(old[!names(old) %in% names(cur)], cur)
  merged <- merged[!is.na(merged)]

  drops <- drop_entries[[user_name]]
  if (!is.null(drops)) merged <- merged[!names(merged) %in% drops]
  # order(NULL) errors, and an empty vector has no names to sort by.
  if (length(merged) > 0) merged <- merged[order(names(merged))]
  if (length(merged) == 0) merged <- empty_lex

  assign(fix_name, merged)
  save(
    list = fix_name,
    file = file.path("data", paste0(fix_name, ".rda")),
    compress = "xz"
  )

  # The local layer ships empty: it belongs to the user, not to the package.
  assign(user_name, empty_lex)
  save(
    list = user_name,
    file = file.path("data", paste0(user_name, ".rda")),
    compress = "xz"
  )

  message(sprintf("%-12s -> %-12s %3d entries", user_name, fix_name, length(merged)))
}
