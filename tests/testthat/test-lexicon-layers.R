# The lexicon has two layers: shipped corrections (*_fix, distributed with the
# package) and local ones (*_lex_user / *_ipa_lex, owned by the user). These
# tests drive the layers in memory; none of them touch the user's data
# directory or the package's data/ files.

# Swaps both layers of `name` for the duration of the calling test, restoring
# whatever was there afterwards.
local_layers <- function(name, fix = NULL, user = NULL, env = parent.frame()) {
  Fonology:::.init_user_lex_state()

  old_fix <- Fonology:::.get_fix_lex(name)
  old_user <- Fonology:::.get_user_lex(name)

  if (!is.null(fix)) Fonology:::.set_fix_lex(name, fix)
  if (!is.null(user)) Fonology:::.set_user_lex(name, user)

  # The restore runs in the caller's frame, so the values are inlined into the
  # expression rather than looked up in this function's (long-gone) frame.
  restore <- bquote({
    Fonology:::.set_fix_lex(.(name), .(old_fix))
    Fonology:::.set_user_lex(.(name), .(old_user))
  })

  do.call(base::on.exit, list(restore, add = TRUE), envir = env)

  invisible(NULL)
}

test_that("shipped corrections are visible when the local layer is empty", {
  local_layers(
    "pt_ipa_lex",
    fix = c(shampoo = "\u0283am.\u02c8pu"),
    user = stats::setNames(character(0), character(0))
  )

  expect_equal(unname(Fonology:::.lex("pt_ipa_lex")["shampoo"]), "\u0283am.\u02c8pu")
})

test_that("the local layer overrides a shipped correction", {
  local_layers(
    "pt_ipa_lex",
    fix = c(shampoo = "\u0283am.\u02c8pu"),
    user = c(shampoo = "\u0283\u00e3.\u02c8pu")
  )

  lex <- Fonology:::.lex("pt_ipa_lex")

  expect_equal(unname(lex["shampoo"]), "\u0283\u00e3.\u02c8pu")
  expect_length(lex, 1)
})

test_that("shipped and local entries are merged, not replaced", {
  local_layers(
    "pt_ipa_lex",
    fix = c(a = "1", b = "2"),
    user = c(b = "22", c = "3")
  )

  lex <- Fonology:::.lex("pt_ipa_lex")

  expect_setequal(names(lex), c("a", "b", "c"))
  expect_equal(unname(lex["b"]), "22")
})

test_that("removing a local override reverts to the shipped correction", {
  local_layers(
    "pt_ipa_lex",
    fix = c(shampoo = "\u0283am.\u02c8pu"),
    user = c(shampoo = "\u0283\u00e3.\u02c8pu")
  )

  dropped <- Fonology:::.drop_lex(
    Fonology:::.get_user_lex("pt_ipa_lex"),
    "shampoo",
    "pt_ipa_lex"
  )

  expect_length(dropped, 0)

  Fonology:::.set_user_lex("pt_ipa_lex", dropped)
  expect_equal(unname(Fonology:::.lex("pt_ipa_lex")["shampoo"]), "\u0283am.\u02c8pu")
})

test_that("removing a shipped-only entry stores a tombstone that hides it", {
  local_layers(
    "pt_ipa_lex",
    fix = c(shampoo = "\u0283am.\u02c8pu"),
    user = stats::setNames(character(0), character(0))
  )

  dropped <- Fonology:::.drop_lex(
    Fonology:::.get_user_lex("pt_ipa_lex"),
    "shampoo",
    "pt_ipa_lex"
  )

  expect_true(is.na(dropped[["shampoo"]]))

  Fonology:::.set_user_lex("pt_ipa_lex", dropped)

  # The tombstone hides the entry without deleting the shipped correction.
  expect_length(Fonology:::.lex("pt_ipa_lex"), 0)
  expect_true("shampoo" %in% names(Fonology:::.get_fix_lex("pt_ipa_lex")))
})

test_that("a shipped correction reaches ipa() with an empty local layer", {
  local_layers(
    "pt_ipa_lex",
    fix = c(zzzword = "\u02c8zzz"),
    user = stats::setNames(character(0), character(0))
  )

  expect_equal(ipa("zzzword", lg = "pt"), "\u02c8zzz")
})

test_that("an IPA override holding orthography is flagged", {
  expect_warning(
    Fonology:::.check_ipa_override("f\u00e9ra", "f\u00e9ras", fn = "add_lex_pt"),
    "orthography"
  )
})

test_that("a diacritized key is reported alongside the bad value", {
  expect_warning(
    Fonology:::.check_ipa_override("f\u00e9ra", "f\u00e9ras", fn = "add_lex_pt"),
    "plain orthographic forms"
  )
})

test_that("legitimate IPA overrides are not flagged", {
  # An accented key is fine when the value really is IPA.
  expect_silent(
    Fonology:::.check_ipa_override("caf\u00e9", "ka.\u02c8f\u025b", fn = "add_lex_pt")
  )

  # Nasality is written with a tilde in the package's IPA.
  expect_silent(
    Fonology:::.check_ipa_override("irma", "i.\u02c8\u027em\u00e3", fn = "add_lex_pt")
  )

  expect_silent(
    Fonology:::.check_ipa_override("shampoo", "\u0283am.\u02c8pu", fn = "add_lex_pt")
  )
})

test_that("promote_lex() refuses to run against an installed package", {
  local_mocked_bindings(.is_source_checkout = function() FALSE, .package = "Fonology")

  expect_error(promote_lex("pt"), "source checkout")
})

test_that("promote_lex() rejects unknown languages", {
  local_mocked_bindings(.is_source_checkout = function() TRUE, .package = "Fonology")

  # Aborts on validation, before anything is written.
  expect_error(promote_lex("klingon"), "klingon")
})

test_that("the local layer persists outside the package installation", {
  path <- Fonology:::.user_lex_path("pt_ipa_lex")

  expect_false(
    startsWith(path, file.path(Fonology:::.pkg_dir(), "data")),
    info = "add_lex_*() must never write into the package's own data/ directory"
  )
})
