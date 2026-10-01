test_that("orthDepth() covers the five languages with valid scores", {
  od <- suppressMessages(orthDepth())
  expect_setequal(od$lg, c("pt", "sp", "fr", "it", "en"))
  for (col in c("read", "write", "read_letters", "write_sounds",
                "oteann_read", "oteann_write")) {
    expect_true(all(od[[col]] > 0 & od[[col]] <= 1), info = col)
  }
  # a word is right only if all its letters are
  expect_true(all(od$read <= od$read_letters))
  expect_true(all(od$write <= od$write_sounds))
  # context can only help
  expect_true(all(od$read >= od$read_nocontext))
  expect_true(all(od$complexity >= 0))
  expect_true(all(od$read_entropy_context <= od$read_entropy))
})

test_that("English is the deepest orthography for reading", {
  od <- suppressMessages(orthDepth())
  expect_equal(od$lg[which.min(od$read)], "en")
})

test_that("orthDepth() prints one or all languages and returns rows invisibly", {
  msgs <- testthat::capture_messages(res <- orthDepth("pt"))
  expect_match(paste(msgs, collapse = ""), "Portuguese")
  expect_equal(nrow(res), 1)
  expect_equal(res$lg, "pt")
  expect_equal(suppressMessages(orthDepth("Portuguese"))$lg, "pt")
  msgs <- testthat::capture_messages(all <- orthDepth())
  expect_match(paste(msgs, collapse = ""), "Orthographic depth")
  expect_equal(nrow(all), 5)
  quiet <- testthat::capture_messages(orthDepth("pt", explain = FALSE))
  expect_false(any(grepl("Higher percentages", quiet)))
})

test_that("orthDepth() rejects unknown languages", {
  expect_error(orthDepth("de"), "not supported")
  expect_error(orthDepth(c("pt", "sp")), "not supported")
})
