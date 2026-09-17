# Add IPA columns to psl, and normalise stemPro.
#
# The PSL keeps its original ASCII notation in every existing column: other
# columns (bigramProb, nDensity, POU, ...) were computed over it, and code that
# reads the original format keeps working. Each column that holds segmental
# material gets an IPA counterpart, appended at the end with the suffix .ipa,
# so existing column positions are unchanged.
#
# stemPro is documented as proU excluding its final phoneme, but the stored
# column departed from that definition in two ways: 6,587 rows carried syllable
# boundaries and stress (a-a-'bO-r), and 449 oxytones ending in a nasal kept
# the whole of proU (abia~ for proU abia~), although the final segment there
# is the nasal coda ~ (the PSL analyses a nasal vowel as oral vowel + nasal
# coda). stemPro is therefore rebuilt from its definition, and stemPro.ipa is
# derived from the rebuilt column.
#
# The script is idempotent: existing .ipa columns are dropped and recomputed.

pkgload::load_all(".")

segmental <- c(
  "pro", "proU", "stemPro", "antSyl", "penSyl", "finSyl",
  paste0(rep(c("onset", "vowel", "coda"), each = 5), ".",
         c("ant", "pen", "fin", "stress", "alt"))
)

psl <- psl[, !grepl("\\.ipa$", names(psl))]

# proU is documented as excluding syllabification and stress; one row
# (dresslerela) still carried a stress mark, which would propagate to stemPro.
proU <- stringr::str_remove_all(as.character(psl$proU), "[-']")
psl$proU <- factor(proU)
psl$stemPro <- factor(substr(proU, 1, nchar(proU) - 1))

for (col in segmental) {
  psl[[paste0(col, ".ipa")]] <- factor(psl_to_ipa(psl[[col]]))
}

stopifnot(
  !any(grepl("[-']", psl$proU)),
  !any(grepl("[-']", psl$stemPro)),
  all(as.character(psl$stemPro) == substr(proU, 1, nchar(proU) - 1)),
  ncol(psl) == 62 + length(segmental)
)

usethis::use_data(psl, overwrite = TRUE, compress = "bzip2")
