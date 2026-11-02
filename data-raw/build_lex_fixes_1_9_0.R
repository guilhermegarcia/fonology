# Additions to the shipped correction layers in Fonology 1.9.0, found while
# re-measuring ipa() against English Wiktionary (via kaikki.org) and hand
# checking the most frequent out-of-vocabulary tokens in subtitle frequency
# lists. Values are respellings: they run through each language's pipeline,
# so narrow transcription and helpers keep working. Idempotent.
#
# Run from the package root after data-raw/build_fix_lexicons.R.

pkgload::load_all(".")

# Italian: function words the lexicon lacks, whose default open mid vowel is
# wrong, or whose <ui> is read as a glide; and frequent forms the rules miss.
# Grave = open-mid, acute = close-mid.
it_new <- c(
  del = "dél", dello = "déllo", della = "délla", delle = "délle",
  nel = "nél", nello = "néllo", nella = "nélla", nelle = "nélle",
  col = "cól", coi = "cói",
  cui = "cùi", sui = "sùi", tue = "tùe",
  stessa = "stéssa", stesse = "stésse",
  dollari = "dòllari", aspetta = "aspètta"
)

load("data/it_lex_fix.rda")
it_lex_fix <- c(it_lex_fix[!names(it_lex_fix) %in% names(it_new)], it_new)
it_lex_fix <- it_lex_fix[order(names(it_lex_fix))]
save(it_lex_fix, file = "data/it_lex_fix.rda", compress = "xz")

# Portuguese additions live in data-raw/pt_manual_lex_fix.tsv, the maintainer
# baseline that build_pt_wikt_corrections.R merges. They are added to the
# shipped layer directly so that this does not require rebuilding from a newer
# Wiktionary dump, which would change the audited set.
pt_manual <- readr::read_tsv(
  "data-raw/pt_manual_lex_fix.tsv",
  show_col_types = FALSE, quote = ""
)
load("data/pt_lex_fix.rda")
new <- stats::setNames(pt_manual$value, pt_manual$word)
pt_lex_fix <- c(pt_lex_fix[!names(pt_lex_fix) %in% names(new)], new)
pt_lex_fix <- pt_lex_fix[order(names(pt_lex_fix))]
save(pt_lex_fix, file = "data/pt_lex_fix.rda", compress = "xz")
