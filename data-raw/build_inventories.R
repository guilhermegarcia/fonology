# Build the per-language phonemic inventory objects.
#
# These vectors are the single source of truth for which segments belong to a
# language: getFeat() and getPhon() read them through .inventory() (see
# R/helper_functions.R), and getWeight_fr()/getWeight_it() read vowels_fr and
# vowels_it. Before this script existed, getFeat() and getPhon() each carried
# their own hardcoded copy and the two had drifted apart.
#
# Every segment listed here must exist in allFeatures (after .norm_ipa()
# aliasing); the assertion at the end of this script enforces that.
#
# Scope: broad phonemic transcription, i.e. the segments ipa(narrow = FALSE)
# can emit. Narrow-only allophones (Portuguese ɪ ʊ ɾ ŋ, palatalised affricates)
# are deliberately excluded.

devtools::load_all(quiet = TRUE)

split_ipa <- function(x) unlist(strsplit(x, ".", fixed = TRUE))

# Portuguese -----------------------------------------------------------------
# /x/ is emitted by ipa(lg = "pt") and was previously absent.
#
# The nasal vowels are deliberately NOT listed: they are taken to be derived
# from an underlying vowel + nasal sequence rather than phonemic in their own
# right, so the inventory has oral vowels only. A consequence is that a nasal
# vowel from ipa(lg = "pt") is not a valid input to getFeat()/getPhon(); the
# error names the segment.
vowels_pt <- split_ipa("a.e.i.o.u.ɛ.ɔ")
consonants_pt <- split_ipa(
  "j.w.p.b.t.d.k.g.f.v.s.z.ʃ.ʒ.x.m.n.ɲ.l.r.ɾ.ʎ"
)

# Spanish --------------------------------------------------------------------
# /n/ was missing.
vowels_sp <- split_ipa("a.e.i.o.u")
consonants_sp <- split_ipa(
  "p.b.f.v.t.d.k.g.s.z.t͡ʃ.θ.m.n.ɲ.w.j.l.r.ɾ.ʎ.x.ʝ"
)

# French ---------------------------------------------------------------------
# ɱ removed: it is an allophone of /m/ before labiodentals, not a phoneme, and
# its presence stopped {m n ɲ ŋ} from being a natural class.
vowels_fr <- split_ipa(
  "a.e.ø.ɑ.i.y.o.u.ɛ.ɔ.ə.œ.ɛ̃.œ̃.ɔ̃.ɑ̃"
)
consonants_fr <- split_ipa(
  "p.b.t.d.k.g.f.v.s.z.ʃ.ʒ.ʁ.m.n.ɲ.ŋ.l.w.j.ɥ"
)

# Italian --------------------------------------------------------------------
vowels_it <- split_ipa("a.e.i.o.u.ɛ.ɔ")
consonants_it <- split_ipa(
  paste0(
    "j.w.p.b.t.d.k.g.t͡ʃ.d͡ʒ.t͡s.d͡z.",
    "f.v.s.z.ʃ.m.n.ɲ.l.r.ʎ"
  )
)

# English --------------------------------------------------------------------
# New: English had no inventory dataset. Matches the segment set used by
# en_lex, including the r-coloured vowels ɚ and ɝ.
vowels_en <- split_ipa(
  "a.e.i.o.u.ɑ.ɛ.ɔ.ə.ɪ.ʊ.æ.ʌ.ɚ.ɝ"
)
consonants_en <- split_ipa(
  paste0(
    "p.b.t.d.k.g.f.v.θ.ð.s.z.ʃ.ʒ.",
    "t͡ʃ.d͡ʒ.m.n.ŋ.l.ɹ.j.w.h"
  )
)

# Validation -----------------------------------------------------------------
inventories <- list(
  pt = c(vowels_pt, consonants_pt),
  sp = c(vowels_sp, consonants_sp),
  fr = c(vowels_fr, consonants_fr),
  it = c(vowels_it, consonants_it),
  en = c(vowels_en, consonants_en)
)

for (lg in names(inventories)) {
  inv <- inventories[[lg]]
  if (anyDuplicated(inv)) {
    stop(lg, ": duplicated segments: ", paste(inv[duplicated(inv)], collapse = " "))
  }
  missing <- setdiff(.norm_ipa(inv), .norm_ipa(allFeatures$ipa))
  if (length(missing) > 0) {
    stop(lg, ": segments absent from allFeatures: ", paste(missing, collapse = " "))
  }
}

save(vowels_pt, file = "data/vowels_pt.rda", compress = "xz")
save(consonants_pt, file = "data/consonants_pt.rda", compress = "xz")
save(vowels_sp, file = "data/vowels_sp.rda", compress = "xz")
save(consonants_sp, file = "data/consonants_sp.rda", compress = "xz")
save(vowels_fr, file = "data/vowels_fr.rda", compress = "xz")
save(consonants_fr, file = "data/consonants_fr.rda", compress = "xz")
save(vowels_it, file = "data/vowels_it.rda", compress = "xz")
save(consonants_it, file = "data/consonants_it.rda", compress = "xz")
save(vowels_en, file = "data/vowels_en.rda", compress = "xz")
save(consonants_en, file = "data/consonants_en.rda", compress = "xz")
