# Build the internal orth_depth table behind orthDepth(): orthographic depth of the five languages
# supported by the package, measured on Wiktionary pronunciations.
#
# Source (CC BY-SA 4.0): English Wiktionary via Wiktextract, as distributed by
# https://kaikki.org/dictionary/<Language>/kaikki.org-dictionary-<Language>.jsonl
#
# The headline measure follows OTEANN (Marjou 2021): the percentage of unseen
# words whose pronunciation is predicted entirely correctly from their
# spelling (read), and whose spelling is predicted entirely correctly from
# their pronunciation (write). OTEANN uses a transformer that sees the whole
# word; here the predictor is the most frequent mapping of each letter (or
# sound) given up to three neighbours on each side.
#
# Every language is measured against the same kind of reference, so the
# scores are comparable across languages. They are independent of the lexicons
# ipa() uses for lookup (PSL, Lexique and CMU for three of the languages).
#
# Pipeline
#   1. Extract (word, POS, tags, IPA) from each dump with jq. Set KAIKKI_TSV_DIR
#      to a directory of pre-extracted <Language>.tsv files to skip the
#      downloads (0.6-3.3 GB each).
#   2. Keep one phonemic transcription per word, for one variety per language.
#   3. Align letters to phones with a many-to-many EM aligner (after
#      Jiampojamarn, Kondrak & Sherif 2007).
#   4. Hold out 10% of words and score:
#      - read: each letter is labelled with the sound(s) it starts, as silent,
#        or as continuing the previous grapheme, and the label is predicted
#        from the letter and up to three neighbours on each side. A word is
#        correct if every letter
#        is. Reading therefore includes finding silent letters and grapheme
#        boundaries, as it does for a reader.
#      - write: each sound unit's spelling (silent letters included) is
#        predicted from the sound, up to three neighbours on each side, and
#        its stress.
#      - complexity: H(label | letter) - H(label | letter, +-1 letter), in
#        bits (plug-in): the uncertainty local context rules remove (Schmalz
#        et al. 2015).
#      - onset entropy: H(first phoneme(s) | first letter), averaged over
#        letters without weighting, after Borgwaldt, Hellwig & De Groot (2005).
#
# Requires: data.table, stringi, Rcpp, jq (only when downloading).

library(data.table)
library(stringi)
library(Rcpp)

set.seed(20260930)

tsv_dir <- Sys.getenv("KAIKKI_TSV_DIR", tempdir())

languages <- data.table(
  lg = c("pt", "sp", "fr", "it", "en"),
  kaikki = c("Portuguese", "Spanish", "French", "Italian", "English"),
  # regex over the comma-separated Wiktionary sound tags; "" = untagged only
  variety_tags = c("Brazil", "", "", "", "General-American|US"),
  # transcriptions to pass over when the word has another one: Spanish keeps
  # the seseo/yeismo variant, matching sp_lex
  avoid = c("", "[\u03b8\u028e]", "", "", ""),
  variety = c("Brazilian", "Latin American (seseo, yeismo)", "Standard",
              "Standard", "General American"),
  # published OTEANN scores (Marjou 2021, Table 3), shipped for comparison
  oteann_read = c(82.4, 85.3, 79.6, 71.6, 31.1),
  oteann_write = c(75.8, 66.9, 28.0, 94.5, 36.1)
)

drop_pos <- c("name", "abbrev", "character", "symbol", "prefix", "suffix",
              "infix", "interfix", "phrase", "proverb", "punct", "affix",
              "circumfix", "combining_form", "romanization", "contraction")

# ---------------------------------------------------------------------------
# 1. Extraction
# ---------------------------------------------------------------------------

extract_kaikki <- function(language) {
  out <- file.path(tsv_dir, paste0(language, ".tsv"))
  if (file.exists(out)) return(out)
  if (!nzchar(Sys.which("jq"))) stop("Extraction requires jq, or set KAIKKI_TSV_DIR.")
  url <- sprintf("https://kaikki.org/dictionary/%s/kaikki.org-dictionary-%s.jsonl",
                 language, language)
  expr <- paste0(
    'select(.word and .sounds) as $e | .sounds[]? | ',
    'select(.ipa and (.ipa | startswith("/"))) | ',
    '[$e.word, $e.pos, (.tags // [] | join(",")), .ipa] | @tsv'
  )
  status <- system(sprintf("curl -s %s | jq -r %s > %s",
                           shQuote(url), shQuote(expr), shQuote(out)))
  if (!identical(status, 0L)) stop("Extraction failed for ", language)
  out
}

# ---------------------------------------------------------------------------
# 2. Selection and segmentation
# ---------------------------------------------------------------------------

select_entries <- function(path, tags_re, avoid = "") {
  raw <- fread(path, sep = "\t", header = FALSE, quote = "", encoding = "UTF-8",
               col.names = c("word", "pos", "tags", "ipa"))
  raw[is.na(tags), tags := ""]
  d <- raw[!pos %chin% drop_pos]
  if (nzchar(tags_re)) {
    d <- d[stri_detect_regex(tags, paste0("(^|,)(", tags_re, ")(,|$)"))]
  } else {
    d <- d[tags == ""]
  }
  d[, word := stri_trans_nfc(word)]
  d <- d[word == stri_trans_tolower(word)]               # no capitalised forms
  d <- d[stri_detect_regex(word, "^\\p{Ll}{2,}$")]       # letters only, >= 2
  d <- d[!stri_detect_regex(ipa, "[()\\s\\-\u203f]")]   # no optional segments, no multiword
  if (nzchar(avoid)) {
    d[, avoid_it := stri_detect_regex(ipa, avoid)]
    setorder(d, word, avoid_it)                          # stable: keeps listing order
  }
  d[, .SD[1], by = word]                                 # first listed pronunciation
}

diph_en <- c("a\u026a", "e\u026a", "\u0254\u026a", "a\u028a", "o\u028a",
             "\u0259\u028a")

# vowel phones (syllable nuclei), incl. syllabic consonants
vowel_re <- paste0("^[aeiouy\u00e6\u0251\u0252\u0254\u0259\u025a\u025b\u025c\u025d",
                   "\u026a\u028a\u028c\u0250\u00f8\u0153\u0264\u0268\u0289\u026f]|\u0329")

# Returns list(phones, stress). `stress` labels each phone: "" for all but the
# primary-stressed vowel, which is labelled by its distance from the end of the
# word (S0 final, S1 penult, S2 antepenult or earlier). Writers hear stress, and
# Portuguese and Spanish written accents follow from it, so the spelling
# direction uses it as context; the reading direction does not.
segment_ipa <- function(ipa, lg) {
  x <- stri_replace_all_regex(ipa, "[/\u02cc.\u02d0\u02d1\u032f]", "")
  x <- stri_trans_nfd(x)
  segs <- stri_extract_all_regex(x, "\\P{M}(?:\u0361\\P{M}|\\p{M}|[\u02b0\u02b2\u02b7\u02e0])*")
  lapply(segs, function(s) {
    s <- stri_replace_all_fixed(s, "\u0361", "")
    # merge plain affricates and (English) diphthongs into single units
    pairs <- c("t\u0283", "d\u0292", if (lg == "en") diph_en,
               if (lg == "it") c("ts", "dz"))
    out <- character(0)
    k <- 1
    while (k <= length(s)) {
      if (k < length(s) && paste0(s[k], s[k + 1]) %chin% pairs) {
        out <- c(out, paste0(s[k], s[k + 1])); k <- k + 2
      } else {
        out <- c(out, s[k]); k <- k + 1
      }
    }
    out
  })
  res <- lapply(segs, function(s) {
    mark <- which(s == "\u02c8")
    ph <- s[s != "\u02c8"]
    st <- character(length(ph))
    if (length(mark)) {
      # position of the mark in the stripped sequence, then the next vowel
      at <- mark[1] - 0L
      v <- which(stri_detect_regex(ph, vowel_re))
      sv <- v[v >= at][1]
      if (!is.na(sv)) st[sv] <- paste0("S", min(sum(v > sv), 2L))
    }
    list(ph = ph, st = st)
  })
  list(phones = lapply(res, `[[`, "ph"), stress = lapply(res, `[[`, "st"))
}

# ---------------------------------------------------------------------------
# 3. Many-to-many EM aligner
# ---------------------------------------------------------------------------
# Chunk shapes (letters:phones): 1:0 silent letter, 1:1, 2:1 digraph, 3:1
# trigraph, and 1:2 (x -> ks). Training runs in two stages: the 1:2 shape is
# only admitted, at a low prior, after the other shapes have converged.
# Admitting it from the start lets EM explain vowel letters as two phones and
# the preceding consonant as silent (e -> t S i for Portuguese -te).

sourceCpp(code = '
#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <cmath>
using namespace Rcpp;
typedef unsigned long long u64;

static inline u64 mk(const IntegerVector& x, int xs, int i,
                     const IntegerVector& y, int ys, int j) {
  u64 k = 0;
  for (int a = 0; a < 3; a++) k = (k << 11) | (u64)(a < i ? x[xs + a] : 0);
  for (int b = 0; b < 2; b++) k = (k << 11) | (u64)(b < j ? y[ys + b] : 0);
  return k;
}

// [[Rcpp::export]]
DataFrame m2m_align(List X, List Y, IntegerVector si, IntegerVector sj,
                    int iters, int ns1) {
  int n = X.size(), ns = ns1;
  std::unordered_map<u64, double> P, C;
  for (int w = 0; w < n; w++) {
    IntegerVector x = X[w], y = Y[w];
    int T = x.size(), V = y.size();
    for (int t = 0; t <= T; t++) for (int v = 0; v <= V; v++)
      for (int s = 0; s < ns; s++) {
        int i = si[s], j = sj[s];
        if (t + i <= T && v + j <= V) P[mk(x, t, i, y, v, j)] = 1.0;
      }
  }
  double tot = P.size();
  for (auto& kv : P) kv.second /= tot;

  for (int it = 0; it < 2 * iters; it++) {
    if (it == iters) {
      ns = si.size();
      for (int w = 0; w < n; w++) {
        IntegerVector x = X[w], y = Y[w];
        int T = x.size(), V = y.size();
        for (int t = 0; t <= T; t++) for (int v = 0; v <= V; v++)
          for (int s = ns1; s < ns; s++) {
            int i = si[s], j = sj[s];
            if (t + i <= T && v + j <= V) {
              u64 k = mk(x, t, i, y, v, j);
              if (P[k] == 0) P[k] = 1e-7;
            }
          }
      }
    }
    C.clear();
    for (int w = 0; w < n; w++) {
      IntegerVector x = X[w], y = Y[w];
      int T = x.size(), V = y.size(), W = V + 1;
      std::vector<double> al((T + 1) * W, 0.0), be((T + 1) * W, 0.0);
      al[0] = 1.0;
      for (int t = 0; t <= T; t++) for (int v = 0; v <= V; v++) {
        if (t == 0 && v == 0) continue;
        double a = 0;
        for (int s = 0; s < ns; s++) {
          int i = si[s], j = sj[s];
          if (t >= i && v >= j) a += al[(t - i) * W + v - j] * P[mk(x, t - i, i, y, v - j, j)];
        }
        al[t * W + v] = a;
      }
      double Z = al[T * W + V];
      if (Z <= 0) continue;
      be[T * W + V] = 1.0;
      for (int t = T; t >= 0; t--) for (int v = V; v >= 0; v--) {
        if (t == T && v == V) continue;
        double b = 0;
        for (int s = 0; s < ns; s++) {
          int i = si[s], j = sj[s];
          if (t + i <= T && v + j <= V) b += be[(t + i) * W + v + j] * P[mk(x, t, i, y, v, j)];
        }
        be[t * W + v] = b;
      }
      for (int t = 0; t <= T; t++) for (int v = 0; v <= V; v++) {
        if (al[t * W + v] == 0) continue;
        for (int s = 0; s < ns; s++) {
          int i = si[s], j = sj[s];
          if (t + i <= T && v + j <= V) {
            u64 k = mk(x, t, i, y, v, j);
            double g = al[t * W + v] * P[k] * be[(t + i) * W + v + j] / Z;
            if (g > 0) C[k] += g;
          }
        }
      }
    }
    double S = 0;
    for (auto& kv : C) S += kv.second;
    for (auto& kv : P) kv.second = 0;
    for (auto& kv : C) P[kv.first] = kv.second / S;
  }

  std::vector<int> ow, ox, oxl, oy, oyl;
  for (int w = 0; w < n; w++) {
    IntegerVector x = X[w], y = Y[w];
    int T = x.size(), V = y.size(), W = V + 1;
    std::vector<double> dl((T + 1) * W, -1.0);
    std::vector<int> bp((T + 1) * W, -1);
    dl[0] = 1.0;
    for (int t = 0; t <= T; t++) for (int v = 0; v <= V; v++) {
      if (t == 0 && v == 0) continue;
      for (int s = 0; s < ns; s++) {
        int i = si[s], j = sj[s];
        if (t >= i && v >= j && dl[(t - i) * W + v - j] > 0) {
          double c = dl[(t - i) * W + v - j] * P[mk(x, t - i, i, y, v - j, j)];
          if (c > dl[t * W + v]) { dl[t * W + v] = c; bp[t * W + v] = s; }
        }
      }
    }
    if (dl[T * W + V] <= 0) continue;
    std::vector<int> rs;
    int t = T, v = V;
    while (t > 0 || v > 0) { int s = bp[t * W + v]; rs.push_back(s); t -= si[s]; v -= sj[s]; }
    t = 0; v = 0;
    for (int r = (int)rs.size() - 1; r >= 0; r--) {
      int s = rs[r];
      ow.push_back(w + 1); ox.push_back(t); oxl.push_back(si[s]);
      oy.push_back(v); oyl.push_back(sj[s]);
      t += si[s]; v += sj[s];
    }
  }
  return DataFrame::create(_["w"] = ow, _["xs"] = ox, _["xl"] = oxl,
                           _["ys"] = oy, _["yl"] = oyl);
}
')

align_lexicon <- function(words, phones, stress) {
  letters <- stri_split_boundaries(words, type = "character")
  letter_tab <- sort(unique(unlist(letters)))
  phone_tab <- sort(unique(unlist(phones)))
  X <- lapply(letters, function(s) match(s, letter_tab))
  Y <- lapply(phones, function(s) match(s, phone_tab))
  al <- setDT(m2m_align(X, Y, si = c(1L, 1L, 2L, 3L, 1L),
                        sj = c(0L, 1L, 1L, 1L, 2L), iters = 10L, ns1 = 4L))
  al[, g := mapply(function(w, s, l) paste(letters[[w]][s + seq_len(l)], collapse = ""),
                   w, xs, xl)]
  al[, p := mapply(function(w, s, l) paste(phones[[w]][s + seq_len(l)], collapse = " "),
                   w, ys, yl)]
  al[, st := mapply(function(w, s, l) {
    lab <- stress[[w]][s + seq_len(l)]
    lab <- lab[lab != ""]
    if (length(lab)) lab[1] else "-"
  }, w, ys, yl)]

  # Reading table: one row per LETTER, labelled with the sound(s) it starts,
  # "0" if silent, or "+" if it continues the previous grapheme (the h of ch).
  # The reader must predict all three from the letters, so grapheme boundaries
  # and silent letters are not given away (cf. OTEANN's character-level input).
  al[, r := .I]
  lt <- al[, .(li = xs + seq_len(xl),
               lab = c(if (p == "") "0" else p, rep("+", xl - 1L))), by = .(r, w)]
  lt[, l := mapply(function(w, i) letters[[w]][i], w, li)]
  setorder(lt, w, li)

  # Writing table: one row per sound unit, the target being every letter the
  # writer must produce for it, silent letters included (merged into the
  # preceding unit; following, word-initially)
  wt <- al[, {
    gg <- g; keep <- rep(TRUE, .N)
    for (k in seq_len(.N)) if (p[k] == "") {
      prev <- which(keep[seq_len(k - 1)])
      if (length(prev)) {
        j <- max(prev); gg[j] <- paste0(gg[j], gg[k]); keep[k] <- FALSE
      }
    }
    while (sum(keep) > 1 && p[which(keep)[1]] == "") {
      f <- which(keep)[1]; nx <- which(keep)[2]
      gg[nx] <- paste0(gg[f], gg[nx]); keep[f] <- FALSE
    }
    .(g = gg[keep], p = p[keep], st = st[keep], pos = seq_len(sum(keep)))
  }, by = w]

  list(letters = lt[, .(w, li, l, lab)], units = wt)
}

# ---------------------------------------------------------------------------
# 4. Metrics
# ---------------------------------------------------------------------------

entropy <- function(n) { p <- n / sum(n); -sum(p * log2(p)) }

cond_entropy <- function(dt, given, out) {
  per <- dt[, .N, by = c(given, out)][, .(n = sum(N), h = entropy(N)), by = given]
  sum(per$n * per$h) / sum(per$n)
}

# Predict `out` in the test rows as its most frequent value in training for
# the same input and the widest context seen in training, backing off through
# `ctx_levels` (widest first) to the input alone. Adds `ok` (with context)
# and `ok_base` (input alone); an input never seen in training is an error.
predict_heldout <- function(train, test, given, ctx_levels, out) {
  best <- function(by) {
    train[, .N, by = c(by, out)][order(-N)][, .SD[1], by = by][, c(by, out), with = FALSE]
  }
  te <- copy(test)
  te[, pred := NA_character_]
  for (ctx in c(ctx_levels, list(character(0)))) {
    tab <- best(c(given, ctx)); setnames(tab, out, "p_lvl")
    te <- merge(te, tab, by = c(given, ctx), all.x = TRUE, sort = FALSE)
    te[is.na(pred), pred := p_lvl]
    if (length(ctx) == 0) te[, pred_base := p_lvl]
    te[, p_lvl := NULL]
  }
  te[, ok := fcoalesce(pred == get(out), FALSE)]
  te[, ok_base := fcoalesce(pred_base == get(out), FALSE)]
  te
}

# Context windows, widest first: up to three letters (or sounds) on each
# side, backing off to two, one, and none. Held-out accuracy (one split, all
# five languages) rose from +-1 to +-2 to +-3 with diminishing returns
# (mean read .53 / .76 / .79, write .55 / .71 / .72); +-3 was fixed then and
# not tuned further. Writing adds stress at every level.
win <- function(pre, post, k) c(paste0(pre, seq_len(k)), paste0(post, seq_len(k)))
read_ctx <- lapply(3:1, function(k) win("pl", "nl", k))
write_ctx <- c(lapply(3:1, function(k) c(win("pp", "np", k), "st")), list("st"))

vowel_letter_re <- "^[aeiouyà-æè-ïò-öø-ýÿœ]$"

measure_language <- function(cfg) {
  message("== ", cfg$kaikki)
  d <- select_entries(extract_kaikki(cfg$kaikki), cfg$variety_tags, cfg$avoid)
  seg <- segment_ipa(d$ipa, cfg$lg)
  al <- align_lexicon(d$word, seg$phones, seg$stress)
  lt <- al$letters
  u <- al$units
  u[, word := d$word[w]]
  message(sprintf("   %d words, %d aligned", nrow(d), uniqueN(u$w)))

  # contexts: neighbouring letters for reading; neighbouring sounds and stress
  # for writing (a writer hears stress, and written accents follow from it)
  for (k in 1:3) {
    lt[, paste0(c("pl", "nl"), k) := .(shift(l, k, fill = "#"), shift(l, -k, fill = "#")),
       by = w]
  }
  u[, `:=`(
    pp1 = shift(stri_extract_last_regex(p, "\\S+"), 1, fill = "#"),
    np1 = shift(stri_extract_first_regex(p, "\\S+"), -1, fill = "#")
  ), by = w]
  for (k in 2:3) {
    u[, paste0(c("pp", "np"), k) := .(shift(get(paste0("pp", k - 1)), 1, fill = "#"),
                                      shift(get(paste0("np", k - 1)), -1, fill = "#")), by = w]
  }

  # one held-out split of words, shared by both directions
  test_w <- sample(unique(u$w), round(0.1 * uniqueN(u$w)))
  rd <- predict_heldout(lt[!w %in% test_w], lt[w %in% test_w], "l", read_ctx, "lab")
  wr <- predict_heldout(u[!w %in% test_w], u[w %in% test_w], "p", write_ctx, "g")
  vowel <- stri_detect_regex(rd$l, vowel_letter_re)

  h_read <- cond_entropy(lt, "l", "lab")
  # complexity uses the immediate neighbours only: the uncertainty that the
  # most local rules remove
  h_read_ctx <- cond_entropy(lt, c("l", "pl1", "nl1"), "lab")

  # sensitivity of the plug-in context entropy to sample size: recompute on a
  # random half of the words; a large rise would mean the full-data value is
  # biased downward by sparse contexts
  half <- lt[w %in% sample(unique(w), uniqueN(w) %/% 2)]
  message(sprintf("   H(P|L,ctx) full = %.3f, half = %.3f",
                  h_read_ctx, cond_entropy(half, c("l", "pl1", "nl1"), "lab")))

  on <- u[pos == 1, .(l1 = stri_sub(word, 1, 1), p)]
  onset <- on[, .(h = entropy(table(p))), by = l1][, mean(h)]

  data.frame(
    lg = cfg$lg,
    language = cfg$kaikki,
    variety = cfg$variety,
    n_words = uniqueN(u$w),
    read = rd[, all(ok), by = w][, mean(V1)],
    write = wr[, all(ok), by = w][, mean(V1)],
    read_letters = mean(rd$ok),
    write_sounds = mean(wr$ok),
    read_vowels = mean(rd$ok[vowel]),
    read_consonants = mean(rd$ok[!vowel]),
    read_nocontext = rd[, all(ok_base), by = w][, mean(V1)],
    write_nocontext = wr[, all(ok_base), by = w][, mean(V1)],
    read_entropy = h_read,
    read_entropy_context = h_read_ctx,
    complexity = h_read - h_read_ctx,
    write_entropy = cond_entropy(u, "p", "g"),
    write_entropy_context = cond_entropy(u, c("p", "pp1", "np1", "st"), "g"),
    onset_entropy = onset,
    oteann_read = cfg$oteann_read / 100,
    oteann_write = cfg$oteann_write / 100
  )
}

orth_depth <- do.call(rbind, lapply(split(languages, by = "lg", sorted = FALSE),
                                    measure_language))
rownames(orth_depth) <- NULL
print(orth_depth, digits = 3)

# internal (R/sysdata.rda): orthDepth() is the only public interface
usethis::use_data(orth_depth, internal = TRUE, overwrite = TRUE)
