# Build app/data/tab5_counts.rds: binned class counts for tab ⑤ (no raw samples).
# Run from sweep/:  Rscript make_tab5_data.R
#
# Design (documented so the in-app size stays ~1 MB):
#   * 7 views: ① MGR, ② eq, ② pulse, ③ plant, ③ who-wins, ④ plant, ④ who-wins
#   * At most 8 axes per view (always including the fertility conditioner S or K_P,
#     and L for ①; the rest are the highest-importance parameters from importance_rf.csv)
#   * 10 bins per axis, 10 fertility bins, 5 light bins (① only)
#   * Integer counts in long format, zeros dropped: (pair, axis-bin ×, axis-bin y,
#     fertility bin, light bin, class)
#   * A separate `band` table (fertility × light × class) for the stacked bar / readout
#   * Importance table stored alongside
#
# Aggregation in the app is a sum over the fertility (and light) bins inside the
# selected slider band — no raw samples and no live sampling.
suppressMessages(library(data.table))

NB <- 10L; NF <- 10L; NL <- 5L; NMIN <- 15L
SEED <- 20261104L
SYM <- c(S = "soil P, S", L = "light, L", Amax = "A_max", Y = "Y", theta = "θ", Kr = "K_r",
         c = "colonisation c", phi = "φ", kappa = "C cost κ", rho = "root suppression ρ",
         Kf = "K_f", U0 = "fungal P need U_0", n = "n", K = "fertility K_P", r = "r",
         g = "γ", a = "a_M", b = "b_M", d = "d_M", e = "e_M", g2 = "γ_Z",
         a1 = "a_M", a2 = "a_Z", b1 = "b_M", b2 = "b_Z", d1 = "d_M", d2 = "d_Z",
         e1 = "e_M", e2 = "e_Z", c1 = "α_MZ", c2 = "α_ZM", s = "sanction s", q0 = "q_0")
RNG <- fread("csv/ranges.csv")
IMP <- fread("csv/importance_rf.csv")
uscale <- function(x, lo, hi, scale) {
  x <- pmin(pmax(x, lo), hi)
  if (identical(scale, "log")) {
    if (lo <= 0) stop("log scale needs lo > 0")
    log(x / lo) / log(hi / lo)
  } else (x - lo) / (hi - lo)
}
bin_of <- function(u, n) as.integer(pmin(floor(pmin(pmax(u, 0), 1 - 1e-12) * n) + 1L, n))
breaks_of <- function(lo, hi, scale, n) {
  u <- seq(0, 1, length.out = n + 1L)
  if (identical(scale, "log")) lo * (hi / lo)^u else lo + u * (hi - lo)
}
rng_par <- function(tab_id, par_name) {
  # extract with [[ so data.table does not treat `tab`/`par` as i-expression columns
  hit <- which(RNG[["tab"]] == as.integer(tab_id) & RNG[["par"]] == par_name)
  if (!length(hit) && as.integer(tab_id) == 4L)
    hit <- which(RNG[["tab"]] == 3L & RNG[["par"]] == par_name)
  if (length(hit) != 1L) stop("expected 1 range row for ", tab_id, ":", par_name, " (got ", length(hit), ")")
  list(lo = RNG[["lo"]][hit], hi = RNG[["hi"]][hit], scale = RNG[["scale"]][hit])
}

# views: id, label, sample file, class column, sample-tab code in ranges.csv,
#        importance design key, axis parameter names (length ≤ 8), default x/y,
#        fertility param, optional light param
views <- list(
  list(id = "tab1",        label = "① Static physiology (MGR)",
       file = "csv/samples_tab1.csv", ycol = "class", rtab = "1", imp = "tab1",
       axes = c("c", "kappa", "rho", "theta", "Y", "U0", "S", "L"),
       def = c("c", "kappa"), fert = "S", light = "L",
       classes = c("mutualism", "commensalism", "parasitism")),
  list(id = "tab2_eq",     label = "② Equilibrium criterion",
       file = "csv/samples_tab2.csv", ycol = "eqClass", rtab = "2", imp = "tab2_eq",
       axes = c("K", "e", "r", "a", "d", "b", "g"),
       def = c("K", "e"), fert = "K", light = NULL,
       classes = c("mutualism", "commensalism", "parasitism", "cannot persist")),
  list(id = "tab2_pulse",  label = "② Pulse criterion",
       file = "csv/samples_tab2.csv", ycol = "pulseClass", rtab = "2", imp = "tab2_pulse",
       axes = c("e", "K", "d", "r", "b", "a", "g"),
       def = c("e", "K"), fert = "K", light = NULL,
       classes = c("mutualism", "parasitism", "cannot persist")),
  list(id = "tab3_plant",  label = "③ Plant response",
       file = "csv/samples_tab3.csv", ycol = "class", rtab = "3", imp = "tab3_plant",
       axes = c("K", "r", "e1", "e2", "a1", "a2", "b2", "d2"),
       def = c("K", "r"), fert = "K", light = NULL,
       classes = c("mutualism", "commensalism", "parasitism", "no fungus persists")),
  list(id = "tab3_winner", label = "③ Who wins",
       file = "csv/samples_tab3.csv", ycol = "winner", rtab = "3", imp = "tab3_winner",
       axes = c("b1", "b2", "d2", "d1", "c2", "c1", "e1", "K"),
       def = c("b1", "b2"), fert = "K", light = NULL,
       classes = c("mutualist", "coexistence", "cheater", "none")),
  list(id = "tab4_plant",  label = "④ Plant response",
       file = "csv/samples_tab4.csv", ycol = "class", rtab = "4", imp = "tab4_plant",
       axes = c("K", "r", "e1", "a1", "e2", "a2", "d2", "b2"),
       def = c("K", "r"), fert = "K", light = NULL,
       classes = c("mutualism", "commensalism", "parasitism", "no fungus persists")),
  list(id = "tab4_winner", label = "④ Who wins",
       file = "csv/samples_tab4.csv", ycol = "winner", rtab = "4", imp = "tab4_winner",
       axes = c("b1", "b2", "d2", "d1", "c2", "c1", "e1", "e2"),
       def = c("b1", "b2"), fert = "K", light = NULL,
       classes = c("mutualist", "coexistence", "cheater", "none"))
)

build_one <- function(v) {
  cat(sprintf("  %s … ", v$id))
  D <- fread(v$file)
  N <- nrow(D)
  y <- as.character(D[[v$ycol]])
  # bins for every axis we need
  need <- unique(c(v$axes, v$fert, v$light))
  B <- data.table(fert = integer(N), light = 1L, class = y)
  breaks <- list(); scales <- list(); lo <- list(); hi <- list()
  for (p in need) {
    r <- rng_par(as.integer(v$rtab), p)
    u <- uscale(D[[p]], r$lo, r$hi, r$scale)
    nb <- if (identical(p, v$fert)) NF else if (identical(p, v$light)) NL else NB
    B[, (p) := bin_of(u, nb)]
    breaks[[p]] <- breaks_of(r$lo, r$hi, r$scale, nb)
    scales[[p]] <- r$scale; lo[[p]] <- r$lo; hi[[p]] <- r$hi
  }
  B[, fert := get(v$fert)]
  if (!is.null(v$light)) B[, light := get(v$light)] else B[, light := 1L]
  # drop classes that never occur
  cls <- v$classes[v$classes %in% y]
  B[, cl := match(class, cls)]
  # band (exact totals by fert × light × class)
  band <- B[, .(n = .N), by = .(fert, light, cl)]
  band[, `:=`(fert = as.integer(fert), light = as.integer(light), cl = as.integer(cl), n = as.integer(n))]
  setorder(band, fert, light, cl)
  totals <- as.integer(table(factor(y, levels = cls)))
  names(totals) <- cls
  # pairwise counts
  ax <- v$axes; nax <- length(ax)
  pairs <- rbindlist(lapply(seq_len(nax - 1L), function(i) data.table(pid = NA_integer_, xa = ax[i], ya = ax[(i + 1L):nax], xi = i, yi = (i + 1L):nax)))
  pairs[, pid := .I]
  chunks <- vector("list", nrow(pairs))
  for (k in seq_len(nrow(pairs))) {
    xa <- pairs$xa[k]; ya <- pairs$ya[k]
    ch <- B[, .(n = .N), by = c(xa, ya, "fert", "light", "cl")]
    setnames(ch, old = c(xa, ya), new = c("bx", "yb"))
    ch[, pid := pairs$pid[k]]
    chunks[[k]] <- ch
  }
  counts <- rbindlist(chunks, use.names = TRUE)
  counts[, pid := as.integer(pid)]
  counts[, bx := as.integer(bx)]
  counts[, yb := as.integer(yb)]
  counts[, fert := as.integer(fert)]
  counts[, light := as.integer(light)]
  counts[, cl := as.integer(cl)]
  counts[, n := as.integer(n)]
  setcolorder(counts, c("pid", "bx", "yb", "fert", "light", "cl", "n"))
  setorder(counts, pid, bx, yb, fert, light, cl)
  # importance (keep listed axes, original 0–1 scale relative to that view's top parameter)
  imp <- IMP[design == v$imp, .(par, symbol, importance)]
  imp <- rbind(imp, data.table(par = setdiff(ax, imp$par), symbol = SYM[setdiff(ax, imp$par)], importance = 0), fill = TRUE)
  imp <- imp[par %in% ax]
  imp[, symbol := SYM[par]]
  setorder(imp, -importance)
  ranges <- rbindlist(lapply(need, function(p) data.table(par = p, lo = lo[[p]], hi = hi[[p]], scale = scales[[p]])))
  # sanity
  stopifnot(sum(band$n) == N, sum(counts$n) == N * nrow(pairs))
  cat(sprintf("%d samples, %d pairs, %d count rows, band %d rows\n", N, nrow(pairs), nrow(counts), nrow(band)))
  list(id = v$id, label = v$label, N = N, classes = cls,
       axes = ax, def_x = v$def[1], def_y = v$def[2],
       fert = v$fert, light = v$light,
       breaks = breaks, ranges = ranges, symbols = SYM[need],
       importance = imp, pairs = pairs[, .(pid, xa, ya, xi, yi)],
       band = band, counts = counts, totals = totals)
}

cat("Building tab ⑤ bins\n")
models <- setNames(lapply(views, build_one), vapply(views, `[[`, "", "id"))
obj <- list(
  meta = list(
    version = "v4.3",
    created = as.character(Sys.Date()),
    seed = SEED,
    n_bins_axis = NB, n_bins_fert = NF, n_bins_light = NL, nmin = NMIN,
    n = sapply(models, `[[`, "N"),
    sampling = paste("Latin hypercube over each tab's slider ranges",
                     "(seed 20261104). Rates, costs, yields and half-saturations",
                     "are log-uniform; fractions, soil P / fertility, γ, α and s are uniform."),
    caveat = paste("Shares depend on the chosen slider ranges; all parameters are",
                   "illustrative placeholders in relative units, not calibrated to data."),
    axes_note = paste("Axis-pair choices are limited to at most 8 parameters per view:",
                      "the fertility conditioner (S or K_P) and light L for ①, plus the",
                      "highest-importance parameters from the random-forest analysis.",
                      "Unordered pairs only (the app transposes when the dropdowns are swapped)."),
    headline = lapply(models, function(m) round(100 * m$totals / m$N, 1))
  ),
  models = models
)
out <- "../app/data/tab5_counts.rds"
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
saveRDS(obj, out, compress = "xz")
info <- file.info(out)
cat(sprintf("Wrote %s (%.1f KB, compress=xz)\n", out, info$size / 1024))
# also a small sidecar of headlines for the report
meta_out <- data.table(view = names(models), N = sapply(models, `[[`, "N"),
                       n_pairs = sapply(models, function(m) nrow(m$pairs)),
                       n_count_rows = sapply(models, function(m) nrow(m$counts)))
fwrite(meta_out, "csv/tab5_build_meta.csv")
print(obj$meta$headline)
print(meta_out)
if (info$size > 1.2 * 1024^2) warning("RDS is larger than ~1.2 MB; consider fewer pairs or bins")
