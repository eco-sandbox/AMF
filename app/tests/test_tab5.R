# Tab ⑤ aggregation: full-band shares equal the sweep headlines; conditioning sums bins.
# Run from shiny/:  Rscript tests/test_tab5.R
source("R/tab5.R")
pass <- 0L; fail <- 0L
ok <- function(c, msg) { if (isTRUE(c)) pass <<- pass + 1L else { fail <<- fail + 1L; cat("FAIL:", msg, "\n") } }
stopifnot(!is.null(TAB5))
# headlines from the sweep report (1 decimal), derived from the same samples
expect <- list(
  tab1        = c(mutualism = 3.5, commensalism = 27.6, parasitism = 68.9),
  tab2_eq     = c(mutualism = 10.9, commensalism = 2.5, parasitism = 86.0, `cannot persist` = 0.7),
  tab2_pulse  = c(mutualism = 4.6, parasitism = 94.8, `cannot persist` = 0.7),
  tab3_plant  = c(mutualism = 9.4, commensalism = 1.7, parasitism = 88.8, `no fungus persists` = 0.2),
  tab3_winner = c(mutualist = 49.3, coexistence = 1.1, cheater = 49.4, none = 0.2),
  tab4_plant  = c(mutualism = 10.0, commensalism = 1.6, parasitism = 88.2, `no fungus persists` = 0.2),
  tab4_winner = c(mutualist = 52.9, coexistence = 1.2, cheater = 45.7, none = 0.2)
)
exact <- list(
  tab1        = c(mutualism = 3518, commensalism = 27561, parasitism = 68921),
  tab2_eq     = c(mutualism = 10903, commensalism = 2451, parasitism = 85985, `cannot persist` = 661),
  tab2_pulse  = c(mutualism = 4557, parasitism = 94782, `cannot persist` = 661),
  tab3_plant  = c(mutualism = 3741, commensalism = 671, parasitism = 35513, `no fungus persists` = 75),
  tab3_winner = c(mutualist = 19728, coexistence = 446, cheater = 19751, none = 75),
  tab4_plant  = c(mutualism = 4017, commensalism = 623, parasitism = 35276, `no fungus persists` = 84),
  tab4_winner = c(mutualist = 21172, coexistence = 472, cheater = 18272, none = 84)
)
for (id in names(expect)) {
  m <- TAB5$models[[id]]
  r <- m$ranges[m$ranges$par == m$fert, ]
  llo <- lhi <- NULL
  if (!is.null(m$light) && !is.null(m$breaks[[m$light]])) {
    br <- m$breaks[[m$light]]; llo <- br[1]; lhi <- br[length(br)]
  }
  sl <- tab5_slice(TAB5, id, m$def_x, m$def_y, r$lo[1], r$hi[1], llo, lhi)
  ok(sl$N == m$N, sprintf("%s: full band N = N_total (%d)", id, sl$N))
  got <- round(100 * sl$share / sl$N, 1)
  for (cl in names(expect[[id]])) {
    ok(isTRUE(all.equal(unname(got[cl]), expect[[id]][[cl]], tolerance = 1e-8)),
       sprintf("%s %s: %.1f vs headline %.1f", id, cl, got[[cl]], expect[[id]][[cl]]))
    ok(isTRUE(sl$share[[cl]] == exact[[id]][[cl]]),
       sprintf("%s %s exact count %s vs %s", id, cl, sl$share[[cl]], exact[[id]][[cl]]))
  }
  ok(abs(sum(sl$share) - sl$N) < 0.5, sprintf("%s: shares sum to N", id))
  ok(sum(sl$cells$n) == sl$N, sprintf("%s: pair cells sum to N", id))
}
# overlap: full range includes every bin
br <- TAB5$models$tab1$breaks$S
ok(identical(tab5_bins_overlap(br, br[1], br[length(br)]), seq_len(length(br) - 1L)),
   "full S range covers all fertility bins")
# conditioning: first fertility bin of tab1 (S ≈ 0.5–6.45) has fewer sets, shares still sum to 100%
m <- TAB5$models$tab1
br <- m$breaks$S
slc <- tab5_slice(TAB5, "tab1", "c", "kappa", br[1], br[2] - 1e-6, m$breaks$L[1], m$breaks$L[length(m$breaks$L)])
ok(slc$N > 0 && slc$N < m$N, sprintf("conditioned N = %d is between 0 and %d", slc$N, m$N))
ok(abs(sum(slc$share) - slc$N) < 0.5, "conditioned shares sum to N")
ok(length(slc$fb) >= 1L && slc$fb[1] == 1L, "first bin is included in a low-S band")
# a mid-range band should not include the lowest and highest bins
slm <- tab5_slice(TAB5, "tab1", "c", "kappa", 20, 30, 0.1, 1)
ok(!1L %in% slm$fb && !10L %in% slm$fb, "mid S band excludes end bins")
# colour helpers: sequential scale and low-n greying (majority + single-class)
ok(identical(toupper(ps_hex_seq("#1a9850", 0)), "#FFFFFF"), "sequential share 0 is white")
ok(identical(tolower(ps_hex_seq("#1a9850", 1)), "#1a9850"), "sequential share 1 is mutualism green")
ok(identical(tolower(ps_hex_seq("#d7301f", 1)), "#d7301f"), "sequential share 1 is parasitism red")
ok(identical(tolower(ps_hex_seq("#e6a817", 1)), "#e6a817"), "sequential share 1 is coexistence amber")
ok(identical(tolower(ps_hex_seq("#555555", 1)), "#555555"), "sequential share 1 is cannot-persist grey")
cts <- c(mutualism = 10, commensalism = 20, parasitism = 70)
ok(identical(tolower(ps_cell_paint(100, cts, "majority")$col),
             tolower(ps_hex_mix("#d7301f", 0.7))), "majority paints the winning class")
ok(identical(tolower(ps_cell_paint(100, cts, "mutualism")$col),
             tolower(ps_hex_seq("#1a9850", 0.1))), "single-class paints that class share")
ok(isTRUE(all.equal(ps_cell_paint(100, cts, "mutualism")$share, 0.1)), "single-class share is n_class/n")
ok(identical(ps_cell_paint(10, cts, "majority")$col, "#d0d0d0"), "majority greys low-n cells")
ok(identical(ps_cell_paint(10, cts, "mutualism")$col, "#d0d0d0"), "single-class greys low-n cells")
ok(identical(ps_cell_paint(0, cts, "mutualism")$col, "#f3f3f3"), "empty cells stay blank")
# mode lists follow the view's classes
m1 <- ps_map_modes(TAB5$models$tab1)
ok(identical(unname(m1[1]), "majority") && identical(names(m1)[1], "Majority class"),
   "first map mode is Majority class")
ok(all(c("mutualism", "commensalism", "parasitism") %in% m1) && !("cheater" %in% m1),
   "tab ① modes are the physiology classes")
mw <- ps_map_modes(TAB5$models$tab3_winner)
ok(all(c("majority", "mutualist", "cheater", "coexistence") %in% mw),
   "who-wins modes include mutualist, cheater, coexistence")
ok("cannot persist" %in% ps_map_modes(TAB5$models$tab2_eq),
   "② eq exposes cannot persist")
ok("no fungus persists" %in% ps_map_modes(TAB5$models$tab3_plant),
   "③ plant exposes no fungus persists")
# a well-sampled cell on the default ① map matches the sequential colour and hover
slm1 <- tab5_slice(TAB5, "tab1", "c", "kappa", m$ranges[m$ranges$par == "S", "lo"][1],
                   m$ranges[m$ranges$par == "S", "hi"][1], m$breaks$L[1], m$breaks$L[length(m$breaks$L)])
gg <- ps_map_grids(slm1, "mutualism")
ok(isTRUE(gg$single) && identical(gg$mode, "mutualism"), "grids record single-class mode")
ij <- which(gg$n == max(gg$n), arr.ind = TRUE)[1, ]
iy <- ij[[1]]; ix <- ij[[2]]
sub <- slm1$cells[slm1$cells$bx == ix & slm1$cells$yb == iy, , drop = FALSE]
nn <- sum(sub$n)
nmut <- sum(sub$n[sub$cl == match("mutualism", slm1$m$classes)])
ok(nn >= gg$nmin, sprintf("busiest cell has n = %d", nn))
ok(identical(tolower(gg$zcol[iy, ix]), tolower(ps_hex_seq("#1a9850", nmut / nn))),
   "busiest cell colour follows mutualism share")
ok(grepl("mutualism:", gg$hover[iy, ix], fixed = TRUE) && grepl(sprintf("n = %d", nn), gg$hover[iy, ix], fixed = TRUE),
   "single-class hover shows share and n")
gm <- ps_map_grids(slm1, "majority")
ok(isFALSE(gm$single), "majority grids are not single-class")
# low-n greying on the real grid if any such cell exists
low <- which(gg$n > 0 & gg$n < gg$nmin, arr.ind = TRUE)
if (nrow(low)) {
  ok(all(gg$zcol[low] == "#d0d0d0") && all(gm$zcol[low] == "#d0d0d0"),
     "real low-n cells are grey in both modes")
} else {
  ok(TRUE, "no low-n cells on the full-band ① map (greying checked via ps_cell_paint)")
}
cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1)
