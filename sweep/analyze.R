# Summaries, variable importance, phase maps. Run from sweep/ after run_sweep.R:  Rscript analyze.R
suppressMessages({ library(data.table); library(ranger); library(rpart); library(ggplot2) })
set.seed(7)
RG <- as.data.frame(fread("csv/ranges.csv")); rng <- function(tb) data.table(RG[RG$tab == tb | (tb == "4" & RG$tab == "3"), ])
CLS <- c(mutualism = "#1a9850", commensalism = "#9e9e9e", parasitism = "#d7301f", "cannot persist" = "#333333",
         "no fungus persists" = "#333333", "no colonisation" = "#333333")
WIN <- c(mutualist = "#1a9850", coexistence = "#777777", cheater = "#d7301f", none = "#333333")
LAB <- c(S = "soil P, S", L = "light, L", Amax = "A_max", Y = "Y", theta = "θ", Kr = "K_r", c = "colonisation c", phi = "φ", kappa = "C cost κ",
         rho = "root suppression ρ", Kf = "K_f", U0 = "fungal P need U_0", n = "n", K = "fertility K_P", r = "r", g = "γ", a = "a_M", b = "b_M",
         d = "d_M", e = "e_M", g2 = "γ_Z", a1 = "a_M", a2 = "a_Z", b1 = "b_M", b2 = "b_Z", d1 = "d_M", d2 = "d_Z", e1 = "e_M", e2 = "e_Z",
         c1 = "α_MZ", c2 = "α_ZM", s = "sanction s", q0 = "q_0")
pct <- function(x, lev = NULL) { t <- table(factor(x, levels = if (is.null(lev)) sort(unique(x)) else lev)); round(100 * t / sum(t), 1) }
OUT <- list(); BIN <- list(); IMP <- list(); PRC <- list(); CC <- list(); RULES <- list()
uscale <- function(D, R) { U <- copy(D[, R$par, with = FALSE])
  for (j in seq_len(nrow(R))) { p <- R$par[j]; x <- U[[p]]
    set(U, j = p, value = if (R$scale[j] == "log") log(x / R$lo[j]) / log(R$hi[j] / R$lo[j]) else (x - R$lo[j]) / (R$hi[j] - R$lo[j])) }; U }
prcc <- function(X, y) { Rk <- as.data.frame(lapply(X, rank)); ry <- rank(y)
  sapply(names(Rk), function(v) { o <- setdiff(names(Rk), v); f <- as.matrix(cbind(1, Rk[, o]))
    e1 <- lm.fit(f, Rk[[v]])$residuals; e2 <- lm.fit(f, ry)$residuals; cor(e1, e2) }) }
summ <- function(tag, D, R, ycls, ycont, binvar, bins, lev, sub = NULL, extra_bin = NULL) {
  y <- D[[ycls]]
  OUT[[tag]] <<- data.table(design = tag, class = names(pct(y, lev)), pct = as.numeric(pct(y, lev)))
  .bb <- cut(D[[binvar]], bins, include.lowest = TRUE, right = FALSE); .yy <- factor(y, levels = lev)
  BIN[[tag]] <<- data.table(bin = .bb, class = .yy)[, .(n = .N), by = .(bin, class)][order(bin, class)][, pct := round(100 * n / sum(n), 1), by = bin][, `:=`(design = tag, binvar = binvar)][]
  if (!is.null(extra_bin)) { .b2 <- cut(D[[extra_bin$var]], extra_bin$bins, include.lowest = TRUE, right = FALSE)
    BIN[[paste0(tag, "_", extra_bin$var)]] <<- data.table(bin = .b2, class = factor(y, levels = lev))[, .(n = .N), by = .(bin, class)][order(bin, class)][, pct := round(100 * n / sum(n), 1), by = bin][, `:=`(design = tag, binvar = extra_bin$var)][] }
  U <- uscale(D, R); ii <- sample(nrow(D), min(30000, nrow(D)))
  rf <- ranger(x = as.data.frame(U[ii]), y = factor(y[ii]), num.trees = 300, importance = "permutation", num.threads = 8, seed = 1)
  imp <- sort(rf$variable.importance, decreasing = TRUE)
  IMP[[tag]] <<- data.table(design = tag, par = names(imp), symbol = LAB[names(imp)], importance = round(imp / max(imp), 3), oob_error = round(rf$prediction.error, 4))
  if (!is.null(ycont)) { ok <- is.finite(D[[ycont]]); if (!is.null(sub)) ok <- ok & sub; jj <- which(ok)[seq_len(min(30000, sum(ok)))]
    pr <- prcc(as.data.frame(U[jj]), D[[ycont]][jj]); PRC[[tag]] <<- data.table(design = tag, response = ycont, par = names(pr), symbol = LAB[names(pr)], prcc = round(pr, 3)) }
  CC[[tag]] <<- rbindlist(lapply(lev[lev %in% y], function(k) data.table(design = tag, class = k, par = R$par, symbol = LAB[R$par],
                           mean_u = round(colMeans(U[y == k]), 3))))
  tr <- rpart(factor(y) ~ ., data = cbind(as.data.frame(D[, R$par, with = FALSE]), y = y), control = rpart.control(maxdepth = 3, cp = 0.005))
  RULES[[tag]] <<- capture.output(print(tr, digits = 3))
  # figures: bars by fertility bin, and phase map of the two most important parameters
  B <- BIN[[tag]]; g <- ggplot(B, aes(bin, pct, fill = class)) + geom_col(width = 0.85) + scale_fill_manual(values = c(CLS, WIN), drop = TRUE) +
    labs(x = paste(LAB[[binvar]], "bin"), y = "% of parameter sets", fill = NULL, title = paste0(tag, ": outcome share by ", LAB[[binvar]])) + theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggsave(sprintf("figs/%s_by_%s.png", tag, binvar), g, width = 7, height = 4.2, dpi = 130)
  if (!is.null(extra_bin)) { B <- BIN[[paste0(tag, "_", extra_bin$var)]]
    g <- ggplot(B, aes(bin, pct, fill = class)) + geom_col(width = 0.85) + scale_fill_manual(values = c(CLS, WIN)) + theme_minimal(base_size = 11) + theme(legend.position = "bottom") +
      labs(x = paste(LAB[[extra_bin$var]], "bin"), y = "% of parameter sets", fill = NULL, title = paste0(tag, ": outcome share by ", LAB[[extra_bin$var]]))
    ggsave(sprintf("figs/%s_by_%s.png", tag, extra_bin$var), g, width = 7, height = 4.2, dpi = 130) }
  top <- names(imp)[1:2]; phase(tag, D, R, y, top[1], top[2])
  if (tag == "tab1") phase(tag, D, R, y, "S", "L")
  invisible(NULL)
}
phase <- function(tag, D, R, y, px, py, nb = 25) {
  rx <- R[R$par == px]; ry <- R[R$par == py]; U <- uscale(D, R)
  bx <- pmin(floor(U[[px]] * nb), nb - 1); byy <- pmin(floor(U[[py]] * nb), nb - 1)
  levs <- intersect(names(c(CLS, WIN)), unique(y))
  M <- data.table(bx, byy, y = factor(y, levels = levs))[, .N, by = .(bx, byy, y)]
  M <- M[CJ(bx = 0:(nb - 1), byy = 0:(nb - 1), y = factor(levs, levels = levs)), on = .(bx, byy, y)][is.na(N), N := 0][, share := N / sum(N), by = .(bx, byy)]
  M <- M[y %in% levs[sapply(levs, function(k) mean(y == k)) > 0.005]]   # drop classes < 0.5 % overall
  back <- function(u, r) if (r$scale == "log") r$lo * (r$hi / r$lo)^u else r$lo + u * (r$hi - r$lo)
  br <- c(0, 0.25, 0.5, 0.75, 1); lab <- function(r) signif(back(br, r), 2)
  M[, `:=`(x = (bx + 0.5) / nb, yv = (byy + 0.5) / nb)]
  M[, fillc := mapply(function(k, a) grDevices::adjustcolor(c(CLS, WIN)[[as.character(k)]], alpha.f = a), y, pmin(1, share))]
  g <- ggplot(M, aes(x, yv)) + geom_tile(aes(fill = fillc), width = 1 / nb, height = 1 / nb) + scale_fill_identity() +
    geom_text(data = M[bx %% 5 == 2 & byy %% 5 == 2], aes(label = round(100 * share)), size = 2.6, colour = "#222") +
    facet_wrap(~ y, nrow = 1) + coord_equal(expand = FALSE) +
    scale_x_continuous(breaks = br, labels = lab(rx)) + scale_y_continuous(breaks = br, labels = lab(ry)) +
    labs(x = paste0(LAB[[px]], if (rx$scale == "log") " (log scale)" else ""), y = paste0(LAB[[py]], if (ry$scale == "log") " (log scale)" else ""),
         title = sprintf("%s: share of each outcome over %s × %s", tag, LAB[[px]], LAB[[py]]),
         subtitle = "colour intensity = share of parameter sets in the cell (numbers: %), other parameters sampled over their ranges") +
    theme_minimal(base_size = 10) + theme(panel.grid = element_blank())
  ggsave(sprintf("figs/%s_phase_%s_%s.png", tag, px, py), g, width = 2.9 * length(unique(M$y)) + 1, height = 3.9, dpi = 130)
}
L3 <- c("mutualism", "commensalism", "parasitism")
# ---- tab 1 ----
D1 <- fread("csv/samples_tab1.csv"); D1[!is.finite(MGR), MGR := -50]
summ("tab1", D1, rng("1"), "class", "MGR", "S", c(0.5, 2, 5, 10, 20, 40, 60), c(L3, "no colonisation"), extra_bin = list(var = "L", bins = c(0.1, 0.3, 0.5, 0.7, 0.9, 1)))
OUT$tab1_U0zero <- data.table(design = "tab1 (same sets, U_0 = 0)", class = L3, pct = as.numeric(pct(D1$class_U0zero, L3)))
A1 <- fread("csv/samples_tab1_alluniform.csv"); OUT$tab1_alluniform <- data.table(design = "tab1 (all uniform)", class = L3, pct = as.numeric(pct(A1$class, L3)))
OUT$tab1_limAM <- D1[, .(pct = round(100 * .N / nrow(D1), 1)), by = .(class = paste0(class, " | AM ", limAM, "-limited"))][, design := "tab1 class × AM limitation"][]
# ---- tab 2 ----
D2 <- fread("csv/samples_tab2.csv"); L2 <- c(L3, "cannot persist"); per <- D2$eqClass != "cannot persist"
summ("tab2_eq", D2, rng("2"), "eqClass", "lnEq", "K", c(0.5, 2, 5, 10, 20, 40, 60), L2, sub = per, extra_bin = list(var = "r", bins = c(0.2, 0.5, 1, 2, 5)))
summ("tab2_pulse", D2, rng("2"), "pulseClass", "J12", "K", c(0.5, 2, 5, 10, 20, 40, 60), L2, sub = per)
OUT$tab2_cross <- D2[per, .(pct = round(100 * .N / sum(per), 1)), by = .(class = paste0("eq ", eqClass, " / pulse ", pulseClass))][, design := "tab2 eq × pulse (persisting only)"][]
A2 <- fread("csv/samples_tab2_alluniform.csv")
OUT$tab2_alluniform <- data.table(design = "tab2 eq (all uniform)", class = L2, pct = as.numeric(pct(A2$eqClass, L2)))
# ---- tabs 3/4 ----
L4 <- c(L3, "no fungus persists"); WL <- c("mutualist", "coexistence", "cheater", "none")
D3 <- fread("csv/samples_tab3.csv"); D4 <- fread("csv/samples_tab4.csv")
summ("tab3_plant", D3, rng("3"), "class", "lnH", "K", c(0.5, 2, 5, 10, 20, 40), L4)
summ("tab3_winner", D3, rng("3"), "winner", NULL, "K", c(0.5, 2, 5, 10, 20, 40), WL)
summ("tab4_plant", D4, rng("4"), "class", "lnH", "K", c(0.5, 2, 5, 10, 20, 40), L4)
summ("tab4_winner", D4, rng("4"), "winner", NULL, "K", c(0.5, 2, 5, 10, 20, 40), WL)
OUT$tab4_s0 <- data.table(design = "tab4 same sets, s = 0", class = L4, pct = as.numeric(pct(D4$class_s0, L4)))
OUT$tab4_s0w <- data.table(design = "tab4 winner, same sets, s = 0", class = WL, pct = as.numeric(pct(D4$winner_s0, WL)))
OUT$tab3_inv <- data.table(design = "tab3 invasion (both persist alone)", class = c("mutual invasion (coexist)", "only Z invades", "only M invades", "neither invades (bistable)"),
  pct = with(D3[Mok & Zok], round(100 * c(mean(Zinv & Minv), mean(Zinv & !Minv), mean(!Zinv & Minv), mean(!Zinv & !Minv)), 1)))
OUT$tab3_vsM <- data.table(design = "tab3 plant vs M-only reference (M persists alone)", class = c("community better than M alone", "worse"),
  pct = with(D3[Mok == TRUE], round(100 * c(mean(lnH > lnH_Monly + 1e-6), mean(lnH < lnH_Monly - 1e-6)), 1)))
d4 <- D4$lnH - D4$lnH_s0
OUT$tab4_eff <- data.table(design = "tab4 sanction effect on ln(H*/K_P), paired vs s = 0", class = c("raises (> +0.01)", "about the same", "lowers (< −0.01)"),
  pct = round(100 * c(mean(d4 > 0.01), mean(abs(d4) <= 0.01), mean(d4 < -0.01)), 1))
OUT$tab34_conv <- data.table(design = "ODE settling (relative residual > 1e-3 at t = 2000)", class = c("tab3", "tab4"), pct = round(100 * c(mean(D3$resid > 1e-3), mean(D4$resid > 1e-3)), 2))
# sanctions effect by s bin
S4 <- D4[, .(n = .N, raises = round(100 * mean(lnH - lnH_s0 > 0.01), 1), cheater_win = round(100 * mean(winner == "cheater"), 1), cheater_win_s0 = round(100 * mean(winner_s0 == "cheater"), 1),
             mutualism = round(100 * mean(class == "mutualism"), 1), mutualism_s0 = round(100 * mean(class_s0 == "mutualism"), 1)), by = .(s_bin = cut(s, c(0, .2, .4, .6, .8, 1), include.lowest = TRUE))][order(s_bin)]
fwrite(S4, "csv/tab4_sanction_effect_by_s.csv")
# ---- write ----
fwrite(rbindlist(OUT, fill = TRUE), "csv/summary_overall.csv"); fwrite(rbindlist(BIN, fill = TRUE), "csv/summary_by_bins.csv")
fwrite(rbindlist(IMP), "csv/importance_rf.csv"); fwrite(rbindlist(PRC), "csv/prcc.csv"); fwrite(rbindlist(CC), "csv/class_conditional_mean_u.csv")
writeLines(unlist(lapply(names(RULES), function(k) c(paste("####", k), RULES[[k]], ""))), "csv/rpart_rules.txt")
# importance + class-conditional heatmap figures
for (k in names(CC)) { C <- CC[[k]]; C$symbol <- factor(C$symbol, levels = rev(unique(IMP[[k]]$symbol)))
  g <- ggplot(C, aes(class, symbol, fill = mean_u - 0.5)) + geom_tile() + geom_text(aes(label = sprintf("%.2f", mean_u)), size = 3) +
    scale_fill_gradient2(low = "#4575b4", mid = "white", high = "#b2182b", limits = c(-0.5, 0.5), name = "mean scaled\nvalue − 0.5") +
    labs(x = NULL, y = "parameter (ordered by RF importance)", title = paste0(k, ": where in each range each outcome sits"), subtitle = "0 = bottom of slider range, 1 = top (log scale for log-sampled)") + theme_minimal(base_size = 11)
  ggsave(sprintf("figs/%s_drivers.png", k), g, width = 6.5, height = 0.35 * nrow(C) / length(unique(C$class)) + 2, dpi = 130) }
print(rbindlist(OUT, fill = TRUE)); print(rbindlist(lapply(IMP, head, 5)))
