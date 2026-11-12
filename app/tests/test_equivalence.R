# R port vs v3 JavaScript (tests/js_reference.json, made by tests/make_js_reference.js) and Python (py_points.json).
# Run from app/:  Rscript tests/test_equivalence.R
for (f in c("R/model_static.R", "R/model_dyn.R")) source(f)
ref <- jsonlite::fromJSON("tests/js_reference.json", simplifyVector = TRUE)
pass <- 0; fail <- 0
ok <- function(c, msg) { if (isTRUE(c)) pass <<- pass + 1 else { fail <<- fail + 1; cat("FAIL:", msg, "\n") } }
md <- function(a, b) { a <- unlist(a); b <- unlist(b); b[is.na(b) & is.infinite(a)] <- a[is.na(b) & is.infinite(a)]  # JSON null = Inf
  d <- abs(a - b); d[a == b] <- 0; max(d) }
# Tolerances: the JS and R integrators run the same algorithm, but the adaptive step-size update uses pow(), whose
# last-bit rounding differs between V8 and R/libm; a 1-ulp change in h can flip an accept/reject decision, so step
# sequences drift apart slightly. Final states then agree to ~1e-6 (the integrator tolerance, rtol = 1e-6), and
# interpolated trajectory samples to ~1e-3. Closed-form quantities agree to rounding.
TOL_FINAL <- 1e-5; TOL_TRAJ <- 2e-3
# 1 static (12 MGR points from Python + full JS outputs)
st <- ref$static; dmax_py <- 0; dmax_js <- 0
for (i in seq_len(nrow(st))) {
  r <- static_model(modifyList(DEFAULTS, list(U0 = 0, S = st$S[i], L = st$L[i], kappa = st$kappa[i])))
  dmax_py <- max(dmax_py, abs(r$MGR - st$py_mgr[i]))
  dmax_js <- max(dmax_js, md(c(r$MGR, r$dF, r$T_eff, r$B_AM, r$B_NM, r$benefit, r$costG),
                            c(st$MGR[i], st$dF[i], st$T_eff[i], st$B_AM[i], st$B_NM[i], st$benefit[i], st$costG[i])))
  ok(classify(r$MGR, 0.05) == st$py_cls[i] && r$limAM == st$limAM[i] && r$limNM == st$limNM[i] &&
     classify2D(r$MGR, r$dF, 0.05) == st$cls2D[i], paste("static classes", i))
}
cat(sprintf("Static: %d points, max |MGR_R - MGR_py| = %.2e, max |R - JS| over MGR,dF,T_eff,B,benefit,cost = %.2e\n", nrow(st), dmax_py, dmax_js))
ok(dmax_py < 1e-12 && dmax_js < 1e-12, "static equivalence")
# 2 Table 2 / Table 1
T2 <- modifyList(CH_DEFAULTS, list(c1 = 1.65, c2 = 0.6128, s = 0))
paper2 <- list(c(2.2094, 1.1525, 2.7125), c(2.1995, 0.887, 2.8556), c(2.6667, 0, 4.3333))
for (i in 1:3) { K <- ref$table2$K[i]; f <- simulate(modifyList(T2, list(K = K)), c(1, .1, .1), 1500, 0.05)$final
  d <- md(f, ref$table2$final[[i]])
  cat(sprintf("Table 2 K_P=%g: R H,M,Z = %s | paper %s | |R-JS| = %.1e\n", K, paste(sprintf("%.4f", f), collapse = ", "), paste(paper2[[i]], collapse = ", "), d))
  ok(d < TOL_FINAL && md(f, paper2[[i]]) < 2e-3, paste("Table 2", K)) }
f <- simulate(modifyList(T2, list(c1 = 1.4, K = 20)), c(1, .1, .1), 3000, 0.05)$final; d <- md(f, ref$table1)
cat(sprintf("Table 1 K_P=20: R H,M,Z = %s | paper 2.4508, 6.2652, 0.0623 | |R-JS| = %.1e\n", paste(sprintf("%.4f", f), collapse = ", "), d))
ok(d < TOL_FINAL, "Table 1")
# 3 analytic pieces
e1 <- eq2(2, 5, 2, 0.3, 3, 1, 1); e2 <- eq2(2, 0, 2, 0.4, 2, 1, 1)
ok(md(c(e1$H, e1$P, e2$H, e2$P), c(ref$eq2$H, ref$eq2$P)[c(1, 3, 2, 4)]) < 1e-12, "eq2")
for (i in 1:3) { c2 <- criteria2(modifyList(DYN_DEFAULTS, list(K = ref$criteria$K[i])))
  ok(md(c(c2$H, c2$P, c2$J12, c2$lnEq, c2$Kthr), c(ref$criteria$H[i], ref$criteria$P[i], ref$criteria$J12[i], ref$criteria$lnEq[i], ref$criteria$Kthr[i])) < 1e-12 &&
     c2$eqClass == ref$criteria$eqClass[i] && c2$pulseClass == ref$criteria$pulseClass[i] && c2$sink == ref$criteria$sink[i], paste("criteria", i)) }
cat("Dyn defaults (eq/pulse) K_P = 2, 8, 40:", sapply(c(2, 8, 40), function(K) { q <- criteria2(modifyList(DYN_DEFAULTS, list(K = K))); paste0(substr(q$eqClass, 1, 1), "/", substr(q$pulseClass, 1, 1)) }), "\n")
iv <- invasion(T2); ok(abs(iv$QinvThr - ref$invasion$QinvThr) < 1e-12 && abs(iv$PinvThr - ref$invasion$PinvThr) < 1e-12 &&
  iv$Qinvades == ref$invasion$Qinvades && iv$Pinvades == ref$invasion$Pinvades, "invasion")
cat(sprintf("Invasion thresholds (Table 2): Z invades iff α_ZM < %.4f, M invades iff α_MZ < %.4f\n", iv$QinvThr, iv$PinvThr))
# 4 trajectories
dy <- simulate(as3(DYN_DEFAULTS), c(1, .5, 0), 40, 0.01, 500); d1 <- md(c(dy$H, dy$P), c(ref$dynTraj$H, ref$dynTraj$P))
ch <- simulate(CH_DEFAULTS, c(1, .1, .1), 300, 0.05, 600); d2 <- md(c(ch$H, ch$P, ch$Q), c(ref$cheatTraj$H, ref$cheatTraj$P, ref$cheatTraj$Q))
cat(sprintf("Trajectories: dyn (501 pts) max |R-JS| = %.1e, steps %d/%d; cheater (601 pts) max |R-JS| = %.1e, steps %d/%d\n",
            d1, dy$steps, ref$dynTraj$steps, d2, ch$steps, ref$cheatTraj$steps))
ok(d1 < TOL_TRAJ && d2 < TOL_TRAJ && abs(dy$steps - ref$dynTraj$steps) <= 10 && abs(ch$steps - ref$cheatTraj$steps) <= 10, "trajectories")
ok(abs(dy$final[1] - criteria2(DYN_DEFAULTS)$H) < 1e-3, "dyn final close to analytic equilibrium")
# 5 sanctions thresholds (minimum s that stops cheater exclusion), expected 0.17, 0.20, 0.21, 0.22
SA <- modifyList(CH_DEFAULTS, list(c1 = 2.0, c2 = 0.3, q0 = 5))
win <- function(p) { f <- simulate(p, c(1, .1, .1), 400, 0.05, 2)$final; winner(f[2], f[3]) }
sneed <- sapply(c(5, 10, 20, 40), function(K) { for (s in seq(0, 1, by = 0.01)) if (win(modifyList(SA, list(K = K, s = s))) != "cheater") return(s); NA })
cat("Sanctions: minimum s that stops cheater exclusion at K_P = 5, 10, 20, 40:", sprintf("%.2f", sneed), "| JS:", sprintf("%.2f", ref$sneed), "\n")
ok(all(abs(sneed - c(0.17, 0.20, 0.21, 0.22)) < 1e-9) && all(abs(sneed - ref$sneed) < 1e-9), "sanction thresholds")
# 6 map cells: scalar R, vectorised R and JS
SAD <- SA_DEFAULTS; mc <- ref$saMapCells
vec <- simulate_final_vec(modifyList(SAD, list(K = mc$K, s = mc$s)), c(1, .1, .1), 300, 0.1)
ds <- 0; dv <- 0; dsv <- 0
for (i in seq_len(nrow(mc))) { f <- simulate(modifyList(SAD, list(K = mc$K[i], s = mc$s[i])), c(1, .1, .1), 300, 0.1, 2)$final
  ds <- max(ds, md(f, mc$final[[i]])); dv <- max(dv, md(c(vec$H[i], vec$P[i], vec$Q[i]), mc$final[[i]])); dsv <- max(dsv, md(f, c(vec$H[i], vec$P[i], vec$Q[i]))) }
sw <- simulate_final_vec(modifyList(CH_DEFAULTS, list(K = ref$chSweep$K)), c(1, .1, .1), 400, 0.1)
dsw <- md(cbind(sw$H, sw$P, sw$Q), do.call(rbind, ref$chSweep$final))
cat(sprintf("Sanctions map cells (16 K_P×s): scalar R vs JS %.1e, vectorised R vs JS %.1e, vectorised vs scalar R %.1e; K_P sweep (vectorised) vs JS %.1e\n", ds, dv, dsv, dsw))
ok(ds < TOL_FINAL && dv < TOL_FINAL && dsw < TOL_FINAL, "map cells vs JS"); ok(dsv == 0, "vectorised integrator bit-identical to scalar R")
cat(sprintf("\nR equivalence tests: %d passed, %d failed\n", pass, fail))
if (fail) quit(status = 1)
