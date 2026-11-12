# Parameter-space sweep of the three model families in the app (v4.2). Run from sweep/:  Rscript run_sweep.R
# Writes csv/samples_tab*.csv. Model code is sourced from ../app/R so the sweep uses exactly the app's equations.
suppressMessages({ library(lhs); library(parallel) })
source("../app/R/model_static.R"); source("../app/R/model_dyn.R")
EPS <- 0.05; set.seed(20261104)
lu <- function(u, a, b) exp(log(a) + u * (log(b) - log(a)))   # log-uniform
un <- function(u, a, b) a + u * (b - a)                          # uniform
cls3 <- function(v) ifelse(v > EPS, "mutualism", ifelse(v < -EPS, "parasitism", "commensalism"))
# ---------- ranges (= app slider ranges); scale: lin = uniform, log = log-uniform ----------
R1 <- data.frame(par = c("S","L","Amax","Y","theta","Kr","c","phi","kappa","rho","Kf","U0","n"),
  lo = c(0.5,0.1,0.2,0.2,0.2,2,0,0,0,0,0.5,0,1), hi = c(60,1,2,2,3,60,1,1,0.6,1,30,0.8,30),
  scale = c("lin","lin","log","log","log","log","lin","lin","lin","lin","log","lin","log"))
R2 <- data.frame(par = c("K","r","g","a","b","d","e"), lo = c(0.5,0.2,0,0.05,0.5,0.2,0.1), hi = c(60,5,10,1,6,3,20),
  scale = c("lin","log","lin","log","log","log","log"))
R3 <- data.frame(par = c("K","r","g","g2","a1","a2","b1","b2","d1","d2","e1","e2","c1","c2"),
  lo = c(0.5,0.2,0,0,0.05,0.05,0.5,0.5,0.2,0.2,0.1,0.1,0,0), hi = c(40,5,10,5,1,1,6,6,3,3,20,20,30,30),
  scale = c("lin","log","lin","lin","log","log","log","log","log","log","log","log","lin","lin"))
R4 <- rbind(R3, data.frame(par = c("s","q0"), lo = c(0,0.5), hi = c(1,30), scale = c("lin","log")))
write.csv(rbind(cbind(tab = "1", R1), cbind(tab = "2", R2), cbind(tab = "3", R3), cbind(tab = "4", R4[15:16, ])), "csv/ranges.csv", row.names = FALSE)
draw <- function(R, N, allLin = FALSE) { U <- randomLHS(N, nrow(R)); X <- as.data.frame(U); names(X) <- R$par
  for (j in seq_len(nrow(R))) X[[j]] <- if (R$scale[j] == "log" && !allLin) lu(U[, j], R$lo[j], R$hi[j]) else un(U[, j], R$lo[j], R$hi[j]); X }
# ---------- tab ①: static model (closed form) ----------
tab1 <- function(X) { X$n <- round(X$n); p <- modifyList(DEFAULTS, as.list(X)); r <- static_model(p)
  X$MGR <- r$MGR; X$class <- ifelse(r$noCol, "no colonisation", cls3(r$MGR)); X$limAM <- r$limAM
  r0 <- static_model(modifyList(p, list(U0 = 0))); X$class_U0zero <- ifelse(r0$noCol, "no colonisation", cls3(r0$MGR)); X }
N1 <- 1e5
write.csv(tab1(draw(R1, N1)), "csv/samples_tab1.csv", row.names = FALSE)
write.csv(tab1(draw(R1, N1, TRUE)), "csv/samples_tab1_alluniform.csv", row.names = FALSE)
# ---------- tab ②: N&F eq. 1, analytic equilibrium (vectorised eq2) ----------
tab2 <- function(X) { with(X, {
  A <- -a * g / r; B <- g - a * K / r - d * e / b; C <- K - d / b; ex <- C > 0
  P <- ifelse(abs(A) < 1e-12, -C / B, (-B - sqrt(pmax(B * B - 4 * A * C, 0))) / (2 * A)); P[!ex] <- 0
  H <- ifelse(ex, d / b * (1 + e * P), K)
  J12 <- r * g * H^2 / (K + g * P)^2 - a * H; lnEq <- log(H / K)
  X$Mstar <- P; X$Hstar <- H; X$lnEq <- lnEq; X$J12 <- J12
  X$eqClass <- ifelse(ex, cls3(lnEq), "cannot persist"); X$pulseClass <- ifelse(ex, ifelse(J12 > 0, "mutualism", "parasitism"), "cannot persist")
  X }) }
N2 <- 1e5
write.csv(tab2(draw(R2, N2)), "csv/samples_tab2.csv", row.names = FALSE)
write.csv(tab2(draw(R2, N2, TRUE)), "csv/samples_tab2_alluniform.csv", row.names = FALSE)
# ---------- tabs ③/④: cheater + sanctions, ODE (app integrator) to t = 2000 from (H, M, Z) = (1, 0.1, 0.1) ----------
TEND <- 2000
ode_par <- function(X) { ch <- split(seq_len(nrow(X)), cut(seq_len(nrow(X)), 8, labels = FALSE))
  out <- mclapply(ch, function(ii) { p <- as.list(X[ii, ]); if (is.null(p$s)) { p$s <- 0; p$q0 <- 1 }
    f <- simulate_final_vec(p, c(1, 0.1, 0.1), TEND, 0.1)
    rr <- rhs3(p, f$H, f$P, f$Q)
    res <- pmax(abs(rr[[1]]) / (f$H + 1e-9), abs(rr[[2]]) / (f$P + 1e-6), abs(rr[[3]]) / (f$Q + 1e-6))
    data.frame(H = f$H, M = f$P, Z = f$Q, resid = res) }, mc.cores = 8)
  do.call(rbind, out) }
tab34 <- function(X, sanc) { t0 <- Sys.time(); o <- ode_par(X); Y <- cbind(X, o)
  Y$lnH <- log(Y$H / Y$K); Y$winner <- winner(Y$M, Y$Z)
  Y$class <- ifelse(Y$winner == "none", "no fungus persists", cls3(Y$lnH))
  # analytic invasion criteria (N&F App. A.2), vectorised
  eqv <- function(K, g, r, a, b, d, e) { A <- -a * g / r; B <- g - a * K / r - d * e / b; C <- K - d / b
    P <- ifelse(abs(A) < 1e-12, -C / B, (-B - sqrt(pmax(B * B - 4 * A * C, 0))) / (2 * A)); P[C <= 0] <- 0
    list(H = ifelse(C > 0, d / b * (1 + e * P), K), ok = C > 0) }
  eP <- with(Y, eqv(K, g, r, a1, b1, d1, e1)); eQ <- with(Y, eqv(K, g2, r, a2, b2, d2, e2))
  lP1 <- with(Y, (b1 / d1 * eP$H - 1) / e1); lP2 <- with(Y, (b2 / d2 * eP$H - 1) / e2)
  lQ1 <- with(Y, (b1 / d1 * eQ$H - 1) / e1); lQ2 <- with(Y, (b2 / d2 * eQ$H - 1) / e2)
  Y$Mok <- eP$ok; Y$Zok <- eQ$ok
  Y$Zinv <- eP$ok & eQ$ok & Y$c2 < Y$e2 * lP2 / lP1; Y$Minv <- eP$ok & eQ$ok & Y$c1 < Y$e1 * lQ1 / lQ2
  Y$lnH_Monly <- log(eP$H / Y$K)
  cat(sprintf("ODE %d cells (%s): %.1f min, resid>1e-3: %d\n", nrow(Y), if (sanc) "sanctions" else "s = 0", as.numeric(difftime(Sys.time(), t0, units = "mins")), sum(Y$resid > 1e-3)))
  Y }
N3 <- 40000
write.csv(tab34(draw(R3, N3), FALSE), "csv/samples_tab3.csv", row.names = FALSE)
X4 <- draw(R4, N3); Y4 <- tab34(X4, TRUE)
Y40 <- tab34(transform(X4, s = 0), FALSE)       # same parameter sets without sanctions (paired)
Y4$lnH_s0 <- Y40$lnH; Y4$class_s0 <- Y40$class; Y4$winner_s0 <- Y40$winner
write.csv(Y4, "csv/samples_tab4.csv", row.names = FALSE)
cat("done\n")
