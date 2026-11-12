# ---------- Tabs ②–④: Neuhauser & Fargione (2004) eq. 1 / eq. 2 + sanctions (our addition) ----------
# Port of v3 <script id="model2">. Internal keys keep the paper's letters (as in v3); displayed notation:
#   state (H, P, Q) -> H, M (mutualist), Z (cheater); K -> K_P; g -> γ; g2 -> γ_Z;
#   a1,b1,d1,e1 -> a_M,b_M,d_M,e_M; a2,b2,d2,e2 -> a_Z,b_Z,d_Z,e_Z; c1 -> α_MZ (effect of Z on M); c2 -> α_ZM;
#   2-species keys a,b,d,e -> a_M,b_M,d_M,e_M; H0,P0,Q0 -> H_0,M_0,Z_0; T -> t_max; phiC -> φ_Z; q0 -> q_0.
# deSolve is not available for webR, so the adaptive RK4 integrator of v3 is ported 1:1.
DYN_DEFAULTS <- list(K = 2, g = 5, r = 2, a = 0.3, b = 3, d = 1, e = 10, H0 = 1, P0 = 0.5, T = 40, eps = 0.05)
CH_DEFAULTS <- list(K = 2, g = 5, g2 = 0, r = 2, a1 = 0.3, a2 = 0.4, b1 = 3, b2 = 2, d1 = 1, d2 = 1, e1 = 1, e2 = 1,
                    c1 = 1.65, c2 = 0.6128, H0 = 1, P0 = 0.1, Q0 = 0.1, T = 300, phiC = 0.1, s = 0, q0 = 0.5, eps = 0.05)
SA_DEFAULTS <- modifyList(CH_DEFAULTS, list(K = 20, c1 = 2.0, c2 = 0.3, s = 0.3, q0 = 5, T = 400))
nz <- function(x) if (is.null(x)) 0 else x

# sanction weights w_M, w_Z (vectorised); s = 0 -> 1, 1
sanctionW <- function(p, P, Q) {
  X <- P + Q; s <- p$s; g2 <- nz(p$g2)
  q1 <- p$g + p$q0; q2 <- g2 + p$q0; qbar <- (P * q1 + Q * q2) / X
  on <- (s > 0) & (X > 0)
  w1 <- ifelse(on, (1 - s) + s * q1 / qbar, 1); w2 <- ifelse(on, (1 - s) + s * q2 / qbar, 1)
  list(w1, w2)
}
rhs3 <- function(p, H, P, Q) {
  H <- pmax(H, 0); P <- pmax(P, 0); Q <- pmax(Q, 0)
  w <- if (all(p$s == 0)) list(1, 1) else sanctionW(p, P, Q)
  list(p$r * H * (1 - H / (p$K + p$g * P + nz(p$g2) * Q)) - p$a1 * w[[1]] * P * H - p$a2 * w[[2]] * Q * H,
       p$b1 * w[[1]] * H * P - p$d1 * P * (1 + p$e1 * P + p$c1 * Q),
       p$b2 * w[[2]] * H * Q - p$d2 * Q * (1 + p$e2 * Q + p$c2 * P))
}
as3 <- function(p) list(K = p$K, g = p$g, g2 = 0, r = p$r, a1 = p$a, a2 = 0, b1 = p$b, b2 = 0, d1 = p$d, d2 = 1,
                        e1 = p$e, e2 = 1, c1 = 0, c2 = 0, s = 0, q0 = 1)
rk4step <- function(p, y, dt) {   # y = list(H, P, Q); dt scalar or vector
  k1 <- rhs3(p, y[[1]], y[[2]], y[[3]])
  k2 <- rhs3(p, y[[1]] + 0.5 * dt * k1[[1]], y[[2]] + 0.5 * dt * k1[[2]], y[[3]] + 0.5 * dt * k1[[3]])
  k3 <- rhs3(p, y[[1]] + 0.5 * dt * k2[[1]], y[[2]] + 0.5 * dt * k2[[2]], y[[3]] + 0.5 * dt * k2[[3]])
  k4 <- rhs3(p, y[[1]] + dt * k3[[1]], y[[2]] + dt * k3[[2]], y[[3]] + dt * k3[[3]])
  lapply(1:3, function(i) y[[i]] + dt / 6 * (k1[[i]] + 2 * k2[[i]] + 2 * k3[[i]] + k4[[i]]))
}
# Adaptive RK4 (step doubling + Richardson extrapolation), trajectory sampled at nout+1 even times.
simulate <- function(p, y0, T, dt0 = 0.05, nout = 400, rtol = 1e-6, atol = 1e-9) {
  y <- as.numeric(y0); t <- 0; h <- min(dt0, T); rawMin <- Inf; steps <- 0
  ts <- T * (0:nout) / nout
  oH <- numeric(nout + 1); oP <- oH; oQ <- oH; oH[1] <- y[1]; oP[1] <- y[2]; oQ[1] <- y[3]; k <- 2
  while (t < T - 1e-12 && steps < 2e5) {
    h <- min(h, T - t)
    yl <- list(y[1], y[2], y[3])
    big <- unlist(rk4step(p, yl, h)); half <- unlist(rk4step(p, rk4step(p, yl, h / 2), h / 2))
    err <- max(abs(half - big) / (atol + rtol * pmax(abs(y), abs(half)))) / 15
    if (err <= 1 || h < 1e-8) {
      yn <- half + (half - big) / 15
      rawMin <- min(rawMin, yn)
      ycl <- pmax(yn, 0); tn <- t + h
      while (k <= nout + 1 && ts[k] <= tn + 1e-12) {
        w <- (ts[k] - t) / h
        oH[k] <- y[1] + w * (ycl[1] - y[1]); oP[k] <- y[2] + w * (ycl[2] - y[2]); oQ[k] <- y[3] + w * (ycl[3] - y[3]); k <- k + 1
      }
      y <- ycl; t <- tn; steps <- steps + 1
      h <- h * min(4, max(1.2, 0.9 * max(err, 1e-10)^(-0.2)))
    } else h <- h * max(0.1, 0.9 * err^(-0.25))
  }
  while (k <= nout + 1) { oH[k] <- y[1]; oP[k] <- y[2]; oQ[k] <- y[3]; k <- k + 1 }
  list(t = ts, H = oH, P = oP, Q = oQ, final = y, rawMin = rawMin, steps = steps)
}
# Same integrator, vectorised over many parameter sets at once (each cell has its own t and step size h):
# used for the K_P sweep and the sanctions × K_P maps. Returns final states only. Arithmetic per cell is
# identical to simulate(). `pv` = list of parameters, each scalar or a vector of length n.
simulate_final_vec <- function(pv, y0, T, dt0 = 0.1, rtol = 1e-6, atol = 1e-9, progress = NULL) {
  n <- max(lengths(pv)); pv <- lapply(pv, function(v) if (length(v) == 1) rep(v, n) else v)
  H <- rep_len(y0[[1]], n); P <- rep_len(y0[[2]], n); Q <- rep_len(y0[[3]], n)   # y0: 3 numbers, or a list of 3 per-cell vectors
  t <- rep(0, n); h <- rep(min(dt0, T), n); steps <- rep(0, n); rawMin <- Inf
  act <- which(t < T - 1e-12); it <- 0
  while (length(act)) {
    it <- it + 1
    pa <- lapply(pv, `[`, act)
    ha <- pmin(h[act], T - t[act])
    yl <- list(H[act], P[act], Q[act])
    big <- rk4step(pa, yl, ha); half <- rk4step(pa, rk4step(pa, yl, ha / 2), ha / 2)
    err <- pmax(abs(half[[1]] - big[[1]]) / (atol + rtol * pmax(abs(yl[[1]]), abs(half[[1]]))),
                abs(half[[2]] - big[[2]]) / (atol + rtol * pmax(abs(yl[[2]]), abs(half[[2]]))),
                abs(half[[3]] - big[[3]]) / (atol + rtol * pmax(abs(yl[[3]]), abs(half[[3]])))) / 15
    acc <- err <= 1 | ha < 1e-8
    ia <- act[acc]
    if (length(ia)) {
      yn <- lapply(1:3, function(i) half[[i]][acc] + (half[[i]][acc] - big[[i]][acc]) / 15)
      rawMin <- min(rawMin, unlist(yn))
      H[ia] <- pmax(yn[[1]], 0); P[ia] <- pmax(yn[[2]], 0); Q[ia] <- pmax(yn[[3]], 0)
      t[ia] <- t[ia] + ha[acc]; steps[ia] <- steps[ia] + 1
      ea <- err[acc]; h[ia] <- ha[acc] * pmin(4, pmax(1.2, 0.9 * pmax(ea, 1e-10)^(-0.2)))
    }
    ir <- act[!acc]
    if (length(ir)) h[ir] <- ha[!acc] * pmax(0.1, 0.9 * err[!acc]^(-0.25))
    act <- act[t[act] < T - 1e-12 & steps[act] < 2e5]
    if (!is.null(progress) && it %% 25 == 0) progress(1 - length(act) / n)
  }
  list(H = H, P = P, Q = Q, rawMin = rawMin, steps = steps)
}
# Analytic interior equilibrium of eq. (1)
eq2 <- function(K, g, r, a, b, d, e) {
  A <- -a * g / r; B <- g - a * K / r - d * e / b; C <- K - d / b
  if (C <= 0) return(list(H = K, P = 0, exists = FALSE))
  P <- if (abs(A) < 1e-12) -C / B else (-B - sqrt(B * B - 4 * A * C)) / (2 * A)
  list(H = (d / b) * (1 + e * P), P = P, exists = TRUE)
}
criteria2 <- function(p) {
  q <- eq2(p$K, p$g, p$r, p$a, p$b, p$d, p$e); H <- q$H; P <- q$P
  vertex <- list(H = p$K / 2 + p$g * p$r / (4 * p$a) + p$a * p$K^2 / (4 * p$r * p$g), P = p$r / (2 * p$a) - p$K / (2 * p$g))
  # exact rules on the host isocline: pulse-mutualistic iff M* < vertex height r/2a_M − K_P/2γ;
  # equilibrium-mutualistic (H* > K_P) iff M* < r/a_M − K_P/γ (= 2 × vertex height)
  thr <- list(pulse = vertex$P, eq = p$r / p$a - p$K / p$g)
  if (!q$exists)   # K_P ≤ d_M/b_M: the fungus cannot persist, no interaction to classify
    return(list(H = H, P = P, J12 = NA_real_, lnEq = 0, vertex = vertex, thr = thr, Kthr = p$g * p$r / p$a, Kmin = p$d / p$b, exists = FALSE,
                eqClass = "no symbiont", pulseClass = "no symbiont", sink = NA_character_))
  J12 <- p$r * p$g * H * H / (p$K + p$g * P)^2 - p$a * H
  lnEq <- log(H / p$K)
  J11 <- -p$r * H / (p$K + p$g * P); J21 <- p$b * P; J22 <- -p$d * p$e * P
  disc <- (J11 + J22)^2 - 4 * (J11 * J22 - J12 * J21)
  list(H = H, P = P, J12 = J12, lnEq = lnEq, vertex = vertex, thr = thr, Kthr = p$g * p$r / p$a, Kmin = p$d / p$b, exists = q$exists,
       eqClass = if (lnEq > p$eps) "mutualism" else if (lnEq < -p$eps) "parasitism" else "commensalism",
       pulseClass = if (J12 > 0) "mutualism" else "parasitism", sink = if (disc >= 0) "nodal sink" else "spiral sink")
}
# K_P where each criterion switches sign (M* crosses its threshold), other parameters fixed; NA if no switch in (d_M/b_M, Kmax]
switch_points <- function(p, Kmax = 60) {
  f <- function(K, w) { q <- eq2(K, p$g, p$r, p$a, p$b, p$d, p$e)
    q$P - if (w == "pulse") p$r / (2 * p$a) - K / (2 * p$g) else p$r / p$a - K / p$g }
  lo <- p$d / p$b * (1 + 1e-9) + 1e-9
  if (p$g <= 0 || lo >= Kmax) return(c(pulse = NA, eq = NA))
  ks <- exp(seq(log(lo), log(Kmax), length.out = 200))
  sapply(c(pulse = "pulse", eq = "eq"), function(w) {
    v <- vapply(ks, f, 0, w = w); i <- which(diff(sign(v)) != 0)[1]
    if (is.na(i)) NA else uniroot(f, ks[c(i, i + 1)], w = w, tol = 1e-10)$root })
}
invasion <- function(p) {
  eP <- eq2(p$K, p$g, p$r, p$a1, p$b1, p$d1, p$e1); eQ <- eq2(p$K, nz(p$g2), p$r, p$a2, p$b2, p$d2, p$e2)
  lP1 <- (p$b1 / p$d1 * eP$H - 1) / p$e1; lP2 <- (p$b2 / p$d2 * eP$H - 1) / p$e2
  lQ1 <- (p$b1 / p$d1 * eQ$H - 1) / p$e1; lQ2 <- (p$b2 / p$d2 * eQ$H - 1) / p$e2
  list(eP = eP, eQ = eQ, QinvThr = p$e2 * lP2 / lP1, PinvThr = p$e1 * lQ1 / lQ2,
       Qinvades = p$c2 < p$e2 * lP2 / lP1, Pinvades = p$c1 < p$e1 * lQ1 / lQ2,
       Pok = eP$exists, Qok = eQ$exists)   # thresholds are meaningful only if that fungus persists alone (K_P > d_i/b_i)
}
winner <- function(P, Q, tol = 1e-3) ifelse(P > tol & Q > tol, "coexistence", ifelse(P > tol, "mutualist", ifelse(Q > tol, "cheater", "none")))
logistic <- function(K, r, H0, t) K / (1 + (K / H0 - 1) * exp(-r * t))
