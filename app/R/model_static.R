# ---------- Tab ①: static physiology (our synthesis, v1) — port of v3 <script id="model"> ----------
DEFAULTS <- list(S = 10, L = 0.6, Amax = 1.0, Y = 1.0, theta = 1.0, ur = 1.0, Kr = 20,
                 c = 0.6, phi = 0.8, kappa = 0.25, rho = 0.3, uf = 1.0, Kf = 3,
                 eps = 0.05, n = 6, Yf = 1.0, thetaF = 0.2, betaF = 0.0, Fref = 0.05, U0 = 0.3)
softmin <- function(x, y, n) (x^(-n) + y^(-n))^(-1 / n)
# vectorised over any parameter (e.g. S as a vector)
static_model <- function(p) {
  with(p, {
    A <- Amax * L
    Ur <- ur * S / (Kr + S); Uf <- uf * S / (Kf + S)
    Ux <- pmax(Uf - U0, 0)                             # hyphal P left after the fungus covers its own need U0 (U0 = 0: Ux = Uf)
    P_NM <- Ur; P_AM <- (1 - rho * c) * Ur + c * phi * Ux
    C_NM <- A + 0 * S; C_AM <- A * (1 - c * kappa)
    B_NM <- softmin(Y * C_NM, P_NM / theta, n)
    B_AM <- softmin(Y * C_AM, P_AM / theta, n)
    B_Ponly <- softmin(Y * C_NM, P_AM / theta, n)
    MGR <- log(B_AM / B_NM)
    cost <- c * kappa * A
    num <- c * phi * Ux - rho * c * Ur
    T_eff <- num / cost; T_eff[!(rep_len(cost, length(T_eff)) > 0)] <- Inf      # vectorised over any input
    noP <- rep_len(P_AM <= 0, length(MGR))           # c = ρ = 1, φ = 0: AM plant gets no P, B_AM = 0, MGR = −Inf
    noCol <- rep_len(c == 0, length(MGR))            # no colonisation: no interaction
    noX <- rep_len(Uf <= U0 & U0 > 0, length(MGR))   # fungus keeps all hyphal P (U_f ≤ U_0)
    limNM <- ifelse(Y * C_NM < P_NM / theta, "C", "P")
    limAM <- ifelse(Y * C_AM < P_AM / theta, "C", "P")
    F <- Yf * softmin(cost + betaF + 1e-12, ((1 - phi) * c * Ux + c * pmin(Uf, U0)) / thetaF + 1e-12, n)
    dF <- log(F / Fref)
    list(A = A, Ur = Ur, Uf = Uf, P_NM = P_NM, P_AM = P_AM, C_NM = C_NM, C_AM = C_AM, B_NM = B_NM, B_AM = B_AM,
         MGR = MGR, benefit = B_Ponly - B_NM, costG = B_Ponly - B_AM, net = B_AM - B_NM,
         T_eff = T_eff, limNM = limNM, limAM = limAM, F = F, dF = dF, noP = noP, noCol = noCol, Ux = Ux, noX = noX)
  })
}
classify <- function(v, eps) ifelse(v > eps, "mutualism", ifelse(v < -eps, "parasitism", "commensalism"))
classify2D <- function(x, y, eps) {
  sx <- if (x > eps) 1 else if (x < -eps) -1 else 0
  sy <- if (y > eps) 1 else if (y < -eps) -1 else 0
  t <- c("1,1" = "mutualism", "0,1" = "commensalism", "-1,1" = "fungus parasitic", "1,-1" = "plant exploits fungus",
         "-1,-1" = "competition", "0,-1" = "amensalism", "1,0" = "commensalism (plant benefits)",
         "-1,0" = "amensalism (plant harmed)", "0,0" = "neutral")
  unname(t[paste0(sx, ",", sy)])
}
