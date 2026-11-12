# Frozen copy of the v4.1 static model (before U_0), used by tests/test_hump.R
# vectorised over any parameter (e.g. S as a vector)
static_model_v41 <- function(p) {
  with(p, {
    A <- Amax * L
    Ur <- ur * S / (Kr + S); Uf <- uf * S / (Kf + S)
    P_NM <- Ur; P_AM <- (1 - rho * c) * Ur + c * phi * Uf
    C_NM <- A + 0 * S; C_AM <- A * (1 - c * kappa)
    B_NM <- softmin(Y * C_NM, P_NM / theta, n)
    B_AM <- softmin(Y * C_AM, P_AM / theta, n)
    B_Ponly <- softmin(Y * C_NM, P_AM / theta, n)
    MGR <- log(B_AM / B_NM)
    cost <- c * kappa * A
    num <- c * phi * Uf - rho * c * Ur
    T_eff <- num / cost; T_eff[!(rep_len(cost, length(T_eff)) > 0)] <- Inf      # vectorised over any input
    noP <- rep_len(P_AM <= 0, length(MGR))           # c = ρ = 1, φ = 0: AM plant gets no P, B_AM = 0, MGR = −Inf
    noCol <- rep_len(c == 0, length(MGR))            # no colonisation: no interaction
    limNM <- ifelse(Y * C_NM < P_NM / theta, "C", "P")
    limAM <- ifelse(Y * C_AM < P_AM / theta, "C", "P")
    F <- Yf * softmin(cost + betaF + 1e-12, (1 - phi) * c * Uf / thetaF + 1e-12, n)
    dF <- log(F / Fref)
    list(A = A, Ur = Ur, Uf = Uf, P_NM = P_NM, P_AM = P_AM, C_NM = C_NM, C_AM = C_AM, B_NM = B_NM, B_AM = B_AM,
         MGR = MGR, benefit = B_Ponly - B_NM, costG = B_Ponly - B_AM, net = B_AM - B_NM,
         T_eff = T_eff, limNM = limNM, limAM = limAM, F = F, dF = dF, noP = noP, noCol = noCol)
  })
}
