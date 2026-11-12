# Per parameter set: does MGR(S) over S in [0.5, 60] peak at intermediate S, and does it show the full parasitism-mutualism-parasitism sequence?
suppressMessages(library(data.table)); source("../app/R/model_static.R")
D <- fread("csv/samples_tab1.csv")[1:20000]; Sg <- exp(seq(log(0.5), log(60), length.out = 120)); EPS <- 0.05
f <- function(U0col) { pk <- logical(nrow(D)); pmp <- logical(nrow(D))
  for (i in seq_len(nrow(D))) { p <- modifyList(DEFAULTS, as.list(D[i, .(L, Amax, Y, theta, Kr, c, phi, kappa, rho, Kf, n)])); p$U0 <- if (U0col) D$U0[i] else 0; p$S <- Sg
    m <- static_model(p)$MGR; m[!is.finite(m)] <- -50; k <- which.max(m)
    pk[i] <- k > 1 && k < length(Sg) && m[k] - m[1] > 0.01 && m[k] - m[length(Sg)] > 0.01
    pmp[i] <- m[1] < -EPS && max(m) > EPS && m[length(Sg)] < -EPS }
  c(interior_peak = 100 * mean(pk), par_mut_par = 100 * mean(pmp)) }
a <- f(TRUE); b <- f(FALSE)
R <- data.table(metric = c("MGR(S) has an interior peak (≥ 0.01 above both ends)", "parasitism → mutualism → parasitism along S"),
                U0_sampled = round(a, 1), U0_zero = round(b, 1))
fwrite(R, "csv/tab1_hump_share.csv"); print(R)
