# U_0 (fungal own P need): U_0 = 0 must reproduce the v4.1 static model bit for bit; default U_0 gives a hump.
# Run from app/:  Rscript tests/test_hump.R
source("R/model_static.R"); source("tests/static_model_v41.R")
pass <- 0; fail <- 0
ok <- function(c, msg) { if (isTRUE(c)) pass <<- pass + 1 else { fail <<- fail + 1; cat("FAIL:", msg, "\n") } }
set.seed(1); N <- 20000
rp <- function() { u <- function(a, b) runif(N, a, b)
  modifyList(DEFAULTS, list(S = u(0.5, 60), L = u(0.1, 1), Amax = u(0.2, 2), Y = u(0.2, 2), theta = u(0.2, 3), Kr = u(2, 60),
    c = u(0, 1), phi = u(0, 1), kappa = u(0, 0.6), rho = u(0, 1), Kf = u(0.5, 30), n = sample(1:30, N, TRUE), thetaF = u(0.05, 1), Fref = u(0.005, 0.3))) }
p <- rp(); p$U0 <- 0
a <- static_model(p); b <- static_model_v41(p)
for (k in names(b)) ok(identical(a[[k]], b[[k]]), paste("U0 = 0 identical:", k))
cat(sprintf("U0 = 0 vs v4.1 on %d random parameter sets: %d of %d outputs identical\n", N, sum(sapply(names(b), function(k) identical(a[[k]], b[[k]]))), length(b)))
# hump at defaults
S <- seq(0.05, 60, 0.01); m <- static_model(modifyList(DEFAULTS, list(S = S)))$MGR; i <- which.max(m)
ok(i > 1 && i < length(S), "default MGR has an interior maximum")
ok(m[1] < -DEFAULTS$eps && m[i] > DEFAULTS$eps && m[length(S)] < -DEFAULTS$eps, "parasitism – mutualism – parasitism along S")
ok(abs(m[1] - log(1 - DEFAULTS$rho * DEFAULTS$c)) < 1e-6, "very low S: MGR = ln(1 − ρc)")
m0 <- static_model(modifyList(DEFAULTS, list(S = S, U0 = 0)))$MGR; ok(which.max(m0) == 1, "U0 = 0: MGR monotone decreasing from low S (no hump)")
cr <- function(v, th) S[which(diff(sign(v - th)) != 0)]
cat(sprintf("Default U0 = %.2f: MGR peak %.3f at S = %.2f; MGR = +eps at S = %s; 0 at S = %s; -eps at S = %s\n", DEFAULTS$U0, m[i], S[i],
  paste(cr(m, DEFAULTS$eps), collapse = ", "), paste(cr(m, 0), collapse = ", "), paste(cr(m, -DEFAULTS$eps), collapse = ", ")))
cat(sprintf("\n%d passed, %d failed\n", pass, fail)); if (fail > 0) quit(status = 1)
