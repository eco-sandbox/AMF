# ---------- plotly figures: traces/layouts ported 1:1 from the v3 JavaScript ----------
CFG <- list(displaylogo = FALSE, responsive = TRUE, modeBarButtonsToRemove = list("lasso2d", "select2d"))
pfig <- function(data, layout, source = NULL) {
  w <- plotly::as_widget(list(data = data, layout = layout, config = CFG))
  if (!is.null(source)) w$x$source <- source
  w
}
baseLayout <- function(xt, yt, ...) modifyList(list(
  margin = list(l = 55, r = 15, t = 10, b = 125), xaxis = list(title = xt, zeroline = FALSE, automargin = FALSE), yaxis = list(title = yt),
  legend = list(orientation = "h", x = 0, xanchor = "left", yref = "container", y = 0.01, yanchor = "bottom", font = list(size = 11)), font = list(size = 12), showlegend = TRUE,
  plot_bgcolor = "#fff", paper_bgcolor = "#fff", hovermode = "closest"), list(...))
bandShapes <- function(eps, axis = "y", lo = -3, hi = 3) {
  mk <- function(a, b, col) if (axis == "y")
    list(type = "rect", xref = "paper", x0 = 0, x1 = 1, yref = "y", y0 = a, y1 = b, fillcolor = col, line = list(width = 0), layer = "below") else
    list(type = "rect", yref = "paper", y0 = 0, y1 = 1, xref = "x", x0 = a, x1 = b, fillcolor = col, line = list(width = 0), layer = "below")
  list(mk(eps, hi, BAND[["mutualism"]]), mk(-eps, eps, BAND[["commensalism"]]), mk(lo, -eps, BAND[["parasitism"]]))
}
vline <- function(x, col = "#555", dash = "dash") list(type = "line", x0 = x, x1 = x, yref = "paper", y0 = 0, y1 = 1, line = list(color = col, dash = dash, width = 1.2))
CLASS_CS <- list(list(0, "#f2b5ab"), list(0.333, "#f2b5ab"), list(0.333, "#e6e6e6"), list(0.667, "#e6e6e6"), list(0.667, "#a8dbb4"), list(1, "#a8dbb4"))
cross_marker <- function(x, y) list(x = I(x), y = I(y), type = "scatter", mode = "markers", hoverinfo = "skip", showlegend = FALSE,
                                    marker = list(symbol = "cross-thin", size = 18, line = list(width = 3, color = "#000")))
line_tr <- function(x, y, name, ...) c(list(x = x, y = y, type = "scatter", mode = "lines", name = name), list(...))

# ===== Tab ① =====
Sgrid <- 0.5 + (0:239) * (59.5 / 239)
st_readout <- function(P, r) {
  cls <- classify(r$MGR, P$eps)
  noP <- isTRUE(r$noP); noCol <- isTRUE(r$noCol)
  lbl <- if (noCol) "NO COLONISATION / NEUTRAL" else toupper(cls)
  cls2 <- if (noCol) "no colonisation / neutral (c = 0)" else classify2D(r$MGR, r$dF, P$eps)
  dFc <- clip3(r$dF); dFtxt <- if (noCol) "" else sprintf(" (Δ<sub>F</sub> = %s%s; sign depends on F<sub>ref</sub>)", sg(dFc, 2), if (abs(r$dF) > 3) ", clipped to ±3" else "")
  lim <- function(l) if (l == "C") "<b>carbon</b>" else "<b>phosphorus</b>"
  why <- if (cls == "mutualism") sprintf("The AM plant gains more from fungal P than it loses in carbon. Its growth is limited by %s.", lim(r$limAM)) else
    if (cls == "parasitism") { if (r$limAM == "C") sprintf("The AM plant is carbon-limited, so the carbon paid to the fungus (%s%% of fixed C) directly reduces growth, and extra P is of little use.", fx(P$c * P$kappa * 100, 0))
      else if (isTRUE(r$noX)) sprintf("Hyphal uptake U<sub>f</sub> = %s does not exceed the fungus's own P need U<sub>0</sub> = %s, so it passes no P on, while direct root uptake is still suppressed (ρ).", fx(r$Uf, 2), fx(P$U0, 2))
      else "The fungal P gain does not offset the loss of direct root uptake (ρ) and the carbon paid." } else
    sprintf("Benefit and cost roughly cancel (|MGR| ≤ ε = %s). Small changes in soil P, light or fungal traits can tip it either way.", P$eps)
  if (noCol) why <- "No roots are colonised (c = 0): the AM and NM plants are identical, so there is no interaction to classify."
  if (noP) why <- "The AM plant acquires no P at all (c = ρ = 1 and φ = 0: the root pathway is fully suppressed and the fungus passes nothing on), so it cannot grow; MGR → −∞ (plots clip it)."
  HTML(sprintf('<div class="readout">
    <div><div class="k">MGR = ln(B<sub>AM</sub>/B<sub>NM</sub>)</div><div class="big" style="color:%s">%s</div>
      <div class="k">AM plant is %s%% of NM plant biomass</div></div>
    <div><div class="k">Outcome (plant view)</div><span class="outcome" style="background:%s;color:%s">%s</span>
      <div class="k" style="margin-top:4px">2-D (plant × fungus): <b>%s</b>%s</div></div>
    <div><div class="k">Limiting resource</div>NM plant: %s<br>AM plant: %s</div>
    <div><div class="k">Effective exchange rate T<sub>eff</sub></div><b>%s</b> P per C
      <div class="k">biomass: B<sub>NM</sub> = %s, B<sub>AM</sub> = %s</div></div>
    <div class="explain">%s</div></div>',
    COL[[cls]], if (is.finite(r$MGR)) sg(r$MGR, 3) else "−∞", fx(exp(r$MGR) * 100, 0), COL[[cls]], if (cls == "commensalism") "#222" else "#fff", lbl,
    cls2, dFtxt, lim(r$limNM), lim(r$limAM), if (is.finite(r$T_eff)) fx(r$T_eff, 2) else "∞ (no C paid)",
    fx(r$B_NM, 3), fx(r$B_AM, 3), why))
}
st_bc <- function(P) {
  r <- static_model(modifyList(P, list(S = Sgrid)))
  ymax <- max(r$benefit, r$costG) * 1.1 + 1e-3
  pfig(list(line_tr(Sgrid, r$benefit, "benefit: growth from extra P", line = list(color = COL[["mutualism"]], width = 2.5)),
            line_tr(Sgrid, r$costG, "cost: growth lost to C paid", line = list(color = COL[["parasitism"]], width = 2.5)),
            line_tr(Sgrid, r$net, "net = B<sub>AM</sub> − B<sub>NM</sub>", line = list(color = "#333", width = 2, dash = "dot"))),
       baseLayout("soil P, S (placeholder units)", "Δ biomass (relative)",
         yaxis = list(title = "Δ biomass (relative)", range = c(min(0, r$net) * 1.15 - 0.01, ymax)),
         shapes = list(list(type = "line", x0 = P$S, x1 = P$S, yref = "paper", y0 = 0, y1 = 1, line = list(color = "#555", dash = "dash", width = 1)),
                       list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0, line = list(color = "#999", width = 1))),
         annotations = list(list(x = P$S, y = ymax, yanchor = "top", text = "current S", showarrow = FALSE, font = list(size = 10, color = "#555")))))
}
st_mgr <- function(P) {
  Ls <- c(0.2, 0.4, 0.6, 0.8, 1.0); dash <- c("dot", "dashdot", "dash", "longdash", "solid")
  tr <- lapply(1:5, function(i) line_tr(Sgrid, pmax(-10, static_model(modifyList(P, list(S = Sgrid, L = Ls[i])))$MGR), paste("L =", Ls[i]),
                                        line = list(color = "#555", width = 1.2, dash = dash[i]), opacity = 0.75))
  tr[[6]] <- line_tr(Sgrid, pmax(-10, static_model(modifyList(P, list(S = Sgrid)))$MGR), paste("current L =", fx(P$L, 2)), line = list(color = "#111", width = 3))
  cur <- static_model(P); cls <- classify(cur$MGR, P$eps)
  tr[[7]] <- list(x = I(P$S), y = I(max(-2, cur$MGR)), type = "scatter", mode = "markers", name = "current state",
                  marker = list(size = 13, color = COL[[cls]], line = list(color = "#000", width = 1.5)))
  all <- unlist(lapply(tr[1:6], `[[`, "y"))
  lo <- max(-2, min(all) - 0.05); hi <- min(3, max(all) + 0.05)
  yr <- c(min(lo, -P$eps - 0.1), max(hi, P$eps + 0.1))
  pfig(tr, baseLayout("soil P, S (placeholder units)", "MGR", yaxis = list(title = "MGR", range = yr), shapes = bandShapes(P$eps, "y", -10, 10),
    annotations = list(list(xref = "paper", x = 0.99, xanchor = "right", y = yr[2], yanchor = "top", text = "mutualism", showarrow = FALSE, font = list(color = COL[["mutualism"]], size = 12)),
                       list(xref = "paper", x = 0.99, xanchor = "right", y = yr[1], yanchor = "bottom", text = "parasitism", showarrow = FALSE, font = list(color = COL[["parasitism"]], size = 12)))))
}
AXES <- list(kappa = list("C cost κ", 0, 0.6), L = list("light L", 0.1, 1), c = list("colonisation c", 0, 1),
             phi = list("cooperativeness φ", 0, 1), rho = list("root suppression ρ", 0, 1), U0 = list("fungal P need U₀", 0, 0.8))
st_map <- function(P, key) {
  ax <- AXES[[key]]; ylab <- ax[[1]]; y0 <- ax[[2]]; y1 <- ax[[3]]
  nx <- 110; ny <- 90
  xs <- 0.5 + (0:(nx - 1)) * 59.5 / (nx - 1); ys <- y0 + (0:(ny - 1)) * (y1 - y0) / (ny - 1)
  g <- expand.grid(x = xs, y = ys)                       # x fastest -> rows of the matrix = y
  pp <- P; pp$S <- g$x; pp[[key]] <- g$y
  m <- static_model(pp)$MGR; cl <- classify(m, P$eps); m <- pmax(m, -10)   # −Inf (no P acquired) clipped for display
  Z <- matrix(m, nrow = ny, byrow = TRUE); H <- matrix(cl, nrow = ny, byrow = TRUE)
  C <- matrix(ifelse(cl == "mutualism", 2, ifelse(cl == "commensalism", 1, 0)), nrow = ny, byrow = TRUE)
  pfig(list(
    list(type = "heatmap", x = xs, y = ys, z = C, zmin = 0, zmax = 2, showscale = FALSE, showlegend = FALSE, customdata = Z, text = H,
         colorscale = CLASS_CS, hovertemplate = paste0("S=%{x:.1f}<br>", ylab, "=%{y:.2f}<br>MGR=%{customdata:.3f}<br>%{text}<extra></extra>")),
    list(type = "contour", x = xs, y = ys, z = Z, showscale = FALSE, showlegend = FALSE, hoverinfo = "skip",
         contours = list(coloring = "none", showlabels = TRUE, start = -1, end = 2, size = 0.25, labelfont = list(size = 9, color = "#333")),
         line = list(color = "rgba(0,0,0,0.45)", width = 0.8)),
    list(type = "contour", x = xs, y = ys, z = Z, showscale = FALSE, showlegend = FALSE, hoverinfo = "skip", autocontour = FALSE,
         contours = list(coloring = "none", start = -P$eps, end = P$eps + 1e-9, size = max(2 * P$eps, 1e-6)), line = list(color = "#000", width = 2)),
    cross_marker(P$S, P[[key]])),
    baseLayout("soil P, S (placeholder units)", ylab, xaxis = list(title = "soil P, S (placeholder units)", range = c(0.5, 60)),
               yaxis = list(title = ylab, range = c(y0, y1)), showlegend = FALSE, margin = list(l = 55, r = 15, t = 10, b = 45)), source = "stmap")
}
clip3 <- function(v) pmax(-3, pmin(3, v))
st_plane <- function(P) {
  r <- static_model(modifyList(P, list(S = Sgrid))); xs <- clip3(r$MGR); ys <- clip3(r$dF)
  cur <- static_model(P); cls <- classify(cur$MGR, P$eps); cx <- clip3(cur$MGR); cy <- clip3(cur$dF)
  lim <- max(1, abs(xs), abs(ys), abs(cy)) * 1.08; e <- P$eps
  rect <- function(x0, x1, y0, y1, col) list(type = "rect", x0 = x0, x1 = x1, y0 = y0, y1 = y1, fillcolor = col, line = list(width = 0), layer = "below")
  ann <- function(x, y, t, c) list(x = x, y = y, text = t, showarrow = FALSE, font = list(size = 12, color = c))
  pfig(list(
    list(x = xs, y = ys, type = "scatter", mode = "lines+markers", name = "sweep S 0.5→60", line = list(color = "#777", width = 1.5),
         marker = list(size = 4, color = Sgrid, colorscale = "Greys", cmin = -20, cmax = 60, showscale = TRUE, colorbar = list(title = "S", thickness = 10, len = 0.6)),
         hovertemplate = "S=%{marker.color:.1f}<br>MGR=%{x:.3f}<br>Δ_F=%{y:.3f}<extra></extra>"),
    list(x = I(cx), y = I(cy), type = "scatter", mode = "markers", name = "current state", marker = list(size = 15, color = COL[[cls]], line = list(color = "#000", width = 1.5)))),
    baseLayout("plant response, MGR", "fungal response, Δ<sub>F</sub> = ln(F/F<sub>ref</sub>)",
      xaxis = list(title = "plant response, MGR", range = c(-lim, lim), zeroline = FALSE),
      yaxis = list(title = "fungal response, Δ<sub>F</sub> = ln(F/F<sub>ref</sub>)", range = c(-lim, lim)),
      shapes = list(rect(e, lim, e, lim, BAND[["mutualism"]]), rect(-lim, -e, e, lim, BAND[["parasitism"]]), rect(-e, e, e, lim, BAND[["commensalism"]]),
                    rect(e, lim, -lim, -e, "rgba(240,160,40,0.13)"), rect(-lim, -e, -lim, -e, "rgba(120,80,160,0.12)"),
                    list(type = "line", x0 = 0, x1 = 0, y0 = -lim, y1 = lim, line = list(color = "#999", width = 1)),
                    list(type = "line", x0 = -lim, x1 = lim, y0 = 0, y1 = 0, line = list(color = "#999", width = 1))),
      annotations = list(ann(lim * 0.6, lim * 0.9, "MUTUALISM (+,+)", COL[["mutualism"]]), ann(-lim * 0.6, lim * 0.9, "FUNGUS PARASITIC (−,+)", COL[["parasitism"]]),
                         ann(0, lim * 0.97, "commensal (0,+)", "#666"), ann(lim * 0.6, -lim * 0.9, "PLANT EXPLOITS FUNGUS (+,−)", "#b06a00"),
                         ann(-lim * 0.6, -lim * 0.9, "COMPETITION (−,−)", "#6a4a8a"))))
}

# ===== Tab ② =====
Kgrid <- 0.5 + (0:239) * (59.5 / 239)
kfmt <- function(k) if (is.na(k)) "none in range" else fx(k, 2)
dy_readout <- function(D, c, sp = switch_points(D)) {
  if (!c$exists) return(HTML(sprintf('<div class="readout2">
   <div class="banner nopersist span2">Fungus cannot persist: K<sub>P</sub> = %s ≤ d<sub>M</sub>/b<sub>M</sub> = %s. The host grows alone to H* = K<sub>P</sub>,
     so neither criterion applies (no interaction to classify). Raise K<sub>P</sub> or b<sub>M</sub>, or lower d<sub>M</sub>.</div></div>', D$K, fx(c$Kmin, 3))))
  agree <- c$eqClass == c$pulseClass
  HTML(sprintf('<div class="readout2">
   <div class="crit"><div class="k">① Equilibrium criterion (long-term experiment)</div>
     host with fungus H* = <b>%s</b> vs without H<sub>NM</sub>* = K<sub>P</sub> = <b>%s</b><br>
     ln(H*/K<sub>P</sub>) = <b style="color:%s">%s</b> → %s</div>
   <div class="crit"><div class="k">② Pulse criterion (add a few fungi at equilibrium)</div>
     J<sub>HM</sub> = ∂(dH/dt)/∂M|* = rγH*²/(K<sub>P</sub>+γM*)² − a<sub>M</sub>H* = <b style="color:%s">%s</b> → %s</div>
   <div class="banner %s">%s</div>
   <div class="k span2">Exact rules (on the host isocline): pulse-mutualistic iff M* &lt; r/2a<sub>M</sub> − K<sub>P</sub>/2γ = <b>%s</b>;
     equilibrium-mutualistic iff M* &lt; r/a<sub>M</sub> − K<sub>P</sub>/γ = <b>%s</b>. Equilibrium M* = <b>%s</b> (%s).
     Switch points along K<sub>P</sub>: pulse at <b>K<sub>P</sub> = %s</b>, equilibrium (ln = 0) at <b>K<sub>P</sub> = %s</b>.
     K<sub>P</sub> = γr/a<sub>M</sub> = %s is only a sufficient bound: beyond it both criteria are always parasitic (current K<sub>P</sub> is %s it).</div></div>',
   fx(c$H, 3), D$K, clsCol(c$eqClass), sg(c$lnEq, 3), pill(c$eqClass), clsCol(c$pulseClass), sg(c$J12, 3), pill(c$pulseClass),
   if (agree) "ok" else "bad",
   if (agree) "✓ Both criteria agree." else "⚠ The criteria DISAGREE: the plant is bigger with the fungus at equilibrium, yet adding more fungus would slow its growth (Neuhauser &amp; Fargione’s “isocline 2” case).",
   fx(c$thr$pulse, 3), fx(c$thr$eq, 3), fx(c$P, 3), c$sink, kfmt(sp[["pulse"]]), kfmt(sp[["eq"]]),
   fx(c$Kthr, 2), if (D$K > c$Kthr) sprintf('<b style="color:%s">above</b>', COL[["parasitism"]]) else "below"))
}
dy_ts <- function(D, sim) {
  HNM <- logistic(D$K, D$r, D$H0, sim$t)
  pfig(list(line_tr(sim$t, sim$H, "host H with fungus M", line = list(color = FUNG[["host"]], width = 2.5)),
            line_tr(sim$t, HNM, "host H<sub>NM</sub> without fungus", line = list(color = FUNG[["hostNM"]], width = 2.5, dash = "dash")),
            line_tr(sim$t, sim$P, "mutualist M", line = list(color = FUNG[["mut"]], width = 2), yaxis = "y2")),
       baseLayout("time (placeholder units)", "host density H",
                  yaxis2 = list(title = list(text = "mutualist M", standoff = 4), overlaying = "y", side = "right", showgrid = FALSE, automargin = TRUE)))
}
dy_pp <- function(D, c, sim) {
  Pmax <- max(D$r / D$a, c$P * 1.3, max(sim$P) * 1.1)
  Ps <- (0:199) * (D$r / D$a) / 199; hostIso <- (D$K + D$g * Ps) * (1 - D$a * Ps / D$r)
  PsL <- (0:49) * Pmax / 49; symIso <- D$d / D$b * (1 + D$e * PsL)
  Hmax <- max(hostIso, c$H, sim$H, D$K) * 1.1
  pfig(list(line_tr(hostIso, Ps, "host isocline dH/dt = 0", line = list(color = "#222", width = 2.5)),
            line_tr(symIso, PsL, "mutualist isocline dM/dt = 0", line = list(color = FUNG[["mut"]], width = 2.5, dash = "dash")),
            line_tr(sim$H, sim$P, "trajectory", line = list(color = "#7570b3", width = 1.8)),
            list(x = I(sim$H[1]), y = I(sim$P[1]), type = "scatter", name = "start", mode = "markers", marker = list(symbol = "circle-open", size = 10, color = "#7570b3", line = list(width = 2))),
            list(x = I(c$vertex$H), y = I(c$vertex$P), type = "scatter", name = "vertex of host isocline", mode = "markers", marker = list(symbol = "diamond", size = 11, color = "#fff", line = list(color = "#000", width = 2))),
            list(x = I(c$H), y = I(c$P), type = "scatter", name = "equilibrium (colour = pulse criterion)", mode = "markers", marker = list(size = 14, color = clsCol(c$pulseClass), line = list(color = "#000", width = 1.5)))),
       baseLayout("host density H", "mutualist density M", xaxis = list(title = "host density H", range = c(0, Hmax)),
                  yaxis = list(title = "mutualist density M", range = c(min(0, c$vertex$P) - 0.2, Pmax)),
                  legend = list(entrywidth = 0.5, entrywidthmode = "fraction"),
                  shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0, line = list(color = "#aaa", width = 1)))))
}
dy_k <- function(D, c, sp = switch_points(D)) {
  q <- lapply(Kgrid, function(K) criteria2(modifyList(D, list(K = K))))
  Hs <- vapply(q, `[[`, 0, "H"); ok <- vapply(q, `[[`, TRUE, "exists")
  eqC <- vapply(q, function(x) switch(x$eqClass, mutualism = 2, commensalism = 1, parasitism = 0, NA_real_), 0)
  puC <- vapply(q, function(x) switch(x$pulseClass, mutualism = 2, parasitism = 0, NA_real_), 0)   # NA = fungus cannot persist (blank)
  ht <- function(y, z, ya) list(type = "heatmap", x = Kgrid, y = I(y), z = matrix(z, nrow = 1), zmin = 0, zmax = 2, colorscale = CLASS_CS,
                                showscale = FALSE, hoverinfo = "skip", xaxis = "x", yaxis = ya, name = y, showlegend = FALSE)
  sh <- list(vline(D$K), vline(c$Kthr, COL[["parasitism"]], "dot"))
  an <- list(list(x = c$Kthr, y = 1, yref = "paper", yanchor = "bottom", text = "γr/a<sub>M</sub> (sufficient)", showarrow = FALSE, font = list(size = 10, color = COL[["parasitism"]])))
  for (w in c("pulse", "eq")) if (!is.na(sp[[w]])) {
    sh[[length(sh) + 1]] <- vline(sp[[w]], "#000", "dot")
    an[[length(an) + 1]] <- list(x = sp[[w]], y = if (w == "pulse") 0.97 else 0.90, yref = "paper", xanchor = "left", yanchor = "top", showarrow = FALSE,
                                 text = sprintf(" %s switch %s", if (w == "pulse") "pulse" else "equilibrium", fx(sp[[w]], 2)), font = list(size = 10, color = "#000"))
  }
  if (any(!ok)) {
    x1 <- c$Kmin
    sh[[length(sh) + 1]] <- list(type = "rect", x0 = 0.5, x1 = x1, yref = "paper", y0 = 0, y1 = 1, fillcolor = "rgba(0,0,0,0.07)", line = list(width = 0), layer = "below")
    an[[length(an) + 1]] <- list(x = 0.5, xanchor = "left", y = 0.80, yref = "paper", showarrow = FALSE, text = " fungus cannot<br> persist (K<sub>P</sub> ≤ d<sub>M</sub>/b<sub>M</sub>)", align = "left", font = list(size = 10, color = "#555"))
  }
  pfig(list(line_tr(Kgrid, Hs, "H* with fungus", line = list(color = FUNG[["host"]], width = 2.5)),
            line_tr(Kgrid, Kgrid, "H<sub>NM</sub>* without fungus (= K<sub>P</sub>)", line = list(color = FUNG[["hostNM"]], width = 2, dash = "dash")),
            ht("pulse", puC, "y2"), ht("equilibrium", eqC, "y3")),
       list(margin = list(l = 80, r = 15, t = 10, b = 110), font = list(size = 12), legend = list(orientation = "h", x = 0, yref = "container", y = 0.01, yanchor = "bottom", font = list(size = 11)), showlegend = TRUE,
            xaxis = list(title = "soil fertility K<sub>P</sub> (host capacity without fungi)", range = c(0.5, 60), anchor = "y2", automargin = FALSE),
            yaxis = list(domain = c(0.32, 1), title = "equilibrium host H*", range = c(0, min(60, max(Hs) * 1.3))),
            yaxis2 = list(domain = c(0, 0.12), showticklabels = TRUE, tickfont = list(size = 10)), yaxis3 = list(domain = c(0.15, 0.27), tickfont = list(size = 10)),
            shapes = sh, plot_bgcolor = "#fff", paper_bgcolor = "#fff", annotations = an))
}

# ===== Tab ③ =====
CH_SWEEP_K <- 0.5 + (0:59) * 39.5 / 59
# M-only reference: analytic host–mutualist equilibrium (no cheater), vectorised over K_P
m_only <- function(p, K = p$K) {
  e <- lapply(K, function(k) eq2(k, p$g, p$r, p$a1, p$b1, p$d1, p$e1))
  list(H = vapply(e, `[[`, 0, "H"), M = vapply(e, `[[`, 0, "P"), ok = vapply(e, `[[`, TRUE, "exists"))
}
ch_sweep <- function(p, resident = FALSE) {   # 60 K_P values, t = 400, vectorised adaptive RK4 (same arithmetic as simulate())
  pv <- modifyList(p, list(K = CH_SWEEP_K))
  v <- simulate_final_vec(pv, c(p$H0, p$P0, p$Q0), 400, 0.1)
  out <- list(Ks = CH_SWEEP_K, Hf = v$H, Pf = v$P, Qf = v$Q, W = winner(v$P, v$Q), Hm = m_only(p, CH_SWEEP_K)$H)
  if (resident) {   # invasion experiment: start at the M-only equilibrium, cheater rare (Z_0 = 0.01)
    mo <- m_only(p, CH_SWEEP_K)
    r <- simulate_final_vec(pv, list(mo$H, mo$M, 0.01), 2000, 0.1)   # longer run: invasion from rare is slow near the knife-edge α_ZM
    # Z below its starting density 0.01 = failed to invade (it decays very slowly near the knife-edge α_ZM)
    out$res <- list(Hf = r$H, Pf = r$P, Qf = r$Q, W = ifelse(r$Q < 0.01 & r$P > 1e-3, "mutualist", winner(r$P, r$Q)))
  }
  out
}
winBands <- function(xs, W, yref = "paper") {
  col <- c(mutualist = "rgba(26,152,80,0.15)", cheater = "rgba(215,48,31,0.14)", coexistence = "rgba(160,160,160,0.18)", none = "rgba(0,0,0,0.05)")
  sh <- list(); i0 <- 1; n <- length(xs)
  for (i in 2:(n + 1)) if (i == n + 1 || W[i] != W[i0]) {
    x0 <- if (i0 == 1) xs[1] else (xs[i0 - 1] + xs[i0]) / 2; x1 <- if (i == n + 1) xs[n] else (xs[i - 1] + xs[i]) / 2
    sh[[length(sh) + 1]] <- list(type = "rect", x0 = x0, x1 = x1, yref = yref, y0 = 0, y1 = 1, fillcolor = col[[W[i0]]], line = list(width = 0), layer = "below"); i0 <- i }
  sh
}
ch_readout <- function(p, sim, Pst) {
  H <- sim$final[1]; Pf <- sim$final[2]; Qf <- sim$final[3]; w <- winner(Pf, Qf); share <- if (Pf + Qf > 0) Pf / (Pf + Qf) else NaN
  ln <- log(H / p$K); cl <- classify(ln, p$eps); iv <- invasion(p); mo <- m_only(p); lnM <- log(mo$H / p$K)
  zThr <- if (iv$Pok) sprintf("α<sub>ZM</sub> &lt; e<sub>Z</sub>λ<sub>Z</sub><sup>(M)</sup>/λ<sub>M</sub><sup>(M)</sup> = %s → <b>%s</b>", fx(iv$QinvThr, 4), if (iv$Qinvades) "yes" else "no") else
    sprintf("n/a: M cannot persist alone at this K<sub>P</sub> (K<sub>P</sub> ≤ d<sub>M</sub>/b<sub>M</sub> = %s)", fx(p$d1 / p$b1, 3))
  mThr <- if (iv$Qok) sprintf("α<sub>MZ</sub> &lt; e<sub>M</sub>λ<sub>M</sub><sup>(Z)</sup>/λ<sub>Z</sub><sup>(Z)</sup> = %s → <b>%s</b>", fx(iv$PinvThr, 4), if (iv$Pinvades) "yes" else "no") else
    sprintf("n/a: Z cannot persist alone at this K<sub>P</sub> (K<sub>P</sub> ≤ d<sub>Z</sub>/b<sub>Z</sub> = %s)", fx(p$d2 / p$b2, 3))
  note <- if (!(iv$Pok && iv$Qok)) "" else if (iv$Qinvades && iv$Pinvades) " Both invade → coexistence expected." else
    if (!iv$Qinvades && !iv$Pinvades) " Neither invades → outcome depends on initial conditions (bistable)." else
    if (!iv$Qinvades && w != "mutualist") " Z cannot invade when rare, but this run did not end with M alone: another stable state exists, so the outcome depends on starting densities (compare the dashed invasion sweep below)." else ""
  moTxt <- if (mo$ok) sprintf("M-only reference (mutualist alone, equilibrium): H = %s, ln(H/K<sub>P</sub>) = <b style=\"color:%s\">%s</b> %s",
                             fx(mo$H, 3), clsCol(classify(lnM, p$eps)), sg(lnM, 3), pill(classify(lnM, p$eps))) else "M-only reference: M cannot persist alone at this K<sub>P</sub>."
  phiMix <- if (is.finite(share)) share * Pst$phi + (1 - share) * p$phiC else Pst$phi
  st <- static_model(modifyList(Pst, list(phi = phiMix))); stcl <- classify(st$MGR, Pst$eps)
  wl <- c(mutualist = "mutualist excludes cheater", cheater = "cheater excludes mutualist", coexistence = "coexistence", none = "both fungi lost")[[w]]
  wc <- c(mutualist = COL[["mutualism"]], cheater = COL[["parasitism"]], coexistence = "#777", none = "#333")[[w]]
  HTML(sprintf('<div class="readout2">
   <div><div class="k">Fungal community at t = t<sub>max</sub> = %s</div><span class="tag" style="background:%s;color:#fff">%s</span>
     <div class="k" style="margin-top:4px">H = %s, M (mutualist) = %s, Z (cheater) = %s; mutualist share M/(M+Z) %s</div></div>
   <div><div class="k">Plant response, equilibrium criterion ln(H/K<sub>P</sub>)</div><b style="font-size:20px;color:%s">%s</b> %s</div>
   <div class="k span2">%s</div>
   <div class="k span2">Invasion criteria: cheater Z invades the M-only equilibrium iff %s;
     mutualist M invades the Z-only equilibrium iff %s.%s</div>
   <div class="k span2">Bridge to tab ① (new here): with colonisation split by abundance, community cooperativeness φ<sub>mix</sub> = %s →
     static MGR at S = %s is <b style="color:%s">%s</b> (%s).</div></div>',
   p$T, wc, toupper(wl), fx(H, 3), fx(Pf, 3), fx(Qf, 3), if (is.finite(share)) paste0(fx(share * 100, 0), "%") else "–",
   clsCol(cl), sg(ln, 3), pill(cl), moTxt, zThr, mThr, note,
   fx(phiMix, 2), Pst$S, clsCol(stcl), fx(st$MGR, 3), stcl))
}
ch_ts <- function(sim) pfig(list(line_tr(sim$t, sim$H, "host H", line = list(color = FUNG[["host"]], width = 2.5)),
                                 line_tr(sim$t, sim$P, "mutualist M", line = list(color = FUNG[["mut"]], width = 2.5)),
                                 line_tr(sim$t, sim$Q, "cheater Z", line = list(color = FUNG[["cheat"]], width = 2.5))),
                            baseLayout("time (placeholder units)", "density"))
ch_k <- function(p, sw) {
  tr <- list(line_tr(sw$Ks, sw$Pf, "mutualist M", line = list(color = FUNG[["mut"]], width = 2.5)),
             line_tr(sw$Ks, sw$Qf, "cheater Z", line = list(color = FUNG[["cheat"]], width = 2.5)),
             line_tr(sw$Ks, sw$Hf, "host H", line = list(color = FUNG[["host"]], width = 1.5, dash = "dot")),
             line_tr(sw$Ks, sw$Hm, "host H, M alone (reference)", line = list(color = FUNG[["mut"]], width = 1.5, dash = "dot")))
  if (!is.null(sw$res)) tr <- c(tr, list(
             line_tr(sw$Ks, sw$res$Pf, "M, start M-only + rare Z", line = list(color = FUNG[["mut"]], width = 1.5, dash = "dash")),
             line_tr(sw$Ks, sw$res$Qf, "Z, start M-only + rare Z", line = list(color = FUNG[["cheat"]], width = 1.5, dash = "dash"))))
  sh <- c(winBands(sw$Ks, sw$W), list(vline(p$K)))
  if (!is.null(sw$res)) {   # top strip: outcome of the invasion experiment (stronger colours)
    rb <- lapply(winBands(sw$Ks, sw$res$W), function(b) modifyList(b, list(y0 = 0.95, y1 = 1, opacity = 1,
            fillcolor = sub(",0\\.1[0-9]\\)", ",0.65)", b$fillcolor))))
    sh <- c(sh, rb)
  }
  pfig(tr, baseLayout("soil fertility K<sub>P</sub>", if (is.null(sw$res)) "density after t = 400" else "density after t = 400 (dashed: 2000)", shapes = sh))
}
ch_mgr <- function(p, sim) {
  HNM <- logistic(p$K, p$r, p$H0, sim$t); mgrT <- log(sim$H / HNM)
  shT <- ifelse(sim$P + sim$Q > 0, sim$P / (sim$P + sim$Q), NA)
  lo <- min(-0.3, mgrT) - 0.05; hi <- max(0.3, mgrT) + 0.05
  pfig(list(line_tr(sim$t, mgrT, "plant response ln(H / H<sub>NM</sub>)", line = list(color = "#111", width = 2.5)),
            line_tr(sim$t, shT, "mutualist share M/(M+Z)", yaxis = "y2", line = list(color = FUNG[["mut"]], width = 1.5, dash = "dash"))),
       baseLayout("time (placeholder units)", "plant response", yaxis = list(title = "ln(H / H<sub>NM</sub>)", range = c(lo, hi)),
                  yaxis2 = list(title = "M/(M+Z)", overlaying = "y", side = "right", range = c(0, 1), showgrid = FALSE),
                  margin = list(l = 55, r = 55, t = 10, b = 125), shapes = bandShapes(p$eps, "y", -20, 20)))
}

# ===== Tab ④ =====
SA_KS <- 1 + (0:26) * 39 / 26; SA_SS <- (0:20) / 20      # same 27 × 21 grid as v3
sa_map_compute <- function(base, progress = NULL) {  # base = SA params (s, K, T ignored); t = 300, dt0 0.1, as v3
  g <- expand.grid(K = SA_KS, s = SA_SS)
  v <- simulate_final_vec(modifyList(base, list(K = g$K, s = g$s)), c(base$H0, base$P0, base$Q0), 300, 0.1, progress = progress)
  nr <- length(SA_SS); m <- function(x) matrix(x, nrow = nr, byrow = TRUE)
  H <- m(v$H); P <- m(v$P); Q <- m(v$Q)
  H0row <- matrix(H[1, ], nrow = nr, ncol = length(SA_KS), byrow = TRUE)    # row s = 0
  Km <- matrix(SA_KS, nrow = nr, ncol = length(SA_KS), byrow = TRUE)
  list(sh = ifelse(P + Q > 1e-6, P / (P + Q), NA), lh = log(H / Km), gain = log(H / H0row))
}
sa_readout <- function(p, sim, sim0) {
  H <- sim$final[1]; Pf <- sim$final[2]; Qf <- sim$final[3]; H0f <- sim0$final[1]; P0f <- sim0$final[2]; Q0f <- sim0$final[3]
  w <- sanctionW(p, Pf, Qf); w1 <- w[[1]]; w2 <- w[[2]]
  cShare <- Pf * w1 * p$a1 / (Pf * w1 * p$a1 + Qf * w2 * p$a2 + 1e-300)
  wn <- function(P, Q) c(mutualist = "mutualist wins", cheater = "cheater wins", coexistence = "coexistence", none = "no fungi")[[winner(P, Q)]]
  wc <- function(P, Q) c(mutualist = COL[["mutualism"]], cheater = COL[["parasitism"]], coexistence = "#777", none = "#333")[[winner(P, Q)]]
  ln <- log(H / p$K); ln0 <- log(H0f / p$K); mo <- m_only(p); lnM <- log(mo$H / p$K)
  drain <- (p$a1 * w1 * Pf + p$a2 * w2 * Qf) / (p$a1 * Pf + p$a2 * Qf + 1e-300)   # host C drain vs. unweighted rewards, same M, Z
  moTxt <- if (mo$ok) sprintf("M-only reference (mutualist alone, no cheater): H = %s, ln(H/K<sub>P</sub>) = <b style=\"color:%s\">%s</b>.",
                             fx(mo$H, 3), clsCol(classify(lnM, p$eps)), sg(lnM, 3)) else "M-only reference: M cannot persist alone at this K<sub>P</sub>."
  HTML(sprintf('<div class="readout2">
   <div><div class="k">With sanctions s = %s</div><span class="tag" style="background:%s;color:#fff">%s</span>
     <div class="k">M = %s, Z = %s, H = %s; ln(H/K<sub>P</sub>) = <b style="color:%s">%s</b></div></div>
   <div><div class="k">Without sanctions (s = 0)</div><span class="tag" style="background:%s;color:#fff">%s</span>
     <div class="k">M = %s, Z = %s, H = %s; ln(H/K<sub>P</sub>) = <b style="color:%s">%s</b></div></div>
   <div class="k span2">Reward per unit fungus at the end state: mutualist w<sub>M</sub> = <b>%s</b>, cheater w<sub>Z</sub> = <b>%s</b>;
     share of host carbon going to the mutualist: <b>%s%%</b>; host carbon drain relative to equal rewards at the same M, Z: <b>%s×</b> (a<sub>M</sub> ≠ a<sub>Z</sub>).
     Sanctions change the plant’s equilibrium by ln(H*/H*<sub>s=0</sub>) = <b>%s</b>. %s</div></div>',
   p$s, wc(Pf, Qf), toupper(wn(Pf, Qf)), fx(Pf, 3), fx(Qf, 3), fx(H, 3), clsCol(classify(ln, p$eps)), fx(ln, 3),
   wc(P0f, Q0f), toupper(wn(P0f, Q0f)), fx(P0f, 3), fx(Q0f, 3), fx(H0f, 3), clsCol(classify(ln0, p$eps)), fx(ln0, 3),
   fx(w1, 2), fx(w2, 2), fx(100 * cShare, 0), if (Pf + Qf > 0) fx(drain, 3) else "–", fx(log(H / H0f), 3), moTxt))
}
sa_ts <- function(p, sim, sim0) pfig(list(
  line_tr(sim$t, sim$P, sprintf("mutualist M (s = %s)", p$s), line = list(color = FUNG[["mut"]], width = 2.8)),
  line_tr(sim$t, sim$Q, sprintf("cheater Z (s = %s)", p$s), line = list(color = FUNG[["cheat"]], width = 2.8)),
  line_tr(sim0$t, sim0$P, "mutualist M (s = 0)", line = list(color = FUNG[["mut"]], width = 1.5, dash = "dot")),
  line_tr(sim0$t, sim0$Q, "cheater Z (s = 0)", line = list(color = FUNG[["cheat"]], width = 1.5, dash = "dot")),
  line_tr(sim$t, sim$H, sprintf("host H (s = %s)", p$s), line = list(color = FUNG[["host"]], width = 1.5)),
  line_tr(range(sim$t), rep(m_only(p)$H, 2), "host H, M alone (reference)", line = list(color = FUNG[["mut"]], width = 1.2, dash = "dot"))),
  baseLayout("time (placeholder units)", "density"))
sa_map1 <- function(p, m) pfig(list(
  list(type = "heatmap", x = SA_KS, y = SA_SS, z = m$sh, zmin = 0, zmax = 1, colorscale = list(list(0, "#d7301f"), list(0.5, "#e0e0e0"), list(1, "#1a9850")),
       colorbar = list(title = "M/(M+Z)", thickness = 10), hovertemplate = "K<sub>P</sub>=%{x:.1f}<br>s=%{y:.2f}<br>M/(M+Z)=%{z:.2f}<extra></extra>"),
  list(type = "contour", showlegend = FALSE, x = SA_KS, y = SA_SS, z = m$sh, showscale = FALSE, hoverinfo = "skip",
       contours = list(coloring = "none", start = 0.001, end = 0.999, size = 0.998), line = list(color = "#000", width = 2)),
  cross_marker(p$K, p$s)), baseLayout("soil fertility K<sub>P</sub>", "sanction strength s", showlegend = FALSE, margin = list(l = 55, r = 15, t = 10, b = 45)), source = "samap")
sa_map2 <- function(p, m, mode) {
  z <- if (mode == "gain") m$gain else m$lh
  z[is.finite(z) & abs(z) < 1e-4] <- 0     # integration noise (~1e-6) would otherwise draw spurious contours
  zt <- if (mode == "gain") "ln(H*/H*<sub>s=0</sub>)" else "ln(H*/K<sub>P</sub>)"
  zf <- z[is.finite(z)]; lim <- max(0.05, abs(zf)); eb <- min(0.49, 0.5 * p$eps / lim)
  pfig(list(
    list(type = "heatmap", x = SA_KS, y = SA_SS, z = z, zmin = -lim, zmax = lim,
         colorscale = list(list(0, "#d7301f"), list(0.5 - eb, "#f4f4f4"), list(0.5 + eb, "#f4f4f4"), list(1, "#1a9850")),
         colorbar = list(title = zt, thickness = 10), hovertemplate = paste0("K<sub>P</sub>=%{x:.1f}<br>s=%{y:.2f}<br>", zt, "=%{z:.3f}<extra></extra>")),
    list(type = "contour", showlegend = FALSE, x = SA_KS, y = SA_SS, z = z, showscale = FALSE, hoverinfo = "skip",
         contours = list(coloring = "none", showlabels = TRUE, labelfont = list(size = 9)), line = list(color = "rgba(0,0,0,0.4)", width = 0.8)),
    cross_marker(p$K, p$s)), baseLayout("soil fertility K<sub>P</sub>", "sanction strength s", showlegend = FALSE, margin = list(l = 55, r = 15, t = 10, b = 45)), source = "samap2")
}
