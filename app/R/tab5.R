# ---------- Tab ⑤: parameter-space shares from precomputed bins (no live sampling) ----------
tab5_candidates <- function(name) {
  unique(c(file.path("data", name),
           file.path("app", "data", name), file.path("shiny", "data", name),
           file.path("..", "app", "data", name), file.path("..", "shiny", "data", name),
           file.path("..", "data", name)))
}
tab5_load <- function() {
  rds <- Filter(file.exists, tab5_candidates("tab5_counts.rds"))
  if (length(rds)) {
    obj <- readRDS(rds[[1]])
  } else {
    b64p <- Filter(file.exists, tab5_candidates("tab5_counts.b64"))
    if (!length(b64p)) stop("tab ⑤ data not found (data/tab5_counts.rds or .b64)")
    raw <- jsonlite::base64_dec(paste(readLines(b64p[[1]], warn = FALSE), collapse = ""))
    tf <- tempfile(fileext = ".rds"); on.exit(unlink(tf), add = TRUE)
    writeBin(raw, tf); obj <- readRDS(tf)
  }
  for (nm in names(obj$models)) {
    m <- obj$models[[nm]]
    for (k in c("counts", "band", "pairs", "importance", "ranges"))
      if (!is.null(m[[k]])) m[[k]] <- as.data.frame(m[[k]], stringsAsFactors = FALSE)
    obj$models[[nm]] <- m
  }
  obj
}
TAB5 <- tryCatch(tab5_load(), error = function(e) { warning(e$message); NULL })

PS_COL <- c(mutualism = "#1a9850", commensalism = "#9e9e9e", parasitism = "#d7301f",
            "cannot persist" = "#555555", "no fungus persists" = "#555555",
            "no colonisation" = "#555555", none = "#555555",
            mutualist = "#1a9850", cheater = "#d7301f", coexistence = "#e6a817")
ps_col <- function(cl) { v <- unname(PS_COL[cl]); ifelse(is.na(v), "#888888", v) }
ps_fmt <- function(x) {
  vapply(x, function(z) {
    if (!is.finite(z)) return("—")
    if (abs(z) >= 10 || abs(z - round(z)) < 1e-9) formatC(z, format = "f", digits = 1)
    else formatC(z, format = "fg", digits = 3)
  }, "")
}
ps_mids <- function(br, log_scale) {
  if (isTRUE(log_scale)) sqrt(pmax(br[-length(br)], 1e-12) * pmax(br[-1], 1e-12))
  else (br[-length(br)] + br[-1]) / 2
}
ps_is_log <- function(m, par) {
  r <- m$ranges[m$ranges$par == par, , drop = FALSE]
  nrow(r) && identical(r$scale[1], "log")
}
ps_sym <- function(m, par) {
  s <- m$symbols[[par]]; if (is.null(s) || !nzchar(s)) par else s
}
ps_choices <- function() {
  if (is.null(TAB5)) return(c("data missing" = "tab1"))
  setNames(names(TAB5$models), vapply(TAB5$models, `[[`, "", "label"))
}
ps_axis_choices <- function(id) {
  if (is.null(TAB5) || !id %in% names(TAB5$models)) return(c())
  m <- TAB5$models[[id]]
  setNames(m$axes, vapply(m$axes, function(p) ps_sym(m, p), ""))
}
# bins whose interval overlaps [lo, hi] (last bin is closed on the right)
tab5_bins_overlap <- function(breaks, lo, hi) {
  n <- length(breaks) - 1L
  if (n < 1L || hi < lo) return(integer())
  which(breaks[seq_len(n)] <= hi + 1e-9 & breaks[seq_len(n) + 1L] >= lo - 1e-9)
}
# Aggregate precomputed counts over the selected fertility (and light) band.
tab5_slice <- function(obj, id, xpar, ypar, fert_lo, fert_hi, light_lo = NULL, light_hi = NULL) {
  m <- obj$models[[id]]
  fb <- tab5_bins_overlap(m$breaks[[m$fert]], fert_lo, fert_hi)
  if (!length(fb)) fb <- seq_len(length(m$breaks[[m$fert]]) - 1L)
  if (is.null(m$light) || is.null(m$breaks[[m$light]])) lb <- 1L
  else {
    if (is.null(light_lo) || is.null(light_hi)) {
      br <- m$breaks[[m$light]]; light_lo <- br[1]; light_hi <- br[length(br)]
    }
    lb <- tab5_bins_overlap(m$breaks[[m$light]], light_lo, light_hi)
    if (!length(lb)) lb <- seq_len(length(m$breaks[[m$light]]) - 1L)
  }
  band <- m$band[m$band$fert %in% fb & m$band$light %in% lb, , drop = FALSE]
  N <- sum(band$n)
  share <- vapply(seq_along(m$classes), function(i) sum(band$n[band$cl == i]), 0)
  names(share) <- m$classes
  fert_n <- if (!nrow(band)) data.frame(fert = integer(), cl = integer(), n = integer())
    else aggregate(n ~ fert + cl, band, sum)
  P <- m$pairs
  swap <- FALSE
  row <- which(P$xa == xpar & P$ya == ypar)
  if (!length(row)) { row <- which(P$xa == ypar & P$ya == xpar); swap <- TRUE }
  if (!length(row)) stop("no precomputed pair for ", xpar, " × ", ypar)
  pid <- P$pid[row[1]]
  ct <- m$counts[m$counts$pid == pid & m$counts$fert %in% fb & m$counts$light %in% lb, , drop = FALSE]
  if (swap && nrow(ct)) { tmp <- ct$bx; ct$bx <- ct$yb; ct$yb <- tmp }
  cells <- if (!nrow(ct)) data.frame(bx = integer(), yb = integer(), cl = integer(), n = integer())
    else aggregate(n ~ bx + yb + cl, ct, sum)
  list(m = m, id = id, xpar = xpar, ypar = ypar, fb = fb, lb = lb, N = N, N_total = m$N,
       share = share, fert_n = fert_n, cells = cells,
       fert_lo = fert_lo, fert_hi = fert_hi, light_lo = light_lo, light_hi = light_hi)
}

ps_hex_mix <- function(hex, share) {
  a <- 0.35 + 0.65 * pmin(pmax(share, 0), 1)
  rgb <- as.numeric(col2rgb(hex) / 255)
  grDevices::rgb(a * rgb[1] + (1 - a), a * rgb[2] + (1 - a), a * rgb[3] + (1 - a))
}
# Sequential 0–100% scale: white at 0, the class colour at 1 (single-class phase map).
ps_hex_seq <- function(hex, share) {
  a <- pmin(pmax(share, 0), 1)
  rgb <- as.numeric(col2rgb(hex) / 255)
  grDevices::rgb(a * rgb[1] + (1 - a), a * rgb[2] + (1 - a), a * rgb[3] + (1 - a))
}
# Dropdown: majority (default) plus each class that exists in this view.
ps_map_modes <- function(m) {
  if (is.null(m)) return(c("Majority class" = "majority"))
  pref <- c("mutualism", "commensalism", "parasitism",
            "mutualist", "cheater", "coexistence",
            "cannot persist", "no fungus persists", "none", "no colonisation")
  cls <- c(intersect(pref, m$classes), setdiff(m$classes, pref))
  c("Majority class" = "majority", stats::setNames(cls, cls))
}
# Colour one cell: grey if n < nmin, else majority mix or sequential class share.
ps_cell_paint <- function(n, counts, mode = "majority", nmin = 15L) {
  n <- as.integer(n)
  if (n <= 0L) return(list(col = "#f3f3f3", share = NA_real_, label = NA_character_))
  if (n < nmin) return(list(col = "#d0d0d0", share = NA_real_, label = NA_character_))
  nms <- names(counts)
  if (is.null(mode) || !nzchar(mode) || !(mode %in% c("majority", nms))) mode <- "majority"
  if (identical(mode, "majority")) {
    k <- which.max(counts)
    share <- as.numeric(counts[[k]]) / n
    list(col = ps_hex_mix(ps_col(nms[k]), share), share = share, label = nms[k])
  } else {
    share <- as.numeric(counts[[mode]]) / n
    list(col = ps_hex_seq(ps_col(mode), share), share = share, label = mode)
  }
}

ps_readout <- function(sl) {
  nmin <- if (is.null(TAB5)) 15L else TAB5$meta$nmin
  pct <- if (sl$N > 0) 100 * sl$share / sl$N else sl$share * 0
  pills <- paste(vapply(names(sl$share), function(cl) {
    sprintf('<span class="pill" style="background:%s;color:%s">%s %s%%</span>',
            ps_col(cl), if (cl %in% c("commensalism", "coexistence")) "#222" else "#fff",
            cl, fx(pct[[cl]], 1))
  }, ""), collapse = " ")
  HTML(sprintf('<div class="readout">
    <div class="banner span2" style="grid-column:1/-1">%s of %s parameter sets (%.1f%%) fall in the selected band.</div>
    <div style="grid-column:1/-1">%s</div>
    <div class="explain" style="grid-column:1/-1">Shares describe this slider box (Latin hypercube; placeholder units), not frequencies in nature.
      Phase-map cells with fewer than %d samples are greyed out.</div></div>',
    format(sl$N, big.mark = ","), format(sl$N_total, big.mark = ","),
    if (sl$N_total) 100 * sl$N / sl$N_total else 0, pills, nmin))
}

ps_bar <- function(sl) {
  m <- sl$m; fn <- sl$fert_n
  br <- m$breaks[[m$fert]]
  cls <- m$classes
  traces <- lapply(seq_along(cls), function(i) {
    d <- fn[fn$cl == i, , drop = FALSE]
    tot <- if (!nrow(fn)) numeric() else tapply(fn$n, fn$fert, sum)
    x <- if (nrow(d)) as.integer(d$fert) else integer()
    y <- if (nrow(d)) 100 * d$n / as.numeric(tot[as.character(d$fert)]) else numeric()
    list(x = x, y = y, type = "bar", name = cls[i],
         marker = list(color = ps_col(cls[i])),
         hovertemplate = paste0(cls[i], ": %{y:.1f}%<extra></extra>"))
  })
  tickv <- sl$fb
  tickt <- vapply(tickv, function(i) paste0(ps_fmt(br[i]), "–", ps_fmt(br[i + 1])), "")
  pfig(traces, baseLayout(ps_sym(m, m$fert), "% of sets in bin", barmode = "stack",
    xaxis = list(title = ps_sym(m, m$fert), tickmode = "array", tickvals = tickv, ticktext = tickt, automargin = TRUE),
    yaxis = list(title = "% of sets in bin", range = c(0, 100)),
    legend = list(orientation = "h", x = 0, y = -0.22)))
}

ps_importance <- function(m) {
  imp <- m$importance[order(m$importance$importance), , drop = FALSE]
  pfig(list(list(x = imp$importance, y = imp$symbol, type = "bar", orientation = "h",
                 marker = list(color = "#4a6b3a"), hovertemplate = "%{y}: %{x:.3f}<extra></extra>",
                 showlegend = FALSE)),
       baseLayout("relative importance (top = 1)", "",
                  xaxis = list(title = "relative importance (top parameter = 1)", range = c(0, 1.05)),
                  yaxis = list(title = "", automargin = TRUE), showlegend = FALSE,
                  margin = list(l = 120, r = 15, t = 10, b = 50)))
}

ps_map_grids <- function(sl, mode = "majority") {
  m <- sl$m; xpar <- sl$xpar; ypar <- sl$ypar
  xb <- m$breaks[[xpar]]; yb <- m$breaks[[ypar]]
  nx <- length(xb) - 1L; ny <- length(yb) - 1L
  nmin <- if (is.null(TAB5)) 15L else as.integer(TAB5$meta$nmin)
  if (is.null(mode) || !nzchar(mode) || !(mode %in% c("majority", m$classes))) mode <- "majority"
  single <- !identical(mode, "majority")
  zcol <- matrix("#f3f3f3", ny, nx)
  hover <- matrix("", ny, nx)
  shares <- matrix(NA_real_, ny, nx)
  ns <- matrix(0L, ny, nx)
  cells <- sl$cells
  for (ix in seq_len(nx)) for (iy in seq_len(ny)) {
    sub <- cells[cells$bx == ix & cells$yb == iy, , drop = FALSE]
    n <- sum(sub$n)
    ns[iy, ix] <- n
    lines <- sprintf("%s: %s–%s", ps_sym(m, xpar), ps_fmt(xb[ix]), ps_fmt(xb[ix + 1]))
    lines <- c(lines, sprintf("%s: %s–%s", ps_sym(m, ypar), ps_fmt(yb[iy]), ps_fmt(yb[iy + 1])))
    if (n == 0) {
      hover[iy, ix] <- paste(c(lines, "no samples in this cell"), collapse = "<br>")
    } else if (n < nmin) {
      zcol[iy, ix] <- "#d0d0d0"
      hover[iy, ix] <- paste(c(lines, sprintf("too few samples (n = %d)", n)), collapse = "<br>")
    } else {
      counts <- integer(length(m$classes)); names(counts) <- m$classes
      for (i in seq_along(m$classes)) counts[i] <- sum(sub$n[sub$cl == i])
      paint <- ps_cell_paint(n, counts, mode, nmin)
      zcol[iy, ix] <- paint$col
      shares[iy, ix] <- paint$share
      brk <- paste(sprintf("%s: %d (%.0f%%)", names(counts), counts, 100 * counts / n), collapse = "<br>")
      if (single) {
        hover[iy, ix] <- paste(c(lines,
          sprintf("%s: %.1f%% (%d of %d)", mode, 100 * paint$share, counts[[mode]], n),
          sprintf("n = %d", n), brk), collapse = "<br>")
      } else {
        hover[iy, ix] <- paste(c(lines, sprintf("n = %d", n), brk), collapse = "<br>")
      }
    }
  }
  list(zcol = zcol, hover = hover, shares = shares, n = ns, mode = mode, single = single,
       nx = nx, ny = ny, xb = xb, yb = yb, nmin = nmin)
}

ps_map <- function(sl, mode = "majority") {
  m <- sl$m; xpar <- sl$xpar; ypar <- sl$ypar
  g <- ps_map_grids(sl, mode)
  nx <- g$nx; ny <- g$ny
  idx <- matrix(seq_len(nx * ny), nrow = ny, ncol = nx, byrow = TRUE)
  col_order <- as.vector(t(g$zcol))
  ncell <- nx * ny
  cs <- lapply(seq_len(ncell), function(k) list((k - 1) / max(1, ncell - 1), col_order[k]))
  traces <- list(list(type = "heatmap", x = seq_len(nx), y = seq_len(ny), z = idx,
                   zmin = 1, zmax = ncell, colorscale = cs, showscale = FALSE,
                   customdata = g$hover, hovertemplate = "%{customdata}<extra></extra>",
                   xgap = 1, ygap = 1))
  if (isTRUE(g$single)) {
    hex <- ps_col(g$mode)
    traces <- c(traces, list(list(
      type = "scatter", mode = "markers",
      x = I(NA_real_), y = I(NA_real_),
      marker = list(
        color = c(0, 100),
        colorscale = list(list(0, "#ffffff"), list(1, hex)),
        cmin = 0, cmax = 100,
        showscale = TRUE,
        colorbar = list(title = list(text = paste0(g$mode, " %")), thickness = 16,
                        len = 0.72, y = 0.52, outlinewidth = 0),
        size = 0.01
      ),
      hoverinfo = "skip", showlegend = FALSE
    )))
    showleg <- FALSE
    marg <- list(l = 70, r = 80, t = 10, b = 70)
  } else {
    traces <- c(traces, lapply(m$classes, function(cl) {
      list(x = I(NA), y = I(NA), type = "scatter", mode = "markers", name = cl,
           marker = list(size = 10, color = ps_col(cl)), hoverinfo = "skip")
    }))
    showleg <- TRUE
    marg <- list(l = 70, r = 15, t = 10, b = 90)
  }
  xm <- ps_mids(g$xb, ps_is_log(m, xpar)); ym <- ps_mids(g$yb, ps_is_log(m, ypar))
  pfig(traces,
       baseLayout(ps_sym(m, xpar), ps_sym(m, ypar),
                  xaxis = list(title = ps_sym(m, xpar), tickmode = "array", tickvals = seq_len(nx),
                               ticktext = ps_fmt(xm), automargin = TRUE, constrain = "domain"),
                  yaxis = list(title = ps_sym(m, ypar), tickmode = "array", tickvals = seq_len(ny),
                               ticktext = ps_fmt(ym), automargin = TRUE),
                  legend = list(orientation = "h", x = 0, y = -0.18), showlegend = showleg,
                  margin = marg))
}
