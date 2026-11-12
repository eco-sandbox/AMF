# ---------- slider definitions (same groups, ranges, steps and hints as v3) ----------
# each item: key, label (plain notation, subified), min, max, step, hint
ST_SLIDERS <- list(
  "Environment" = list(list("S", "Soil-available P, S", 0.5, 60, 0.5, "mg P kg⁻¹ (placeholder scale); ↔ fertility K_P in tabs ②–④"),
                       list("L", "Light / C supply, L", 0.1, 1, 0.01, "relative to maximum")),
  "Plant" = list(list("Amax", "Max C fixation, A_max", 0.2, 2, 0.05, "relative C units"),
                 list("Y", "C → biomass yield, Y", 0.2, 2, 0.05, ""),
                 list("theta", "P requirement per biomass, θ", 0.2, 3, 0.05, "higher = more P-hungry plant"),
                 list("Kr", "Root P half-saturation, K_r", 2, 60, 1, "higher = poorer root foraging (coarse roots)")),
  "Fungus" = list(list("c", "Colonisation, c", 0, 1, 0.01, "fraction of root length"),
                  list("phi", "Cooperativeness, φ", 0, 1, 0.01, "share of fungal P passed to plant"),
                  list("kappa", "C cost, κ", 0, 0.6, 0.01, "fraction of fixed C per unit colonisation"),
                  list("rho", "Root-pathway suppression, ρ", 0, 1, 0.01, "down-regulation of direct uptake"),
                  list("Kf", "Hyphal P half-saturation, K_f", 0.5, 30, 0.5, "lower = better scavenging at low P"),
                  list("U0", "Fungal own P need, U_0", 0, 0.8, 0.01, "hyphal P kept before any is passed on; 0 = no hump")),
  "Classification" = list(list("eps", "Commensalism band, ε", 0, 0.3, 0.01, "|MGR| ≤ ε counts as commensal"),
                          list("n", "Co-limitation sharpness, n", 1, 30, 1, "large = strict Liebig minimum")),
  "Fungal side (plot ⑤)" = list(list("thetaF", "Fungal P requirement, θ_f", 0.05, 1, 0.01, ""),
                                list("Fref", "Reference fungal fitness, F_ref", 0.005, 0.3, 0.005, "fitness with a reference partner")))
DY_SLIDERS <- list(
  "Fertility" = list(list("K", "Soil fertility / host capacity, K_P", 0.5, 60, 0.5, "higher = more fertile (↔ soil P, S, in tab ①)")),
  "Host (plant)" = list(list("r", "Intrinsic growth rate, r", 0.2, 5, 0.1, "reference value: 2"),
                        list("g", "Benefit coefficient, γ", 0, 10, 0.1, "host capacity K_P + γM; reference value: 5"),
                        list("a", "Cost to host per mutualist, a_M", 0.05, 1, 0.01, "reference value: 0.3")),
  "Mutualist fungus M" = list(list("b", "Growth on host, b_M", 0.5, 6, 0.1, "reference value: 3"),
                              list("d", "Death rate, d_M", 0.2, 3, 0.05, "placeholder (tables use 1)"),
                              list("e", "Self-limitation, e_M", 0.1, 20, 0.1, "placeholder (tables use 1; 10 shows all three regimes)")),
  "Simulation" = list(list("H0", "Initial host, H_0", 0.05, 10, 0.05, ""), list("P0", "Initial mutualist, M_0", 0.01, 5, 0.01, ""),
                      list("T", "Time horizon, t_max", 5, 200, 5, "placeholder time units"),
                      list("eps", "Commensal band ε (equilibrium criterion)", 0, 0.3, 0.01, "")))
CH_SLIDERS <- list(
  "Fertility" = list(list("K", "Soil fertility / host capacity, K_P", 0.5, 40, 0.5, "higher = more fertile")),
  "Host H" = list(list("r", "Intrinsic growth rate, r", 0.2, 5, 0.1, "reference value: 2"), list("g", "Benefit coefficient, γ", 0, 10, 0.1, "reference value: 5"),
                  list("g2", "Cheater benefit coefficient, γ_Z", 0, 5, 0.1, "0 = cheater gives nothing (new here)")),
  "Mutualist fungus M" = list(list("a1", "Cost to host, a_M", 0.05, 1, 0.01, "reference value: 0.3"), list("b1", "Growth on host, b_M", 0.5, 6, 0.1, "reference value: 3"),
                              list("d1", "Death rate, d_M", 0.2, 3, 0.05, "reference value: 1"), list("e1", "Self-limitation, e_M", 0.1, 20, 0.05, "reference value: 1; plant-limited preset: 10"),
                              list("c1", "Effect of Z on M, α_MZ", 0, 30, 0.01, "reference value: 1.65 (alt. 1.40)")),
  "Cheater fungus Z" = list(list("a2", "Cost to host, a_Z", 0.05, 1, 0.01, "reference value: 0.4"), list("b2", "Growth on host, b_Z", 0.5, 6, 0.1, "reference value: 2 (b_M/d_M > b_Z/d_Z)"),
                            list("d2", "Death rate, d_Z", 0.2, 3, 0.05, "reference value: 1"), list("e2", "Self-limitation, e_Z", 0.1, 20, 0.05, "reference value: 1; plant-limited preset: 10"),
                            list("c2", "Effect of M on Z, α_ZM", 0, 30, 0.0001, "reference value: 0.6128")),
  "Simulation" = list(list("H0", "Initial host, H_0", 0.05, 5, 0.05, ""), list("P0", "Initial mutualist, M_0", 0, 3, 0.01, ""), list("Q0", "Initial cheater, Z_0", 0, 3, 0.01, ""),
                      list("T", "Time horizon, t_max", 20, 1000, 10, ""), list("phiC", "Cheater cooperativeness, φ_Z (bridge to tab ①)", 0, 1, 0.01, "mutualist uses φ from tab ①")))
SA_SLIDERS <- list(
  "Sanctions" = list(list("s", "Sanction strength, s", 0, 1, 0.01, "0 = no discrimination, 1 = rewards fully proportional to benefit delivered"),
                     list("q0", "Baseline reward / recognition noise, q_0", 0.5, 30, 0.5, "higher = plant tells partners apart less well")),
  "Fertility" = list(list("K", "Soil fertility / host capacity, K_P", 0.5, 40, 0.5, "higher = more fertile")),
  "Competition (demo values)" = list(list("c1", "Effect of Z on M, α_MZ", 0, 30, 0.01, "demo 2.0 (reference value: 1.65)"),
                                     list("c2", "Effect of M on Z, α_ZM", 0, 30, 0.01, "demo 0.3 (reference value: 0.6128, knife-edge)"),
                                     list("g2", "Cheater benefit coefficient, γ_Z", 0, 5, 0.1, "0 = pure cheater")),
  "Self-limitation" = list(list("e1", "Self-limitation of M, e_M", 0.1, 20, 0.05, "reference value: 1; plant-limited preset: 10"),
                           list("e2", "Self-limitation of Z, e_Z", 0.1, 20, 0.05, "reference value: 1; plant-limited preset: 10")),
  "Simulation" = list(list("P0", "Initial mutualist, M_0", 0, 3, 0.01, ""), list("Q0", "Initial cheater, Z_0", 0, 3, 0.01, ""),
                      list("T", "Time horizon, t_max", 20, 1000, 10, "")))

# One slider; label carries the glossary hover text (title attribute), as in v3.
slider_ctl <- function(prefix, tab, it, value) {
  k <- it[[1]]
  div(class = "ctl",
      sliderInput(paste0(prefix, "_", k), label = tags$span(title = gloss_tip(tab, k), class = "glab", HTML(subify(it[[2]]))),
                  min = it[[3]], max = it[[4]], value = value, step = it[[5]], ticks = FALSE, width = "100%"),
      if (nzchar(it[[6]])) tags$small(HTML(subify(it[[6]]))))
}
slider_panel <- function(prefix, tab, groups, defaults, extra = NULL) {
  tagList(lapply(names(groups), function(gn) tagList(tags$h3(class = "grp", gn),
            lapply(groups[[gn]], function(it) slider_ctl(prefix, tab, it, defaults[[it[[1]]]])))),
          extra, actionButton(paste0(prefix, "_reset"), "↺ Reset to defaults", class = "btn-reset w-100"))
}
slider_keys <- function(groups) unlist(lapply(groups, function(g) vapply(g, `[[`, "", 1)))
# collect current slider values into a parameter list on top of defaults
collect <- function(input, prefix, groups, defaults) {
  p <- defaults
  for (k in slider_keys(groups)) { v <- input[[paste0(prefix, "_", k)]]; if (!is.null(v)) p[[k]] <- v }
  p
}
# plant-limited preset for tabs ③/④: e_M = e_Z = 10 as in tab ②, competition scaled by the same factor
plant_limited <- function(d) modifyList(d, list(e1 = 10, e2 = 10, c1 = 10 * d$c1, c2 = 10 * d$c2))
reset_sliders <- function(session, prefix, groups, vals) for (k in slider_keys(groups)) updateSliderInput(session, paste0(prefix, "_", k), value = vals[[k]])

# ---------- small formatting helpers (JS toFixed equivalents) ----------
fx <- function(v, d) formatC(v, format = "f", digits = d)
sg <- function(v, d) paste0(if (v >= 0) "+" else "", fx(v, d))
COL <- c(mutualism = "#1a9850", commensalism = "#9e9e9e", parasitism = "#d7301f")
BAND <- c(mutualism = "rgba(26,152,80,0.16)", commensalism = "rgba(160,160,160,0.20)", parasitism = "rgba(215,48,31,0.14)")
FUNG <- c(mut = "#1a9850", cheat = "#d7301f", host = "#222", hostNM = "#9e9e9e")
clsCol <- function(c) if (!is.na(COL[c])) unname(COL[c]) else "#555"
pill <- function(c) sprintf('<span class="tag" style="background:%s;color:%s">%s</span>', clsCol(c), if (c == "commensalism") "#222" else "#fff", toupper(c))
