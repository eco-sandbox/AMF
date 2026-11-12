# Plant–AMF interaction spectrum — v4: R Shiny port of the v3 HTML app (runs natively or in the browser via Shinylive/webR).
# All parameters are illustrative placeholders in relative units, not calibrated to data.
# Helper files in R/ are sourced automatically by Shiny (model_static.R, model_dyn.R, glossary.R, ui_helpers.R, plots.R).
suppressPackageStartupMessages({ library(shiny); library(bslib); library(plotly) })

card_h <- function(title, sub = NULL, ...) card(full_screen = TRUE, card_header(HTML(title), ...), if (!is.null(sub)) div(class = "sub", HTML(sub)))
plot_card <- function(title, sub, id, ..., height = "370px") {
  card(full_screen = TRUE, card_header(HTML(title), ...), div(class = "sub", HTML(sub)), plotlyOutput(id, height = height))
}
tab_layout <- function(side, ...) layout_sidebar(sidebar = sidebar(width = 300, side, bg = "#fff"), ..., fillable = FALSE)

PLOTLY_DEPS <- htmltools::findDependencies(htmltools::as.tags(pfig(list(list(x = I(1), y = I(1), type = "scatter")), list())))

# explicit in-memory cache: under Shinylive/webR there is no app-level cache option, so bindCache() needs one passed in
APP_CACHE <- cachem::cache_mem(max_size = 64 * 1024^2)

ui <- page_navbar(
  title = "Plant–AMF spectrum · v4",
  id = "tabs",
  theme = bs_theme(version = 5, bg = "#fafaf7", fg = "#222222", primary = "#4a6b3a",
                   base_font = font_collection("Segoe UI", "system-ui", "-apple-system", "Roboto", "Helvetica", "Arial", "sans-serif"),
                   "font-size-base" = "0.9rem"),
  navbar_options = navbar_options(bg = "#ffffff", theme = "light"),
  fillable = FALSE,
  header = tagList(
    # load plotly.js / crosstalk / htmlwidgets up front: in webR (Shinylive) several plotly outputs arriving at once
    # otherwise race the dynamically injected scripts ("Plotly is not defined", "crosstalk is not defined")
    htmltools::attachDependencies(tags$span(), PLOTLY_DEPS),
    tags$head(includeCSS("www/styles.css"),
      tags$script(HTML("document.addEventListener('toggle', function(e){ if (e.target.id === 'how' && window.MathJax && MathJax.Hub) MathJax.Hub.Queue(['Typeset', MathJax.Hub, 'how']); }, true);"))),
    div(class = "apphdr",
      tags$h1("Plant–AMF symbiosis: the mutualism – commensalism – parasitism spectrum"),
      tags$p("Outcome colours:",
             span(class = "pill pm", "mutualism"), span(class = "pill pc", "commensalism"), span(class = "pill pp", "parasitism"),
             HTML("&nbsp;·&nbsp;"), tags$b("All parameters are illustrative placeholders in relative units, not calibrated to data.")))),
  # ---------------- ① static ----------------
  nav_panel("① Static physiology", value = "static",
    tab_layout(slider_panel("st", "static", ST_SLIDERS, DEFAULTS),
      layout_columns(col_widths = c(6, 6, 6, 6, 12),
        card(card_header("① Current state"), uiOutput("stReadout")),
        plot_card("② Benefit vs. cost along soil P", "Growth gained from extra fungal P vs. growth lost to the C paid to the fungus (current L, c, φ, κ …). Split evaluated P first; the reverse order gives the same net but a different split.", "pBC"),
        plot_card("③ MGR vs. soil P at several light levels", "MGR = ln(B<sub>AM</sub>/B<sub>NM</sub>); background bands = outcome classes (±ε); dot = current state", "pMGR"),
        plot_card("④ Outcome map", "Click anywhere on the map to move the sliders there. Lines = MGR contours; ✚ = current state.", "pMap",
                  selectInput("axisSel", NULL, width = "190px", selectize = FALSE,
                              c("S × C cost κ" = "kappa", "S × light L" = "L", "S × colonisation c" = "c", "S × cooperativeness φ" = "phi", "S × root suppression ρ" = "rho", "S × fungal P need U₀" = "U0"))),
        plot_card("⑤ Plant × fungus outcome plane",
                  "x = plant response (MGR); y = fungal response Δ<sub>F</sub> = ln(F/F<sub>ref</sub>). Grey line = trajectory as soil P sweeps 0.5→60 (light→dark); big dot = current S; values clipped to ±3 for display. The fungal side is the most speculative part of the model (see “How the model works”); the sign of Δ<sub>F</sub>, and so the quadrant label, depends on the arbitrary F<sub>ref</sub>.",
                  "pPlane", height = "400px")))),
  # ---------------- ② dynamics ----------------
  nav_panel("② Population dynamics", value = "dyn",
    tab_layout(slider_panel("dy", "dyn", DY_SLIDERS, DYN_DEFAULTS,
                 extra = actionButton("dy_paper", HTML("Preset: d<sub>M</sub> = e<sub>M</sub> = 1"), class = "btn-reset w-100")),
      layout_columns(col_widths = c(12, 6, 6, 12),
        card(card_header("Two ways to classify the interaction"),
             div(class = "sub", HTML("dH/dt = rH(1 − H/(K<sub>P</sub>+γM)) − a<sub>M</sub>HM,&nbsp;&nbsp; dM/dt = b<sub>M</sub>HM − d<sub>M</sub>M(1+e<sub>M</sub>M). Slide K<sub>P</sub> (fertility) to watch mutualism → disagreement → parasitism.")),
             uiOutput("dynReadout")),
        plot_card("Time series: host with vs. without fungus", "Without fungus the host grows logistically to K<sub>P</sub>.", "dynTS"),
        plot_card("Phase plane (H, M) with isoclines", "Equilibrium below the vertex ◇ → pulse-mutualistic; above it → pulse-parasitic.", "dynPP"),
        plot_card("Both criteria along fertility K<sub>P</sub>",
                  "Top: equilibrium host density with (black) vs. without (dashed, = K<sub>P</sub>) the fungus. Strips: classification by each criterion (pulse: strict sign, no commensal band; blank = fungus cannot persist). Black dotted lines: exact switch points; red dotted line: K<sub>P</sub> = γr/a<sub>M</sub>, a sufficient bound beyond which both criteria are parasitic.",
                  "dynK", height = "380px")))),
  # ---------------- ③ cheater ----------------
  nav_panel("③ Cheater fungus", value = "cheat",
    tab_layout(slider_panel("ch", "cheat", CH_SLIDERS, CH_DEFAULTS,
                 extra = actionButton("ch_plim", HTML("Preset: plant-limited as tab ② (e<sub>M</sub> = e<sub>Z</sub> = 10, α ×10)"), class = "btn-reset w-100")),
      layout_columns(col_widths = c(12, 6, 6, 12),
        card(card_header("Mutualist M vs. cheater Z competing for one host"),
             div(class = "sub", HTML("Defaults (γ=5, r=2, a<sub>M</sub>=0.3, a<sub>Z</sub>=0.4, b<sub>M</sub>=3, b<sub>Z</sub>=2, α<sub>MZ</sub>=1.65, α<sub>ZM</sub>=0.6128, d<sub>M</sub>=d<sub>Z</sub>=e<sub>M</sub>=e<sub>Z</sub>=1). The cheater gives the host no benefit but is the better competitor.")),
             uiOutput("chReadout")),
        plot_card("Coexistence / exclusion over time", "Densities of host H, mutualist M and cheater Z.", "chTS"),
        plot_card("Fertility shifts the fungal community toward the cheater",
                  "Final densities vs. K<sub>P</sub> (solid: same initial conditions as the time series). Background: green = mutualist only, grey = coexistence, red = cheater only. Green dotted: host with the mutualist alone.
                   Dashed + top strip: invasion experiment starting at the M-only equilibrium with Z<sub>0</sub> = 0.01 (t = 2000; strip green if Z stayed below 0.01, i.e. failed to invade); where it differs from the solid run the outcome depends on starting densities (bistability).", "chK",
                  checkboxInput("ch_res", "invasion sweep", value = TRUE, width = "150px")),
        plot_card("Plant response with mixed colonisation",
                  "ln(H / H<sub>NM</sub>) over time (bands: ±ε), and the mutualist share M/(M+Z) of the fungal community.", "chMGR")))),
  # ---------------- ④ sanctions ----------------
  nav_panel("④ Plant sanctions", value = "sanc",
    tab_layout(slider_panel("sa", "sanc", SA_SLIDERS, SA_DEFAULTS,
                 extra = actionButton("sa_plim", HTML("Preset: plant-limited as tab ② (e<sub>M</sub> = e<sub>Z</sub> = 10, α ×10)"), class = "btn-reset w-100")),
      layout_columns(col_widths = c(12, 12, 6, 6),
        card(card_header(HTML('Plant sanctions: rewarding fungi in proportion to the phosphorus they deliver')),
             div(class = "sub", HTML("Built on the model of tab ③; s = 0 gives it back exactly. Other parameters as in tab ③ except the demo competition values α<sub>MZ</sub> = 2.0, α<sub>ZM</sub> = 0.3.")),
             uiOutput("saReadout")),
        plot_card("Outcome over time: with sanctions (solid) vs. without (dotted)", "", "saTS"),
        plot_card("Who wins? sanction strength s × fertility K<sub>P</sub>",
                  "Mutualist share M/(M+Z) after t = 300. Black lines: exclusion boundaries. ✚ = current; click to move the K<sub>P</sub> and s sliders there. <span class='status'>(27 × 21 grid; first computation takes a few seconds in the browser)</span>", "saMap1"),
        plot_card("Plant outcome map", "Green = plant better off, red = worse off, grey = within ±ε.", "saMap2",
                  selectInput("saMapSel", NULL, width = "250px", selectize = FALSE,
                              c("plant response ln(H*/K_P)" = "lh", "benefit of sanctions ln(H*/H*_(s=0))" = "gain")))))),
  nav_panel("⑤ Parameter space", value = "pspace",
    tab_layout(
      tagList(
        tags$h3(class = "grp", "Model"),
        selectInput("ps_model", NULL, width = "100%", selectize = FALSE, choices = ps_choices(), selected = "tab1"),
        tags$h3(class = "grp", "Phase-map axes"),
        selectInput("ps_x", "Horizontal", width = "100%", selectize = FALSE, choices = ps_axis_choices("tab1"), selected = "c"),
        selectInput("ps_y", "Vertical", width = "100%", selectize = FALSE, choices = ps_axis_choices("tab1"), selected = "kappa"),
        selectInput("ps_mapmode", "Colour cells by", width = "100%", selectize = FALSE,
                    choices = if (is.null(TAB5)) c("Majority class" = "majority") else ps_map_modes(TAB5$models[["tab1"]]),
                    selected = "majority"),
        tags$h3(class = "grp", "Condition on"),
        sliderInput("ps_fert", label = HTML("Fertility (S or K<sub>P</sub>)"), min = 0.5, max = 60, value = c(0.5, 60),
                    step = 0.5, ticks = FALSE, width = "100%"),
        tags$small("Soil P, S, in ①; fertility K_P in ②–④. Bins that overlap the selected range are included."),
        conditionalPanel("input.ps_model == 'tab1'",
          sliderInput("ps_light", label = HTML("Light, L"), min = 0.1, max = 1, value = c(0.1, 1),
                      step = 0.01, ticks = FALSE, width = "100%"),
          tags$small("Only tab ① has a light axis.")),
        actionButton("ps_reset", "↺ Full parameter box", class = "btn-reset w-100"),
        tags$small(class = "status", "Precomputed Latin-hypercube sample; there is no live sampling.")),
      div(class = "caveat", HTML("Shares depend on the chosen slider ranges; all parameters are illustrative placeholders in relative units, not calibrated to data.")),
      layout_columns(col_widths = c(12, 6, 6, 12),
        card(card_header("Outcome share in the selected band"), uiOutput("psReadout")),
        plot_card("Class share along the fertility band",
                  "Each bar is one fertility bin inside the selected range (100% stacked). Narrow the range to condition.", "psBar"),
        plot_card("Which parameters drive the outcome",
                  "Random-forest permutation importance on the same sample, scaled so the top parameter is 1.", "psImp"),
        plot_card("Phase map of the chosen axes",
                  "Majority-class mode: colour = the most common class (paler = lower majority share). Single-class mode: colour = that class's share (0–100%) on a sequential scale in the class colour. Light grey = too few samples. Hover for share and n.",
                  "psMap", height = "480px")))),
  nav_panel("Glossary", value = "gloss", glossary_ui()),
  footer = withMathJax(div(class = "howwrap", includeHTML("www/how.html")))
)

server <- function(input, output, session) {
  # ---------- ① static ----------
  st <- debounce(reactive(collect(input, "st", ST_SLIDERS, DEFAULTS)), 60)
  output$stReadout <- renderUI({ P <- st(); st_readout(P, static_model(P)) })
  output$pBC <- renderPlotly(st_bc(st()))
  output$pMGR <- renderPlotly(st_mgr(st()))
  output$pMap <- renderPlotly(st_map(st(), input$axisSel))
  output$pPlane <- renderPlotly(st_plane(st()))
  observeEvent(suppressWarnings(event_data("plotly_click", source = "stmap")), {
    ev <- suppressWarnings(event_data("plotly_click", source = "stmap")); if (is.null(ev) || !nrow(ev)) return()
    updateSliderInput(session, "st_S", value = round(ev$x[1] * 2) / 2)
    updateSliderInput(session, paste0("st_", input$axisSel), value = round(ev$y[1] * 100) / 100)
  })
  observeEvent(input$st_reset, reset_sliders(session, "st", ST_SLIDERS, DEFAULTS))
  # ---------- ② dynamics ----------
  dy <- debounce(reactive(collect(input, "dy", DY_SLIDERS, DYN_DEFAULTS)), 60)
  dyC <- reactive(criteria2(dy()))
  dySim <- reactive({ D <- dy(); simulate(as3(D), c(D$H0, D$P0, 0), D$T, 0.01, 500) })
  output$dynReadout <- renderUI(dy_readout(dy(), dyC()))
  output$dynTS <- renderPlotly(dy_ts(dy(), dySim()))
  output$dynPP <- renderPlotly(dy_pp(dy(), dyC(), dySim()))
  output$dynK <- renderPlotly(dy_k(dy(), dyC()))
  observeEvent(input$dy_reset, reset_sliders(session, "dy", DY_SLIDERS, DYN_DEFAULTS))
  observeEvent(input$dy_paper, reset_sliders(session, "dy", DY_SLIDERS, modifyList(DYN_DEFAULTS, list(d = 1, e = 1))))
  # ---------- ③ cheater ----------
  ch <- debounce(reactive(modifyList(collect(input, "ch", CH_SLIDERS, CH_DEFAULTS), list(s = 0))), 120)
  chSim <- reactive({ p <- ch(); simulate(p, c(p$H0, p$P0, p$Q0), p$T, 0.05, 600) })
  chSw <- reactive({ p <- ch(); res <- isTRUE(input$ch_res)
    withProgress(message = if (res) "K_P sweeps (2 × 60 simulations)…" else "K_P sweep (60 simulations)…", value = 0.2, ch_sweep(p, res)) }) |>
    bindCache(ch()[setdiff(names(CH_DEFAULTS), c("K", "T", "phiC", "eps"))], isTRUE(input$ch_res), cache = APP_CACHE)
  output$chReadout <- renderUI(ch_readout(ch(), chSim(), st()))
  output$chTS <- renderPlotly(ch_ts(chSim()))
  output$chK <- renderPlotly(ch_k(ch(), chSw()))
  output$chMGR <- renderPlotly(ch_mgr(ch(), chSim()))
  observeEvent(input$ch_reset, reset_sliders(session, "ch", CH_SLIDERS, CH_DEFAULTS))
  observeEvent(input$ch_plim, reset_sliders(session, "ch", CH_SLIDERS, plant_limited(CH_DEFAULTS)))
  # ---------- ④ sanctions ----------
  sa <- debounce(reactive(collect(input, "sa", SA_SLIDERS, SA_DEFAULTS)), 250)
  saSims <- reactive({ p <- sa(); list(sim = simulate(p, c(p$H0, p$P0, p$Q0), p$T, 0.05, 600),
                                       sim0 = simulate(modifyList(p, list(s = 0)), c(p$H0, p$P0, p$Q0), p$T, 0.05, 600)) })
  saMap <- reactive({
    p <- sa()
    withProgress(message = "Computing s × K_P map (567 simulations)…", value = 0, {
      sa_map_compute(p, progress = function(f) setProgress(f))
    })
  }) |> bindCache(sa()[setdiff(names(SA_DEFAULTS), c("s", "K", "T", "eps"))], cache = APP_CACHE)
  output$saReadout <- renderUI({ x <- saSims(); sa_readout(sa(), x$sim, x$sim0) })
  output$saTS <- renderPlotly({ x <- saSims(); sa_ts(sa(), x$sim, x$sim0) })
  output$saMap1 <- renderPlotly(sa_map1(sa(), saMap()))
  output$saMap2 <- renderPlotly(sa_map2(sa(), saMap(), input$saMapSel))
  observeEvent(suppressWarnings(event_data("plotly_click", source = "samap")), {
    ev <- suppressWarnings(event_data("plotly_click", source = "samap")); if (is.null(ev) || !nrow(ev)) return()
    updateSliderInput(session, "sa_K", value = round(ev$x[1] * 2) / 2)
    updateSliderInput(session, "sa_s", value = round(ev$y[1] * 100) / 100)
  })
  observeEvent(input$sa_reset, reset_sliders(session, "sa", SA_SLIDERS, SA_DEFAULTS))
  observeEvent(input$sa_plim, reset_sliders(session, "sa", SA_SLIDERS, plant_limited(SA_DEFAULTS)))
  # ---------- ⑤ parameter space ----------
  observeEvent(input$ps_model, {
    if (is.null(TAB5) || !input$ps_model %in% names(TAB5$models)) return()
    m <- TAB5$models[[input$ps_model]]
    ch <- ps_axis_choices(input$ps_model)
    updateSelectInput(session, "ps_x", choices = ch, selected = m$def_x)
    updateSelectInput(session, "ps_y", choices = ch, selected = m$def_y)
    r <- m$ranges[m$ranges$par == m$fert, , drop = FALSE]
    updateSliderInput(session, "ps_fert", label = HTML(subify(ps_sym(m, m$fert))),
                      min = r$lo[1], max = r$hi[1], value = c(r$lo[1], r$hi[1]),
                      step = if (r$hi[1] <= 2) 0.01 else 0.5)
    modes <- ps_map_modes(m)
    keep <- input$ps_mapmode
    if (is.null(keep) || !(keep %in% unname(modes))) keep <- "majority"
    updateSelectInput(session, "ps_mapmode", choices = modes, selected = keep)
  }, ignoreNULL = TRUE)
  observeEvent(input$ps_x, {
    if (is.null(TAB5) || !identical(input$ps_x, input$ps_y)) return()
    m <- TAB5$models[[input$ps_model]]
    alt <- setdiff(c(m$def_x, m$def_y, m$axes), input$ps_x)[1]
    if (!is.na(alt)) updateSelectInput(session, "ps_y", selected = alt)
  })
  observeEvent(input$ps_y, {
    if (is.null(TAB5) || !identical(input$ps_x, input$ps_y)) return()
    m <- TAB5$models[[input$ps_model]]
    alt <- setdiff(c(m$def_y, m$def_x, m$axes), input$ps_y)[1]
    if (!is.na(alt)) updateSelectInput(session, "ps_x", selected = alt)
  })
  observeEvent(input$ps_reset, {
    if (is.null(TAB5) || is.null(input$ps_model)) return()
    m <- TAB5$models[[input$ps_model]]
    r <- m$ranges[m$ranges$par == m$fert, , drop = FALSE]
    updateSliderInput(session, "ps_fert", value = c(r$lo[1], r$hi[1]))
    if (!is.null(m$light)) updateSliderInput(session, "ps_light", value = c(0.1, 1))
  })
  ps <- debounce(reactive({
    req(input$ps_model, input$ps_x, input$ps_y, input$ps_fert, TAB5)
    fert <- input$ps_fert; light <- input$ps_light
    if (is.null(light)) light <- c(0.1, 1)
    tab5_slice(TAB5, input$ps_model, input$ps_x, input$ps_y, fert[1], fert[2], light[1], light[2])
  }), 80)
  output$psReadout <- renderUI({ sl <- ps(); req(sl); ps_readout(sl) })
  output$psBar <- renderPlotly({ sl <- ps(); req(sl); ps_bar(sl) })
  output$psImp <- renderPlotly({ req(input$ps_model, TAB5); ps_importance(TAB5$models[[input$ps_model]]) })
  output$psMap <- renderPlotly({
    sl <- ps(); req(sl)
    mode <- input$ps_mapmode
    if (is.null(mode) || !nzchar(mode)) mode <- "majority"
    ps_map(sl, mode)
  })
}

shinyApp(ui, server)
