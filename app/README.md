# Plant–AMF spectrum · v4 (R Shiny / Shinylive)

R Shiny port of the v3 HTML teaching app (`../amf_spectrum_app.html`). Six tabs: ① Static physiology, ② Population dynamics (Neuhauser & Fargione 2004), ③ Cheater fungus, ④ Plant sanctions, ⑤ Parameter space, and the Glossary ("One notion to rule them all"). All parameters are illustrative placeholders in relative units. They are not calibrated to data.

## Layout
```
app.R              UI (bslib page_navbar / layout_sidebar / cards) + server
R/model_static.R   static model (softmin, MGR, classification)
R/model_dyn.R      N&F eq. 1/2 + sanctions, adaptive RK4 (ported 1:1 from v3 JS; deSolve is not on webR) + vectorised grid version
R/plots.R          plotly figures (same traces/colours as v3)
R/tab5.R           tab ⑤: load precomputed bins, aggregate, plot
R/glossary.R       glossary table + slider tooltips, read from glossary.json
R/ui_helpers.R     slider definitions (same ranges as v3), formatting
glossary.json      SINGLE source of the glossary (app tooltips, Glossary tab, ../glossary.md)
data/tab5_counts.rds  binned class counts for tab ⑤ (~0.4 MB; no raw samples)
www/styles.css, www/how.html   v3 styles and the "How the model works" section (MathJax)
tools/make_glossary_md.R       regenerates ../glossary.md from glossary.json
tests/                         equivalence + glossary-sync + hump + tab ⑤ tests
```

## Run locally (native R)
Needs R ≥ 4.3 with `shiny`, `bslib`, `plotly`, `jsonlite`, `cachem`.
```r
install.packages(c("shiny", "bslib", "plotly", "jsonlite"))
shiny::runApp("app")          # from the repository root
```

## Shinylive (runs entirely in the browser via webR, no R server)
```r
install.packages("shinylive")
# export only the app files (tests/ and tools/ are not needed in the browser)
shinylive::export("app", "shinylive_site")
```
Then **serve the folder over HTTP**. Opening `index.html` via `file://` does not work, because service workers and WebAssembly need http(s):
```sh
cd shinylive_site && python3 -m http.server 8008   # open http://localhost:8008
```
First load downloads webR + packages (~110 MB uncompressed, mostly plotly's dependency tree). Expect ~15–20 s on a fast connection (more on slow ones). After that the browser caches it.

## Free hosting (static files only)
- **GitHub Pages:** commit the contents of `shinylive_site/` to a repo (e.g. a `docs/` folder or a `gh-pages` branch), then enable Pages in *Settings → Pages*. Add an empty `.nojekyll` file so folders are served as-is.
- **Netlify:** drag and drop the `shinylive_site/` folder onto app.netlify.com/drop, or connect the repo with publish directory `shinylive_site`.
- Any static host that serves `.wasm` files with the right MIME type works too (Cloudflare Pages, GitLab Pages).

## Tests (run from `app/`)
```sh
node tests/make_js_reference.js        # writes tests/js_reference.json from the v3 JS model
Rscript tests/test_equivalence.R       # R vs JS vs Python: MGR points, N&F Table 1/2, sanctions thresholds, maps, sweep
Rscript tests/test_hump.R             # U_0 = 0 reproduces the v4.1 static model bit for bit; default U_0 gives a hump
Rscript tests/test_tab5.R             # tab ⑤ full-band shares match the sweep headlines; conditioning sums bins
node tests/check_glossary_sync.js      # v3 HTML GLOSSARY == glossary.json
Rscript tools/make_glossary_md.R       # regenerate ../glossary.md
```
