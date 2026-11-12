# AMF

Interactive model of the plant–arbuscular mycorrhizal fungus (AMF) spectrum from mutualism through commensalism to parasitism.

**Live app:** https://eco-sandbox.github.io/AMF/

All parameter values are illustrative placeholders in relative units; they are not calibrated to data.

## Tabs

1. **① Static physiology** — biomass with and without AMF, mycorrhizal growth response (MGR), Liebig co-limitation of C and P, and a hump-shaped response of MGR to soil phosphorus when the fungus keeps some P for itself (`U_0`).
2. **② Population dynamics** — Neuhauser & Fargione (2004) host–mutualist model (eq. 1): time series, isoclines, and the equilibrium vs pulse mutualism/parasitism criteria.
3. **③ Cheater fungus** — mutualist vs cheater competing for the host (eq. 2); invasion criteria and who wins.
4. **④ Plant sanctions** — reciprocal rewards that shift carbon toward the better partner (inspired by biological-market ideas; not a model from the papers below).
5. **⑤ Parameter space** — precomputed Latin-hypercube class shares (no live sampling in the browser): choose a model and two axes, condition on a fertility (and, for ①, light) band, see stacked class shares, variable importance, and a phase map coloured by majority class or by one class's share (0–100%).

A glossary tab lists every symbol.

## Sources

Conceptual sources (the app is a synthesis, not a 1:1 replica of any one paper):

- **Neuhauser & Fargione (2004)**, *Ecol. Model.* 177: 337–352 — host–symbiont model with both benefit and cost; rising soil fertility turns mutualism into parasitism.
- **Hochberg, Gomulkiewicz, Holt & Thompson (2000)**, *J. Evol. Biol.* 13: 213–222 — habitat productivity gradients: mutualists favoured in poor/sink habitats, virulent symbionts in productive ones.
- **Schwartz & Hoeksema (1998)**, *Ecology* 79: 1029–1038 — biological-market / comparative-advantage view; trade is mutualistic only inside a window of exchange rates; P fertilisation shrinks gains.
- **Grasso, Ryan, Albornoz & Renton (2025)**, *New Phytol.* 248: 1429–1441 — Liebig co-limitation and one-to-one C-for-P trade; giving C is costly only when C limits growth.
- **Hammerstein & Noë (2016)**, *Phil. Trans. R. Soc. B* 371: 20150101 — biological trade and markets: partner choice, sanctions, exchange rates.

## Files

- `app/` — Shiny app (`app.R`, `R/`, `www/`, `glossary.json`, `data/tab5_counts.rds`).
- `app/tests/` — equivalence, hump-shaped fertility, and tab ⑤ checks (run from `app/`).
- `app/tools/make_glossary_md.R` — regenerates `glossary.md` from `glossary.json`.
- `glossary.md` — the same glossary as a Markdown table.
- `sweep/` — scripts that resample the slider box and rebuild tab ⑤ (`run_sweep.R`, `analyze.R`, `make_tab5_data.R`, and related helpers). Small summary CSVs and figures are included; raw `samples_*.csv` files are not (they are larger than 5 MB).
- `.github/workflows/deploy-shinylive.yml` — exports `app/` with `shinylive` (including `data/tab5_counts.rds`) and deploys to GitHub Pages.

## License

BSD 2-Clause. See `LICENSE`.
