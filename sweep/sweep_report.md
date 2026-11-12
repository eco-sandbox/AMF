# Parameter-space sweep of the interactive model (v4.2)

This sweep shows what share of parameter combinations gives mutualism, commensalism (|response| ≤ ε = 0.05) or parasitism in each tab's model, and which parameters drive each outcome.
Scripts: `run_sweep.R` (sampling + models; sources `../shiny/R/` so the equations are exactly the app's), `analyze.R` (tables, importance, figures), `extra.R`, `hump_share.R`. Data: `csv/`. Figures: `figs/`.

**Caveat.** All parameters are illustrative placeholders in relative units. The percentages describe the *slider box* (`csv/ranges.csv`), not nature. Changing the ranges or the sampling scale changes the shares (see the "all uniform" rows). Read them as "how much of the app's parameter box falls into each outcome", and the drivers as robust *directions*.

## Design
| Tab | Model and outcome | Samples | Parameters (ranges = app sliders) |
|---|---|---|---|
| ① | static softmin model, closed form; MGR = ln(B_AM/B_NM) | 10⁵ Latin hypercube (+10⁵ all-uniform) | S, L, A_max, Y, θ, K_r, c, φ, κ, ρ, K_f, U_0 (new hump parameter), n |
| ② | N&F eq. 1, **analytic** equilibrium; equilibrium criterion ln(H*/K_P) ±ε, pulse criterion sign J_HM; "cannot persist" if K_P ≤ d_M/b_M | 10⁵ LHS (+10⁵ all-uniform) | K_P 0.5–60, r, γ, a_M, b_M, d_M, e_M |
| ③ | eq. 2 (host + M + cheater Z), app's adaptive RK4 to t = 2000 from (1, 0.1, 0.1); ln(H*/K_P) ±ε and who wins | 4·10⁴ LHS | K_P 0.5–40, r, γ, γ_Z, a, b, d, e of M and Z, α_MZ, α_ZM (0–30) |
| ④ | as ③ + sanctions (s, q_0); each set is also run with s = 0 (paired) | 4·10⁴ LHS (×2) | ③ + s 0–1, q_0 0.5–30 |

- Sampling scale: rates, costs, yields and half-saturations are log-uniform (A_max, Y, θ, K_r, K_f, n, r, a, b, d, e, q_0). Fractions, soil P/fertility, γ, α and s are uniform.
- ε = 0.05 and the fixed app values (u_r = u_f = 1) are kept.
- ODE settling: 0.16 % of sets in ③/④ still had a relative residual > 10⁻³ at t = 2000. A continuation test on 4000 sets moved H by < 2·10⁻⁴.
- Variable importance: random-forest permutation importance (ranger, 300 trees, 3·10⁴ sets, inputs scaled to [0, 1] of their range). Directions come from partial rank correlation (PRCC) with the continuous response, and from where each class sits in each range (`*_drivers.png`).
- Readable rules: depth-3 classification trees, in `csv/rpart_rules.txt`.

## Headline shares (% of parameter sets)
| Model | mutualism | commensalism | parasitism | other |
|---|---|---|---|---|
| ① static MGR | **3.5** | 27.6 | **68.9** | no colonisation 0.0 |
| ① same sets, U_0 = 0 | 7.8 | 29.1 | 63.1 | |
| ① all-uniform sampling | 9.6 | 25.9 | 64.5 | |
| ② equilibrium criterion | **10.9** | 2.5 | **86.0** | cannot persist 0.7 |
| ② pulse criterion (strict sign) | 4.6 | – | 94.8 | cannot persist 0.7 |
| ② equilibrium, all-uniform | 19.8 | 5.8 | 73.9 | cannot persist 0.6 |
| ③ plant ln(H*/K_P), community | **9.4** | 1.7 | **88.8** | no fungus persists 0.2 |
| ④ plant, with sanctions | 10.0 | 1.6 | 88.2 | 0.2 |

**Who wins.**
- Tab ③: mutualist 49.3 %, cheater 49.4 %, coexistence 1.1 %, none 0.2 %.
- Tab ④: mutualist 52.9 %, cheater 45.7 % (the same sets without sanctions: 49.3 / 49.4).

### Other cross-tabulations
- **①:** 58 % of all sets are parasitic with the AM plant C-limited. Mutualism is almost always P-limited (2.6 of the 3.5 %).
- **① hump** (`csv/tab1_hump_share.csv`, 2·10⁴ sets): MGR(S) has an interior peak in 23.3 % of sets with U_0 sampled, versus 0.9 % with U_0 = 0. The full parasitism → mutualism → parasitism sequence along S appears in 4.4 % of sets, versus 0 % with U_0 = 0.
- **② equilibrium vs pulse** (persisting sets only):
  - both parasitic: 86.6 %
  - eq. mutualism but pulse parasitism: 6.9 %
  - both mutualism: 4.1 %
  - pulse mutualism always implies H* ≥ K_P, as the exact rules require.
- **③ invasion analysis** (N&F App. A.2; sets where both fungi persist alone):
  - neither can invade the other, so the outcome is set by priority (bistable): 58.2 %
  - only Z invades: 20.2 %
  - only M invades: 20.5 %
  - mutual invasion (coexistence): 1.1 %
- **③ vs the M-only reference:** the community leaves the plant *better off* than M alone in 13.8 % of sets and worse off in 37.1 % (the rest: M wins, so the two are equal).
- **④ sanctions** (paired with s = 0):
  - ln(H*/K_P) rises by more than 0.01 in 3.8 % of sets, falls in 2.5 %, and is unchanged in 93.6 %.
  - Cheater wins drop from 49.5 % (s = 0) to 42.5 % for s > 0.8 (`csv/tab4_sanction_effect_by_s.csv`).
  - With strong, well-discriminating sanctions (s > 0.5, q_0 < 2), cheater wins drop from 50 % to 41 %, but plant mutualism only rises from 9.2 % to 10.6 %.

## By fertility (and light)
% mutualism / commensalism / parasitism per bin (`csv/summary_by_bins.csv`, `figs/*_by_*.png`):

| Bin | ① S | ② eq. K_P | ② pulse K_P | ③ K_P |
|---|---|---|---|---|
| 0.5–2 | 11.2 / 21.6 / 67.2 | 44.4 / 4.6 / 30.3 (+20.7 cannot persist) | 13.2 / – / 66.1 | 46.0 / 5.1 / 44.0 (+4.8 none) |
| 2–5 | 9.9 / 23.9 / 66.2 | 37.0 / 5.6 / 54.7 | 14.7 / – / 82.6 | 27.6 / 3.6 / 68.8 |
| 5–10 | 7.2 / 25.9 / 66.9 | 24.9 / 4.1 / 71.0 | 10.1 / – / 89.9 | 15.3 / 2.5 / 82.2 |
| 10–20 | 4.4 / 27.8 / 67.8 | 13.6 / 3.2 / 83.3 | 5.8 / – / 94.2 | 7.8 / 1.6 / 90.6 |
| 20–40 | 2.4 / 28.3 / 69.3 | 6.9 / 2.1 / 91.0 | 3.2 / – / 96.8 | 3.1 / 1.0 / 95.9 |
| 40–60 | 1.8 / 28.1 / 70.1 | 3.6 / 1.4 / 95.0 | 1.8 / – / 98.2 | – |

- **Light (tab ① only):** mutualism rises from 1.2 % at L < 0.3 to 5.7 % at L > 0.9. Parasitism falls from 70.8 % to 67.0 %, so more C makes mutualism likelier.
- **Tabs ②–④** have no light parameter. The closest analogue, host growth rate r, raises ② mutualism from 2.4 % (r < 0.5) to 22.6 % (r > 2).
- **The ① hump at population level:** in fine S bins (`figs/tab1_share_along_S_U0.png`), mutualism stays flat at about 11 % for S < 3 and then falls. With U_0 = 0 it falls monotonically from 43 % at S ≈ 0.75. U_0 removes most mutualism in very P-poor soil.

## What drives each outcome
| Model | Top drivers (RF importance, normalised) | Direction |
|---|---|---|
| ① | **c** 1.00, **κ** 0.72; then ρ, θ, Y, A_max, K_r, S, U_0, φ (each ≤ 0.06) | **Commensalism** is mostly weak colonisation (c < 0.15) or a low C cost. **Parasitism** comes from high c × κ, i.e. a C-limited AM plant. **Mutualism** needs moderate c with low κ and ρ, a P-hungry plant with plenty of C (high θ, Y, A_max, L), poor root foraging (high K_r), good hyphae (low K_f, high φ, low U_0) and low S. |
| ② equilibrium | **K_P** 1.00, **e_M** 0.91, **r** 0.82, **a_M** 0.73; then d_M, b_M, γ | Mutualism needs low fertility, a self-limited fungus (high e_M, keeping M* small), a fast-growing host (high r), a cheap fungus (low a_M), a small M* (high d_M, low b_M) and high γ. Tree: K_P < 9.3, e_M ≥ 1.2 and r ≥ 0.8 → 64 % mutualism. "Cannot persist" = K_P < d_M/b_M (low K_P, high d_M, low b_M). |
| ② pulse | **e_M** 1.00, K_P 0.66, d_M 0.55, r 0.51, b_M 0.46, a_M 0.45 | Same directions, with a stricter condition (M* < r/2a_M − K_P/2γ). |
| ③/④ plant | **K_P** 1.00, **r** 0.72, e_M 0.35–0.40, e_Z 0.20–0.25, a_M 0.22–0.26 | As in ②: low fertility, a fast host, self-limited and cheap fungi. α, γ_Z, s and q_0 hardly matter for the plant over the whole box. Tree: K_P < 5.6, r ≥ 1.6, e_M ≥ 0.64 → 63 % mutualism. |
| ③/④ winner | **b_M** 1.00, **b_Z** 0.98, **d_Z** 0.86, **d_M** 0.83; then α_ZM, α_MZ (≈ 0.24) | The fungus with the higher b/d (lower host requirement d/b) wins in 88 % of the sets that have a single winner. Coexistence needs weak competition (low α) and strong self-limitation (high e). Sanctions s rank low (0.01) over the whole box but shift ~7 percentage points of cheater wins at s → 1. |

## Figures (`figs/`)
- Outcome share by bin:
  - `tab1_by_S.png`, `tab1_by_L.png`
  - `tab2_eq_by_K.png`, `tab2_eq_by_r.png`, `tab2_pulse_by_K.png`
  - `tab3_plant_by_K.png`, `tab3_winner_by_K.png`
  - `tab4_plant_by_K.png`, `tab4_winner_by_K.png`
- 2-D phase maps of the top two drivers (share of each outcome per cell):
  - `tab1_phase_c_kappa.png`, plus `tab1_phase_S_L.png` (fertility × light)
  - `tab2_eq_phase_K_e.png`, `tab2_pulse_phase_e_K.png`
  - `tab3_plant_phase_K_r.png`, `tab3_winner_phase_b1_b2.png`
  - `tab4_plant_phase_K_r.png`, `tab4_winner_phase_b1_b2.png`
- Drivers (where each class sits in each range, ordered by importance): `*_drivers.png`
- Hump: `tab1_share_along_S_U0.png`
- Colours as in the app: mutualism green, commensalism grey, parasitism red, cannot persist / none dark grey; winner: mutualist green, cheater red, coexistence grey.

## CSVs (`csv/`)
- `ranges.csv`
- `samples_tab1.csv`, `samples_tab1_alluniform.csv`, `samples_tab2.csv`, `samples_tab2_alluniform.csv`, `samples_tab3.csv`, `samples_tab4.csv`: every set with its outcome
- `summary_overall.csv`, `summary_by_bins.csv`
- `importance_rf.csv`, `prcc.csv`, `class_conditional_mean_u.csv`, `rpart_rules.txt`
- `tab1_hump_share.csv`, `tab1_share_along_S.csv`, `tab4_sanction_effect_by_s.csv`

## Reading the results
- **Parasitism dominates every tab** within the slider box. The cost terms (κ, a) span wide ranges, and the C-limited regime fills most of tab ①'s box.
- **Mutualism occupies a corner:** low fertility, ample host C or fast host growth, and a self-limited, cheap, P-efficient fungus. This is consistent with the fertility gradients in Neuhauser & Fargione (2004) and Hochberg et al. (2000).
- **Fertility is the main condition** in ②–④. In tab ①, colonisation and C cost dominate and S acts mainly through the P-limited corner.
- **Who wins (③/④) is almost independent of who benefits the plant:** it is set by the fungi's own b/d. Fertility sets the plant's outcome.

## How it became a tab (implemented in v4.3)
Tab ⑤ in the app ships `shiny/data/tab5_counts.rds` (binned integer counts, about 0.4 MB; built by `make_tab5_data.R`). The model selector, axis dropdowns, fertility/light band, stacked bar and importance chart match the plan above. There is no live sampling. Clicking a phase-map cell to jump back to another tab is not wired up.
