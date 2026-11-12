# One notion to rule them all

Glossary and notation for the plant–AMF interaction-spectrum model. One notation is used in every tab, formula, slider label and hover text.

**Competition convention:** α_ij = effect of species j on species i. So α_MZ = effect of the cheater Z on the mutualist M (paper c₁), and α_ZM = effect of M on Z (paper c₂). In the model they appear as dM/dt = b_M·H·M − d_M·M(1 + e_M·M + α_MZ·Z) and dZ/dt = b_Z·H·Z − d_Z·Z(1 + e_Z·Z + α_ZM·M).

**Source column:** short reference of the paper a symbol comes from; blank = introduced here or a generic term. **Source symbol** = the symbol used in that source.

## Symbols

| Symbol | Meaning | Notes | Units | Tab(s) | Source | Source symbol |
|---|---|---|---|---|---|---|
| H | Host density | Plant population density. | relative density | ②③④ | Neuhauser & Fargione 2004 | H |
| H* | Equilibrium host density | Long-run host density with the fungi present. | relative density | ②③④ | Neuhauser & Fargione 2004 | H*, H̄ |
| H_NM | Host density without fungi | Logistic growth to K_P; at equilibrium H_NM = K_P. | relative density | ②③ | Neuhauser & Fargione 2004 | – |
| M | Mutualist symbiont density | Fungus that can provide benefit (raises host capacity via γ). | relative density | ②③④ | Neuhauser & Fargione 2004 | P |
| Z | Cheater density | Fungus that provides no benefit (γ_Z = 0 in the paper) but competes for the host. | relative density | ③④ | Neuhauser & Fargione 2004 | Q |
| K_P | Phosphorus / soil fertility (host carrying capacity without fungi) | Higher K_P = more fertile. Conceptually increases with soil-available P, S (tab ①); no quantitative mapping K_P(S) is implied. | relative density | ②③④ | Neuhauser & Fargione 2004 | K |
| γ | Benefit coefficient | Increase in host carrying capacity per mutualist: capacity = K_P + γM. | host capacity per unit M | ②③④ | Neuhauser & Fargione 2004 | γ |
| γ_Z | Benefit coefficient of the cheater | 0 = pure cheater (paper). Capacity becomes K_P + γM + γ_Z Z. | host capacity per unit Z | ③④ | cf. Neuhauser & Fargione 2004 | – |
| a_M, a_Z | Cost to host per symbiont | Carbon or other cost (exploitation) per unit mutualist / cheater. | per density per time | ②③④ | Neuhauser & Fargione 2004 | a, a₁, a₂ |
| α_MZ, α_ZM | Competition coefficients between M and Z | Convention α_ij = effect of j on i: α_MZ = effect of the cheater Z on the mutualist M (paper c₁), α_ZM = effect of M on Z (paper c₂). As in the paper they act inside the density-dependent death term, d_M M(1 + e_M M + α_MZ Z). | dimensionless | ③④ | Neuhauser & Fargione 2004 | c₁, c₂ |
| r | Host intrinsic growth rate | Per-capita host growth at low density. | per time | ②③④ | Neuhauser & Fargione 2004 | r |
| b_M, b_Z | Symbiont growth rate on the host | Per-capita fungal growth per unit host. The paper assumes b_M/d_M > b_Z/d_Z (mutualist grows faster). | per host density per time | ②③④ | Neuhauser & Fargione 2004 | b, b₁, b₂ |
| d_M, d_Z | Symbiont density-independent death rate |  | per time | ②③④ | Neuhauser & Fargione 2004 | d, d₁, d₂ |
| e_M, e_Z | Symbiont self-limitation (density-dependent death) | Strength of intraspecific competition. | per density | ②③④ | Neuhauser & Fargione 2004 | e, e₁, e₂ |
| M* | Equilibrium mutualist density | Interior equilibrium of the host–mutualist model (solved analytically). Pulse-mutualistic iff M* < r/2a_M − K_P/2γ (vertex height); equilibrium-mutualistic iff M* < r/a_M − K_P/γ. | relative density | ② | Neuhauser & Fargione 2004 | P* |
| J_HM | Pulse-criterion value ∂(dH/dt)/∂M at equilibrium | = H*[rγH*/(K_P+γM*)² − a_M]; > 0 mutualism, < 0 parasitism. | per time | ② | Neuhauser & Fargione 2004 | ∂(dH/dt)/∂P |
| γr/a_M | Sufficient parasitism bound | If K_P > γr/a_M the vertex of the host isocline lies below M = 0 and both criteria are always parasitic. Only sufficient: the actual switch points (see M*) lie at lower K_P. | like K_P | ② | Neuhauser & Fargione 2004 | γr/a |
| λ_M^(M), λ_Z^(M), λ_M^(Z), λ_Z^(Z) | Symbiont carrying capacities for invasion criteria | λ_i^(X) = (1/e_i)((b_i/d_i) H̄^(X) − 1), host fixed at the X-only equilibrium. Z invades the M-only equilibrium iff α_ZM < e_Z λ_Z^(M)/λ_M^(M); M invades the Z-only equilibrium iff α_MZ < e_M λ_M^(Z)/λ_Z^(Z). | relative density | ③ | Neuhauser & Fargione 2004 | λ₁ᴾ, λ₂ᴾ, λ₁ᑫ, λ₂ᑫ |
| H_0, M_0, Z_0 | Initial densities |  | relative density | ②③④ |  | – |
| t, t_max | Time and simulated time horizon |  | placeholder time units | ②③④ |  | – |
| s | Sanction strength | 0 = no discrimination; 1 = rewards fully proportional to benefit delivered. | dimensionless, 0–1 | ④ | cf. Hammerstein & Noë 2016 | – |
| q_0 | Baseline reward / recognition noise | Added to every fungus’ benefit before rewards are computed; higher = plant tells partners apart less well. | like γ | ④ | cf. Hammerstein & Noë 2016 | – |
| q_M, q_Z, q̄ | Perceived benefit per fungus and its abundance-weighted mean | q_M = γ + q_0, q_Z = γ_Z + q_0, q̄ = (M q_M + Z q_Z)/(M + Z). | like γ | ④ | cf. Hammerstein & Noë 2016 | – |
| w_M, w_Z | Reward weights (carbon per unit fungus, relative) | w_i = (1 − s) + s q_i/q̄; M w_M + Z w_Z = M + Z, so sanctions redistribute the reward per unit fungus without changing its total. The host’s carbon drain a_M w_M M + a_Z w_Z Z is not conserved when a_M ≠ a_Z. Multiply both a_i and b_i. | dimensionless | ④ | cf. Hammerstein & Noë 2016 | – |
| M/(M+Z) | Mutualist share of the fungal community | Used for mixed colonisation and the sanctions map. | 0–1 | ③④ |  | – |
| φ_Z, φ_mix | Cheater cooperativeness and community cooperativeness | Bridge to tab ①: φ_mix = [M φ + Z φ_Z]/(M + Z) replaces φ in the static model. | 0–1 | ③ (→①) |  | – |
| ε | Commensalism band | \|response\| ≤ ε counts as commensal (neutral). Used for MGR, ln(H*/K_P) and Δ_F. | log-ratio | ①②③④ |  | – |
| S | Soil-available phosphorus | Drives root and hyphal P uptake in tab ①. Relation: higher S ↔ higher fertility K_P in tabs ②–④ (conceptual only). | placeholder “mg P kg⁻¹” scale | ① | cf. Hochberg et al. 2000; Schwartz & Hoeksema 1998 | – |
| L | Light / relative carbon supply |  | 0–1, relative to maximum | ① | cf. Schwartz & Hoeksema 1998 | – |
| A_max, A | Maximum and actual carbon fixation | A = A_max L. | relative C units | ① |  | – |
| Y | Carbon → biomass yield |  | biomass per C | ① |  | – |
| θ | Phosphorus requirement per unit biomass | Higher = more P-hungry plant. | P per biomass | ① | cf. Grasso et al. 2025 | – |
| u_r, K_r | Root P uptake: maximum rate and half-saturation constant | Michaelis–Menten; K_r is a half-saturation constant, not a carrying capacity (unrelated to K_P). Subscript r = root (unrelated to growth rate r). | relative; K_r in S units | ① |  | – |
| u_f, K_f | Hyphal P uptake: maximum rate and half-saturation constant | K_f < K_r: hyphae scavenge better at low P. | relative; K_f in S units | ① |  | – |
| U_r, U_f | Root and hyphal P uptake rates | U = u S/(K + S). | relative P units | ① |  | – |
| U_0 | Fungal own P need | Hyphal P the fungus keeps before passing any on: transfer = cφ·max(U_f − U_0, 0). At very low S nothing is passed, so MGR ≈ ln(1 − ρc) < 0 and MGR peaks at intermediate S. U_0 = 0 recovers the model without a hump. | relative P units (as U_f) | ① | cf. Schwartz & Hoeksema 1998 | X |
| c | Colonisation | Fraction of root length colonised. | 0–1 | ① |  | – |
| φ | Cooperativeness of the (mutualist) fungus | Share of fungal P (above the fungus's own need U_0) passed to the plant. | 0–1 | ①③ | cf. Grasso et al. 2025 | – |
| κ | Carbon cost | Fraction of fixed C per unit colonisation passed to the fungus. | 0–1 | ① | cf. Grasso et al. 2025 | – |
| ρ | Root-pathway suppression | Down-regulation of direct root uptake in colonised roots. | 0–1 | ① |  | – |
| P_AM, P_NM | Phosphorus acquired by the mycorrhizal (AM) / non-mycorrhizal (NM) plant | P here always means phosphorus (never a fungal density). | relative P units | ① |  | – |
| C_AM, C_NM | Carbon available for growth (AM / NM plant) | C_AM = A(1 − cκ). | relative C units | ① |  | – |
| B_AM, B_NM | Plant biomass with / without mycorrhiza | B = softmin(Y C, P/θ). Renamed from M_AM/M_NM to avoid a clash with mutualist density M. | relative biomass | ① |  | – |
| n | Co-limitation sharpness | softmin(x,y) = (x⁻ⁿ + y⁻ⁿ)^(−1/n); n → ∞ is the strict Liebig minimum. | dimensionless | ① | cf. Grasso et al. 2025 | – |
| MGR | Mycorrhizal growth response | ln(B_AM/B_NM). In tabs ②–④ the analogous population-level response is ln(H*/K_P). | log-ratio | ① |  | – |
| T_eff | Effective exchange rate | Extra P gained per unit C paid: (cφU_f − ρcU_r)/(cκA). Market view after Schwartz & Hoeksema 1998. | P per C | ① | cf. Schwartz & Hoeksema 1998 | – |
| F, Y_f, θ_f, β_f | Fungal growth, its yield, P requirement and own C uptake | F = Y_f softmin(cκA + β_f, (cU_f − cφ·max(U_f − U_0, 0))/θ_f). Speculative. | relative | ① |  | – |
| F_ref, Δ_F | Reference fungal fitness and fungal response | Δ_F = ln(F/F_ref); F_ref = fitness with a reference partner (AMF are obligate). The sign of Δ_F, and so the 2-D label, depends on the choice of F_ref. | relative; log-ratio | ① | cf. Grasso et al. 2025 | – |

Tabs: ① static physiology, ② population dynamics, ③ cheater fungus, ④ plant sanctions, ⑤ parameter space.

## Terms

- **Mutualism**: Both partners are better off together. Here: the plant’s response (MGR or ln(H*/K_P)) is above +ε. Colour: green.
- **Commensalism**: One partner gains while the other is essentially unaffected; here the plant response lies within ±ε. Colour: grey.
- **Parasitism**: The fungus gains at the plant’s expense: the plant response is below −ε. Colour: red.
- **Amensalism / competition**: Other sign combinations on the plant × fungus plane (tab ①): (0,−) amensalism, (−,−) competition.
- **MGR (mycorrhizal growth response)**: How much bigger (or smaller) a mycorrhizal plant is than a non-mycorrhizal one, on a log scale: ln(B_AM/B_NM). MGR = 0.1 ≈ 10% bigger.
- **Liebig limitation / co-limitation**: Growth is set by the scarcest resource (here carbon or phosphorus). The smooth version (softmin) lets both matter near the switch point.
- **C-limited / P-limited**: Which resource currently sets growth. A P-limited plant profits from fungal P; a C-limited plant mainly feels the carbon it pays.
- **Exchange rate**: How much P the plant gets per unit carbon given (T_eff). Trade is beneficial only within a window of exchange rates (biological market view).
- **Carrying capacity**: The density a population settles at when resources limit it. K_P is the host’s carrying capacity without fungi and stands in for soil fertility.
- **Equilibrium criterion**: Long-term experiment: grow the plant with and without the fungus until both settle; mutualism if H* > K_P (ln(H*/K_P) > ε).
- **Pulse criterion**: Short-term experiment: at equilibrium, add a few more fungi; mutualism if this speeds up host growth (J_HM > 0). Can disagree with the equilibrium criterion.
- **Isocline (nullcline)**: Curve in the phase plane where one population does not change. Equilibria are where the host and fungus isoclines cross.
- **Vertex**: The peak of the host isocline. An equilibrium below it is pulse-mutualistic, above it pulse-parasitic.
- **Cheater**: A fungus that takes host carbon but delivers no (or little) benefit (Z, γ_Z ≈ 0).
- **Invasion criterion**: Can a rare fungus increase when the other fungus is established? If both can invade they coexist; if neither can, the outcome depends on who arrives first (bistability).
- **Coexistence / exclusion**: Both fungi persist / one drives the other out.
- **Sanctions (reciprocal rewards)**: The plant gives more carbon to partners that deliver more P and less to cheaters (s = strength). Here modelled as a redistribution of the reward per unit fungus; total carbon paid still changes when partners differ in cost.
- **Partner choice**: Choosing or favouring better partners in a biological market (Hammerstein & Noë 2016); sanctions are one mechanism.
- **Placeholder units**: All parameter values are illustrative and in relative units; the app shows qualitative patterns, not calibrated predictions.
- **Parameter-space share**: The fraction of Latin-hypercube samples, drawn across a tab's slider ranges, that fall in an outcome class (mutualism, commensalism, parasitism, or 'cannot persist'). Shares describe that slider box, not frequencies in nature; they change if the ranges or the sampling scale change. Colour: as in the other tabs (green / grey / red).
- **Importance**: How strongly a parameter shifts the predicted outcome class, measured as random-forest permutation importance on the same Latin-hypercube sample (scaled so the top parameter is 1). It identifies which sliders drive the classification, not a causal effect size.
