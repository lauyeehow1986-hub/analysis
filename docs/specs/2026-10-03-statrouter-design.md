# StatRouter — design specification

| | |
|---|---|
| Status | Draft for review (approved plan, 2026-10-03) |
| Repo | `<repo root>` (the `analysis` folder) (standalone; `main` holds specs, build in worktrees) |
| Prefix | `sr_` |
| Scope note | Research/governance tool; outputs require statistician review. Never commit real patient data — synthetic samples only. |


## Context
YH (biostatistics / HTA / cardiology research, NHCS) wants one portable Shiny tool that takes a
clinical dataset from raw file → profiled & cleaned → correctly-chosen, pre-specified,
assumption-checked analysis → **manuscript-ready Word output (Methods + Results, tables, figures,
reporting checklist, reproducible code)**, archived so it can be **reopened months later to answer peer
reviewers** (paste comment → self-diagnosis → extra analysis on the frozen data → reply draft → numbers
refreshed in the user's edited manuscript). Decisions are made by a **pluggable System One router**:
deterministic rules → typed-judgment model (TypeSafe **Jev** cloud; open **Von / CLM / Strands** locally)
for genuine semantic ambiguity → the user.

Two evidence-based design principles came out of the critical review:
1. **Models route, they never compute or write numbers.** AI routers fail mainly by picking analyses
   whose assumptions are violated (StatQA NeurIPS'24, StatABench 2026, JMIR 2026). Rules and
   diagnostics own everything that can be decided from the data.
2. **Data-driven switching of the primary analysis is itself a reviewer critique** (forking paths,
   normality pre-testing). Defaults are *robust by design* (Welch, robust/cluster SE, RCS,
   RMST pre-specified alongside HR). A failed diagnostic **adds a sensitivity analysis**; it changes the
   primary analysis only when a conditional rule was written into the **locked SAP**.

### Decisions made with the user
| Topic | Decision |
|---|---|
| Name / prefix | **StatRouter**, `sr_` (internal `.sr_`); repo/folder `analysis` |
| Router backends | All pluggable behind TypeSafe's `POST /v1/systemone` protocol: `rules` (always) · `jev` (cloud; user will get a key) · `local` (Von by default) · `clm` (Contrastive-LM, laptop GPU) |
| Hardware switch | Profiles **`locked16`** (CPU-only 16 GB) ↔ **`laptop`** (i7-13650HX, 24 GB, RTX 4060 8 GB); auto-detected, overridable; caps workers, model tier and RAM use |
| Modes | `airgap` (real data; no network ever) and `connected` (de-identified data; Jev with key + per-session consent + payload preview) |
| Scope | Core + **all families** (prediction/ML/DCA, ML survival, causal/TTE, diagnostic accuracy/agreement, meta, latent class, HTA, deep learning). Nothing is cut; the phases are sequenced so each one ships |
| Users | **Expert + Guided**. Guided outputs carry a "not reviewed by statistician" watermark until a logged sign-off |
| Prose | Generated from **decision-record fragments** with locked number placeholders; optional LLM polish later |
| Output | **Word .docx** (officer/flextable, pandoc-free) with **bookmarked numbers** + figure folder + R script + lockfile |
| Repo | New standalone repo (decoupled from the stray home `.git`); spec on `main`; build in a worktree |

## Architecture
```
Downloads/analysis/                       own git repo
  app/app.R, app/global.R                 bslib page_navbar; Shiny modules; ExtendedTask + mirai for long jobs
  app/R/
    core/     project.R events.R audit.R settings.R hardware.R libsnap.R schema_migrate.R
    io/       read.R sniff.R ingest.R store.R dictionary.R redcap_dict.R reshape.R   (format sniffing; streamed ingest → project Parquet via DuckDB; dictionaries; long/wide; joins)
    intake/   pico.R roles.R                    research question (PICO/PECO, design, estimand) + variable-role inference
    profile/  profile.R dq_*.R missing.R suggest.R recipe.R flow.R
    design/   spec.R compose.R                  typed design spec → pipeline; refuses invalid layer combinations
    router/   graph.R nodes/*.R state.R sysone.R calibrate.R abstain.R shadow.R backends/{rules,jev,local,clm}.R
    routes/   registry.R + one file per family  (spec_schema / compile→code / present)
    diagnostics/ checks.R plots.R              test + plot + verdict → sensitivity nodes
    resample/ boot.R                            bootstrap/permutation layer
    impute/   impute.R pool.R sensitivity.R
    sap/      sap.R deviations.R                SAP lock (hash), deviation tracking, primary/sensitivity/exploratory labels
    writer/   docx.R fragments.R numfmt.R figures.R checklist.R cite.R script.R guard.R refresh.R sdc_out.R
    reviewer/ split.R critiques.R diagnose.R reply.R builder.R
  app/python/sysone_local/   stdio JSON-lines runner (processx), lazy load/unload; Von (ONNX/OpenVINO), CLM heads + llama.cpp encoder
  inst/templates/  fragments/*.txt  guidelines/*.yml  critiques/*.yml  questions/*.yml  journals/*.yml
  samples/   make_samples.R → synthetic data with PLANTED defects & known truths (never real data)
  tests/     testthat/ + golden/ (routing items, reviewer comments) + textbook/ (reference reproductions)
  tools/     stage_r.ps1 stage_py.ps1 stage_models.ps1 build_bundle.ps1 audit_pathlen.ps1 check_closure.ps1 verify_clean_machine.ps1
  docs/      specs/ roadmap.md packaging.md router_eval.md validation.md
  run.bat run.ps1
```
Conventions: pure, headless-testable core; `getOption("sr.x", Sys.getenv("SR_X", default))`; one concern
per file; `testServer` for reactive logic; ASCII-only generated scripts.

### Core mechanics (all changed by the critical review)
- **Code-first execution**: each route's `compile(spec)` emits an **R code string**, and that string is
  what gets evaluated (in a mirai/callr worker). The exported `analysis.R` is literally what ran, so it
  always matches the results. Results are **content-addressed**: hash(data, spec, seed, library snapshot).
- **Design spec + composition layer**: a typed spec (roles, estimand, missing-data strategy, weights,
  clusters, resampling, time scale) compiles into a pipeline. Each layer declares `composes_with`.
  Invalid or contested combinations are refused or forced into an explicit choice, for example
  MI × propensity score (MIte vs MIps), bootstrap-then-impute vs impute-then-bootstrap, or Rubin pooling
  of RMST/AUC on the wrong scale.
- **Route contract** in three parts:
  - `spec_schema` (validated);
  - `compile(spec) → code` with `estimand`, `composes_with` and `version`;
  - `present(result) → tables / figures / fragments`.

  `applicable()` returns its reasons, not just TRUE/FALSE.
- **Event-sourced project**: `events.jsonl` (an fsync per accepted action) is authoritative. Reopening
  the project or recovering from a crash replays it, and Shiny state is never authoritative. `fits/*.rds`
  are a cache only. `project.json` carries a `schema_version` plus migrations.
- **Library snapshot**: the bundle pins a CRAN snapshot date and records a lockfile.
  - Reviewer mode will not silently recompute frozen results under a different library. It requires an
    explicit **re-baseline** and shows a diff report.
  - The audit hash-chain tip is anchored in the freeze manifest and in the docx properties and footer.
- **Long jobs**: Shiny `ExtendedTask` + **mirai** daemons; callr isolation for crash-prone fits (xgboost,
  torch).
  - Bootstrap and MI run in checkpointed chunks and can be cancelled.
  - Parallel seeds are reproducible regardless of worker count.
  - The number of workers is capped by the hardware profile.

### Router
- **Where System One actually adds value**, and is used: research-question intake (design, estimand,
  outcome type, guideline from free-text PICO), **variable-role inference** (outcome, exposure, ID,
  date, cluster, confounder, mediator, post-outcome leakage) from names and labels, interpreting data
  issues (entry error vs unit error vs true extreme), and reviewer-comment tagging. Nodes that can be
  decided from the data stay rule-only, which also avoids circular "golden labels".
- The **decision graph is a DAG**. Veto/redirect back-edges are bounded and logged. Resolved nodes are
  frozen. On reopen, only nodes that are new or invalidated (through their dependencies) re-route. A
  past decision is never re-asked: the full payload, model id/hash and raw output are stored so it can
  be replayed.
- **Escalation**: `rules → local default → (laptop: CLM) → jev (connected only) → user`.
  - **Abstain thresholds** are set per node from risk–coverage curves with a **cost matrix** (a
    dangerous misroute costs far more than a benign one), not from a global τ.
  - Calibration (temperature, per backend) uses pooled items; split-conformal sets are optional.
- **State builder**: metadata only, rendered as `key: value` lines, ≤8k tokens. No rows, no free-text
  values, and cells below 5 suppressed. The exact payload is previewed before any network call, with
  optional column-name masking. Jev in connected mode needs a DPA / PDPA and institutional IT sign-off
  (documented, not assumed).
- **Shadow mode** is on from day 1: the router predicts silently, the user decides, and both are
  logged. This gives the only non-circular, real-distribution accuracy data.
- **Decision record** (node, resolver, probabilities, choice, override + reason, citation): written to
  the hash-chained `audit.log`, and drives the Methods fragments.
- **Expert** mode is an editable decision table. **Guided** mode presents one decision at a time with a
  plain-language "why", requires a justification for risky overrides, and watermarks output until
  sign-off.

### Local System One backends (research outcome, Apache-2.0)
| Profile | Default | Power | Notes |
|---|---|---|---|
| `locked16` | **Von 1.3** (`wfzyx/von`, 395M ModernBERT, 8k ctx, ONNX/OpenVINO ~0.1–0.4 s, native `/v1/systemone`) | — | DLL inside signed python.exe; weights SHA-pinned, safetensors/ONNX only |
| `laptop` | Von 1.3 | **CLM-8B** (Q8_0 Qwen3-8B GGUF via llama.cpp `--embedding --pooling last` + 75 MB heads; top-1 agreement 0.979 vs bf16) | Strands Decider 2B kept as a later option; Kev/Clef are CUDA/vLLM-only and listed as server options |
- Runner = **stdio JSON-lines via processx** (no port, no server). It loads the model lazily and unloads it before heavy fits.
- All these models are weeks old and benchmarked only by their authors. The **bake-off** decides which nodes they're allowed on:
  - Baseline: safe-default + ask user.
  - Labels: hand-written items labelled by two biostatisticians, with κ reported as the ceiling.
  - Comparison: paired McNemar with CIs, cost-weighted error, split by node type, tested on a held-out *dataset family*.
  - A model is enabled per node only if it beats the baseline by a margin set in advance.
  - Von may be fine-tuned on the laptop GPU if zero-shot accuracy is weak.

### Archive, SAP lock & reviewer mode
- **SAP lock**: before outcome analyses, the decision graph + design spec + pre-specified sensitivity
  analyses are hash-frozen into `SAP_v1.docx`. Anything run afterwards is labelled
  **primary / sensitivity / exploratory / post hoc**, and deviations are listed automatically. Every
  run is logged; a multiverse / specification curve is offered as a legitimate sensitivity analysis.
  The lock requires a **study feasibility verdict** (catalogue O) to have been decided for every
  primary analysis.
- **Freeze snapshots** (`submission-v1`, `revision-r1`) contain data hashes, events, code, lockfile and
  outputs. They are exported as `.srproj` and are drive-letter independent. A shared-drive lock file
  prevents concurrent edits. Data versioning covers new follow-up data arriving during revision.
- **Reviewer mode**:
  1. Paste the letter. It is split into points **deterministically** (numbering and paragraph
     heuristics).
  2. Each point is tagged by rules plus a System One Choice over the critique taxonomy, which the user
     confirms.
  3. **Self-diagnosis** runs that class's checks on the frozen archive.
  4. Extra analyses re-run under the pinned library and are labelled post hoc.
  5. Output: a reply docx (Comment / Response / Action / Manuscript change).
  6. **"Refresh numbers in my edited manuscript"** updates bookmarks and content controls in the
     user's own docx without touching their prose.
  7. The custom plot and table builder saves specs as code, so they are re-runnable.

## Catalogues
**A. Profiling & data quality** (Kahn conformance / completeness / plausibility):
- Type and coercion failures; conformance to the dictionary or REDCap dictionary.
- Completeness by column, row and pattern; missingness-indicator models (Little's test shown as
  descriptive only).
- Implausible values; unit errors (kg/lb, cm/m, creatinine, glucose, HbA1c); heaping (terminal-digit test).
- Date logic, including immortal time; duplicates; leakage (single-variable AUC≈1, post-outcome variables).
- Near-zero variance; high cardinality; collinearity; separation; **Riley sample-size criteria** (EPV
  is not used); robust outliers (flagged, not deleted).
- **Outcome-blind until SAP lock.**
  - Before lock, profiling and the Data Action Suggestor see outcome *completeness and plausibility
    only*. No outcome-by-exposure summaries and no effect estimates.
  - Leakage is reported as a **flag only** (e.g. "variable X is near-deterministic for the outcome"),
    without showing AUCs.
- Clustering detection; count offsets / person-time detection; informative-censoring screen.
- False-positive rates are calibrated on clean synthetic twins, so tests don't fire on everything at
  large n.

**A+. Data Action Suggestor** (loops until the data are ready). For each finding, one routed action
with its reason:
1. Recheck at source, via a **data-query sheet** (.xlsx), then re-import and diff.
2. Reversible in-app fix (cleaning **recipe** with undo and branching).
3. Drop cases by an explicit rule. This feeds a **flow diagram** (grid-based `consort` package) and
   warns about outcome-dependent exclusion.
4. Drop variables (leakage, NZV, redundant), never protected roles.
5. Set to missing and impute (routes to D).
6. Keep and run a robust sensitivity analysis.

Every action is written to the cleaning log, the Methods fragment and the audit trail. A readiness
panel shows what still blocks analysis.

**B. Diagnostics: test + plot + verdict, always.**
- Residual / QQ / scale-location / leverage-Cook's plots; partial-residual / RCS plots; DHARMa;
  rootograms.
- Schoenfeld and log(−log) plots; CIF vs 1−KM; random-effects QQ.
- Propensity-score overlap / love plot / weights; calibration curve + risk histogram; ROC / DCA;
  optimism and stability plots.
- LCA elbow; Markov trace; CE plane / CEAC; PSA convergence; funnel / Baujat; Bland–Altman.
- Checks: check_model, overdispersion, zero-inflation, influence, linearity, PH, proportional odds,
  separation, heteroscedasticity, convergence / singular fit, balance / ESS, LCA enumeration (the full
  table of BIC / aBIC / BLRT / class sizes is reported; **entropy is never used to choose k**).
- Pre-specified, not test-gated:
  - Penalised likelihood is used when sparse data / separation is *anticipated*: **FLIC/FLAC**
    (Puhr 2017) for prediction, Firth with profile-penalised-likelihood CIs for inference.
  - GEE working correlation is set by design, not QIC.
  - Proportional odds is assessed by a partial-PO (VGAM) sensitivity node, not by a Brant gate.
  - Clustering is set by design and never dropped because the ICC ≈ 0.
- Verdicts use effect-size or graphical thresholds, not p-values alone (a large n rejects everything).
- A failed check creates a **sensitivity node**; it replaces the primary analysis only through a
  SAP-conditional rule.

**C. Analyses.**
- **Descriptive**: Table 1 with SMD; no p-values in an RCT Table 1.
- **Comparisons**: Welch, Wilcoxon reported with the **probabilistic index / win probability**
  (Hodges–Lehmann only when a location shift is plausible), χ²/Fisher, McNemar, **non-inferiority /
  equivalence (TOST)**.
- **ANOVA family**:
  - one-way (classical + **Welch ANOVA**), factorial, ANCOVA, **repeated-measures / mixed ANOVA**
    (afex; **Greenhouse–Geisser applied by default**, not gated on Mauchly; Mauchly is reported
    descriptively only; MMRM is offered as the preferred alternative), MANOVA (Pillai);
  - non-parametric: Kruskal–Wallis, Friedman, aligned-rank transform (ARTool) for factorial designs;
  - simple effects / interaction contrasts.
- **Post-hoc & multiple comparisons**:
  - **All pairwise**: **Games–Howell is the pre-specified default**, regardless of any variance test.
    **Tukey HSD** is used when equal variances are justified *a priori* in the SAP.
  - **Versus control**: **Dunnett** (incl. step-down).
  - **Planned / custom contrasts**: polynomial trend and sequential contrasts.
  - **Unplanned (data-snooped) contrasts**: Scheffé, labelled exploratory.
  - **After rank tests**: **pairwise Wilcoxon / Brunner–Munzel with Holm** is the default. Dunn and
    Conover are available with a caveat that their mean ranks depend on the other groups
    (Benavoli 2016). Nemenyi and Durbin are not offered.
  - **Categorical**: pairwise proportion tests and adjusted standardised residuals after χ².
  - **p-value adjustments**: **Bonferroni, Holm, Hochberg, Hommel, Šidák, Benjamini–Hochberg FDR,
    Benjamini–Yekutieli**, permutation-based **Westfall–Young maxT**.
  - **Model-based** via **emmeans + multcomp** for *any* fitted model (GLM, linear mixed models
    with Kenward–Roger df (LMM only; GLMMs use asymptotic or parametric bootstrap), GEE, Cox), with **simultaneous CIs**, not just adjusted p-values.
  - **Gatekeeping / graphical procedures** (graphicalMCP) for trials.
  - Packages: emmeans, multcomp, rstatix, PMCMRplus, FSA, DescTools, afex, ARTool, graphicalMCP.
  - **Rules**:
    - The comparison family and method are declared in the SAP.
    - Planned contrasts are preferred over all-pairwise.
    - No omnibus-significance gate before Tukey/Dunnett (it is unnecessary and distorts error rates).
    - The router picks the method from the **design question** (pairwise vs control vs trend), never
      from a preliminary test, and the user confirms.
    - Unadjusted exploratory comparisons are allowed only when labelled as such.
- **Regression**: lm, logistic, modified Poisson, ordinal, count models with offsets where **the
  family (Poisson / NB / ZI / hurdle) is pre-specified in the SAP** from the data-generating
  rationale (no Vuong test, no sequential escalation; alternatives become sensitivity nodes), RCS,
  **GAM (mgcv), quantile, beta, Tobit, robust**.
- **Clustered**: GEE vs mixed models chosen by estimand; MMRM; **few-cluster corrections**
  (Kenward–Roger, CR2 `clubSandwich`).
- **Survival**: KM, Cox, RMST, Fine–Gray vs cause-specific, multistate, flexsurv, **recurrent events,
  left truncation, time-varying covariates, interval censoring, relative survival, joint models /
  landmarking, IPCW**.
- **Epidemiology**: age-standardised rates; **complex survey weights (`survey`)**.
- **Trials**: **ICH E9(R1) estimands** / intercurrent events, **covariate adjustment (ANCOVA,
  RobinCar / g-computation per FDA 2023)**, rbmi, **multiplicity (Holm / Hochberg / gatekeeping /
  graphical)**, win ratio, **Bayesian re-analysis (rstanarm, precompiled)**.
- **Prediction**: pmsampsize, rms/glmnet, bootstrap optimism, pminternal stability, CalibrationCurves /
  ICI, dcurves, fairness, external validation / updating, conformal. **TRIPOD model presentation**:
  full equation, nomogram / score chart, and the interactive calculator + handover (catalogue **N**).
- **ML, ML survival, tabular foundation models, ensembles, AutoML, deep learning**: see catalogue **L**.
- **Causal**: dagitty, TTE protocol table, MatchIt / WeightIt (overlap weights), cobalt,
  marginaleffects g-computation, AIPW / TMLE / lmtp, E-value, negative controls, **mediation
  (CMAverse), HTE (grf causal forests, RERI / additive interaction), IV, quasi-experimental (ITS,
  DiD incl. staggered adoption, synthetic control, RDD), QBA / measurement error (episensr, simex)**.
- **Diagnostic accuracy**: epiR, pROC, **clustered accuracy**, verification bias.
- **Agreement**: ICC, Bland–Altman, κ / AC1, **Passing–Bablok / Deming (mcr)**.
- **Meta-analysis**: meta / metafor, mada, **NMA (netmeta / multinma)**.
- **Latent / unsupervised**: poLCA, tidyLPA / mclust, lcmm, flexmix, **LTA (LMest), k-prototypes,
  consensus clustering + stability**.
- **Psychometrics / PROMs**: EFA / CFA, ω, **IRT (mirt)**, invariance, MCID.
- **HTA**: dampack, heemod, hesim, flexsurv + **cure models**, BCEA, voi, **trial-based CEA (two-part
  / gamma costs, bivariate bootstrap), EQ-5D value sets (incl. Singapore — verify), MAIC / STC (NICE
  TSD 18), distributional CEA**, ACE presets.

**C+. Effect sizes** are mandatory with a 95% CI for every route: MD, g/d, η²/ω², HL, rank-biserial,
Cliff's δ, RD/RR/OR/NNT, marginal RD/RR, IRR, HR, RMST difference, risk at t, common OR, win ratio,
κ/ICC/LoA, Se/Sp/LR/DOR, ΔAUC/ΔNB, ATE/ATT/ATO + E-value, ICER/INMB. Conversions for meta-analysis are
included. **Design-stage sample size and power**: pwr, pmsampsize / pmvalsampsize, Schoenfeld,
cluster design effect, simr, precision-based (Buderer, Bland–Altman, ICC). **Post-hoc power is refused.**

**C++. Bootstrap & resampling.** A router node decides when it is needed.
- Methods: percentile / BCa / studentised; cluster/block; wild; parametric (pbkrtest, BLRT);
  optimism; MI-boot; propensity / g-computation; ICER; permutation tests.
- Seed, B and method are recorded. If BCa fails, the app falls back and logs the reason.
- CI coverage of the layers in combination is validated by simulation.

**D. Missing data.**
- TARMOS pre-specification; complete-case validity (White & Carlin).
- Methods: mice, missRanger, smcfcs, jomo/mitml, Nelson–Aalen, deployment-aware handling (Sisk 2023).
- m via von Hippel; Rubin's rules on the correct scale; D1/D2/D3; MI-boot.
- Sensitivity: delta-adjusted tipping point; rbmi.

**E. Reviewer critique taxonomy**: dichotomisation; stepwise selection; missing data; overfitting; no
calibration; causal language; Table 2 fallacy; multiplicity; subgroup claims; baseline p-values;
change scores; immortal time; KM with competing risks; PH; clustering; post-hoc power; DeLong on nested
models; normality pre-testing; over- or collider adjustment; sample size; generalisability; **unjustified or overly influential
prior; Bayes factor misuse; MCMC convergence not shown**; other.

**F. Checklists** (each item mapped to an artifact, scored green / amber / red): STROBE, RECORD,
TARGET, CONSORT 2025, SPIRIT 2025, TRIPOD+AI (+ PROBAST+AI self-audit), STARD 2015 / STARD-AI,
CHEERS 2022 (+AI), SAMPL, GRRAS, PRISMA / PRISMA-NMA, **BARG (Kruschke 2021 Bayesian Analysis
Reporting Guidelines) / ROBUST**.

**G. Writer.**
- Prose is assembled from decision-record fragments.
- Numbers sit in **locked placeholders** (`{{E1}}`), so any LLM polish never sees digits.
- A **claim guard** checks direction, significance and causal verbs against the design.
- Output: a bookmarked docx; supplementary docx; journal presets (fonts, widths, DPI); CSL-JSON / RIS
  + Vancouver list; SEQ-field numbering; `analysis.R` + lockfile.
- **Small-cell suppression** (n<5) in output tables, reusing the structured_deid SDC ideas.

**H. Bayesian statistics.** This is a cross-cutting layer, not a single route.
- `framework: frequentist | bayesian | both` is a field in the design spec and is fixed when the SAP is
  locked. It is never chosen from the data.
- **Portability rule.** The locked PC has no compiler, so only packages that ship **precompiled Stan
  models** or non-Stan engines are used:
  - **rstanarm** for regression: `stan_glm` (linear, logistic, Poisson, NB), `stan_glmer` (multilevel),
    `stan_polr` (ordinal), `stan_betareg`, `stan_gamm4`, `stan_lm`, and `stan_jm` (joint models).
  - Meta-analysis and networks: **multinma** (Bayesian NMA, precompiled), **bayesmeta**
    (random-effects meta-analysis with τ priors), **RBesT** (meta-analytic-predictive / robust-MAP
    priors for historical borrowing).
  - Other engines: **BayesFactor**, and **dbarts/bartCause** (BART for causal effects and prediction).
  - Post-processing: **bayestestR / posterior / loo / bayesplot**.
  - **brms/cmdstanr** are available only in the `laptop` profile, and only when Rtools + CmdStan are
    detected. They cover models rstanarm lacks (survival, custom families, distributional models).
  - On `locked16`, Bayesian survival falls back to rstanarm `stan_jm` or a piecewise-exponential
    Poisson `stan_glm`.
- **Priors**
  - Explicit presets: weakly informative (default), sceptical, enthusiastic, and informative from
    evidence (RBesT MAP / power prior). Each comes with a written justification that goes into the
    Methods.
  - Prior predictive check plot.
  - **Prior sensitivity** across the preset set, which counts as a sensitivity node.
- **Diagnostics (test + plot, as in B)**
  - R-hat < 1.01, bulk/tail ESS, divergences, tree depth, E-BFMI.
  - Trace and rank plots, posterior predictive checks.
  - **PSIS-LOO** with Pareto-k plot, and LOO model comparison.
  - Automatic remedies: increase `adapt_delta` or iterations, reparameterise. Each remedy is logged.
- **Reporting**
  - Posterior median with 95% credible interval.
  - **Pr(effect > 0)**, **Pr(effect > MCID)**, ROPE + HDI, and BF10 (only when the hypothesis is
    sharp), all on the clinical scale (RR / RD / HR / NNT via posterior draws).
  - Posterior density plot with the ROPE/MCID shaded.
- **Composition rules**
  - MI + Bayesian means fitting each imputed dataset and **pooling draws, not Rubin's rules**.
  - Bootstrap is refused for Bayesian fits, because the posterior already carries the uncertainty.
  - Clustering is handled through random effects, not sandwich SEs.
- **Uses**
  - Bayesian re-analysis of null or underpowered RCTs.
  - Hierarchical models (hospital and surgeon profiling with shrinkage).
  - Small samples and rare events.
  - Bayesian meta-analysis and NMA.
  - Historical-control borrowing.
  - Bayesian HTA, where PSA is natively Bayesian (posterior draws feed it).
  - BART for HTE and prediction.
- **Compute**
  - Chains run in mirai workers with a callr crash guard.
  - Chains and RAM are capped by the hardware profile.
  - Seeds and draws are content-addressed and cached.

**I. Validation: internal, out-of-sample and published-model.** This is a first-class workflow, not
a single line under Prediction.
- **Resampling designs.** All are leakage-safe: every preprocessing, imputation and selection step
  runs *inside* each fold.
  - Repeated k-fold and nested CV for tuning.
  - **Grouped/cluster CV**, so a patient's repeat visits or a hospital never span train and test.
  - **Temporal splits** (rolling origin).
  - Bootstrap optimism (Harrell), plus .632+.
  - **Internal–external CV** by centre or period, with heterogeneity in performance meta-analysed
    (metamisc).
  - Imputation happens within folds, with deployment-aware handling (Sisk 2023).
  - The router picks the design from the data structure (clusters, dates, n, events) and the stated
    purpose.
- **Validation types** (each labelled per TRIPOD): apparent → internal → temporal → geographic/domain
  → fully external.
  - Case-mix comparison: a membership model plus Debray's framework, which separates reproducibility
    from transportability.
  - Sample-size adequacy for validation via **pmvalsampsize** (Riley 2021). A warning is given when
    there are fewer than about 100–200 events.
- **Performance, each with a 95% CI (bootstrapped where needed)**
  - Discrimination: C / AUC, Uno's C, time-dependent AUC.
  - **Calibration**:
    - calibration-in-the-large / **O:E ratio**, calibration slope;
    - flexible calibration curve (loess/RCS), **ICI / E50 / E90**;
    - for survival, calibration at time *t* (Austin 2020), IPCW-based and competing-risk aware
      (riskRegression `Score`);
    - risk-group calibration tables.
  - Overall: Brier and scaled Brier.
  - **Net benefit / DCA** at clinically stated thresholds.
  - Subgroup and fairness performance.
- **Validating a risk algorithm from a paper.** This is the new module.
  1. **Model entry.** Import the published model as a JSON/CSV spec, in the same shape as the
     `sts_coefficients.json` from the STS project. The spec carries:
     - model type (logistic / Cox / Fine–Gray / points score);
     - intercept, or baseline survival S0(t) at the horizon;
     - coefficients, transformations (logs, published spline knots, centring constants, caps);
     - units, and the points-to-risk lookup table.

     Presets cover common cardiac models, entered from their published formulas (e.g. EuroSCORE II,
     with the formula and paper cited). Users add their own.
  2. **Predictor mapping.** Each published predictor definition is mapped to a local column, with
     unit and category recodes. This is a good System One task (semantic match of "eGFR < 50",
     "NYHA III–IV" or "critical preoperative state" to local fields). Every mapping is confirmed by
     the user and logged. Unmappable predictors block validation, or force a documented
     proxy/imputation choice.
  3. **Formula self-check.** Recompute the paper's worked example or reported example patients, so
     transcription errors are caught before scoring.
  4. **Score the local data** → full performance panel, compared against the performance the paper
     reported. Several published models can be compared **head-to-head** on the same patients
     (ΔC, ΔNB, calibration side by side; no DeLong on nested models).
  5. **Updating hierarchy.** Recalibrate the intercept → logistic recalibration (intercept + slope) →
     revise selected coefficients → extend with new predictors. **Vergouwe/Steyerberg closed testing**
     chooses the minimal adequate update, and the updated model is validated again (`predRupdate`,
     rms). For survival, S0(t) is re-estimated.
  6. Output: TRIPOD+AI **validation** items, the Methods/Results fragments, a calibration plot +
     DCA figure, and an updated-model equation/calculator if the model was updated.
- Packages: rms (`val.prob`, `val.surv`), CalibrationCurves, pmcalibration, riskRegression, timeROC,
  pROC, dcurves, pminternal, pmvalsampsize, predRupdate, metamisc, rsample/tidymodels (group/temporal
  splits).

**J. Data import & very large files.**
- **Supported formats**
  - Delimited text: CSV, TSV, TXT and pipe-delimited, with automatic delimiter, encoding
    (UTF-8/Latin-1/UTF-16 BOM), decimal/thousands and **DMY/MDY date** detection.
  - Compressed: `.gz` / `.bz2` / `.xz` / `.zip`.
  - Excel: **XLSX/XLS/XLSM** (sheet + header-row + range picker; merged headers flattened).
  - Statistical packages: **SPSS** `.sav/.zsav/.por`, **Stata** `.dta`, **SAS** `.sas7bdat/.xpt`
    (haven; value labels → factors + dictionary).
  - Columnar and R: **Parquet**, **Feather/Arrow IPC**, `.fst`, `.rds/.RData`.
  - **JSON / NDJSON**.
  - Databases: **SQLite / DuckDB** files.
  - **REDCap**: CSV export + data dictionary, and REDCap **ODM XML** (parser reused from the elso
    project).
  - Several files can be joined, with a key check and a join report.
- **Large-file design.** Target: multi-GB, tens of millions of rows on `locked16` without running out
  of memory.
  - Files are opened **by path** with a server-side file picker (`shinyFiles`), never pushed through
    a browser upload. The Shiny upload limit applies only to small drag-and-drop files.
  - **DuckDB is the ingest and storage engine.** It streams the source into a project **Parquet**
    store (compressed, typed, SHA-256 hashed, chunked + resumable). One portable package covers CSV,
    Parquet, JSON and SQLite out of core; arrow is optional.
  - **Out-of-core profiling.** DQ checks, missingness, distributions, duplicates and Table 1 run as
    DuckDB SQL aggregates over the full data. Plots use a reproducible, recorded random sample.
  - **Modelling** loads only the needed columns and filtered rows.
  - A **memory estimator** runs before every fit and compares the need with the profile's RAM budget.
    If it doesn't fit, it routes to a big-data engine (`biglm`/`speedglm`, `mgcv::bam`, `fastglm`,
    data.table) or a documented subsample (ML tuning). Each such choice is logged in the Methods.
  - Progress bars and cancellation throughout; ingest runs in a mirai worker.
  - Re-import diffs (recheck-at-source loop) run in DuckDB.
- **Scale target: 100 GB source files** (~0.5–1 billion rows), on both profiles.
  - **Pre-flight.** Before ingest the app estimates the time, checks drive type and free space, and
    refuses to start if space is insufficient. Free space needed:
    - Parquet, typically 15–30% of the CSV size;
    - DuckDB spill space (`temp_directory` on the data drive);
    - headroom.

    It warns when the source is on a network share and offers to copy it locally first.
  - **DuckDB settings per profile**:
    - `memory_limit` (`locked16` ≈ 6–8 GB, `laptop` ≈ 14 GB) and `threads`;
    - `preserve_insertion_order=false` for streaming;
    - out-of-core aggregation and joins that spill to disk.

    Ingest is chunked, checkpointed and resumable after a crash or unplugging, using the same
    pattern as the structured_deid checkpoint engine.
  - **Hashing.** A fast fingerprint (size + mtime + sampled-block hash) is used for reopen checks.
    The full streaming SHA-256 runs in the background and is required before freeze.
  - **External-data mode.** The project stores the source path + hash + Parquet store, and does not
    copy 100 GB into the `.srproj`. Freeze archives the **query code + source hash + analysis-ready
    extract**, so reviewer mode replays the extraction exactly when the source is available and
    still works from the extract when it isn't.
  - **Format limits at this scale.**
    - **Direct and streamed**: CSV/TSV (incl. gz), Parquet, NDJSON, SQLite/DuckDB.
    - **Chunked**: SAS/Stata/SPSS. pyreadstat `read_file_in_chunks` → Parquet in the bundled
      Python; falls back to haven `skip/n_max` chunks (slower).
    - **Not feasible**: XLSX is capped at about 1M rows and is fully in-memory, so the app tells the
      user to export to CSV. Single-document JSON arrays over a few GB are converted to NDJSON first.
  - **Cohort builder / feature extraction.** This is the realistic 100 GB workflow: long EHR, labs or
    claims data → filtered cohort → per-patient features → small analysis-ready table.
    - It is a dbplyr-over-DuckDB UI (inclusion/exclusion criteria, index date, look-back windows,
      aggregations, first/last/count/min/max, time-to-event derivation).
    - It emits SQL/dplyr code (code-first, exported) and **feeds the flow diagram** with the row
      counts at each step.
  - **Analysis at scale** is routed by the memory estimator. Each strategy is logged in the Methods.

    | Analysis | How it runs at this scale |
    |---|---|
    | Descriptives / DQ / Table 1 | Exact SQL aggregates on the full data |
    | Linear regression | Exact, from SQL-computed sufficient statistics (X'X, X'y) |
    | GLMs | `bigglm` streaming passes over DuckDB chunks |
    | Count / binary models with categorical covariates | **Lossless aggregation to covariate patterns** (counts + exposure) → Poisson/binomial |
    | Cox models | Piecewise-exponential Poisson on aggregated person-time. Exact only with a cut at every event time, categorical covariates and Breslow ties; otherwise reported as an approximation |
    | ML / mixed models | Fit on a documented stratified sample (or case–control subsample with weights), with chunked scoring and validation on the full data |
    | Bootstrap | Replaced by bag-of-little-bootstraps / subsampling, or analytic SEs |
    | MI | Routed to a model-based single-pass imputation or a sample-fitted imputation model, plus sensitivity |

  - **UI.** Raw rows never go to the browser. Tables use server-side SQL paging, and plots use
    samples or binned aggregates (hexbin / histograms computed in SQL).

**K. Structural equation modelling & latent variables.** Pure-R and portable: **lavaan** core.
- **Models**
  - Core: path analysis, **CFA**, full SEM.
  - Dimensionality and categories: **ESEM / EFA-in-SEM**, **bifactor** and higher-order, **MIMIC**,
    ordinal/categorical indicators (WLSMV), **IRT** link (mirt).
  - Groups and levels: **multigroup SEM + measurement invariance** (configural → metric → scalar →
    strict; semTools; alignment where full invariance fails), **multilevel SEM**.
  - Longitudinal: **latent growth curve models**, **latent change score**, **RI-CLPM / cross-lagged
    panel**.
  - **SEM mediation** (bootstrapped indirect effects; with a counterfactual cross-check against
    CMAverse), moderated mediation / latent interactions.
  - Mixtures: **growth mixture / latent class growth** (lcmm; OpenMx precompiled where needed).
  - Alternatives: **PLS-SEM** (seminr / cSEM) for composite models, **Bayesian SEM (blavaan,
    precompiled Stan)**.
- **Missing data and robustness**: FIML, robust MLR, auxiliary variables, MI + pooling (semTools).
- **Diagnostics (test + plot, as in B)**
  - Identification and convergence; Heywood cases.
  - χ², CFI/TLI, RMSEA with 90% CI, SRMR. **Dynamic fit-index cut-offs** (McNeish & Wolf 2023)
    replace rigid Hu–Bentler cut-offs.
  - Local fit (residual correlations).
  - Reliability: ω, AVE / HTMT validity.
  - **Modification indices are shown but labelled exploratory.** Accepting one is a logged post-hoc
    deviation from the SAP.
  - Equivalent-model warning.
  - Path diagrams via `tidySEM` (ggplot, no DiagrammeR).
- **Sample size**: semPower / simsem Monte Carlo.
- **Reporting**: JARS-Quant SEM items. The critique taxonomy gains MI fishing, fit cut-off misuse,
  causal claims from cross-sectional mediation, and untested invariance.

**L. Machine learning: complete catalogue** (research-verified 2026-10-03; CRAN/PyPI/HF status and
licences checked; re-verified at each phase start).

*Framework.* tidymodels is the spine:
- parsnip, recipes, rsample, tune, finetune (racing / simulated annealing), workflow_sets, stacks,
  probably, bonsai, censored.
- mlr3 is a second spine for survival breadth: mlr3proba, which is r-universe only, so it is bundled
  as a binary.
- Every learner sits behind one route contract: same resampling (catalogue I), same calibration
  panel, same explainers.

*Learners: classification and regression (R, CRAN Windows binaries unless noted)*
- **Penalised / sparse**:
  - glmnet (lasso / ridge / elastic net, relaxed and adaptive lasso);
  - ncvreg (SCAD / MCP), grpreg (group lasso), L0Learn (best subset);
  - logistf (Firth), pls.
- **Smooth / additive**: mgcv GAM (interpretable baseline), earth (MARS), mboost (component-wise
  boosting: glmboost / gamboost).
- **Trees and rules**: rpart (CART), C50, partykit (ctree, cforest, **mob / glmertree for subgroup
  identification**), Cubist, **pre (RuleFit)**, xrf.
- **Random forests**: ranger (incl. extra-trees, quantile), randomForest, **randomForestSRC**
  (classification / regression / multivariate / quantile), **aorsf** (oblique), **grf**
  (regression / quantile / causal).
- **Boosting**:
  - **xgboost 3.x** (pinned API) and **lightgbm**;
  - **catboost** (GitHub binary via bonsai; the Python catboost wheel is the fallback);
  - gbm, mboost.
- **Kernel / instance / probabilistic**: kernlab and e1071 (SVM, Gaussian processes), kknn,
  naivebayes, discrim (LDA / QDA / RDA / FDA).
- **Bayesian trees**: dbarts / BART (BART, also used for causal and HTE).
- **Neural (R)**: nnet, brulee (torch MLP), tabnet. torch uses pre-staged LibTorch via
  `TORCH_URL` / `LANTERN_URL`.

*Learners: Python, via the bundled runner (stdio, offline, pure-Python wheels + CPU torch)*
- **RealMLP, TabM, xRFM, TabR / ModernNCA** (pytabkit / tabm / xrfm / TALENT). These are the current
  strongest trained-per-dataset deep tabular models.
- EBM (InterpretML; glass-box GAM-boosting).
- NGBoost-style probabilistic regression is optional.
- Not included: h2o and bartMachine (they need Java), and Keras / TensorFlow.

*Tabular foundation models (in-context learners)* run as the `tfm` route family through the Python
runner. Weights are downloaded once on a connected machine, SHA-pinned, and pointed to via
`TABPFN_MODEL_CACHE_DIR` / `model_path`. Nothing is fetched at runtime; the CRAN `tabpfn` R package's
uv auto-install is **disabled** and it is pointed at the bundled Python.

| Model | Licence (weights) | Data limits | Use in app |
|---|---|---|---|
| **TabPFN v2** | Prior Labs Licence (Apache-2 + attribution) | 10k rows × 500 features | **Default-enabled.** CPU feasible to ~1–5k rows on `locked16`; larger on the laptop GPU |
| **TabICLv2** | **BSD-3** | 300–100k rows (~500k with offload) | **Default-enabled.** CPU-friendly for medium data; built-in SHAP and fine-tuning |
| TabDPT / TabDPT-Turbo | Apache-2.0 | — | Enabled |
| Mitra / Mitra-v2 | Apache-2.0 | — | Enabled; fine-tuning on laptop CUDA only |
| Nori | Commercial use allowed (licence to verify) | — | Regression only |
| TabPFN 2.5 / 2.6 / **3 / 3.5** | **Non-commercial / evaluation-only** | 3.5: up to 1M rows | **Available, user decides**: listed with the other models behind a one-click toggle. On first enable the licence terms are shown and acknowledged (logged once per project). The licence is cited in the Methods. TabPFN 3.5 is the most capable foundation model when enabled |
| LimiX-2 | Non-commercial (academic free) | — | Opt-in licence gate |
| sap-rpt-1-oss | Research only | — | Opt-in licence gate; needs ≥80 GB GPU, so listed but not feasible on this hardware |

- tabpfn-extensions (Apache-2, experimental) adds: SHAP / feature selection, many-class, outlier
  detection, synthetic data, conditional-randomisation-test (CRT) feature relevance, and
  **SurvivalTabPFN** (no competing risks; pulls in GPL scikit-survival, so it is flagged).
- TabPFN-TS is excluded: it defaults to a cloud API.
- **Router rules**:
  - Foundation models are offered when n ≤ the model's context limit.
  - They are evaluated **under exactly the same resampling, calibration and DCA panel** as everything
    else, with calibration checked rather than assumed.
  - They never displace a pre-specified primary model without a SAP rule.

*Ensembles*
- Bagging (baguette), **stacking** (`stacks`, cross-validated penalised meta-learner),
  **SuperLearner** (discrete + ensemble), weighted averaging, post-hoc foundation-model ensembles.
- Ensemble weights come from out-of-fold predictions only.
- **The whole selection procedure is evaluated in nested resampling**: learner choice, tuning,
  ensembling, AutoML and the "ML must beat regression" comparison. The reported performance is the
  outer-loop estimate of the *procedure*, not of the chosen winner. Disclosing the search is not
  enough on its own.

*AutoML*
- **tidymodels workflow_sets + finetune racing** (default, pure R); mlr3 AutoML.
- **AutoGluon 1.6** is optional on the laptop. Use its commercial-safe preset only, with the
  non-commercial preset behind the licence gate. Its multi-hour presets are capped by the hardware
  profile.
- AutoML is always reported with the full search space and the number of models tried, as a
  forking-path disclosure.

*ML survival / time-to-event*
- **R**:
  - Forests: **randomForestSRC RSF (incl. competing risks)**, ranger survival, **aorsf**, grf
    survival / causal survival forest.
  - Boosting: **CoxBoost** (back on CRAN 2026-09), mboost / gbm Cox, xgboost `survival:cox` /
    `survival:aft`.
  - Penalised / other: glmnet Cox, penalized, survivalsvm, BART survival, pre (survival RuleFit),
    censored (parsnip engines).
- **Python, optional**:
  - Deep models: **pycox (DeepSurv, DeepHit incl. competing risks, Cox-Time, PC-Hazard, N-MTLR,
    logistic hazard)**, torchsurv (FDA regulatory-science tool).
  - scikit-survival (RSF, gradient-boosted / component-wise survival, SSVM). **GPL-3, so it ships
    only in the `cpu-ai` / `gpu-ai` bundles with a licence note.**
  - lifelines, xgbse, SurvivalTabPFN.
- **Metrics**:
  - Harrell / Uno C, time-dependent AUC;
  - **integrated Brier (IPCW)**;
  - D-calibration and calibration at *t*;
  - competing-risk CIF calibration;
  - DCA at the time horizon (riskRegression, survex).

*Deep learning beyond tabular*
- MLP / ResNet / FT-Transformer for tabular (torch or Python).
- Images, signals (ECG) and text are out of scope for v1 but the route contract allows them.
- Gated to large n, or to cases where the regression-first comparison shows a gain.

*Explainability & uncertainty* (every ML / foundation-model route)
- **SHAP**: shapviz, kernelshap, treeshap. fastshap is not used: it was archived from CRAN in 2026-05.
- Effects and interactions: PDP / ICE / **ALE** (iml), DALEX, vivid interaction plots, permutation
  importance with CIs.
- Survival explanations: **survex** (SurvSHAP(t), SurvLIME).
- **Conformal prediction**: probably for regression; MAPIE / crepes for classification and
  Mondrian / class-conditional.
- Model cards. Caveats are printed: SHAP is not causal, and correlated features split credit.

*Clinical ML guardrails* (rule-enforced; also feed the reviewer critique taxonomy)
- Penalised regression is always fitted as the comparator. ML is reported as superior only if ΔAUC or
  ΔNB beats it with CI (Christodoulou 2019).
- **Class imbalance is a user decision, with a recommendation.** For risk prediction the router
  *recommends* no resampling, or class weights + threshold choice (van den Goorbergh 2022: resampling
  distorts calibration).
  - The user may still choose **SMOTE / SMOTE-NC (mixed types) / ADASYN / borderline-SMOTE / ROSE /
    random over- or under-sampling / Tomek / NearMiss** (themis).
  - The app then applies the chosen method **inside each resampling fold only**, and **automatically
    recalibrates within the fold**. Recalibration is logistic (intercept *and* slope), because an
    intercept-only correction does not repair SMOTE's slope distortion.
  - It shows a side-by-side panel (no resampling vs the chosen method): AUC, calibration slope/O:E,
    ICI, net benefit.
  - The choice is logged with its justification, and the Methods fragment states it.
  - For pure classification tasks where calibration doesn't matter (e.g. screening triage with a
    fixed operating point), resampling is presented neutrally.
- Hyper-parameter tuning is nested inside resampling, and so is feature selection.
- Patient-grouped splits.
- Calibration is mandatory before any threshold or DCA.
- Fairness checks across sex, ethnicity and age.
- PROBAST+AI and TRIPOD+AI items are auto-mapped.
- The licence of every model used is recorded in the audit log and the Methods.

**M. Statistical-editor review (round 3): global rules + completed coverage.**

*M1. Global validity rules* (enforced by `design/spec.R` + `compose.R`; apply to EVERY family in C, H, I, K, L)

1. **No test-gated method switching anywhere.**
   - Each family's defaults are pre-specified:
     - Greenhouse–Geisser on;
     - Games–Howell;
     - count family from the SAP;
     - FLIC/FLAC when sparse data is anticipated;
     - GEE correlation by design;
     - overlap weights / trimming only if declared in the SAP;
     - SEM partial invariance / alignment only as a sensitivity analysis;
     - LCA k from the pre-declared criterion.
   - Diagnostics only **add** sensitivity nodes. A CI lint rule fails the build if any route's
     `compile()` branches on a diagnostic p-value.
2. **Mandatory estimand per route.**
   - Each route declares population, variable(s), intercurrent-event strategy and summary measure.
     Families with non-trial estimands (SEM, ML, LCA, agreement, meta-analysis) still declare one.
   - **Marginal vs conditional is explicit**:
     - non-collapsible OR/HR are flagged;
     - the marginal version comes via g-computation / marginaleffects;
     - Bayesian random-effects ORs are marginalised over the draws;
     - overlap weights / trimming state the changed target population (ATO / trimmed).
3. **`purpose` field (describe / predict / explain-causal) hard-blocks cross-talk**:
   - no risk-factor or causal language from prediction models;
   - no SHAP in causal text;
   - no lasso confounder selection except post-double-selection (hdm);
   - the writer suppresses or labels covariate coefficients in causal designs (Table 2 fallacy);
   - NRI/IDI are discouraged and listed in the critique taxonomy.
4. **Multiplicity-family registry (SAP).** Covers primary and secondary outcomes, subgroups, multiple
   exposures, ML model comparisons, head-to-head published-model comparisons, and reviewer-mode post
   hoc additions.
   - Sensitivity analyses are declared *not* separate inferential families.
   - Subgroups are reported with **interaction p and ICEMAN credibility**, never within-subgroup
     p-values.
   - **Every interaction is reported on both additive (RERI / AP / SI) and multiplicative scales**,
     with effect modification distinguished from interaction (VanderWeele).
5. **Composition matrix, validated by simulation**:
   - survey × MI: design variables go in the imputation model;
   - survey × bootstrap: replicate weights (svrep);
   - IPW × bootstrap: re-estimate the PS in every replicate;
   - matching × bootstrap: pair/cluster bootstrap, since the naive bootstrap is invalid
     (Abadie–Imbens);
   - clustered MI: 2l methods; weighted prediction performance; `svycoxph`;
   - **clustering is declared in every family**:
     - Cox: `cluster()` or frailty;
     - PS: within-cluster matching and cluster-level confounders;
     - meta-analysis: dependent effects;
     - CEA: multicentre;
     - DTA: multiple lesions or readers;
     - informative cluster size: cluster-weighted GEE.
6. **Numerical reproducibility.**
   - The exported `analysis.R` pins `RNGkind` (incl. `sample.kind`) and uses L'Ecuyer streams.
   - LightGBM `deterministic=TRUE` with a fixed `nthread`; torch deterministic algorithms.
   - **The hardware profile + BLAS + CPU/GPU are part of the content hash.** Cross-profile differences
     are disclosed, not hidden.
   - **Monte Carlo SE** is reported for bootstrap / MI / MCMC / PSA / permutation, and printed digits
     never exceed what the MCSE supports.
7. **Reporting corrections.**
   - NNTB/NNTH when the CI crosses 0 (Altman), with the baseline risk stated whenever it is derived
     from an OR.
   - MCID is anchor-based (primary) with distribution-based as supportive, plus a responder-analysis
     caveat.
   - Wilson / Clopper–Pearson / mid-p intervals for proportions.
   - Meta-analysis default: **REML or Paule–Mandel + HKSJ + prediction interval**. DerSimonian–Laird
     is never the default.
   - The RMST horizon τ is fixed in the SAP.

*M2. Methods added* (packages: CRAN Windows binaries, no compiler)

- **Longitudinal / correlated data**:
  - glmmTMB (NB / ZI / beta / Tweedie GLMMs), ordinal::clmm;
  - nlme corAR1/corCAR1 (serial correlation);
  - pattern-mixture models + IPW for dropout;
  - truncation by death (while-alive / composite strategies, SACE sensitivity).
- **Time series / forecasting** (e.g. cath-lab volumes, bed occupancy): fable / forecast (STL,
  seasonal ARIMA / ETS), segmented ITS with ARIMA errors or Newey–West SEs, prediction intervals,
  rolling-origin evaluation.
- **Dose–response / non-linear**: fractional polynomials (mfp), threshold models (segmented),
  dosresmeta.
- **Survival completion**:
  - cure models outside HTA: flexsurvcure, smcure;
  - **shared frailty**: coxme, frailtypack;
  - explicit **AFT**;
  - **RMST regression and pseudo-observation regression for risk at t** (eventglm);
  - **Pohar-Perme net survival** (relsurv, with SingStat life tables);
  - multistate details: transition hazards, Aalen–Johansen, clock-forward / reset (mstate);
  - **competing risks are the default in prediction**: cause-specific Cox / Fine–Gray / CSC
    (riskRegression). 1−KM risk is refused.
- **Costs / CEA**:
  - Tweedie, gamma / two-part models;
  - **censored costs (Lin, Bang–Tsiatis IPCW)**;
  - MI of costs / QALYs (Faria 2014);
  - the cost family is pre-specified (no Park-test switching).
- **Measurement**: regression calibration (mecor); reference intervals and **centile curves
  (gamlss)** for echo / biomarker z-scores; compositional data (robCompositions, optional).
- **Agreement & diagnostic accuracy**:
  - **multi-rater** Fleiss κ / Krippendorff α / Gwet AC2 (irrCAC), weighted κ;
  - ICC form declared (McGraw & Wong / Koo & Li);
  - Bland–Altman with repeated measures and proportional bias, exact CIs for the LoA;
  - **imperfect reference standard** (latent class: randomLCA), paired test comparison (DTComPair);
  - cut-point optimism (cutpointr bootstrap), **MRMC reader studies (MRMCaov)**.
- **Trials**:
  - crossover (period / carry-over);
  - CRT with variable cluster size, **stepped-wedge** (time effects; swCRTdesign);
  - **group-sequential / adaptive with bias-adjusted estimates** (rpact, gsDesign);
  - NI / equivalence **margin justification** (fixed-margin vs synthesis), with both ITT and
    per-protocol estimands;
  - win ratio / win odds with IPCW (WINS); N-of-1 (optional).
- **Causal**:
  - **target-trial emulation: clone–censor–weight, sequential trials (TrialEmulation), grace periods**;
  - **g-methods for time-varying confounding**: parametric g-formula (gfoRmula), **LTMLE** (ltmle),
    MSM-IPTW;
  - generalised PS for continuous / multi-level exposures, entropy balancing, CBPS (WeightIt);
  - high-dimensional confounding (hdPS, post-double-selection);
  - IV: ivreg with weak-instrument F and a declared LATE (one-sample MR only);
  - **transportability / generalisability** (inverse-odds-of-participation weights, g-computation).
- **Meta-analysis completion**:
  - rare events: Mantel–Haenszel / Peto / binomial-normal GLMM (`rma.glmm`), no continuity correction;
  - dependent effects: `rma.mv` + RVE; meta-regression with permutation p-values;
  - **IPD meta-analysis** (one- and two-stage);
  - DTA bivariate / HSROC;
  - **NMA inconsistency** (node-splitting, design-by-treatment), CINeMA, SUCRA caveat;
  - **publication bias beyond funnel plots**: selection models, PET-PEESE, limit meta-analysis, Copas
    (metasens).
- **HTA completion**:
  - **survival extrapolation per NICE TSD 14/21**: standard parametric, Royston–Parmar, mixture /
    non-mixture cure, background mortality from SingStat, hazard plots;
  - **PSM vs state-transition** (TSD 19); **microsimulation** (hesim);
  - **budget impact**; **EVPPI / EVSI**; utility mapping (aldvmm, beta mixtures);
  - half-cycle correction; ACE discounting preset (verify the rate);
  - **ML-NMR** (multinma) alongside MAIC / STC.
- **Provider comparison & surgical quality monitoring** (cardiac surgery):
  - **funnel plots with overdispersion** (FunnelPlotR, Spiegelhalter);
  - caterpillar plots with shrinkage;
  - **risk-adjusted CUSUM / VLAD / RA-EWMA** on EuroSCORE II or STS risk (spcadjust, qcc);
  - this pairs with the published-model module in I.
- **Sample size completion**: NI / equivalence, crossover, stepped-wedge, longitudinal (longpower),
  RMST / non-PH (npsurvSS), ordinal (Whitehead), competing risks (simulation), and a generic
  **simulation-based power engine** for anything else.
- **Explicitly out of scope** (stated in the docs and the UI): multi-omics / high-dimensional
  genomics, pharmacometrics / PK-PD, functional data analysis, Bayesian adaptive design
  *planning* (re-analysis is in scope), data fusion, image / signal deep learning (v1).
  Spatial small-area (CARBayes / spdep) is optional and late.

**N. Risk calculator builder + developer handover** (research-verified 2026-10-03; no existing R tool
emits a standalone offline JS calculator, so this fills a real gap).
- **Sources**: any model fitted in the app (logistic, linear, ordinal, Cox at horizon t, Fine–Gray,
  cause-specific CR, points score, glmnet, RCS, GAM via RCS refit or bounded lookup grid, xgboost /
  lightgbm / ranger / small RSF) or a **published model** from catalogue I (the same JSON spec).
- **Single source of truth**: `model_spec.json`, validated against `model_spec.schema.json`
  (JSON Schema 2020-12). It holds:
  - inputs: canonical unit + alternative units with factors, hard/soft limits, levels and reference,
    a missing rule (required / constant / submodel), LOINC/SNOMED codes;
  - a typed transform expression tree (centring, log, RCS knots with normalisation, interactions);
  - the linear predictor; the baseline (S0(t), CIF0(t), or a step table) plus the centring constant;
  - tree blocks (`<` vs `<=`, float32, default direction for missing values);
  - output scale, rounding, and thresholds marked validated or not; performance; privacy; intended
    use; version; SHA-256.
- **`calculator.html`**: a single offline file built by plain R string templating. No CDN, no pandoc,
  no server; the spec + hash are embedded.
  - Unit toggle (SI ↔ conventional). Blocks a calculation outside hard limits; warns outside soft
    limits (extrapolation). Flags any result that used an imputed predictor.
  - Absolute risk as a % plus a natural frequency, and a 100/1000-person icon array.
  - Optional contribution waterfall on the logit/log-hazard scale, labelled "not causal".
  - Optional uncertainty display, off by default.
  - WCAG 2.2 (contrast, keyboard, 24 px targets, text alternative for the icon array); colour-blind-safe.
  - Model-Facts label: intended use, population, horizon, validation status, version/hash,
    "research use only" banner, review-by date.
- **Handover zip**:
  - spec + schema; `MODEL_CARD` (Model Facts + TRIPOD+AI item map); `calculator.html`;
  - spec-driven reference predictors `predict.R` / `predict.py` / `predict.js`;
  - `test_vectors` (csv + json) + `tolerances.json` + `run_equivalence.R` + equivalence report;
  - validation figures with no row-level data;
  - optional PMML / FHIR Questionnaire + CQL Library / ONNX (for Python-trained ML models);
  - `traceability.csv` (requirement → test → result, mapped to IEC 62304);
  - `MANIFEST.sha256` + ed25519 signature (reuse `crypto.R`); CHANGELOG, LICENCE, DISCLAIMER.
- **Equivalence gate**: four-way agreement is required before export:
  1. the fitted object's `predict` / `riskRegression::predictRisk`;
  2. the R spec evaluator;
  3. the Python spec evaluator;
  4. the actual HTML JS, run headless via the V8 package.

  Tolerances: 1e-9 for regression, 1e-6 for float32 trees, the declared bound for lookup grids;
  display strings must match exactly.
- **Test vectors**: every category level, limits, spline knots ± ε, inputs exactly at tree split
  thresholds, every missingness pattern, extreme linear predictors (overflow), about 1000 synthetic
  LHS profiles (never real rows), every horizon and outcome.
  - JS uses a stable logistic, `log1p`/`expm1`, clamping, and `Math.fround` for trees.
- **Privacy**: only the spec is exported, never R model objects.
  - **Hard-blocked**: kNN, RBF-SVM, Gaussian processes, LOESS, and TabPFN and other in-context
    learners (the training data is the model).
  - Trees: minimum leaf n, cover/OOB stats stripped. Baselines thinned to the shipped horizons.
  - Small categorical levels are merged; the manifest lists what is embedded (reuses the `sdc.R` ideas).
- **Size**: forests are capped (trees/depth) to keep the HTML small. A distilled model is a new model
  and is re-validated.
- **Regulatory framing**: the research-use labelling follows HSA GL-04, FDA CDS (2022) and EU MDR
  Rule 11; the docs note that clinical deployment may make it a SaMD.
- **Phase**: built in P6 (after prediction/validation); a published-model calculator can ship with
  the P6 validation module.

**O. Study feasibility verdict** (addendum 2026-10-11; built in **P1c.1**). Not to be confused with
the P0 *technical* feasibility spike. Before the SAP is locked, the app answers "can this dataset
answer this question precisely enough to be worth running?" and advises the user to proceed, proceed
as exploratory, revise the design, or stop. It never answers "will the result be significant or
interesting?": choosing whether to analyse from an expected effect is outcome selection (file-drawer
bias) and is refused, like post-hoc power.

- **Outcome-blind by construction.** The verdict uses only what catalogue A already shows while blind:
  - analysed n after the recipe's exclusions and the complete-case filter of each primary analysis;
  - the outcome's **own marginals** (event count and marginal risk, marginal SD, total events and
    follow-up, the pooled exposure-blind Kaplan–Meier curve);
  - exposure marginals and **exposure-on-covariate** structure (R² of the exposure model, propensity
    overlap) — these contain no outcome information;
  - cluster counts and sizes; the number of model parameters from the compiled terms.

  It never uses any outcome-by-exposure or outcome-by-covariate statistic. Quantities that would need
  one (outcome ICC, outcome R² explained by covariates) are **user-entered assumptions** with a
  written justification, never estimated from the data. Without them the verdict uses the marginal
  values, which is conservative (wider than the eventual adjusted CI).
- **Required input:** the **smallest clinically important difference** δ on the estimand's scale
  (e.g. 2 percentage points RD, HR 0.80, 5 mmHg), with its source (MCID literature, guideline, trial
  convention). δ is optional; without it the verdict reports precision only and says so.
- **Computation**, per primary analysis, from the compiled spec (so it matches what will run).
  - Approximate null-effect standard errors, with v = n·Var(x)·(1 − R²ₓ), where R²ₓ is the exposure
    on the planned covariates (Var(x) = p(1 − p) for a binary exposure; for a factor exposure each
    contrast uses 1/n_k + 1/n_ref):
    - MD: σ_y·√(1 − R²_y,assumed) / √v;
    - RD: √(π(1 − π) / v);
    - log RR: √((1 − π) / (π·v));
    - log OR: 1 / √(v·π(1 − π));
    - log HR (Schoenfeld): 1 / √(D·Var(x)·(1 − R²ₓ)), where D is total events;
    - RMST difference: simulation from the pooled KM curve with exposure assigned at the observed
      prevalence (exposure-blind).
  - Clustering multiplies the variance by the design effect 1 + ((cv² + 1)·m̄ − 1)·ICC_assumed
    (Eldridge 2006).
  - Missing data: complete-case n; for MI the verdict shows the range from complete-case n to n with
    observed outcome.
  - Results: expected 95% CI half-width h = 1.96·SE, and the **minimum detectable effect** at 80%
    power MDE = 2.80·SE. Both are on the reporting scale (ratios are exponentiated).
- **Verdict tiers**:
  - **Adequate**: MDE ≤ δ.
  - **Limited**: MDE > δ ≥ h. The study can probably rule out an effect as large as δ but has under
    80% power to detect one. The default advice is *proceed*, with the precision stated in the Methods.
  - **Inadequate**: h > δ. The study can neither reliably detect nor rule out a clinically important
    effect. The default advice is *revise or stop*, or *proceed as exploratory / hypothesis-generating*.
  - **Cannot run** (hard blockers, independent of δ):
    - an exposure with fewer than two populated levels or an empty contrast group;
    - no events, or an outcome that is constant;
    - more model parameters than events, or than observations for a continuous outcome;
    - exposure perfectly predicted by the covariates (separation of the exposure model, i.e. a
      positivity violation).
  - **Warnings** (shown, never pass/fail):
    - near-positivity violations (propensity outside [0.01, 0.99] for more than 5% of rows);
    - few events per model parameter as a sparse-data-bias screen, offering pre-specified Firth;
    - for prediction routes, the Riley criteria (pmsampsize) feed the verdict;
    - few clusters (< 30) for cluster-robust SE.
- **Decision** (rules only; System One is not involved because everything is computed):
  - **Proceed**: the analysis stays primary.
  - **Proceed as exploratory**: the analysis is labelled exploratory. The writer and claim guard
    require hypothesis-generating language, and the Methods state why.
  - **Revise**: go back and change the design before lock, for example fewer terms, pooled sparse
    levels, a different estimand, a composite outcome or a longer horizon. Revisions are outcome-blind
    and logged. They are not deviations, because nothing is locked yet.
  - **Stop**: the project is marked stopped with the reason, and `sr_run` refuses. Reversing a stop
    is allowed before lock with a logged reason.
  - A feasibility report is exportable for data-request, ethics or grant use. It is a structured
    result in P1c.1, and the docx comes with the P1d writer.
- **Events and lock gate**:
  - `feasibility.assessed` {analysis_id, inputs, assumptions + justifications, SEs, h, MDE, tier,
    data/spec hash}.
  - `feasibility.decided` {analysis_id, decision, reason}.
  - The SAP lock refuses while any primary analysis has no decision, has *stop*, or has an assessment
    whose data/spec hash is stale.
  - The verdict is frozen into the SAP hash and **never recomputed after lock**. After lock the
    stored verdict is shown as recorded, and power from an observed effect stays refused.
- **Reporting**: a Methods fragment states that the anticipated precision was assessed before outcome
  analysis and gives n, events, exposure prevalence, h, MDE, δ and its source. Guided mode (P4) adds
  plain-language wording, for example: "with this many patients the 95% CI will be about ±4
  percentage points; a 2-point difference cannot be detected or ruled out".

## Phases (each ends shippable, tested, committed; ask before any push)

Cross-cutting placement:

- The M1 global rules are built into **P1** (spec, estimand, purpose, multiplicity registry,
  composition matrix, RNG/MCSE). They are not retrofitted.
- The other M2 methods land in the phase of their family: longitudinal / time series / dose-response
  / survival completion in P9, trials completion in P5, causal completion in P7, HTA completion in
  P11.
- The risk calculator builder (catalogue N) lands in **P6**; published-model calculators can ship
  with the P6 validation module.

- **P0 Repo + spec + feasibility spike (≤2 wk).**
  - `git init` Downloads\analysis and verify the toplevel. Write
    `docs/specs/2026-10-03-statrouter-design.md` (everything above + catalogues), then the user reviews it.
  - **Spike on the real locked-down PC**: portable R + Shiny launch under WDAC/AppLocker, officer docx,
    mirai/callr, processx + Python + Von ONNX latency/RAM, **DuckDB streaming of a multi-GB CSV
    within 16 GB**, **rstanarm DLL load + a 4-chain
    `stan_glm` sample under AppLocker**. Go/no-go per component.
  - Then `writing-plans` for P1 and create the worktree.
- **P1 MVP: Expert mode, rules router + Jev adapter, shadow logging.** End to end, the user can:
  1. create a project and import any catalogue-J format by path, streamed via DuckDB into the
     project's Parquet store, with out-of-core profiling, the memory estimator, and a dictionary /
     REDCap dictionary; **100 GB-capable pre-flight, resumable ingest, external-data mode and cohort
     builder**;
  2. enter a PICO and confirm variable roles;
  3. run the DQ checks + Data Action Suggestor, giving a recipe and flow diagram;
  4. **lock the SAP**;
  5. produce Table 1 and primary models: linear, logistic, modified Poisson, Cox. Options: RCS,
     robust/cluster SE, pre-specified RMST, complete-case or mice + Rubin; effect sizes + CIs;
     bootstrap where routed; diagnostics with plots → sensitivity nodes;
  6. export a bookmarked docx (Methods / Results / Tables / KM + forest plots / STROBE) +
     `analysis.R` + lockfile;
  7. freeze → `.srproj` → reimport on another path with an identical hash.

  **Exit**: reproduce published values for NHEFS (causal), `survival::lung`, GBSG/Rotterdam
  (prediction); MI + bootstrap CI coverage checked by simulation; planted-defect recall 100% with a low
  false-positive rate on clean twins.
- **P1c.1 Study feasibility verdict** (catalogue O; addendum 2026-10-11): built right after P1c
  merges and before P1d. It covers the headless engine, the `feasibility.*` events, the lock gate
  and the tests. The panel UI arrives with P1d's UI, the feasibility-report docx with the P1d
  writer, and plain-language wording with Guided mode (P4).
- **P2 Reviewer mode v1**: split → tag → self-diagnose → re-run on the frozen archive under the pinned
  library → reply docx + **refresh numbers in the edited manuscript** + plot/table builder.
- **P3 Local System One**: Von (stdio) + CLM on the laptop; golden set + shadow logs → bake-off →
  enable models per node; calibration and cost-matrix thresholds.
- **P4 Guided mode** + statistician sign-off workflow + watermark.
- **P5 Trials + Bayesian core.**
  - Trials: estimands, covariate adjustment, MMRM, rbmi, multiplicity; CONSORT/SPIRIT.
  - **Bayesian layer H**: rstanarm regression family, priors + prior sensitivity, MCMC diagnostics,
    PSIS-LOO, Pr(benefit) / ROPE reporting, sceptical/enthusiastic re-analysis of RCTs, BARG
    checklist.
  - From here on, every later family adds its Bayesian counterpart where one exists: multinma/bayesmeta
    in P8, BART in P6/P7, Bayesian PSA in P11.
- **P6 Prediction + validation + ML (catalogue L) + ML survival + DCA** (TRIPOD+AI, PROBAST+AI).
  - **6a**: R learners, ensembles and tidymodels AutoML.
  - **6b**: the Python runner tabular stack (RealMLP / TabM / EBM) and foundation models with the
    licence gate.
  - **6c**: deep survival (pycox / torchsurv) and AutoGluon on the laptop.
  - Validation catalogue **I**: leakage-safe CV designs, internal–external CV, the full calibration
    panel, the **published-model validation module** (entry → mapping → self-check → score →
    head-to-head → updating), and pmvalsampsize.
  - Validation-only projects (no model development) are a first-class project type.
  - **6d**: risk calculator builder (catalogue N): `model_spec.json` + schema, single-file offline
    `calculator.html`, R/Python/JS reference predictors, test vectors, four-way equivalence gate,
    signed handover zip.
  - If you want external validation of published scores sooner, it can move ahead of P5. It depends
    only on P1 + P2.
- **P7 Causal / TTE + quasi-experimental + mediation + HTE** (TARGET).
- **P8 Diagnostic accuracy, agreement, meta / NMA** (STARD, GRRAS, PRISMA), including the M2
  additions (multi-rater, MRMC, imperfect reference, IPD-MA, NMA inconsistency, selection models)
  and **surgical quality monitoring (CUSUM / VLAD / funnel)**.
- **P9 Extended regression / survival / epidemiology** (GAM, quantile, recurrent, joint models, survey,
  standardisation).
- **P10 Latent variables: SEM (catalogue K) + latent class/profile/growth mixture + unsupervised +
  psychometrics / PROMs.**
- **P11 HTA** (trial-based CEA, Markov / PSM, MAIC, NMA inputs, VOI, DCEA; CHEERS 2022; ACE presets).
- **P12 Deep learning + optional LLM polish + packaging variants** (`core`, `cpu-ai`, `gpu-ai`), with
  path-length and closure audits and a clean-machine verify.

## Reuse (structured_deid worktree `…\structured_deidentification\.claude\worktrees\shiny-deidentification-app-6a7932`)
- `app/R/project.R` (`se_project_*`, `se_manifest_write`, `se_sha256_file`) → `core/project.R`
- `app/R/hashchain.R` (`se_audit_append/verify/tip`) → `core/audit.R`
- `app/R/engine_py.R` (passive bundled-Python probe; never `py_available(initialize=TRUE)`) + `app/python/run_engine.py` (`_forbid_network`) → local runner, ported to processx stdio
- `app/R/sdc*.R` ideas → output small-cell suppression; `app/R/report.R` → optional audit certificate
- `tools/stage_r.ps1`, `stage_r_deps.R`, `build_bundle.ps1`, `audit_pathlen.ps1`, `check_closure.ps1`, `verify_clean_machine.ps1`, `run.ps1/.bat`; AndyFishing `run.bat` (R_LIBS + `--vanilla` + log) and `stage_ollama.ps1` (model staging)
- Gotchas:
  - MAX_PATH: build in a short directory and prune `include/`.
  - Write generated .ps1/.R scripts as ASCII only.
  - Antivirus quarantines unsigned exes.
  - `Rscript -e` with multiline code segfaults, so use script files.
  - Pin the xgboost 3.x API.
  - brms/rstan are not portable; use rstanarm. Prefer flexsurv over survHE.
  - No webshot/chromote/DiagrammeR PNG export.
  - Fonts on the locked PC: use ragg/systemfonts with an Arial fallback.

## Verification
- `testthat` per module.
- **Textbook reproduction** suite (NHEFS / Hernán & Robins, `survival::lung`, GBSG/Rotterdam, lalonde,
  `mice::nhanes`) must match published values.
- Simulation checks of CI coverage for MI × bootstrap × clustering.
- Import / large-data checks:
  - A round-trip fixture for every format in J (labels, dates, encodings, compressed files).
  - **Stress test on `locked16` limits**: a synthetic 20M-row × 60-column CSV (~5 GB) plus a
    1M-row XLSX ingest with peak RSS under the profile budget.
  - Out-of-core profile = in-memory profile on a subsample.
  - The memory estimator correctly routes an oversize GLM to biglm/speedglm.
  - **100 GB scale test on the `laptop` profile**: a synthetic ~1-billion-row long-format "EHR" CSV
    is ingested end to end, then run through the cohort builder, Table 1 and a logistic regression.
    Recorded: wall time, peak RSS (must stay under the DuckDB limit + R overhead), disk spill, and
    resume after a killed process.
  - Aggregated-Poisson and SQL sufficient-statistic fits must equal `glm`/`coxph`/`lm` on a 1M-row
    subsample within numerical tolerance.
  - On `locked16`, a 20 GB proxy in P0 is extrapolated, followed by a full 100 GB run in P1 if the
    locked PC's disk allows.
- Validity-rule checks:
  - A lint test fails if any route branches on a diagnostic p-value.
  - Every route declares an estimand + purpose, or registration fails.
  - Composition-matrix simulations reach nominal CI coverage (survey × MI, IPW × bootstrap, matching
    × pair bootstrap, clustered MI).
  - Pre-lock views contain no outcome-by-exposure statistic.
  - MCSE-limited digit printing is unit-tested.
- ML checks:
  - Every learner passes a shared conformance test (fit / predict / probabilities / calibration /
    explain) on reference data.
  - A planted-signal benchmark shows ML ≈ logistic regression when the truth is linear, and ML wins
    when the truth is non-linear.
  - A foundation-model offline test: no network, cache only.
  - The licence gate blocks non-commercial weights until attestation.
  - Survival learners are compared against `coxph` on `survival::lung` / GBSG.
- SEM checks: reproduce lavaan's reference examples (HolzingerSwineford CFA, PoliticalDemocracy SEM,
  invariance sequence) and a planted non-invariant item.
- Validation checks:
  - On synthetic data with **planted miscalibration** (known intercept shift and slope), the O:E
    ratio, slope and ICI recover the truth, and closed testing picks the correct update level.
  - Grouped CV is proven leak-free: a planted patient-ID leak inflates AUC under naive CV but not
    under grouped CV.
  - A published model's worked example reproduces to the reported decimal.
  - Results agree with `rms::val.prob` and `riskRegression::Score` on reference datasets.
- Bayesian checks:
  - With flat or weak priors, rstanarm posteriors match `glm` within Monte Carlo error.
  - The MI draw-pooling path is checked against known truth.
  - Planted divergent and non-converging models must trip the diagnostics.
- Risk calculator checks:
  - Four-way equivalence (fitted object / R spec evaluator / Python spec evaluator / HTML JS run
    headless via V8) passes on every test vector for logistic, Cox (centred), Fine–Gray, ordinal
    (both sign conventions), RCS, glmnet, xgboost and lightgbm (inputs exactly at split thresholds).
  - A published preset (e.g. EuroSCORE II) reproduces its worked example in the HTML.
  - `calculator.html` opens from `file://` with networking disabled; no external requests.
  - Instance-based models (kNN, RBF-SVM, GP, LOESS, TabPFN) are refused; exported spec contains no
    row-level data; tree leaves respect the minimum leaf n.
  - Accessibility smoke test (keyboard-only use, contrast, icon-array text alternative).
- Feasibility verdict (catalogue O):
  - **Blindness test**: permuting the observed outcome values among rows with a non-missing
    outcome (NA positions kept) leaves the verdict byte-identical.
  - Predicted SEs are within 10% of the empirical SD of the estimates over 1000 simulated datasets
    for each estimand (MD, RD, RR, OR, HR, RMST), with and without clustering.
  - On NHEFS the marginal-value prediction is conservative: it is at least 0.9 × the SE actually
    obtained after lock.
  - Each hard blocker fires on a planted case.
  - The lock refuses with a missing, *stop* or stale decision.
  - No verdict is recomputed after lock.
- Profiler: planted-defect recall and clean-twin false-positive rate.
- Router: golden set + shadow logs → accuracy, cost-weighted error, ECE, abstain rate, latency and RAM
  per profile; vetoes and sensitivity nodes fire on planted violations.
- Writer: every placeholder resolves to a fit object; the claim guard passes; bookmark refresh
  round-trips on an edited docx.
- Archive: freeze → export → import at a different path → identical hashes. A library mismatch
  triggers re-baseline.
- Air-gap: Jev disabled, runner stdio-only, no sockets → fully functional.
- Crash recovery: kill the R session mid-bootstrap → reopen → events replay and the checkpoint resumes.
- UI: `shiny::runApp('app', port=7790)` driven in the built-in browser.
- P0 and P12: `verify_clean_machine.ps1` passes on the real locked-down PC.

## Open items to re-verify before the phase that needs them
The research passes could not confirm these online; re-check at the start of the relevant phase.

- TRIPOD+AI item numbers; PROBAST+AI (BMJ 2025) and Riley 2025 uncertainty-paper citations (P6).
- FDA CDS guidance revision (reported Jan 2026), MDCG 2019-11 revision, IEC 62304 edition 2 (P6, N).
- CRAN status/versions: pmml, DynNom, rms, xgboost 3.x API, lightgbm, tidypredict, orbital (P6).
- xgboost default save format (UBJSON vs JSON) and `base_score` scale in 3.x (P6, N).
- TabPFN distillation availability and licence terms of each TabPFN/LimiX/Nori release (P6b).
- Singapore EQ-5D value set and ACE discount rate (P11).
- Model-licence and CRAN/PyPI status of every learner in catalogue L at P6 start.
