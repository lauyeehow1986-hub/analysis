# StatRouter analysis guide (P1c)

This guide covers the analysis layer built in phase P1c: the statistical analysis plan (SAP) and its
lock, the run engine, the analysis routes, multiple imputation, the bootstrap, diagnostics, sensitivity
nodes, deviations and multiplicity. The data layer (ingest, store, profiling, data quality) is described
in [data.md](data.md). The validation record (CI-coverage simulation and textbook reproductions) is in
[validation.md](validation.md).

StatRouter is a research and governance tool, not for clinical or diagnostic use. Every output needs
review by a qualified statistician.

## 1. Principles

- **Code first.** A route's `compile(spec)` returns an R code string. That string is what runs and what
  is exported as `analysis.R`. Emitted code is built only by quoting (`.sr_q` for names, `.sr_lit` for
  values) and every compiled string passes `.sr_emit_check()` before it is stored or run.
- **Numbers come from evaluated code.** No model writes numbers. Each route's `present()` turns the
  evaluated value into effect rows with the shared effect columns, checked by `sr_result_check()`.
- **No test-gated method switching.** A route's `compile()` never branches on a diagnostic. Diagnostics
  live in `app/R/diagnostics/`, outside `app/R/routes/`, and emitted analysis code never names a
  diagnostic; the lint fails the build if a file under `routes/` does. A failed diagnostic adds a
  sensitivity node; the primary analysis changes only through a rule written into the locked SAP.
- **Effect sizes, not p-values.** Diagnostic verdicts are effect-size thresholds. Descriptive routes
  (Table 1 and the `*_describe` twins) carry no p-values.
- **Event-sourced.** Every SAP change, lock, run and post-lock analysis is an event in `events.jsonl`;
  replaying the events gives the same state as the live session.

## 2. The analysis spec

Every analysis is a route id plus a spec (a named list). The fields every model route requires are
`layers` and `seed`; each route's own fields are in section 5. `sr_spec_validate()` checks the spec
against the route's schema, and `sr_compose_check()` checks that the layers can be combined.

**Layers** (`spec$layers`):

| layer | values |
|---|---|
| `missing` | `cc` (complete cases) or `mi` (multiple imputation, section 6) |
| `resampling` | `none` or `bootstrap` (section 7) |
| `clustered` | `TRUE` / `FALSE` |
| `weights`, `matching`, `framework` | declared in the layer vocabulary; weighting, matching and Bayesian frameworks are refused at compile in P1c |

**Terms** (`spec$terms`): each term names a column and a type: `linear`, `factor`, `rcs` (restricted
cubic spline) or `quad`. For `rcs`, `knots` is `NULL` (4 knots), a number from 3 to 7, or 3 to 7 knot
locations. **Interactions** (`spec$interactions`) are allowed only between linear and factor terms; a
quadratic or spline term cannot enter an interaction (enter a squared column as a linear term instead).

**`exclude_rows`** (lm, logistic, modpois, cox): 1-based row positions to drop, used by the influence
sensitivity. The positions refer to the row order of the analysis extract, which `sr_extract()` sorts by
the selected columns, not to the order of the store. `exclude_rows` is refused on rmst.

## 3. The SAP workflow: add, family, rule, lock

The SAP is drafted before any outcome data is seen and locked before anything runs.

1. **Add.** `sr_sap_add()` adds an analysis (route, table, label, spec, parent). It validates the spec
   and the composition and refuses a bad route, an unsupported layer, a duplicate id or a missing parent;
   it does not compile (compilation happens at run time against the extract).
   `sr_sap_remove()` removes an analysis before the lock.
2. **Family.** `sr_sap_family()` groups analyses into a multiplicity family with a p-value adjustment
   method: `holm`, `hochberg`, `hommel`, `bonferroni`, `BH`, `BY` or `none`.
3. **Rule.** `sr_sap_rule()` pre-specifies what a diagnostic may do: `add_sensitivity` (the default
   behaviour) or `replace_primary`, where the alternative must be a sensitivity analysis whose parent is
   the analysis the rule protects.
4. **Lock.** `sr_sap_check()` lists anything that blocks the lock; `sr_sap_lock()` refuses while any
   block remains, then records the SAP hash (`sap_hash`), the store hash of every table used
   (`store_hash`), the library snapshot hash (`libsnap_hash`) and `locked_at`. Every run requires the
   lock. Amending a locked SAP is a later phase (section 11).

## 4. The run engine (`app/R/routes/run.R`)

- **Extract.** `sr_extract()` selects the analysis columns from the store, ordered by every selected
  column (`SELECT cols FROM t ORDER BY cols`), so row positions are stable for a given column set. The
  extract is serialised (`serialize(version = 3)`) to `data/extracts/<sha>.rds`, named by its SHA-256.
- **Prologue.** The emitted code starts with a prologue that reads the extract and stops with
  "extract changed" unless its SHA matches the one recorded for the run, then applies the RNG preamble
  for the spec's seed.
- **Cache key.** A run is identified by `sr_content_hash()` of the extract SHA, the route key with the
  spec, and the seed. The cache is given `cache_inputs = list(extract = sha)`, so the project path is not
  part of the identity: a project moved to another folder or machine reuses its cache.
- **Records.** A finished run writes a `run.completed` event. If the library snapshot differs from the
  locked one, the run is flagged `lib_changed` (and listed as a deviation). `sr_result()` returns the
  stored result: effect rows, tables, figure data and notes.

## 5. Routes

All rows a route reports share the effect columns (term, measure, estimate, interval, p, role, ...).
The primary-role rows are the estimand; covariate rows are reported for completeness; descriptive rows
carry no p-values.

| route | purpose | model | primary effect | SE options |
|---|---|---|---|---|
| `table1` | describe | baseline characteristics by `by` (mean (SD), median [IQR] for `nonnormal`, n (%)) | SMD per group vs the first level, with CI; no p-values | none |
| `lm` | explain-causal | `outcome ~ exposure + terms (+ interactions)`, complete cases | MD (adjusted mean difference); with `marginal = TRUE` the g-computation marginal MD | `model` (default), `robust` (HC3), `cluster` (HC1, t on G - 1 df) |
| `logistic` | explain-causal | binomial logit glm, or Firth (`penalty = "firth"`, logistf) | conditional OR; marginal RD, RR and NNT by g-computation (`marginal` default TRUE, off with Firth) | `model` (default), `robust` (HC0), `cluster` (HC1) |
| `modpois` | explain-causal | Poisson log-link glm (Zou 2004) | conditional RR; marginal RD, RR, NNT as for logistic | `robust` (default) or `cluster`; `model` is refused |
| `cox` | explain-causal | `Surv(time, status == status_event) ~ exposure + terms [+ strata] [+ cluster]`; ties `efron` (default) or `breslow` | HR (adjusted HR when terms or strata are present) | model-based; robust (cluster) only with `layers$clustered = TRUE` and `cluster` |
| `rmst` | explain-causal | Kaplan-Meier per exposure level up to the pre-specified `tau` | RMST difference vs the reference level, normal interval | none; clustered data refused |

Notes per route:

- **Describe twins.** `lm_describe`, `logistic_describe` and `cox_describe` share compile and present
  with their explain-causal routes but are descriptive: no p-values.
- **Binary outcomes.** A logical or 0/1 outcome is used as is; any other outcome needs `event_value`
  (checked by the emitted code at run time, since validation has no data).
- **Marginal effects.** `standardize_over` is `analysis` (the fitted rows) or `all` (also the rows lost
  only to a missing outcome). For a continuous exposure the glm marginal measures are skipped with a
  note unless `marginal = TRUE` was asked for explicitly, which is then an error. A conditional OR is
  flagged as non-collapsible and the marginal measures sit next to it. The NNT is derived from the RD.
- **RMST.** `tau` is required and must lie within the follow-up of every group (the emitted code stops
  otherwise); a group with fewer than 10 at risk at `tau` gets a note; an exposure with more than 8
  distinct values is refused ("categorise it first"). There is no covariate adjustment.
- **Estimands.** Every route declares an estimand (population, variables, intercurrent events, summary
  measure, conditional or marginal level). The population is "complete cases", or "all analysed (MI)"
  under multiple imputation.

## 6. Multiple imputation (`layers$missing = "mi"`)

- Options in `spec$mi`: `m` (a whole number of at least 2, or `"auto"`), `maxit` (default 10) and `aux`
  (auxiliary columns used only in the imputation model).
- Imputation is by `mice` with methods `pmm` (numeric), `logreg` (binary) and `polyreg` (categorical);
  the outcome is imputed too. Transformations (splines, quadratics, interactions) are computed after
  imputation, in each completed dataset ("impute, then transform").
- Estimates are pooled by Rubin's rules on the estimation scale (log scale for ratios) with the
  Barnard-Rubin degrees of freedom.
- `m = "auto"`: a pilot of 20 imputations estimates the fraction of missing information, then
  von Hippel's rule `m = ceiling(1 + 0.5 * (fmi_upper / 0.05)^2)`, capped at 200.
- Refused: MI with clustered data, and MI with Firth.

## 7. Bootstrap (`layers$resampling = "bootstrap"`)

- Options: `spec$boot$B` (at least 20, default 2000) and `spec$boot$type`: `percentile`, `bca` or
  `normal`. `boot_type = "cluster"` resamples whole clusters.
- The bootstrapped statistic is the route's primary effect as reported: with marginal effects the
  g-computation is re-run in every replicate. Ratio measures are bootstrapped on the log scale.
  Covariate and descriptive rows keep their model-based intervals.
- Replicates run in chunks of 100, each on its own `nextRNGStream`, so results do not depend on the
  number of workers. BCa falls back to percentile when it cannot be computed, and the fallback and its
  reason are recorded. A Monte Carlo SE of the interval limits comes from 10 batches. Seed, B and method
  are recorded with the result.
- MI together with the bootstrap needs `choices$mi_x_boot`: `boot_then_impute` or `impute_then_boot`.
- The coverage of the MI, bootstrap, MI-bootstrap and cluster intervals is checked by simulation; see
  [validation.md](validation.md).

## 8. Diagnostics (`app/R/diagnostics/`)

`sr_diag_checks()` is the catalogue; `sr_diag_applicable()` picks the checks that apply to an analysis.
Diagnostics fit the analysis model once on the analysis data, ignoring the bootstrap layer; under MI
they run on the complete cases of the analysis columns and say so. Verdicts (`ok` / `warn` / `fail`) are
effect-size thresholds; a reported statistic such as the `cox.zph` p-value is shown but never feeds the
verdict. A value that cannot be computed is a `warn`.

| check | routes | value | warn | fail | sensitivity node |
|---|---|---|---|---|---|
| `ph` | cox | max time-varying log HR (per coefficient for a factor exposure); carries `tau_suggest`, the 75th centile of event times | >= log 1.25 | >= log 1.5 | rmst at the pre-specified tau, else `tau_suggest` |
| `influence` | lm, logistic, modpois | max Cook's distance | > 0.5 | > 1 | refit with `exclude_rows` |
| `influence` | cox | max scaled dfbeta | >= 0.5 | >= 1 | refit with `exclude_rows` |
| `separation` | logistic | separation detected | none | >= 1 | Firth |
| `few_clusters` | model routes with a cluster | number of clusters | < 50 | < 30 | cluster bootstrap, B = 2000, percentile |
| `inf_censoring` | cox, rmst | max abs log HR of a Cox model for censoring on the baseline terms (rmst: the exposure) | > log 1.25 | > log 1.5 | none: recorded as a limitation |
| `linearity` | lm, logistic, modpois, cox | worst departure from linearity over continuous linear terms (interaction terms excluded) | > 0.1 | > 0.25 | that term as RCS(4) |
| `heteroscedasticity` | lm | variance ratio | > 1.2 | > 1.5 | robust SE |

- With `penalty = "firth"` the `separation`, `influence` and `linearity` checks do not apply (the
  unpenalised fit is not the analysis model); the reason is stated.
- `inf_censoring` screens covariate-dependent censoring. Censoring that is informative given the
  covariates cannot be tested; IPCW is a later phase.
- Figures (`app/R/diagnostics/plots.R`) are ggplot objects exported deterministically with ragg;
  plot data are capped at 5000 points by a seeded sample.

## 9. After the lock: sensitivity nodes and post-lock analyses

- **`sr_analysis_add()`** adds an analysis after the lock (an exploratory, post hoc or unplanned
  sensitivity analysis). It needs a reason, an id matching `^[a-z][a-z0-9_]{0,39}$`, and the spec is
  validated and compiled at add time.
- **`sr_diag_apply()`** turns a diagnostic result into its consequence. If a SAP rule covers the check,
  the rule fires; otherwise it adds a sensitivity analysis with id `<analysis>_<check>` (truncated to
  40 characters; a clash is refused) built by the sensitivity mapping in section 8; a check whose
  sensitivity is a limitation records a `diag.limitation` instead. Applying the same result twice
  changes nothing.

## 10. Deviations and multiplicity

- **`sr_deviations()`** lists every departure from the locked SAP, by kind: `exploratory`, `post_hoc`,
  `unplanned_sensitivity`, `rule_fired` and `lib_changed`.
- **`sr_family_adjust()`** applies the family's method with `stats::p.adjust` over the primary rows of
  the family's analyses that share the first primary measure.

## 11. Out of scope in P1c

Each item is a ruling; the owning phase is in brackets.

- The SAP_v1.docx document and the lock UI [P1d]. P1c owns the `sap.*` events, the lock and its
  refusals, because every run needs them.
- SAP amendment after the lock and data versioning [P2]. In P1c a run on data that changed after the
  lock is refused.
- Re-baselining under a changed library [P2]. P1c records the snapshot at the lock and flags a run
  made under a different one.
- Big-data engines (SQL Table 1, bigglm, aggregated Poisson, piecewise-exponential Cox) [P1e scale].
  P1c refuses a fit whose `sr_mem_estimate()$fits` is FALSE.
- CR2 / Kenward-Roger few-cluster corrections, Fine-Gray and IPCW [P9]. P1c's few-cluster
  sensitivity is the cluster bootstrap.
- Delta-adjusted tipping-point MI and rbmi; Little's test (descriptive only) [P5].
- The studentised bootstrap [P5]. P1c has percentile, BCa and normal intervals.
- GBSG/Rotterdam published prediction-performance values [P6]. P1c checks GBSG Cox equivalence
  against a direct `coxph` only.
- Chunk-level checkpoint/cancel and parallel workers for the bootstrap and MI [P1d, ExtendedTask].
  P1c already uses per-chunk L'Ecuyer streams, so results will not depend on chunking or workers.
- Methods/Results prose [P1d writer]. P1c returns fragment inputs (named values and short template
  ids), not prose.

Weighting, matching and Bayesian frameworks are part of the layer vocabulary but are refused at
compile in P1c.

The authoritative list is the "Out of scope here" section of the P1c plan
(`docs/superpowers/plans/2026-10-11-p1c-analysis.md`).
