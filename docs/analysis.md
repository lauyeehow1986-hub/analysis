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
  live in `app/R/diagnostics/`, outside `app/R/routes/`. At run time `.sr_emit_check()` refuses any
  emitted analysis string that calls a diagnostic or names a p-value; separately, the lint fails the
  build if a route's `compile()` body under `routes/` branches on a p-value. A failed diagnostic adds a
  sensitivity node; the primary analysis changes only through a rule written into the locked SAP.
- **Effect sizes, not p-values.** Diagnostic verdicts are effect-size thresholds. Descriptive routes
  (Table 1 and the `*_describe` twins) carry no p-values.
- **Event-sourced.** Every SAP change, lock, run and post-lock analysis is an event in `events.jsonl`;
  replaying the events gives the same state as the live session.

## 2. The analysis spec

Every analysis is a route id plus a spec (a named list). Every spec carries a `purpose` (`describe`,
`predict` or `explain-causal`) that must match the route's; every model route also requires `layers`
and `seed` (Table 1 requires `seed`, and also a `layers` list because the composition check reads
it). Each route's own fields are in section 5. `sr_spec_validate()`
checks the spec against the route's schema, and `sr_compose_check()` checks that the layers can be
combined (disallowed combinations and unresolved choices such as `choices$mi_x_boot`).

**Layers** (`spec$layers`): all six fields are required.

| layer | values |
|---|---|
| `missing` | `cc` (complete cases) or `mi` (multiple imputation, section 6) |
| `resampling` | `none` or `bootstrap` (section 7) |
| `clustered` | `TRUE` / `FALSE` |
| `weights` | `none`, `ipw`, `overlap`, `survey` |
| `matching` | `TRUE` / `FALSE` |
| `framework` | `frequentist`, `bayesian`, `both` |

Weighting, matching and the Bayesian framework are part of the vocabulary but cannot run in P1c. The
spec is compiled when it is added (`sr_sap_add`, `sr_analysis_add`), so they are refused there, never
silently ignored.

**Terms** (`spec$terms`): a list of terms, each naming a column (`var`) and a type: `linear`, `factor`,
`rcs` (restricted cubic spline) or `quad`. For `rcs`, `knots` is `NULL` (4 knots), a number from 3 to 7,
or 3 to 7 knot locations. **Interactions** (`spec$interactions`) are pairs of variables and are allowed
only between linear and factor terms (including the exposure); a quadratic or spline term cannot enter
an interaction (enter a squared column as a linear term instead).

**`exclude_rows`** (lm, logistic, modpois, cox): 1-based row positions to drop, used by the influence
sensitivity. The positions refer to the row order of the analysis extract, which `sr_extract()` sorts by
the selected columns, not to the order of the store. `exclude_rows` is refused on rmst.

## 3. The SAP workflow: add, family, rule, lock

The SAP is drafted before any outcome data is seen and locked before anything runs.

1. **Add.** `sr_sap_add()` adds an analysis (id, route, table, label, spec, parent, addresses) or
   replaces the analysis with the same id. The id must match `^[a-z][a-z0-9_]{0,39}$`. It refuses a bad
   route or spec, a disallowed layer combination or unresolved choice, an unknown table or parent, and a
   bad label or family. It also compiles the spec and refuses one that can never run ("cannot run on
   route ..."): a layer P1c cannot run (weighting, matching, Bayesian), invalid MI settings (no `m`,
   `m = 1`, MI with clustered data or Firth) or a bootstrap with B below 20. A binary outcome or survival
   status with no `event_value` / `status_event` is refused unless the stored column is logical or all
   0/1. `sr_sap_remove()` removes an analysis before the lock.
2. **Family.** `sr_sap_family()` groups analyses into a multiplicity family with a p-value adjustment
   method: `holm`, `hochberg`, `hommel`, `bonferroni`, `BH`, `BY` or `none`.
3. **Rule.** `sr_sap_rule()` pre-specifies the response when a check fails for an analysis: show the
   pre-specified alternative as a sensitivity result (`add_sensitivity`) or make it the primary result
   (`replace_primary`). For both actions the alternative must be a sensitivity analysis whose parent is
   that analysis.
4. **Lock.** `sr_sap_check()` lists anything that blocks the lock (including an `event value` row,
   re-checked against the current data); `sr_sap_lock()` refuses while any
   block remains, then records the SAP hash (`sap_hash`), the store hash of every table used
   (`tables$store_hash`), the library snapshot hash (`libsnap_hash`) and `locked_at`. Every run requires
   the lock. Amending a locked SAP is a later phase (section 11).

## 4. The run engine (`app/R/routes/run.R`)

- **Extract.** `sr_extract()` selects the analysis columns from the store, ordered by every selected
  column (`SELECT cols FROM t ORDER BY cols`), so row positions are stable for a given column set. The
  extract is serialised (`serialize(version = 3)`) to `data/extracts/<sha>.rds`, named by its SHA-256.
- **Prologue.** The executed code is the RNG preamble for the spec's seed, then a prologue that reads
  the extract and stops with "extract changed" unless its SHA matches the one recorded for the run,
  then the route's compiled code.
- **Cache key.** A run is identified by `sr_content_hash()` of the extract SHA, the route key with the
  spec, the seed, the library snapshot hash and the hardware fingerprint (profile, BLAS, GPU present,
  R version). The cache is given `cache_inputs = list(extract = sha)`, so the project path is not part of
  the identity: a project moved to another folder, or to a machine with the same environment, reuses
  its cache; a different library or hardware fingerprint recomputes.
- **Records.** A finished run writes a `run.completed` event. If the library snapshot differs from the
  locked one, the run is flagged `lib_changed` (and listed as a deviation). `sr_result()` returns the
  stored result: effect rows, tables, figure data, fragment inputs and flags.

## 5. Routes

All rows a route reports share the effect columns (`term`, `measure`, `est`, `lo`, `hi`, `p`, `role`,
...). The primary-role rows are the estimand; covariate rows are reported for completeness; descriptive
rows carry no p-values.

Spec fields per route (besides `purpose`, `layers` and `seed`):

- `table1`: `by`, `vars`, optional `nonnormal`.
- `lm`: `outcome`, `exposure`, optional `exposure_ref` (makes the exposure a factor with that reference
  level), `terms`, `interactions`, `se`, `cluster`, `marginal` (default FALSE), `standardize_over`,
  `exclude_rows`.
- `logistic`, `modpois`: as lm plus `event_value`; `marginal` defaults to TRUE; logistic also takes
  `penalty` (`none` or `firth`).
- `cox`: `time`, `status`, optional `status_event` (number or text), `exposure`, `exposure_ref`, `terms`, `interactions`,
  `strata`, `cluster`, `ties`, `exclude_rows`.
- `rmst`: `time`, `status`, optional `status_event`, `exposure`, `exposure_ref`, and the required `tau`.

| route | purpose | model | primary effect | SE options |
|---|---|---|---|---|
| `table1` | describe | baseline characteristics by `by` (mean (SD), median [IQR] for `nonnormal`, n (%)) | SMD per group vs the first level (with a CI for continuous and two-level variables); no p-values | none |
| `lm` | explain-causal | `outcome ~ exposure + terms (+ interactions)`, complete cases | MD (adjusted mean difference); with `marginal = TRUE` the g-computation marginal MD | `model` (default), `robust` (HC3), `cluster` (HC1, t on min(residual df, G - 1) df) |
| `logistic` | explain-causal | binomial logit glm, or Firth (`penalty = "firth"`, logistf) | conditional OR; marginal RD, RR and NNT by g-computation (`marginal` default TRUE, off with Firth) | `model` (default), `robust` (HC0), `cluster` (HC1) |
| `modpois` | explain-causal | Poisson log-link glm (Zou 2004) | conditional RR; marginal RD, RR, NNT as for logistic | `robust` (default) or `cluster`; `model` is refused |
| `cox` | explain-causal | `Surv(time, status == status_event) ~ exposure + terms [+ strata] [+ cluster]`; ties `efron` (default) or `breslow` | HR (adjusted HR when terms or strata are present) | model-based; robust (cluster) only with `layers$clustered = TRUE` and `cluster` |
| `rmst` | explain-causal | Kaplan-Meier per exposure level up to the pre-specified `tau` | RMST difference vs the reference level, normal interval | none; clustered data refused |

Notes per route:

- **Describe twins.** `lm_describe`, `logistic_describe` and `cox_describe` share compile and present
  with their explain-causal routes but are descriptive: no p-values.
- **Binary outcomes.** A logical or 0/1 outcome (or status) is used as is; any other needs
  `event_value` (`status_event` for a survival status). This is checked against the stored column at
  `sr_sap_add` and at the lock, and again by the emitted code at run time. A CSV column of Yes/No or
  TRUE/FALSE is stored as logical, so its event value, if given, must be `"TRUE"` or `"FALSE"`.
- **Convergence.** A modified Poisson fit that does not converge (for example under complete
  separation) is refused rather than reported. Firth convergence is read from logistf's iteration count.
- **Cox with strata only.** With strata but no other model terms the note says "stratified by <strata>
  (no other model terms)" and the HR is labelled conditional.
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
  (auxiliary columns used only in the imputation model). A categorical auxiliary column with more than
  50 distinct levels is refused at run time; drop it from the imputation model.
- `spec$mi$m` is required.
- Imputation is by `mice` with methods `pmm` (numeric), `logreg` (binary) and `polyreg` (categorical).
  For lm, logistic and modpois the outcome is imputed too and those rows are analysed; for cox and rmst,
  rows with a missing time or status are dropped. Transformations (splines, quadratics, interactions)
  are computed after imputation, in each completed dataset ("impute, then transform").
- Estimates are pooled by Rubin's rules on the estimation scale (log scale for ratios). Where the
  complete-data analysis has finite degrees of freedom (lm) the interval is a t interval on the
  Barnard-Rubin df; otherwise Rubin's original df are reported with a normal interval.
- `m = "auto"`: a pilot of 20 imputations estimates the fraction of missing information (fmi); m is
  then von Hippel's `ceiling(1 + 0.5 * (fmi_upper / 0.05)^2)`, where `fmi_upper` is the upper confidence
  limit of the largest fmi, never below the pilot's 20 and capped at 200.
- Refused: MI with clustered data, and MI with Firth.

## 7. Bootstrap (`layers$resampling = "bootstrap"`)

- Options: `spec$boot$B` (a whole number of at least 20, default 2000) and `spec$boot$type`:
  `percentile` (default), `bca` or `normal`. With `layers$clustered = TRUE` the spec must set
  `boot_type = "cluster"`, which resamples whole clusters (and leaves out whole clusters in the BCa
  jackknife); `pair` and `replicate_weights` are refused at compile.
- The bootstrapped statistic is the route's primary effect as reported: with marginal effects the
  g-computation is re-run in every replicate. Ratio measures are bootstrapped on the log scale.
  Covariate and descriptive rows keep their model-based intervals.
- Replicates run in chunks of 100, each on its own `nextRNGStream`, so results do not depend on the
  number of workers. BCa falls back to percentile when it cannot be computed, and the fallback and its
  reason are recorded. A Monte Carlo SE of the interval limits comes from 10 batches. Seed, B and method
  are recorded with the result.
- MI together with the bootstrap needs `choices$mi_x_boot`:
  - `boot_then_impute`: every bootstrap sample is imputed (`spec$mi$m` times; `"auto"` uses 5) and
    pooled; the estimate is the mean of the B pooled estimates and the interval their percentile
    interval (other interval types are refused);
  - `impute_then_boot`: every completed dataset is bootstrapped (B replicates) and the bootstrap
    variance replaces the model variance of the primary terms in Rubin's rules.
- The coverage of the MI, bootstrap, MI-bootstrap and cluster intervals is checked by simulation; see
  [validation.md](validation.md).

## 8. Diagnostics (`app/R/diagnostics/`)

`sr_diag_checks()` is the catalogue; `sr_diag_applicable()` picks the checks that apply to an analysis.
`sr_diag_run()` runs the applicable checks (or the ones named) of an analysis that has run
successfully, in the isolated worker on the analysis extract, behind the fits cache, and records one
`diag.completed` event per check. Diagnostics fit the analysis model once on the analysis data,
ignoring the bootstrap layer; under MI they run on the complete cases of the analysis columns and say
so. Verdicts (`ok` / `warn` / `fail`) are
effect-size thresholds; a reported statistic such as the `cox.zph` p-value is shown but never feeds the
verdict. A value that cannot be computed is a `warn`.

| check | routes | value | warn | fail | sensitivity node |
|---|---|---|---|---|---|
| `ph` | cox | abs(log HR early - log HR late) of the exposure, split at the median event time (max over the coefficients of a factor exposure; `cox.zph` reported only); carries `tau_suggest`, the 75th centile of event times | >= log 1.25 | >= log 1.5 | rmst at the pre-specified tau, else `tau_suggest` |
| `influence` | lm, logistic, modpois | max Cook's distance | > 0.5 | > 1 | refit with `exclude_rows` |
| `influence` | cox | max absolute scaled dfbeta of the exposure | >= 0.5 | >= 1 | refit with `exclude_rows` |
| `separation` | logistic | refit with maxit 100, epsilon 1e-12: some abs(beta) grows by more than 1 and some fitted p is within 1e-8 of 0 or 1 (1 = yes) | none | >= 1 | Firth |
| `few_clusters` | model routes with a cluster | number of clusters | < 50 | < 30 | cluster bootstrap, B = 2000, percentile |
| `inf_censoring` | cox, rmst | max abs log HR of a Cox model for censoring on the baseline terms (rmst: the exposure) | > log 1.25 | > log 1.5 | none: recorded as a limitation |
| `linearity` | lm, logistic, modpois, cox | max abs(linear - RCS(4) partial prediction) over the 5th-95th centile (outcome SD units for lm, else the linear-predictor scale), worst continuous linear term (interaction terms excluded) | > 0.1 | > 0.25 | that term as RCS(4) |
| `heteroscedasticity` | lm with `se = "model"` | robust (HC3) SE / model SE of the exposure | > 1.2 | > 1.5 | robust SE |

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
- **`sr_diag_apply()`** acts on every `fail` verdict of an analysis's diagnostics (`ok` and `warn`
  verdicts change nothing):
  - if a SAP rule covers the analysis and check, the rule fires (`sap.rule_fired`). `replace_primary`
    makes the pre-specified alternative the primary result and the analysis a sensitivity analysis;
    `add_sensitivity` adds nothing new, because the alternative is already pre-specified;
  - otherwise it adds the check's sensitivity analysis (the mapping in section 8) as
    `<analysis>_<check>`: the analysis part is lower-cased, characters outside `[a-z0-9_]` become `_`,
    an `a` is prefixed if it does not start with a letter, and it is shortened so the whole id stays within 40 characters; an id already held by another
    analysis is refused before anything is written;
  - a check with no sensitivity analysis (`inf_censoring`), or one whose analysis cannot be built, is
    recorded as a limitation (`diag.limitation`), never skipped.

  The primary analysis never changes without a rule. Applying twice changes nothing.

## 10. Deviations and multiplicity

- **`sr_deviations()`** lists every departure from the locked SAP, by kind: `exploratory`, `post_hoc`,
  `unplanned_sensitivity`, `rule_fired` and `lib_changed`.
- **`sr_family_adjust()`** returns the family's adjusted p-values (`analysis`, `term`, `p`, `p_adj`,
  `method`). Each member contributes the primary-role rows of its latest successful run that share the
  measure of its first primary row, so the exposure effect counts once (one row per non-reference
  level) and its marginal re-expressions (RD, RR, NNT) do not. `stats::p.adjust` then adjusts over all
  rows; a p of NA (as under the bootstrap) stays NA and is not counted. A member replaced by a fired
  `replace_primary` rule is represented by its alternative. It is refused for an unknown family or when
  a member has not run successfully.

## 11. Out of scope in P1c

The phase that owns each item is in brackets.

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
