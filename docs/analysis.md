# StatRouter analysis guide (P1c, P1c.1)

This guide covers the analysis layer built in phases P1c and P1c.1: the statistical analysis plan (SAP)
and its lock, the study feasibility verdict that the lock requires, the run engine, the analysis
routes, multiple imputation, the bootstrap, diagnostics, sensitivity nodes, deviations and
multiplicity. The data layer (ingest, store, profiling, data quality) is described in
[data.md](data.md). The validation record (CI-coverage simulation, feasibility SE simulation and
textbook reproductions) is in [validation.md](validation.md).

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
  build if a route's `compile()` body under `routes/` runs, names or branches on a diagnostic or a p-value. A failed diagnostic adds a
  sensitivity node; the primary analysis changes only through a rule written into the locked SAP.
- **Effect sizes, not p-values.** Diagnostic verdicts are effect-size thresholds. Descriptive routes
  (Table 1 and the `*_describe` twins) carry no p-values.
- **Event-sourced.** Every SAP change, feasibility assessment and decision, lock, run and post-lock
  analysis is an event in `events.jsonl`;
  replaying the events gives the same state as the live session.

## 2. The analysis spec

Every analysis is a route id plus a spec (a named list). Every spec carries a `purpose` (`describe`,
`predict` or `explain-causal`) that must match the route's; every model route also requires `layers`
and `seed` (Table 1 requires `seed`, and also a `layers` list because the composition check reads
it). Each route's own fields are in section 6. `sr_spec_validate()`
checks the spec against the route's schema, and `sr_compose_check()` checks that the layers can be
combined (disallowed combinations and unresolved choices such as `choices$mi_x_boot`).

**Layers** (`spec$layers`): all six fields are required.

| layer | values |
|---|---|
| `missing` | `cc` (complete cases) or `mi` (multiple imputation, section 7) |
| `resampling` | `none` or `bootstrap` (section 8) |
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

## 3. The SAP workflow: add, assess, decide, family, rule, lock

The SAP is drafted before any outcome data is seen and locked before anything runs. The order is add,
assess, decide, family, rule, lock. A lock without an assessment and a decision for every primary
analysis is refused (section 4).

1. **Add.** `sr_sap_add()` adds an analysis (id, route, table, label, spec, parent, addresses) or
   replaces the analysis with the same id. The id must match `^[a-z][a-z0-9_]{0,39}$`. It refuses a bad
   route or spec, a disallowed layer combination or unresolved choice, an unknown table or parent, and a
   bad label or family. It also compiles the spec and refuses one that can never run ("cannot run on
   route ..."): a layer P1c cannot run (weighting, matching, Bayesian), invalid MI settings (no `m`,
   `m = 1`, MI with clustered data or Firth) or a bootstrap with B below 20. A binary outcome or survival
   status with no `event_value` / `status_event` is refused unless the stored column is logical or all
   0/1. `sr_sap_remove()` removes an analysis before the lock; it refuses one whose feasibility decision is
   `stop`.
2. **Assess.** `sr_feasibility_assess()` computes the outcome-blind feasibility verdict of an
   analysis and records it (section 4).
3. **Decide.** `sr_feasibility_decide()` records the decision on that verdict: `proceed`,
   `exploratory`, `revise` or `stop` (section 4).
4. **Family.** `sr_sap_family()` groups analyses into a multiplicity family with a p-value adjustment
   method: `holm`, `hochberg`, `hommel`, `bonferroni`, `BH`, `BY` or `none`.
5. **Rule.** `sr_sap_rule()` pre-specifies the response when a check fails for an analysis: show the
   pre-specified alternative as a sensitivity result (`add_sensitivity`) or make it the primary result
   (`replace_primary`). For both actions the alternative must be a sensitivity analysis whose parent is
   that analysis.
6. **Lock.** `sr_sap_check()` lists anything that blocks the lock (including an `event value` row,
   re-checked against the current data, and the `feasibility` rows of section 4); `sr_sap_lock()`
   refuses while any block remains, then records the SAP hash (`sap_hash`), the store hash of every
   table used (`tables$store_hash`), the library snapshot hash (`libsnap_hash`) and `locked_at`. Every
   run requires the lock, and `sr_run()` refuses a project with a stopped analysis. Amending a locked SAP is a later phase (section 12).

## 4. Study feasibility (before the lock)

Before the SAP is locked, each primary analysis gets a feasibility verdict: can this dataset estimate
the analysis precisely enough to be worth running? The verdict is the expected precision of the
estimate, never the expected result. Choosing whether to analyse from an expected effect is outcome
selection, and power from an observed effect stays refused. The code is in `app/R/sap/feas_engine.R`
(the pure engine) and `app/R/sap/feasibility.R` (the events, decisions and the lock gate).

### What is blind, and why

The verdict is computed from what the project can already show while blind:

- the analysed n, after the complete-case filter over the columns the analysis model uses (outcome,
  time, status, exposure, term variables, cluster, strata). This is the filter the compiled run
  applies, so the n is the n the run fits. A cluster column is always one the run uses: cox fits
  `cluster()`, and lm and glm refuse a cluster column (or `layers$clustered = TRUE`) without
  `se = "cluster"`. Only a `strata` or `cluster` field that the route does not take (strata on lm or
  glm, either on rmst) is ignored by the run and still drops rows here, which is conservative.
  An `mi$aux` column never drops a row. `exclude_rows` is ignored, see below;
- the outcome's own marginals: the marginal risk (binary), the marginal SD (continuous), and the
  total events and the pooled, exposure-blind (time, event) pairs (survival);
- the exposure's marginals and the exposure-on-covariate structure: its levels and counts, the R² of
  the exposure on the planned covariates, the overlap of the propensity (near-positivity) and
  separation;
- cluster counts and sizes, and the number of model parameters of the compiled design (the engine
  evaluates the route's own term builder, so the parameters and the exposure model are those that
  will run).

It never computes an outcome-by-exposure or outcome-by-covariate statistic. The outcome marginals are
taken over all rows with an observed outcome, not only the complete cases. Counting the events among
the complete cases would cross the outcome with the missingness of the other columns, which is an
outcome-by-covariate statistic. The expected events among the complete cases are therefore scaled
(total events x n of complete cases / n with an observed outcome), never counted.

Two parts of a spec would let the outcome reach the exposure model, so the engine handles them apart:

- **`exclude_rows` is ignored.** Its positions refer to the extract, which is sorted by every spec
  column with the outcome first, so the rows it drops depend on the outcome values. The verdict is
  computed on every row, and a non-empty `exclude_rows` gives the warning "exclude_rows is ignored by
  the feasibility verdict (it is an influence sensitivity for the locked run)". It stays part of the
  spec, so changing it still makes the assessment stale.
- **An outcome column used as a covariate is blocked.** When the outcome (or, for a survival outcome,
  the time or status column) is also a term, an interaction variable, a stratum or the cluster, it is
  left out of the exposure model and the verdict is `cannot_run` with the blocker "design: the outcome
  column '<c>' cannot be a term, stratum or cluster".

A test permutes the outcome among its non-missing rows and checks that the stored assessment does not
change, byte for byte. It runs with and without `exclude_rows`, and with the outcome used as a term,
an interaction variable, a stratum and the cluster.

Two quantities need an outcome-by-covariate statistic: the outcome R² explained by the covariates
(continuous outcome) and the intra-cluster correlation (ICC). They are never estimated from the data.
You enter each as an assumption with a written justification, or the verdict uses the marginal value,
which is conservative (wider than the eventual adjusted interval). A clustered analysis must state an
ICC, because no conservative marginal value exists.

### Assess

```r
sr_feasibility_assess(project, analysis_id,
                      delta = list(value = , measure = , source = ),
                      assumptions = list(r2_y = list(value = , justification = ),
                                         icc  = list(value = , justification = )))
```

It returns the project with a `feasibility.assessed` event appended. Both `delta` and `assumptions`
are optional (an ICC is required for a clustered analysis).

- **`delta`** is the smallest clinically important difference on the scale of the estimand, with its
  source (an MCID from the literature, a guideline, a trial convention). `measure` must be one the
  analysis reports. A difference measure takes an absolute value above 0. A ratio measure takes a
  ratio above 0 and not 1; the effect is taken as |log(delta)|. Without a delta the verdict reports
  precision only and says so.
- **`assumptions$r2_y`** (a number in [0, 1)) applies to a continuous outcome only. **`assumptions$icc`**
  (a number in [0, 1)) applies to a clustered analysis only. Each needs a non-empty justification.
  An assumption given as `NULL` counts as not given. Without `r2_y`, the MD row uses `r2_y = 0` and
  the verdict says so in a warning. Without `icc`, a clustered analysis is refused.

One example per measure (the route that reports it is in the comment):

```r
# MD (lm): 5 mmHg, with an assumed outcome R2 explained by the covariates
p <- sr_feasibility_assess(p, "bp",
  delta = list(value = 5, measure = "MD", source = "difference the clinical team agreed matters"),
  assumptions = list(r2_y = list(value = 0.20, justification = "similar models in prior cohort work")))

# RD (logistic or modpois with marginal effects): 2 percentage points
p <- sr_feasibility_assess(p, "mortality",
  delta = list(value = 0.02, measure = "RD", source = "guideline threshold for a worthwhile change"))

# RR (modpois, or logistic with marginal effects): a 20% relative reduction
p <- sr_feasibility_assess(p, "readmit_rr",
  delta = list(value = 0.80, measure = "RR", source = "trial convention for this endpoint"))

# OR (logistic): clustered by ward, so an ICC is required
p <- sr_feasibility_assess(p, "infection",
  delta = list(value = 0.70, measure = "OR", source = "minimum odds ratio the unit would act on"),
  assumptions = list(icc = list(value = 0.05, justification = "ICC reported for infection by ward in a prior audit")))

# HR (cox): total events are what matters
p <- sr_feasibility_assess(p, "survival",
  delta = list(value = 0.80, measure = "HR", source = "hazard ratio the steering group set as important"))

# RMST_diff (rmst): in the unit of the time column
p <- sr_feasibility_assess(p, "rmst_36m",
  delta = list(value = 3, measure = "RMST_diff", source = "3 months of event-free time at tau, from the protocol"))
```

The assessment is tied to the data and the spec it was made on (the store hash of the table and a
hash of the route, version, table and spec). If either changes, the assessment is stale.

### The formulas

The engine computes the approximate standard error of each (contrast x measure) under no effect.
Every number comes from the formula applied to the inputs above; nothing is fitted to the outcome.

- `x` is the exposure. `v = n * Var(x) * (1 - R2x)`, where `n` is the number of complete cases, `Var(x)`
  is `p(1 - p)` for a binary exposure and the variance for a continuous one, and `R2x` is the R² of the
  exposure on the planned covariates. For a factor exposure each contrast against the reference has
  `v = (1 - R2x) / (1 / n_k + 1 / n_ref)`.
- `pi` is the marginal risk and `sigma_y` the marginal SD of the outcome.

| measure | standard error | scale |
|---|---|---|
| MD | `sigma_y * sqrt(1 - R2y) / sqrt(v)`, with `R2y` the assumed value (default 0) | identity |
| RD | `sqrt(pi * (1 - pi) / v)` | identity |
| RR | `sqrt((1 - pi) / (pi * v))` | log |
| OR | `1 / sqrt(v * pi * (1 - pi))` | log |
| HR | Schoenfeld: `1 / sqrt(D * Var(x) * (1 - R2x))`, where `D` is the expected events among the complete cases (for a contrast, `Var(x)` is `p_k (1 - p_k)` and `D` is scaled to the two groups) | log |
| RMST_diff | simulation: draw both groups with replacement from the pooled (time, event) pairs at the observed group sizes, take each group's RMST up to `tau`, and take the SD of the difference over 500 draws (`sr.feas_rmst_sims`, environment `SR_FEAS_RMST_SIMS`). A draw that has no RMST is dropped. The number of draws and the seed used are stored with the assessment (`inputs$rmst = list(sims, seed)`). | identity |

- **Clustering.** A clustered analysis multiplies the variance by the design effect
  `1 + ((cv^2 + 1) * m - 1) * ICC` (Eldridge 2006), where `m` is the mean cluster size and `cv` the
  coefficient of variation of the cluster sizes.
- **Missing data.** The verdict is computed at the complete-case n. Under multiple imputation the
  report also gives `se_upper`, the SE at n with an observed outcome, so the range from the
  complete-case end to the other end is visible. The tier uses the complete-case end.
- **Results.** The 95% CI half-width is `h = 1.96 * SE` and the minimum detectable effect at 80% power
  is `MDE = 2.80 * SE`. Both are on the reporting scale, so ratio measures are exponentiated.
- **What comes from the complete cases.** The design the engine builds (the parameter count and the
  exposure model) uses factor levels and RCS knots computed on the complete cases. The compiled glm,
  lm and cox code computes them on every row of the table (rmst: on its complete cases). So:
  - an exposure level seen only in incomplete rows is the blocker "exposure level '<l>' is observed
    but has no complete-case row";
  - a level of a factor term seen only in incomplete rows is left out of the engine's design, which
    then counts one parameter fewer than the run (whose column for that level is all zero);
  - RCS knots can differ slightly. That changes R2x a little, not the parameter count.
- **The rmst `tau` check is pooled.** The blocker compares `tau` with the longest follow-up of the
  pooled, exposure-blind (time, event) pairs, so it cannot see a `tau` beyond one group's follow-up
  only. The compiled run refuses that case ("tau = ... exceeds the follow-up of group ...").

The predicted SEs are checked against the spread of the estimates the analysis actually produces,
over 1000 simulated datasets per cell, in [validation.md](validation.md) (section 3). All 11 cells
(each of MD, RD, RR, OR, HR and RMST, with and without clustering; clustered RMST is refused by the
route and has no cell) are within 10%. On NHEFS the predicted SE of the marginal MD, from the marginal
values with no assumptions, is 0.472 against 0.463 actually obtained.

### Tiers, blockers and warnings

With a `delta`, the rows of the delta's measure are tiered. With `d = |log(delta)|` for a ratio and
`d = delta` for a difference:

| tier | rule | advice |
|---|---|---|
| `adequate` | `2.80 * SE <= d` (the MDE is no larger than the effect that matters) | proceed |
| `limited` | `1.96 * SE <= d < 2.80 * SE`: the 95% CI half-width is at most `d`, so an estimate near the null would exclude an effect that large, but the power to detect it is between 50% and 80% | proceed, stating the precision (the half-width and the MDE) in the Methods |
| `inadequate` | `1.96 * SE > d`: the study can neither reliably detect nor rule out the effect | revise, stop, or exploratory |
| `precision_only` | no `delta`, or a row of another measure than the delta's | proceed |
| `not_applicable` | the route reports no effect measure (Table 1 and other descriptive or unknown routes) | proceed |
| `cannot_run` | at least one blocker | revise or stop |

The overall tier is the worst tier among the rows of the delta's measure. Rows of the other measures
(for example RR and OR when the delta is on RD) are `precision_only`. A Table 1 or unknown route is
`not_applicable`, but a primary one still needs a decision (`proceed`) before the lock.

**Blockers** make the tier `cannot_run` whatever the delta is:

- no complete-case row; the analysis has no single exposure column;
- a continuous exposure that does not vary (`Var(x) == 0`); a continuous exposure has no level blocker;
- a categorical exposure with fewer than two levels that have a complete-case row, or a level that is
  observed but has no complete-case row;
- a constant outcome: no events (or every row an event) for a binary outcome, no events for a
  survival outcome, an SD of 0 for a continuous outcome. An `event_value` that codes no row shows up
  here as "no events", now before the lock rather than at the run;
- at least as many model parameters as complete-case rows (continuous outcome: "the model has <p>
  parameters but only <n> complete-case rows (it needs more rows than parameters)"), or more
  parameters than the expected events (survival) or the smaller of the expected events and
  non-events (binary): "<p> model parameters exceed the <e> expected events among the complete
  cases". On rmst, which fits no regression, the count is of exposure groups: "the <p> exposure
  groups exceed the <e> expected events among the complete cases";
- a clustered analysis whose cluster column has fewer than two distinct clusters among the complete
  cases: "design: the cluster column '<c>' has fewer than 2 clusters";
- an exposure that the covariates predict perfectly (a positivity violation);
- an rmst `tau` beyond the longest follow-up;
- an outcome column (outcome, time or status) used as a term, an interaction variable, a stratum or
  the cluster: "design: the outcome column '<c>' cannot be a term, stratum or cluster";
- an outcome that cannot be coded or a design that cannot be built (for example a spec column missing
  from the data), given as "outcome: ..." or "design: ...";
- a precision that could not be computed.

**Warnings** are shown and never pass or fail:

- near-positivity: more than 5% of the rows have a propensity outside [0.01, 0.99];
- fewer than 10 events per model parameter (sparse-data bias); for a logistic analysis the warning
  offers a pre-specified Firth penalty. An rmst analysis fits no regression, so its warning is about
  fewer than 10 expected events per exposure group, which make the Kaplan-Meier estimates behind the
  RMST imprecise;
- a continuous outcome assessed without `r2_y`: the MD standard error uses `r2_y = 0`
  (conservative), and the warning says so;
- fewer than 30 clusters, where cluster-robust SEs are unreliable;
- a non-empty `exclude_rows`, which the verdict ignores (it is an influence sensitivity for the locked
  run).

### Decide

```r
sr_feasibility_decide(project, analysis_id, decision, reason)
```

`decision` is one of:

- `proceed`: the analysis stays primary.
- `exploratory`: the analysis is labelled exploratory. The decision is stored and appears in the report;
  the writer (P1d) will require hypothesis-generating language for it.
- `revise`: go back and change the design before the lock (fewer terms, pooled sparse levels, a
  different estimand, a composite outcome, a longer horizon). A revise decision blocks the lock.
  Assessing again clears it. Revisions are outcome-blind and are not deviations, because nothing is
  locked yet.
- `stop`: the project is stopped with the reason.

A `reason` is required for `exploratory`, `revise` and `stop`, for any decision that replaces a stop,
and for `proceed` when the tier is `inadequate` or `precision_only`. `proceed` and `exploratory` are
refused when the tier is `cannot_run`. A decision needs a current assessment: with none, or with a stale
one (the data or the spec changed since it was made), the call is refused and you assess again. The
one exception is replacing a stop, which is allowed on a stale assessment (with a reason).

### Stop

A stop is sticky. While an analysis carries one:

- it cannot be assessed again, and `sr_sap_remove()` refuses it;
- `sr_run()` refuses the whole project ("the project is stopped: ...");
- the lock is blocked.

Reverse it with `sr_feasibility_decide()` and a reason, which replaces the stop with another decision.
A stop on any pre-specified analysis, a secondary one included, blocks the lock.

### The lock gate

`sr_sap_check()` adds one `feasibility` row per problem, and `sr_sap_lock()` refuses while any is a
block. The rows are, per analysis:

| row | applies to |
|---|---|
| `<id>: no feasibility assessment` | primary |
| `<id>: the feasibility assessment is stale (the data changed)` or `(the spec changed)` | primary |
| `<id>: no feasibility decision` | primary |
| `<id>: decision is revise: change the design, assess and decide again` | primary |
| `<id>: decision is stop (<reason>)` | any pre-specified analysis |

A secondary analysis can be assessed but is never gated, other than by a stop. When nothing blocks,
the row reads "every primary analysis has a current feasibility decision".

### Frozen at the lock

The feasibility record (assessment and decision) of every pre-specified analysis is part of the SAP
hash, so changing a verdict afterwards would change the hash. The element is left out of the hash when
there is none, so a SAP locked before P1c.1 keeps its hash. After the lock, `sr_feasibility_assess()`
and `sr_feasibility_decide()` are refused with "the SAP is locked: the feasibility verdict is frozen",
and the verdict is never recomputed: `sr_feasibility()` and the report show it as recorded.

### Report and Methods inputs

- `sr_feasibility(project, analysis_id)` returns `list(assessment, decision, stale)`.
- `sr_feasibility_report(project)` returns one row per pre-specified, assessed analysis and contrast
  and measure: `analysis`, `label`, `route`, `contrast`, `measure`, `n_cc`, `events` (expected events
  among the complete cases), `prevalence`, `se`, `h`, `mde`, `se_upper`, `delta`, `delta_measure`,
  `delta_source`, `tier`, `decision`, `reason`, `stale`, `blockers` and `warnings`. `prevalence` is the
  exposure prevalence among the complete cases (`n_k / n_cc` for the row's level), and NA for a
  continuous exposure.
- `sr_feasibility_fragment(project, analysis_id)` returns the inputs of the Methods sentence, as named
  values and a template id, not prose: `n`, `events`, `prevalence` (a named list of each level's
  share of the complete cases), `h`, `mde`, `delta`, `delta_source` and `tier`. `h` and `mde` are those
  of the least precise contrast (the largest MDE) of the delta's measure, or of the first measure when
  there is no delta. The prose is written in P1d.

## 5. The run engine (`app/R/routes/run.R`)

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

## 6. Routes

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
| `lm` | explain-causal | `outcome ~ exposure + terms (+ interactions)`, complete cases | MD (adjusted mean difference); with `marginal = TRUE` the g-computation marginal MD | `model` (default), `robust` (HC3), `cluster` (HC1, t on min(residual df, G - 1) df); `cluster` needs `layers$clustered = TRUE` and `cluster`, and either of those needs `se = "cluster"` |
| `logistic` | explain-causal | binomial logit glm, or Firth (`penalty = "firth"`, logistf) | conditional OR; marginal RD, RR and NNT by g-computation (`marginal` default TRUE, off with Firth) | `model` (default), `robust` (HC0), `cluster` (HC1); clustering as for lm |
| `modpois` | explain-causal | Poisson log-link glm (Zou 2004) | conditional RR; marginal RD, RR, NNT as for logistic | `robust` (default) or `cluster`; `model` is refused; clustering as for lm |
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

## 7. Multiple imputation (`layers$missing = "mi"`)

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

## 8. Bootstrap (`layers$resampling = "bootstrap"`)

- Options: `spec$boot$B` (a whole number of at least 20, default 2000) and `spec$boot$type`:
  `percentile` (default), `bca` or `normal`. With `layers$clustered = TRUE` the spec must set
  `boot_type = "cluster"`, which resamples whole clusters (and leaves out whole clusters in the BCa
  jackknife), and lm and glm also `se = "cluster"` (section 6); `pair` and `replicate_weights` are
  refused at compile.
- The bootstrapped statistic is the route's primary effect as reported: with marginal effects the
  g-computation is re-run in every replicate. Ratio measures are bootstrapped on the log scale.
  Covariate and descriptive rows keep their analytic intervals (cluster-robust for clustered data).
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

## 9. Diagnostics (`app/R/diagnostics/`)

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
- The Firth node of a clustered logistic analysis is unclustered (Firth has model-based intervals
  only), and a cluster `boot_type` is not carried over; the node's note says so.
- `inf_censoring` screens covariate-dependent censoring. Censoring that is informative given the
  covariates cannot be tested; IPCW is a later phase.
- Figures (`app/R/diagnostics/plots.R`) are ggplot objects exported deterministically with ragg;
  plot data are capped at 5000 points by a seeded sample.

## 10. After the lock: sensitivity nodes and post-lock analyses

- **`sr_analysis_add()`** adds an analysis after the lock (an exploratory, post hoc or unplanned
  sensitivity analysis). It needs a reason, an id matching `^[a-z][a-z0-9_]{0,39}$`, and the spec is
  validated and compiled at add time.
- **`sr_diag_apply()`** acts on every `fail` verdict of an analysis's diagnostics (`ok` and `warn`
  verdicts change nothing):
  - if a SAP rule covers the analysis and check, the rule fires (`sap.rule_fired`). `replace_primary`
    makes the pre-specified alternative the primary result and the analysis a sensitivity analysis;
    `add_sensitivity` adds nothing new, because the alternative is already pre-specified;
  - otherwise it adds the check's sensitivity analysis (the mapping in section 9) as
    `<analysis>_<check>`: the analysis part is lower-cased, characters outside `[a-z0-9_]` become `_`,
    an `a` is prefixed if it does not start with a letter, and it is shortened so the whole id stays within 40 characters; an id already held by another
    analysis is refused before anything is written;
  - a check with no sensitivity analysis (`inf_censoring`), or one whose analysis cannot be built, is
    recorded as a limitation (`diag.limitation`), never skipped.

  The primary analysis never changes without a rule. Applying twice changes nothing.

## 11. Deviations and multiplicity

- **`sr_deviations()`** lists every departure from the locked SAP, by kind: `exploratory`, `post_hoc`,
  `unplanned_sensitivity`, `rule_fired` and `lib_changed`.
- **`sr_family_adjust()`** returns the family's adjusted p-values (`analysis`, `term`, `p`, `p_adj`,
  `method`). Each member contributes the primary-role rows of its latest successful run that share the
  measure of its first primary row, so the exposure effect counts once (one row per non-reference
  level) and its marginal re-expressions (RD, RR, NNT) do not. `stats::p.adjust` then adjusts over all
  rows; a p of NA (as under the bootstrap) stays NA and is not counted. A member replaced by a fired
  `replace_primary` rule is represented by its alternative. It is refused for an unknown family or when
  a member has not run successfully.

## 12. Out of scope in P1c and P1c.1

The phase that owns each item is in brackets.

- The feasibility panel and the feasibility-report docx [P1d]. P1c.1 returns the report as a data
  frame and the Methods inputs as a fragment. The Methods sentence from the fragment, and the
  claim-guard rule that an exploratory analysis needs hypothesis-generating language [P1d writer].
  Plain-language guided wording [P4].
- The Riley (pmsampsize) criteria for prediction routes [P6]. No prediction route exists yet; the
  engine marks a route it does not know as `not_applicable`.
- Clustered RMST. The route refuses it, so it has no feasibility cell.
- The SAP_v1.docx document and the lock UI: done in the SAP panel (`app/R/ui/mod_sap.R`, P1d UI sub-project 1). P1c owns the `sap.*` events, the lock and its
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

## 13. Manuscript writer (P1d.1)

The writer turns a locked, run project into the manuscript, the supplement, the figures, the reference
list, the STROBE checklist and the analysis script that re-runs the results. It never computes a number.
The guide is [writer.md](writer.md): the export files, the placeholder grammar, the claim guard (G1-G9),
small-cell suppression, the journal presets, how to re-run `analysis.R`, and the known limitations.

## 14. The Analyses panel (P1d UI sub-project 2)

The Analyses tab (`app/R/ui/mod_analyses.R`, with the worker in `analyses_job.R` and the table builders in
`analyses_view.R`) runs the locked SAP's analyses and shows their results. It sits after SAP and is hidden
until a project is open. It holds five cards. Every card is derived from the replayed project
(`events.jsonl` stays authoritative); the only session state about a run is the live job handle.

### Running

- **Card 1, Runs.** One row per analysis in SAP order (primary, secondary, then sensitivity analyses
  indented under their parent): id, label, route, status (`not run`, `running`, `ok`, `failed`,
  `cancelled`, `interrupted`), last run time, elapsed, cached, diagnostics summary.
  **Run selected** re-runs the chosen analysis (also an `ok` one; the fits cache makes that fast).
  **Run all** queues every analysis whose status is not `ok`, in SAP order. With no row selected, Run
  selected says "select an analysis first" and writes nothing.
- Run is enabled only when the SAP is locked, the project is not read-only, the app is not busy and the
  project is not stopped. The server re-checks each condition and refuses with the core or busy text
  verbatim; the buttons are also rendered disabled.
- **The queue and the worker.** For each id the panel releases the project lock (closing DuckDB, then
  the project, and holding a read-only copy) and starts a `mirai` worker through a Shiny `ExtendedTask`.
  The worker sources the app, opens the project, writes its job file, calls `sr_run(job = job)` and, when
  the run is `ok`, `sr_diag_run`, then closes the project. Back in the session the panel re-opens the
  project (re-taking the lock) and starts the next id. The worker is an ephemeral mirai process: no
  `mirai::daemons()` are set, so nothing is reused and a killed worker is simply gone.
- A failed fit is recorded (`ok = FALSE`) and the queue continues; a sensitivity analysis whose parent
  failed still runs. A refusal stops the queue (see the table).
- The run continues while you are on another tab. Closing the browser cancels nothing: the worker
  finishes and records its events. A job that settles while a different project is open is settled on a
  temporary handle to its own folder (the notice ends " (in <project name>)") and the open project is
  never replaced.
- While a queue is running the tab shows a `running` badge. The app keeps one busy flag per job module (the busy hub in `app/app.R`). The Analyses module
  receives `busy_for("analyses")`, which is every flag except its own, so its own `running()` never blocks
  its Run buttons; every other panel receives `busy_for()` (any job). An import, a Design save or an
  SAP edit is refused during a run with `.sr_ui_busy_text` ("wait for the running import, analysis, archive
  operation or project import to finish"), and the Data Import button is disabled.

### Worker protocol, Cancel and interrupted

- Three temp files per job: a progress file (`fitting` / `diagnostics`, read for the progress line), a
  job file and a cancel file. The job file holds `"<pid> <process create time>"`. It is written to
  `<job file>.part` and renamed into place, so a reader never sees half a line; the panel removes the
  `.part` file with the other three when it cleans up. If the cancel file exists when the worker starts,
  it returns `skipped` before fitting.
- **Cancel** empties the queue, creates the cancel file and, only while the job's mirai is still
  unresolved, calls `.sr_an_kill`. `.sr_an_kill` waits (up to 10 s) for a parseable job file, refuses to
  kill when the PID is gone or its create time differs by more than 0.01 s (a reused PID), then, in this
  order: **suspends the worker, snapshots and kills its descendants (the callr fit), then kills the
  worker**. The suspend closes the race in which a worker could start a new child between the snapshot and
  the kill. It then waits until no process runs. If a process survives, the survivors are resumed (so a
  suspended worker never keeps the lock) and `.sr_an_kill` raises; the panel shows "Cancel: <message>" and
  does not re-take the project until the mirai resolves. If no worker is found it says so, and the job is
  settled when it ends. Cancel blocks the session for up to the kill wait.
- **Settling a cancel** (`.sr_an_settle`) re-takes the project and calls `sr_run_cancel` for the job. The
  outcomes: the kill stopped the fit, so `run.cancelled` is written ("Cancelled <id>."); the run completed
  first, so `run.completed` wins, `sr_run_cancel` is refused as stale, nothing is written and the notice is
  "<id> finished before it could be cancelled."; the worker was killed before `run.started` was written, so
  "Cancelled before <id> started.". Cancelling while diagnostics are running leaves the fit standing: the
  fit's `run.completed` is already recorded, and only the diagnostics are lost. If a cancel meets a
  `refused` result the notice appends "The worker reported: <message>".
- **Interrupted is derived, not an event.** An analysis whose latest run event is `run.started` is
  `running`; when no live job in this session owns that `job` id it is shown as `interrupted` (the R
  session died, the machine lost power, or the worker was killed outside the panel). Run selected then
  appends a new `run.started`, so the crashed attempt stays in the log. `run.completed` carries the `job`
  id (as `run.started` and `run.cancelled` do), so a result can be tied to its attempt.
- A failed or cancelled attempt never removes an earlier good result; Card 2 keeps showing the last good
  run, labelled with its time.

### The five cards

1. **Runs** (above), with the progress line "Running i of n: <id>, <step> (<elapsed>)".
2. **Result of the selected analysis**, read through `sr_result()`: a header (status, elapsed, cached or
   fresh, a warning when the library snapshot differs from the lock's, a failed run's error text
   verbatim), the effects table (primary rows first), the route's tables, its figures (drawn with the
   diagnostics plotting helpers), its fragments (prose with `{{E1}}`-style placeholders shown as tokens),
   and one diagnostics row per check (verdict, value, warn and fail thresholds, detail, plus a figure when
   the check has plot data). Below that, the **Apply diagnostics** preview: the rows of
   `sr_diag_apply_plan` in words (rule to fire, sensitivity to add, limitation to record). The button is
   gated like Run and executes `sr_diag_apply`, which runs exactly the previewed rows. An analysis it adds
   appears in Card 1 as `not run`. A plan with no rows says "Nothing to apply."
3. **Primary vs sensitivity**: one block per primary or secondary analysis, with its primary-role
   estimate and the same-measure rows of each sensitivity analysis. After a fired `replace_primary` rule
   the alternative is marked "now primary" and the replaced analysis is listed as a sensitivity row with
   status `replaced`. A sensitivity analysis that has not run shows its status, never a blank.
4. **Multiplicity**: one table per SAP family, equal to `sr_family_adjust` (p to three and p_adj to four significant
   digits). A family with a member that has not run successfully shows "Waiting for: a, b".
5. **Deviations and run history**: the `sr_deviations` table, then every cancelled and interrupted attempt
   (analysis, job, started, ended or `interrupted`, reason). Cancelled and interrupted attempts are run
   history, not SAP deviations.

Cards read only the event state and small cached RDS files; none re-runs `sr_sap_check` or scans DuckDB, so
nothing scales with the row count. Values are held in value-compared reactives, so an unrelated project
change does not redraw a card.

### Failure table

| Condition | Behaviour |
|---|---|
| Cancel races completion | `run.completed` wins; `sr_run_cancel` is refused as stale; no `run.cancelled`; a notice says it finished. |
| Cancel before the worker's run started | "Cancelled before <id> started."; `sr_run_cancel` is refused as stale, so nothing is written (no `run.cancelled`). |
| Kill cannot stop a process | Survivors are resumed; the error is shown; the project is not re-taken until the mirai resolves. |
| Browser closed mid-run | The worker finishes and records; reopening shows the result. |
| R process or machine dies | Reopening shows `interrupted`; Run selected completes it. |
| Lock handover race | The worker reports "the project is locked by another session"; shown as a refusal; the queue stops. |
| `sr_run` or `sr_diag_run` refusal | The queue stops; the core message is shown verbatim ("diagnostics: " prefixes a diagnostics refusal). |
| Fit fails | Recorded `ok = FALSE`; the queue continues; the error is shown in Card 2. |
| Worker error outside the core | A `refused` with "worker error: ..." or "worker stopped unexpectedly (mirai error <code>)"; the queue stops. |
| Import, Design or SAP edit during a run | Refused with `.sr_ui_busy_text` ("wait for the running import, analysis, archive operation or project import to finish"). |
