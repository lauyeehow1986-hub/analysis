# P1c.1 Study Feasibility Verdict Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Before the SAP is locked, answer "can this dataset estimate each primary analysis precisely
enough to be worth running?" from outcome-blind inputs only. Record the answer as an event, take the
user's decision (proceed / proceed as exploratory / revise / stop), and refuse the lock until every
primary analysis has a current, non-stop decision.

**Architecture:** Two new headless files under `app/R/sap/`.
- **The pure engine (`feas_engine.R`).** Data frame + spec + route → blind inputs → SEs, h, MDE,
  blockers, warnings and a tier.
  - It reads the table the way the run extract does.
  - It evaluates the route's own term-builder code (`sr_terms_code`, checked by `.sr_emit_check`)
    in-process, so the parameter count and the exposure model match what will run.
  - It never computes a statistic that crosses the outcome with another column.
- **The workflow (`feasibility.R`).** The `feasibility.assessed` / `feasibility.decided` events, the
  assess / decide API, the report and fragment inputs, and the gate rows used by `sr_sap_check`.
- **Edits to existing code.**
  - `sr_sap_hash` freezes the verdicts.
  - `sr_run` and `sr_sap_remove` honour a stop.
  - A simulation harness in `resample/` checks the predicted SEs against empirical SDs.

**Tech Stack:** R 4.5.2; stats, survival (pooled KM / RMST via `summary(survfit, rmean = tau)`),
Hmisc (through the existing term builder), openssl, testthat 3e. **No new packages.**

**Spec:** `docs/specs/2026-10-03-statrouter-design.md`. Relevant sections:
- catalogue **O** (Study feasibility verdict);
- "Archive, SAP lock & reviewer mode" → SAP lock (the lock requires the verdict);
- Phases → **P1c.1**;
- Verification → "Feasibility verdict (catalogue O)".

**Builds on (P1c, on `main` at 33a9201):**
- SAP: `sr_sap_add`, `sr_sap_check(project) -> data.frame(item, status, detail)`, `sr_sap_lock`,
  `sr_sap_hash`, `sr_sap_remove`, `.sr_sap_analyses(project)`, `.sr_sap_get(project, id)`,
  `.sr_sap_open(project, msg)`, `.sr_sap_q`, `.sr_sap_chr1`, `.sr_sap_chrv`, `.sr_mem_method`.
- Routes: `sr_route_get(id, version)`, `sr_spec_validate`, `sr_spec_cols(spec)`,
  `sr_terms_code(spec) -> list(setup, design, rhs, coef_map)`, `sr_outcome_code`,
  `.sr_exclude_rows(spec)`, `.sr_glm_marg_on(spec)`, `.sr_emit_check(code, "analysis")`.
- Run: `sr_extract(project, table, cols)` (ORDER BY every column), `sr_run`.
- Data: `sr_store_manifest`, `sr_store_hash`, `sr_store_view`, `sr_duck`, `sr_duck_query`,
  `sr_blind`, `.sr_assert_blind(x, outcome_cols, other_cols)`, `SR_OUTCOME_FAMILY_ROLES`.
- Hash / exec: `sr_canonical_json`, `.sr_sha256`, `sr_streams(seed, n)`.
- Events: `sr_reducer_register`, `sr_project_append`, `sr_project_open` (replay).
- Simulation plumbing: `resample/coverage.R` (`.sr_cov_layers`, the in-process route evaluation of
  `.sr_cov_rep`, `.sr_cov_serial` / `.sr_cov_parallel`, `sr_streams`).
- Tests: `.fx_ready_project(df, table, roles, env)`, `helper-spy.R`.

**Out of scope here** (each is a ruling; the owning phase is in brackets):
- The feasibility panel UI [P1d UI]. The feasibility-report docx [P1d writer]. The Methods sentence
  from the fragment inputs, and the claim-guard rule that exploratory analyses need
  hypothesis-generating language [P1d writer]. Plain-language guided wording [P4].
- Riley / pmsampsize criteria for prediction routes [P6]. No prediction route exists yet. The engine
  marks a route it does not know as `not_applicable`.
- Clustered RMST. The route refuses it (ruling R23b), so there is no clustered RMST feasibility cell.

## Global Constraints
- **All P1a/P1b/P1c Global Constraints still apply.**
  - Naming: `sr_` prefix for exported functions, `.sr_` for internal ones.
  - Files: ASCII-only `.R` with LF line endings; never multi-line `Rscript -e`.
  - Data and output: synthetic or public package data only; tests write under
    `withr::local_tempdir()`; JSON via `.sr_json`; no column named like a p-value.
  - Emitted code: quoted via `.sr_q` / `.sr_lit`, never pasted, and checked by `.sr_emit_check`
    before it is evaluated.
- Every new `app/R/**` file is appended to `.sr_core_files` in `app/global.R`:
  - `"sap/feas_engine"` and `"sap/feasibility"` after `"sap/deviations"`;
  - `"resample/feas_sim"` after `"resample/coverage"`.
- **Command prefix:** `RS="/c/Program Files/R/R-4.5.2/bin/Rscript.exe"`.
  - One test file: `"$RS" tools/run_tests.R <filter>`.
  - Whole suite: `"$RS" tools/run_tests.R`.
  - Lint: `"$RS" tools/lint.R` → `LINT OK (N files)`.
- **The public repo carries no personal details.** No usernames, emails, hostnames or absolute user
  paths in any committed file. NHEFS is never committed. It is read from env `SR_NHEFS_CSV`, and its
  test skips when that is unset.
- **Air-gap.** No network call anywhere.
- **Outcome-blind by construction** (spec O). The engine may use:
  - the analysed n;
  - the outcome's own marginals, computed over **every row with a non-missing outcome**, never only
    the complete cases;
  - exposure marginals and exposure-on-covariate structure;
  - cluster sizes;
  - the parameter count.

  It never uses an outcome-by-exposure or outcome-by-covariate statistic. Expected events among the
  complete cases are `events_total × n_cc / n_obs`, not a count.
- **Byte-identical under outcome permutation.**
  - Every computation runs on canonically ordered data: the non-missing outcome values (survival:
    `(time, event)` pairs) sorted, and the complete-case covariate frame sorted by all its columns.
    No result can depend on the table's row order.
  - The RMST simulation samples from the sorted pairs.
- **Seeds.** The RMST simulation draws from stream 1 of `sr_streams(spec$seed, 1)` and restores the
  caller's RNG kind and `.Random.seed`.
- **Constants** (in `feas_engine.R`):
  - `SR_FEAS_Z_CI <- 1.96`, `SR_FEAS_Z_MDE <- 2.80`;
  - `SR_FEAS_PS_RANGE <- c(0.01, 0.99)`, `SR_FEAS_PS_SHARE <- 0.05`;
  - `SR_FEAS_EPV_WARN <- 10`, `SR_FEAS_MIN_CLUSTERS <- 30`;
  - `SR_FEAS_SEP_EPS <- 1e-8`, `SR_FEAS_R2X_MAX <- 0.999`.
- **Option.** RMST simulations: `getOption("sr.feas_rmst_sims", Sys.getenv("SR_FEAS_RMST_SIMS", 500))`.
- **Event types added by P1c.1**, both with reducers in `sap/feasibility.R`: `feasibility.assessed`,
  `feasibility.decided`.
- **State key:** `state$feasibility[[analysis_id]] = list(assessment, decision)`.
  - `feasibility.assessed` replaces `assessment` and sets `decision <- NULL`.
  - `feasibility.decided` sets `decision = list(decision, reason)`.
- **Vocabularies:**
  - `SR_FEAS_TIERS <- c("adequate", "limited", "inadequate", "cannot_run", "precision_only",
    "not_applicable")`;
  - `SR_FEAS_DECISIONS <- c("proceed", "exploratory", "revise", "stop")`;
  - measures `MD, RD, RR, OR, HR, RMST_diff` (the effect-measure names of `routes/result.R`).
- **No element of an assessment is named** `auc, or, rr, hr, estimate, effect, crosstab,
  by_outcome, smd_by_outcome`. `.sr_assert_blind` must pass on every assessment.

## Review Focus
1. **The outcome is permuted among its non-missing rows, NA positions kept.**
   - What could break it: the store's ORDER BY changes the row order, float summation order changes,
     and the RMST sampler draws different rows.
   - Expected: the canonical JSON of the verdict (everything but `data_hash`) is byte-identical.
   - Pinned: Task 1 (inputs), Task 2 (RMST), Task 3 (end to end through `sr_feasibility_assess`).
2. **The design or the data change after a decision.**
   - Examples: a re-import, `sr_sap_add` re-setting the analysis with one more term, a new route
     version.
   - Expected: `sr_sap_check` blocks with "<id>: the feasibility assessment is stale (the data
     changed)" or "(the spec changed)". Re-assessing clears the old decision.
   - Pinned: Task 3 and Task 4.
3. **A factor exposure has a level that is present in the data but has no complete-case row, or
   `exposure_ref` is absent.**
   - Expected: tier `cannot_run`, with a blocker naming the empty level. No error escapes from `glm`.
   - Pinned: Task 2.
4. **A clustered analysis has no ICC assumption, or an unclustered one is given an ICC.**
   - Expected: `sr_feasibility_assess` refuses with "a clustered analysis needs assumptions$icc (value
     and justification)", or "icc applies to a clustered analysis only". Fewer than 30 clusters gives
     a warning.
   - Pinned: Task 2 (engine) and Task 3 (API).
5. **The user tries to escape a stop** by re-assessing, removing the analysis, or calling `sr_run`.
   - Expected: each is refused, naming the stop. Only `sr_feasibility_decide(..., reason)` reverses it.
   - Pinned: Task 3 and Task 4.

Further cases a reviewer should probe, each pinned in its task:
- a hostile column name in a term reaches the in-process term code (Task 1);
- a logical outcome with a text `event_value` (P1c limitation R45) surfaces as the "no events" blocker
  before the lock, because the engine codes the outcome by the route's own rule (Task 1);
- a `table1` or test-fake primary route gives tier `not_applicable` and can be decided `proceed`
  (Task 3).

## File Structure
```
app/global.R                       + sap/feas_engine, sap/feasibility, resample/feas_sim
app/R/routes/run.R                 .sr_extract_frame() factored out of sr_extract(); sr_run refuses a stopped project
app/R/sap/feas_engine.R            pure engine: blind inputs, SE formulas, design effect, MI range, RMST simulation, tiers
app/R/sap/feasibility.R            feasibility.* events, assess / decide, stopped, report, fragment inputs, gate rows
app/R/sap/sap.R                    sr_sap_check gains the feasibility rows; sr_sap_hash freezes verdicts; sr_sap_remove refuses a stopped analysis
app/R/resample/feas_sim.R          SE-validation cells (in-process, like coverage.R)
tools/feas_sim.R                   full SE validation (manual; results in docs/validation.md)
tests/testthat/helper-data.R       + .fx_feasible(p)
tests/testthat/test-feas-engine.R  engine
tests/testthat/test-feasibility.R  events, API, replay, stop
tests/testthat/test-feas-sim-smoke.R
tests/testthat/test-sap.R          + the gate; the existing lock call sites in 12 test files go through .fx_feasible
tests/testthat/test-textbook.R     + NHEFS conservative-SE check (skips without SR_NHEFS_CSV)
docs/analysis.md, docs/validation.md, memory.md
```

---

### Task 1: Blind inputs

**Files:**
- Create: `app/R/sap/feas_engine.R` (inputs part), `tests/testthat/test-feas-engine.R`
- Modify: `app/R/routes/run.R`, `app/global.R`

**Interfaces:**
- Produces (run.R):
  - `.sr_extract_frame(project, table, cols) -> data.frame`: the body of `sr_extract` up to the
    query, with the same checks and the same ORDER BY every column. `sr_extract` now calls it.
    The bytes and the sha are unchanged, and the existing test-run.R still passes.
- Produces (feas_engine.R):
  - `.sr_feas_kind(route_id) -> chr`, vectorised:
    - `"continuous"` for `lm*`;
    - `"binary"` for `logistic*` and `modpois*`;
    - `"surv"` for `cox*` and `rmst*`;
    - `"none"` otherwise.
  - `.sr_feas_inputs(df, spec, route) -> list`. Pure; `df` is the frame `.sr_extract_frame` returns
    for `sr_spec_cols(spec)`. Steps, in order:
    1. Drop `.sr_exclude_rows(spec)` by position.
    2. Code the outcome exactly as `sr_outcome_code` would. Binary: `as.integer(y == event_value)`,
       or the `.h_bin` rule. Survival: the same with status and `status_event`. Continuous: the
       `.h_num` rule.
    3. A coding failure becomes the blocker `"outcome: <message>"`, not an error.
    4. Compute the outcome marginals over rows with a non-missing (coded) outcome, from sorted
       values.
    5. Complete cases are rows with every spec column non-missing. Build the covariate frame (the
       exposure, term vars, cluster and strata of those rows) and sort it by all its columns.
    6. Evaluate `sr_terms_code()` setup + design in a fresh environment under `globalenv()`, with
       `sr_data` bound to the sorted covariate frame. Do this for a covariate-only spec (exposure,
       outcome, time and status removed; interactions involving the exposure dropped; strata added
       as factor terms) and for the full spec minus the outcome. Both code strings pass
       `.sr_emit_check(code, "analysis")` before `eval`.
    7. The exposure levels come from the exclusion-dropped frame (all rows with a non-missing
       exposure), with `exposure_ref` first, as `.h_levels` orders them. The counts per level come
       from the complete cases.
    8. For a linear exposure whose values are only {0, 1}, the exposure type is `"binary"`.

    Returns `list(kind, n_rows, n_cc, n_obs, outcome, exposure, contrasts_in, clusters, params,
    blockers)`:
    - `outcome`: `list(sd, risk, events_total, events_cc, followup_total, max_time, pairs)`. `pairs`
      is the sorted `data.frame(time, event)` for survival, NULL otherwise. Fields that do not apply
      are NA.
    - `exposure`: `list(var, type = "linear"|"binary"|"factor", ref, levels = data.frame(level,
      n_all, n_cc), variance)`.
    - `contrasts_in`: data.frame with one row per contrast (`level`, `n_k`, `n_ref`, `r2x`,
      `ps_outside`, `separated`).
      - `r2x` is the R² of `lm.fit` of the exposure (linear) or of `1{x = level}` among rows in
        {level, ref} on the covariate design. It is 0 with no covariates.
      - `ps_outside` is the share of fitted propensities (binomial `glm`, `maxit = 50`, warnings
        suppressed) outside `SR_FEAS_PS_RANGE`. It is NA for a continuous exposure.
      - `separated` is TRUE when that glm did not converge, or when any fitted value is within
        `SR_FEAS_SEP_EPS` of 0 or 1. For a continuous exposure it is TRUE when `r2x >=
        SR_FEAS_R2X_MAX`.
    - `clusters`: `list(G, m_bar, cv)` when `layers$clustered`, else NULL. `cv` = SD with
      denominator G over m_bar.
    - `params`: `ncol(model.matrix(~ rhs))` of the full design (minus 1 for survival kinds: no
      intercept). For rmst it is the number of exposure levels.
    - `blockers`: character.
  - `.sr_feas_blind_frame(x) -> x`: wraps `.sr_assert_blind` with the analysis's outcome columns and
    the other spec columns.

- [ ] **Step 1: Write the inputs tests in `test-feas-engine.R`.** Use a synthetic frame (n = 500):
  binary `x` with `P(x) = plogis(z1)`, covariates `z1, z2`, binary `y` coded "yes"/"no",
  `event_value = "yes"`, 10% NA in `z2` and 5% NA in `y`.
  - `n_cc == sum(complete.cases(df))`; `n_obs == sum(!is.na(df$y))`.
  - `risk == mean(df$y[!is.na(df$y)] == "yes")`.
  - `events_cc == events_total * n_cc / n_obs`.
  - `r2x` equals `summary(lm(x ~ z1 + z2, cc))$r.squared` to 1e-10.
  - `params == 4`.
  - **Blindness:** permute `y` among its non-NA rows (`set.seed(2)`). Then
    `identical(sr_canonical_json(.sr_feas_inputs(df_perm, ...)), sr_canonical_json(.sr_feas_inputs(df, ...)))`.
    Repeat with rows shuffled.
  - A factor term `list(var = "site", type = "factor")` with 3 levels gives `params == 6`. An RCS(4)
    term adds 3.
  - A logical outcome with `event_value = "Yes"` gives `risk == 0` and a blocker (no error).
  - A hostile term name (`` "x`); system('calc'); (`" ``) gives finite inputs, and
    `.sr_emit_check` saw the code.
  - `.sr_feas_blind_frame(inputs)` passes.
- [ ] **Step 2: Run** `"$RS" tools/run_tests.R feas-engine`. Expected: FAIL (`.sr_feas_inputs` not
  found).
- [ ] **Step 3: Implement** `.sr_extract_frame` (run.R) and the inputs half of `feas_engine.R`. Add
  `"sap/feas_engine"` to `app/global.R`.
- [ ] **Step 4: Run** `feas-engine`, `run` and the lint. Expected: PASS, `LINT OK`.
- [ ] **Step 5: Commit** `feasibility: outcome-blind inputs from the compiled spec`.

### Task 2: SEs, MDE, blockers and tiers

**Files:** Modify `app/R/sap/feas_engine.R`, `tests/testthat/test-feas-engine.R`.

**Interfaces:**
- Consumes: `.sr_feas_inputs` (Task 1).
- Produces:
  - `.sr_feas_measures(route_id, spec, exposure_type) -> chr`:
    - lm* → `MD`;
    - logistic* → `OR`, plus `RD`, `RR` when `.sr_glm_marg_on(spec)` and the exposure is binary or
      factor;
    - modpois → `RR`, plus `RD` under the same condition;
    - cox* → `HR`;
    - rmst → `RMST_diff`;
    - otherwise `character()`.
  - `.sr_feas_check_args(delta, assumptions, kind, measures, clustered) -> invisible(TRUE)`. It stops
    unless all of these hold:
    - `delta` is NULL or `list(value, measure, source)`;
      - `measure` is in `measures`;
      - `source` is non-empty text;
      - a ratio measure has `value > 0` and `value != 1`; a difference measure has `value > 0`
        (absolute).
    - `assumptions` names only `r2_y` and `icc`, each `list(value, justification)` with
      non-empty text;
      - `r2_y` ∈ [0, 1), for kind `"continuous"` only ("r2_y applies to a continuous outcome
        only");
      - `icc` ∈ [0, 1), required when `clustered` ("a clustered analysis needs assumptions$icc
        (value and justification)") and refused otherwise ("icc applies to a clustered analysis
        only").
  - `.sr_feas_rmst_se(pairs, n_k, n_ref, tau, sims, seed) -> num`. For each simulation, draw `n_k`
    and `n_ref` pairs with replacement from the sorted `pairs`. Take each group's
    `summary(survival::survfit(Surv(time, event) ~ 1), rmean = tau)$table[["rmean"]]`. The result is
    the SD of the differences. Draws come from stream 1 of `sr_streams(seed, 1)`; the caller's RNG is
    restored.
  - `.sr_feas_compute(inputs, spec, route, delta = NULL, assumptions = list()) -> list(contrasts,
    blockers, warnings, tier, advice)`.
    - `contrasts`: data.frame with one row per (contrast × measure) and columns `contrast, level,
      measure, scale, se, h, mde, se_upper, n_k, n_ref, r2x, deff, tier`.
- Formulas (spec O; `p_k = n_k/(n_k+n_ref)`):
  - **v.** Linear or binary exposure: `v = n_cc·Var(x)·(1−r2x)` (Var with denominator n). Factor
    contrast: `v = (1−r2x)/(1/n_k + 1/n_ref)`.
  - **SE.**
    - `MD`: `sd·sqrt(1 − r2_y)/sqrt(v)`, with `r2_y` 0 when not assumed.
    - `RD`: `sqrt(π(1−π)/v)`.
    - `RR` (log): `sqrt((1−π)/(π·v))`.
    - `OR` (log): `1/sqrt(v·π(1−π))`.
    - `HR` (log): `1/sqrt(D·Var·(1−r2x))`. For a linear or binary exposure, `D = events_cc` and
      `Var = Var(x)`. For a factor contrast, `D = events_cc·(n_k+n_ref)/n_cc` and
      `Var = p_k(1−p_k)`.
    - `RMST_diff`: `.sr_feas_rmst_se`.
  - **Design effect.** When clustered: `deff = 1 + ((cv² + 1)·m_bar − 1)·icc`, and `se ← se·sqrt(deff)`.
    `deff` is 1 otherwise.
  - **Reporting scale.**
    - Ratio measures: `h = exp(1.96·se)` and `mde = exp(2.80·se)`.
    - Difference measures: `h = 1.96·se` and `mde = 2.80·se`.
  - **MI.** When `layers$missing == "mi"`, `se_upper` is the SE with `n_obs` in place of `n_cc` (v and
    D scaled by `n_obs/n_cc`). It is NA otherwise. The tier uses `se`, the conservative end.
- Blockers (tier `cannot_run`, independent of δ), each one string naming its cause:
  - fewer than 2 exposure levels with complete-case rows; a level with `n_all > 0` and `n_cc == 0`
    ("exposure level '<l>' has no complete-case row");
  - `risk` ∈ {0, 1}, `events_total == 0`, or `sd == 0`;
  - binary: `params > min(events_cc, n_cc − events_cc)`; survival: `params > events_cc`; continuous:
    `params >= n_cc`;
  - any `separated` ("the exposure is perfectly predicted by the covariates (positivity)");
  - rmst: `tau > max_time`;
  - the Task 1 coding blockers.
- Warnings:
  - any `ps_outside > SR_FEAS_PS_SHARE`;
  - events per parameter `< SR_FEAS_EPV_WARN`, with "consider pre-specifying penalty = \"firth\""
    on logistic routes without Firth;
  - clustered with `G < SR_FEAS_MIN_CLUSTERS`.
- Tier:
  - `cannot_run` if there are blockers;
  - else `not_applicable` if there are no measures;
  - else `precision_only` if `delta` is NULL;
  - else per contrast of `delta$measure`, with `d = |log δ|` for a ratio or δ for a difference, and
    `s = se`:
    - `adequate` if `2.80·s <= d`;
    - `limited` if `2.80·s > d >= 1.96·s`;
    - `inadequate` if `1.96·s > d`.
  - The overall tier is the worst contrast.
- Advice:
  - `adequate`, `limited`, `precision_only`, `not_applicable` → `"proceed"`;
  - `inadequate` → `c("revise", "stop", "exploratory")`;
  - `cannot_run` → `c("revise", "stop")`.

- [ ] **Step 1: Write the compute tests.**
  - **Hand-computed SEs.** Fix inputs (`n_cc = 400`, `Var(x) = 0.25`, `r2x = 0.2`, `π = 0.3`,
    `sd = 2`, `events_cc = 120`) and assert each formula to 1e-12. For example, `OR`:
    `1/sqrt(400·0.25·0.8·0.3·0.7)`; `HR`: `1/sqrt(120·0.25·0.8)`.
  - **Design effect.** `G = 20`, `m_bar = 10`, `cv = 0.5`, `icc = 0.05` → `deff == 1 + (1.25·10 −
    1)·0.05`.
  - **Tiers.** δ = HR 0.80 with `se` set so that `2.80·se = 0.9·|log 0.8|` → `adequate`;
    `1.96·se < |log 0.8| < 2.80·se` → `limited`; `1.96·se > |log 0.8|` → `inadequate`; no δ →
    `precision_only`.
  - **Each blocker fires on a planted frame:**
    - a single-level exposure;
    - a factor level with only NA covariates;
    - an all-"no" outcome;
    - a constant continuous outcome;
    - 8 terms with 5 events;
    - `x` equal to `z1 > 0` (positivity);
    - rmst `tau` beyond follow-up.
  - **Each refusal of `.sr_feas_check_args` fires:** δ with an unknown measure, HR 1, no source;
    `r2_y` on a binary route; clustered without icc; icc unclustered.
  - **RMST.** The same seed gives the same SE. Permuting the input pairs leaves it identical. Swapped
    groups give the same SE.
  - **MI.** `se_upper < se` when `n_obs > n_cc`.
- [ ] **Step 2: Run** `feas-engine`. Expected: FAIL.
- [ ] **Step 3: Implement** the compute half of `feas_engine.R`.
- [ ] **Step 4: Run** `feas-engine` and the lint. Expected: PASS, `LINT OK`.
- [ ] **Step 5: Commit** `feasibility: SE formulas, design effect, blockers and tiers`.

### Task 3: Events, assess and decide

**Files:**
- Create: `app/R/sap/feasibility.R`, `tests/testthat/test-feasibility.R`
- Modify: `app/global.R`

**Interfaces:**
- Consumes: `.sr_extract_frame`, `.sr_feas_inputs`, `.sr_feas_compute`, `.sr_feas_check_args`,
  `.sr_feas_measures`.
- Produces:
  - `sr_feasibility_assess(project, analysis_id, delta = NULL, assumptions = list()) -> project`
    appends `feasibility.assessed`. Its payload is `{analysis, data_hash, spec_hash, delta,
    assumptions, inputs, contrasts, blockers, warnings, tier, advice}`.
    - `inputs` drops `pairs`.
    - `contrasts` is stored as a named list of columns.
    - `data_hash = sr_store_hash(sr_store_manifest(project, table))`.
    - `spec_hash = .sr_sha256(sr_canonical_json(list(route, route_version, table, spec)))`.
    - It refuses when:
      - the project is read-only;
      - the SAP is locked ("the SAP is locked: the feasibility verdict is frozen");
      - the analysis is unknown or not pre-specified;
      - its current decision is `stop` ("analysis '<id>' is stopped: reverse the stop with
        sr_feasibility_decide first");
      - the arguments are invalid.
  - `sr_feasibility_decide(project, analysis_id, decision, reason = NA) -> project` appends
    `feasibility.decided`. It refuses when:
    - the SAP is locked;
    - there is no assessment, or the assessment is stale;
    - the decision is `proceed` / `exploratory` on `cannot_run`;
    - `reason` is missing where it is required. It is required for `exploratory`, `revise`, `stop`,
      for any decision replacing `stop`, and for `proceed` on `inadequate` or `precision_only`.
  - `.sr_feas_stale(project, id) -> NA | "data" | "spec"`: NA when current. It recomputes only the
    two hashes, never the verdict.
  - `sr_feasibility(project, analysis_id) -> list(assessment, decision, stale)`: the stored record
    as recorded.
  - `sr_project_stopped(project) -> NULL | list(analysis, reason)`: the first pre-specified
    analysis, by id, whose current decision is `stop`.
  - `sr_feasibility_report(project) -> data.frame(analysis, label, route, contrast, measure, n_cc,
    events, prevalence, se, h, mde, se_upper, delta, delta_measure, delta_source, tier, decision,
    reason, stale, blockers, warnings)`. One row per contrast, or one row with NA contrast columns
    when there is none. It reads stored records only.
  - `sr_feasibility_fragment(project, analysis_id) -> list(template = "feasibility_methods", values =
    list(n, events, prevalence, h, mde, delta, delta_source, tier))`: fragment inputs, not prose
    (the P1c convention).
  - `.sr_feas_gate(project) -> character`: one detail per blocking problem of a pre-specified
    **primary** analysis:
    - "<id>: no feasibility assessment";
    - "<id>: the feasibility assessment is stale (the data changed)", or "(the spec changed)";
    - "<id>: no feasibility decision";
    - "<id>: decision is stop (<reason>)";
    - "<id>: decision is revise: change the design, assess and decide again".
- Reducers normalise their payload so that live folding equals replay (the P1c rule): `contrasts`
  becomes a data.frame with fixed column types, and NULL becomes NA.

- [ ] **Step 1: Write `test-feasibility.R`.** Use `.fx_ready_project` with the Task 1 frame and a
  logistic primary `p1`.
  - Assess → `sr_feasibility(p, "p1")$assessment$tier == "precision_only"`, and
    `decision` is NULL. `sr_project_open` of the folder gives `identical(state$feasibility,
    p$state$feasibility)`.
  - **Blindness end to end.** A second project with `y` permuted among non-NA rows gives the same
    canonical JSON of the assessment minus `data_hash`.
  - **Decide.** `proceed` on `precision_only` without a reason is refused; with a reason it passes.
    `exploratory` without a reason is refused. Re-assessing clears the decision.
  - **Stop.** After `stop` with a reason: re-assess is refused; `sr_project_stopped(p)$analysis ==
    "p1"`; `sr_feasibility_decide(p, "p1", "revise", reason = "pool sites")` reverses it, and
    `sr_project_stopped(p)` is NULL.
  - **Staleness.** `sr_sap_add` re-setting `p1` with one more term makes `.sr_feas_stale == "spec"`,
    and decide is refused. Re-ingesting the table with one row changed makes it `"data"`.
  - **Clustered.** A clustered logistic analysis without icc is refused; with `icc = list(value =
    0.05, justification = "pilot")` it passes, with a few-clusters warning on 12 clusters.
  - **Not applicable.** A `table1` primary gives tier `not_applicable`, and `proceed` without a
    reason passes.
  - **Report and fragment.** Report rows equal the number of contrasts, with
    `sr_feasibility_fragment()$values$n == n_cc`. `.sr_assert_blind` passes on both.
- [ ] **Step 2: Run** `"$RS" tools/run_tests.R feasibility`. Expected: FAIL.
- [ ] **Step 3: Implement** `feasibility.R`. Add `"sap/feasibility"` to `app/global.R`.
- [ ] **Step 4: Run** `feasibility`, `feas-engine` and the lint. Expected: PASS.
- [ ] **Step 5: Commit** `feasibility: assessed / decided events, stop and report`.

### Task 4: The lock gate, the frozen verdict and the stop

**Files:**
- Modify:
  - `app/R/sap/sap.R` (`sr_sap_check`, `sr_sap_hash`, `sr_sap_remove`, header);
  - `app/R/routes/run.R` (`sr_run`);
  - `tests/testthat/helper-data.R`;
  - `tests/testthat/test-sap.R`, `test-feasibility.R`;
  - every test file that calls `sr_sap_lock`: `test-boot, test-cox, test-deviations,
    test-diagnostics, test-glm, test-impute, test-lm, test-rmst, test-run, test-table1,
    test-textbook`.

**Interfaces:**
- Consumes: `.sr_feas_gate`, `sr_project_stopped` (Task 3).
- Produces:
  - `sr_sap_check` adds one `"feasibility"` row per `.sr_feas_gate` detail, with status `block`. With
    none it adds one `ok` row ("every primary analysis has a current feasibility decision").
  - `sr_sap_hash` adds `feasibility = <by_id list of state$feasibility for ids in
    .sr_sap_analyses>`. The element is added **only when that list is non-empty**, so a SAP locked
    before P1c.1 keeps its hash.
  - `sr_sap_remove` refuses an analysis whose decision is `stop` ("reverse the stop with
    sr_feasibility_decide first").
  - `sr_run` refuses, before extracting, when `sr_project_stopped(project)` is non-NULL ("the
    project is stopped: <reason>").
  - `.fx_feasible(p, decision = "proceed")` in `helper-data.R` assesses and decides every
    pre-specified primary analysis without δ, with reason "test fixture", and returns `p`. Each
    existing lock site becomes `p <- sr_sap_lock(.fx_feasible(p))`. In test-run.R it goes inside
    the `sr_sap_lock(...)` call at line 54.

- [ ] **Step 1: Add the gate tests to `test-sap.R`.**
  - Lock with no assessment → the error lists "feasibility: p1: no feasibility assessment".
  - After assess, no decision → "no feasibility decision".
  - `stop` → "decision is stop".
  - `revise` → "decision is revise".
  - A spec re-set after the decision → "stale (the spec changed)".
  - `proceed` → the lock succeeds.
  - A secondary analysis with no assessment does not block.
- [ ] **Step 2: Add the frozen-verdict tests to `test-feasibility.R`.**
  - After the lock, `sr_sap_hash(p) == p$state$sap$lock$sap_hash`.
  - Assess and decide are refused ("frozen").
  - `trace(".sr_feas_compute", ...)` (or a counting spy on the engine) proves that
    `sr_feasibility()` and `sr_feasibility_report()` call it zero times after the lock.
  - Changing the stored decision's reason in the state changes `sr_sap_hash`.
  - A project with a P1c-style lock and no feasibility records keeps its old hash: build the hash
    with `state$feasibility` NULL, and compare it with the same project before the edit.
- [ ] **Step 3: Add the stop tests.** `sr_sap_remove` of a stopped analysis is refused. `sr_run` on a
  project whose state carries a stop (set in memory, as `.fx_unblind` does) is refused before any
  extract file appears.
- [ ] **Step 4: Run** `sap`, `feasibility`. Expected: FAIL.
- [ ] **Step 5: Implement** the sap.R and run.R changes and `.fx_feasible`, and update the lock call
  sites.
- [ ] **Step 6: Run** the whole suite and the lint. Expected: 0 failures, `LINT OK`.
- [ ] **Step 7: Commit** `sap: the lock requires a current feasibility decision; verdicts are frozen`.

### Task 5: SE validation and the NHEFS check

**Files:**
- Create: `app/R/resample/feas_sim.R`, `tools/feas_sim.R`, `tests/testthat/test-feas-sim-smoke.R`
- Modify: `tests/testthat/test-textbook.R`, `docs/validation.md`, `app/global.R`

**Interfaces:**
- Consumes:
  - `.sr_feas_inputs`, `.sr_feas_compute`;
  - the in-process route evaluation and stream plumbing of `resample/coverage.R`. Factor a shared
    helper out of `.sr_cov_rep` if that is cleaner, and keep `test-coverage-smoke` green.
- Produces:
  - `sr_feas_sim_cells() -> data.frame(cell, route, measure, clustered, reps, n, pred_every)`.
  - `sr_feas_sim_cell(cell, reps = NULL, n = NULL, seed = 1L, workers = 1L) -> list(cell, reps,
    pred_se, emp_sd, ratio, pass, failed, errors)`.
    - `emp_sd` is the SD over successful reps of the route's primary estimate of the measure (log
      scale for ratios).
    - `pred_se` is the mean predicted SE over every `pred_every`-th rep. The prediction is given the
      generator's true `r2_y` / `icc` as assumptions.
    - `ratio = pred_se/emp_sd`; `pass = abs(ratio − 1) <= 0.10`.
    - Rep r uses stream r of `sr_streams(seed, reps)`.
- Cells. All are null-effect. The exposure depends on a covariate (`P(x) = plogis(0.8 z1)`), so
  `r2x > 0`, except in the clustered cells, where the exposure is at cluster level. Reps are 1000
  everywhere.

| cell | outcome generator | analysis (route, row) | n | pred_every |
|---|---|---|---|---|
| `md` | `y = 1 + 0.8 z1 + 0.4 z2 + N(0,1)`; true `r2_y = 0.8/1.8` | lm, MD row | 400 | 1 |
| `rd` | `y ~ Bern(0.3)` | logistic, marginal RD row | 600 | 1 |
| `rr` | `y ~ Bern(0.3)` | modpois `marginal = FALSE`, RR row | 600 | 1 |
| `or` | `y ~ Bern(0.3)` | logistic `marginal = FALSE`, OR row | 600 | 1 |
| `hr` | exponential(1) times, uniform censoring giving about 30% censored | cox, HR row | 400 | 1 |
| `rmst` | as `hr`, binary `x` with p = 0.5, no covariate; tau the 75th percentile of time | rmst, RMST_diff row | 400 | 10 |
| `md_cl` | 40 clusters × 10; `y = 1 + 0.5 z + u + e`, `u ~ N(0, 0.2)`, `e ~ N(0, 0.8)`; `r2_y = 0.2`, `icc = 0.2` | lm `se = "cluster"` | 400 | 1 |
| `rd_cl`, `rr_cl`, `or_cl` | 40 clusters × 15; cluster risk `~ Beta` with mean 0.3 and ICC 0.05 (a + b = 19); `icc = 0.05` | as rd / rr / or with `se = "cluster"` | 600 | 1 |
| `hr_cl` | 40 clusters × 10; gamma frailty (variance 0.3) on exponential times, administrative censoring at t = 1.5; `icc` = the event-indicator ICC of the generator, computed once by a one-way ANOVA estimator on 400 000 draws and stored as a constant | cox clustered | 400 | 1 |

- Ruling (recorded in docs/validation.md): `rmst` predicts on every 10th rep. Its prediction is itself
  a 500-draw simulation, and the mean of 100 predictions estimates the mean predicted SE to well under
  1%. Clustered RMST is refused by the route, so it has no cell.

- [ ] **Step 1: Write `test-feas-sim-smoke.R`.**
  - `sr_feas_sim_cells()$cell` equals the 11 cells above.
  - Each cell with `reps = 20` (rmst `reps = 10`) returns the documented fields with `failed == 0`
    and finite `ratio`.
  - The same seed gives an identical result with `workers = 1` and `workers = 2`.
  - It does **not** assert `pass`.
- [ ] **Step 2: Add the NHEFS test to `test-textbook.R`.** It skips without `SR_NHEFS_CSV`. On the
  existing NHEFS lm spec, the engine's predicted MD SE (marginal values, no assumptions) is `>= 0.9 ×
  (hi − lo)/(2·qnorm(0.975))` of the locked run's primary MD row.
- [ ] **Step 3: Run** `feas-sim` and `textbook`. Expected: FAIL (smoke), and textbook passes or
  skips.
- [ ] **Step 4: Implement** `feas_sim.R` and `tools/feas_sim.R`. The tool mirrors
  `tools/coverage_sim.R`: `[cells] [reps_scale]`, workers from `sr.feas_sim_workers` /
  `SR_FEAS_SIM_WORKERS`, a printed table and CSV block, nothing written into the repo.
- [ ] **Step 5: Run** the smoke test, the whole suite and the lint. Expected: PASS.
- [ ] **Step 6: Run the full validation once** in the background: `"$RS" tools/feas_sim.R`.
  Expected: every cell `pass = TRUE`. Also run `textbook` once with `SR_NHEFS_CSV` set to a copy
  kept outside the repo.
  - A failing cell is a finding. Investigate it with superpowers:systematic-debugging and report it
    to the controller. **Never widen the 10% tolerance, and never change a formula away from spec
    O** without a ruling.
- [ ] **Step 7: Add a "Feasibility SE validation" section to `docs/validation.md`.** It holds the
  cell table (cell, reps, pred_se, emp_sd, ratio, pass), the NHEFS predicted vs actual SE, and the
  date and versions. No paths or machine names.
- [ ] **Step 8: Commit** `validation: feasibility SE simulation and NHEFS conservativeness`.

### Task 6: Guide and memory

**Files:** Modify `docs/analysis.md`, `memory.md`.

- [ ] **Step 1: Add a "Study feasibility (before the lock)" section to `docs/analysis.md`.**
  - What is blind and why the outcome marginals are taken over all observed outcomes.
  - The δ and assumptions arguments, with one example per measure.
  - The formulas table and the tiers, advice and decisions.
  - The gate messages, stop and its reversal, and that verdicts are frozen at lock.
  - The Out of scope rulings above.
  - The SAP workflow line becomes add → assess → decide → family → rule → lock.
- [ ] **Step 2: Update `memory.md`.**
  - Add a P1c.1 status row with the final test count.
  - **Next:** the P1d plan, then P1e.
  - Gotchas:
    - every new lock needs `.fx_feasible` in tests;
    - expected events among complete cases are scaled, not counted (blindness);
    - the feasibility element of the SAP hash is omitted when empty (old locks keep their hash);
    - R45 now surfaces as a "no events" blocker before the lock.
- [ ] **Step 3: Run** the whole suite and the lint. Expected: PASS, `LINT OK`.
- [ ] **Step 4: Commit** `docs: feasibility guide and memory update for P1c.1`.

---

## Self-review (done while writing)
- **Spec coverage (catalogue O).**
  - Outcome-blind inputs (n after exclusions and the complete-case filter, outcome marginals,
    exposure marginals, R²ₓ, propensity overlap, cluster sizes, parameter count) → Task 1.
  - Assumptions with justification and the δ with source → Task 2.
  - The SE formulas, design effect, MI range, h and MDE → Task 2.
  - Tiers, hard blockers and warnings → Task 2. The Riley criteria are ruled out of scope: no
    prediction route exists yet.
  - Decisions, stop and its reversal → Task 3. "Labelled exploratory" is the stored decision, which
    the report exposes for the P1d writer.
  - The report as a structured result → Task 3. Events → Task 3.
  - The lock gate, frozen verdict and no recompute → Task 4.
  - The Methods fragment inputs → Task 3 (prose in P1d).
- **Spec coverage (Verification → O).**
  - Blindness → Tasks 1–3.
  - SEs within 10% over 1000 datasets per estimand, with and without clustering → Task 5. Clustered
    RMST is ruled out (the route refuses it).
  - NHEFS conservative → Task 5.
  - Each blocker on a planted case → Task 2.
  - Lock refusals → Task 4.
  - No recompute after lock → Task 4.
- **Rulings made here** (beyond spec O, for the executor to keep):
  - A `revise` decision blocks the lock.
  - A clustered analysis must state an ICC (no conservative marginal value exists).
  - The tier uses the complete-case end of the MI range.
  - Expected complete-case events are scaled from the marginal count.
  - A table1 or unknown route is `not_applicable` but still needs a decision.
  - A stopped analysis cannot be re-assessed or removed.
  - Assessing secondary analyses is allowed but never gated.
