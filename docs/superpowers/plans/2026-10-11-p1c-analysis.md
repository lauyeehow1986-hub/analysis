# P1c Analysis Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Take analysis-ready data inside a StatRouter project to locked, reproducible results. The
user (headless in P1c; the UI is P1d) can:
- write a SAP: primary / secondary / sensitivity analyses, multiplicity families, and SAP-conditional
  rules, then **lock** it (hash-frozen; refused while the data are not ready);
- run Table 1 and the primary models: linear, logistic, modified Poisson and Cox, with RCS terms,
  model / robust / cluster SEs, pre-specified RMST, marginal RD/RR via g-computation, complete-case
  or mice + Rubin, and the bootstrap where the spec asks for it;
- get every effect size with a 95% CI, MCSE-limited digits and Table 2 / non-collapsibility flags;
- run diagnostics (test + plot + effect-size verdict) that add **sensitivity nodes**, never a silent
  change of the primary analysis;
- see deviations from the SAP and multiplicity-adjusted families;
- reproduce textbook values (`survival::lung`, GBSG, `mice::nhanes`, NHEFS when supplied) and
  pass a CI-coverage simulation of MI × bootstrap × clustering.

**Architecture:** Headless modules under `app/R/routes/`, `app/R/sap/`, `app/R/impute/`,
`app/R/resample/` and `app/R/diagnostics/`, on the P1a core and the P1b data layer.
- **Code-first.** Each route's `compile(spec)` emits an R code string (the *body*). `sr_run`
  prepends a fixed *prologue* that loads a hashed, column-pruned extract of the store, and the whole
  string runs in a callr worker via `sr_exec_cached`. What ran is what gets exported in P1d.
- **Self-contained code.** Emitted code calls only allow-listed namespaces. Shared helpers (Rubin
  pooling, bootstrap CIs, g-computation, sandwich SEs) are R functions in this repo, inlined into the
  body as `.h_<name> <- <deparse(fn)>` so `analysis.R` runs without StatRouter.
- **SAP gate.** Every run needs a locked SAP. The lock freezes analyses, families, rules, the store
  hash of every analysed table and the library snapshot. Diagnostics may only *add* sensitivity
  analyses unless a SAP rule says otherwise.
- **Results.** Emitted code returns plain data (no model objects). `route$present(value)` turns it
  into tables, figure data, fragments and an `effects` frame that `sr_result_check` validates.

**Tech Stack:** R 4.5.2; survival, mice 3.19, sandwich, boot (cross-check only), splines, Hmisc
(`rcspline.eval`), logistf (Firth sensitivity), ggplot2 + ragg (figures), callr (via `sr_exec`),
openssl, testthat 3e. **No new packages** (not installed and not added: survRM2, marginaleffects,
clubSandwich, geepack, EValue, biglm, detectseparation).

**Spec:** `docs/specs/2026-10-03-statrouter-design.md`. Relevant sections:
- Design principles 1–2 (models route, never compute; diagnostics add sensitivity nodes);
- Core mechanics (code-first execution, content-addressed results, route contract);
- catalogue **B** (diagnostics: test + plot + verdict), **C** (Table 1, regression, survival),
  **C+** (effect sizes with CIs), **C++** (bootstrap), **D** (missing data);
- **M1** global rules 1–7 (no test-gated switching, estimand + purpose, multiplicity registry,
  composition matrix, numerical reproducibility, reporting corrections);
- "Archive, SAP lock & reviewer mode" → SAP lock and labels;
- Phases → **P1** items 4 (lock the SAP) and 5, and the P1 Exit criteria on textbook values and
  coverage;
- Verification → "Textbook reproduction", "Simulation checks of CI coverage", "Validity-rule checks".

**Builds on (P1a + P1b, on `main` at 807c708):**
- Exec: `sr_exec(code, inputs, seed, timeout)`, `sr_exec_cached(project, key_hash, code, inputs,
  seed, ...)`, `sr_rng_preamble(seed)`, `sr_streams(seed, n)`.
- Hashing: `sr_content_hash(data_hash, spec, seed, libsnap_hash, hw_fp)`, `sr_canonical_json`,
  `sr_libsnap_hash()`, `.sr_core_pkgs()`.
- Routes (`app/R/design/spec.R`): `sr_route(id, version, family, purpose, estimand, spec_schema,
  compile, present, composes_with, applicable)`, `sr_route_register`, `sr_route_get(id, version)`,
  `sr_routes()`, `.sr_routes_reset()`, `sr_spec_validate(spec, route)`, `sr_compile(route, spec)`,
  `sr_estimand(population, variables, intercurrent, summary, level)`. Schema fields are
  `list(type = chr|num|lgl|list, required, values)`.
- Composition (`app/R/design/compose.R`): `sr_compose_check(spec) -> list(ok, refused, choices,
  unmet, notes)` over `spec$layers` and `spec$choices`.
- Formatting (`app/R/writer/numfmt.R`): `sr_mcse_digits`, `sr_fmt_num`, `sr_fmt_ci`, `sr_fmt_p`.
- Events: `sr_reducer_register(type, fn)` (replaces an existing reducer), `sr_project_append`
  (errors on a readonly project).
- Data: `sr_store_manifest`, `sr_store_hash`, `sr_store_view`, `sr_duck`, `sr_duck_query`,
  `sr_roles_set(project, table, roles)`, `sr_roles`, `sr_role_cols`, `sr_blind`,
  `.sr_assert_blind`, `sr_dq_run`, `sr_dq_findings`, `sr_suggest`, `sr_action_decide`, `sr_action`,
  `sr_readiness(project, table) -> data.frame(item, status ok|warn|block, detail)`,
  `sr_mem_estimate(project, table, cols, method = glm|lm|cox|ml, ...) -> list(need_gb, budget_gb,
  fits, route, reason, rows, p)`, `sr_ingest`.
- Lint (`app/R/core/lint.R`, `tools/lint.R`): the p-gate runs on `app/R/routes/**` only and flags
  diagnostic calls/symbols and p-names inside compile bodies (rule 3b) and string literals that
  combine a branch keyword with a diagnostic or p-name (rule 3d). Diagnostics therefore live in
  `app/R/diagnostics/`.

**Out of scope here** (each is a ruling; the owning phase is in brackets):
- the SAP_v1.docx document and the lock UI [P1d]. P1c owns the `sap.*` events, the lock and its
  refusals (moved here from P1d because every run needs it).
- SAP amendment after lock and data versioning [P2]. In P1c a run on data that changed after the
  lock is refused.
- re-baselining under a changed library [P2]. P1c records the snapshot at lock and flags a run made
  under a different one.
- big-data engines (SQL Table 1, bigglm, aggregated Poisson, piecewise-exponential Cox) → a new
  **P1e scale** plan. P1c refuses a fit whose `sr_mem_estimate()$fits` is FALSE.
- CR2 / Kenward–Roger few-cluster corrections, Fine–Gray, IPCW [P9]. P1c's few-cluster sensitivity
  is the cluster bootstrap.
- delta-adjusted tipping-point MI and rbmi [P5]. Little's test (descriptive only) [P5].
- the studentised bootstrap [P5]. P1c has percentile, BCa and normal.
- GBSG/Rotterdam *published* prediction-performance values [P6]. P1c checks GBSG Cox equivalence
  against direct `coxph` only.
- chunk-level checkpoint/cancel and parallel workers for bootstrap and MI [P1d, ExtendedTask].
  P1c already uses per-chunk L'Ecuyer streams, so results will not depend on chunking or workers.
- Methods/Results prose [P1d writer]. P1c returns fragment *inputs* (named values and short
  template ids), not prose.

## Global Constraints
- All P1a/P1b Global Constraints still apply: `sr_` prefix for exported functions, `.sr_` for
  internal ones; ASCII-only `.R` files with LF line endings (non-ASCII test strings built with
  `intToUtf8()`); never multi-line `Rscript -e`; synthetic or public package data only; tests write
  under `withr::local_tempdir()`; JSON via `.sr_json`; ISO-8601 UTC millisecond timestamps.
- Every new `app/R/**` file is appended to `.sr_core_files` in `app/global.R` in the order of the
  File Structure below.
- Command prefix: `RS="/c/Program Files/R/R-4.5.2/bin/Rscript.exe"`.
  - Single test file: `"$RS" tools/run_tests.R <filter>`.
  - Whole suite: `"$RS" tools/run_tests.R`.
  - Lint: `"$RS" tools/lint.R` → `LINT OK (N files)`.
- **The public repo carries no personal details.** No usernames, emails, hostnames or absolute user
  paths in any committed file, including `docs/validation.md`. NHEFS is never committed: the test
  reads it from the path in env `SR_NHEFS_CSV` and skips when it is unset.
- **Air-gap.** No network call anywhere, including in emitted code.
- **Emitted code is built by quoting, never by pasting user text.**
  - Column names: `.sr_q(x)` = `deparse(as.name(x), backtick = TRUE)` (round-trips `"a b"`, a name
    containing `` `); system('calc'); (` ``, a newline, a backslash and `é`).
  - Literals: `.sr_lit(x)` = `paste(deparse(x, control = c("keepNA", "digits17")), collapse = "")`.
  - Derived columns get generated safe names (`t1`, `t1_s2`, ...), never user text.
- **Emitted code is checked before it runs** by `.sr_emit_check(code, kind)` (Task 1):
  - `::` calls only into `base, stats, utils, methods, parallel, survival, mice, sandwich, splines,
    Hmisc, logistf, openssl` (openssl in the prologue only);
  - never `system, system2, shell, shell.exec, Sys.setenv, Sys.unsetenv, setwd, unlink,
    file.remove, file.rename, file.copy, download.file, url, socketConnection, source, sys.source,
    library, require, requireNamespace, attachNamespace, loadNamespace, dyn.load, eval, evalq,
    parse, get, assign, q, quit, saveRDS, writeLines, writeBin, sink`; never `do.call` with a
    character first argument; `readRDS`, `readBin` only in the prologue;
  - `kind = "analysis"`: no diagnostic call or p-name symbol at all (the lists in `core/lint.R`);
  - `kind = "diagnostic"`: diagnostic calls allowed, but no `if` / `switch` / `ifelse` condition may
    reference one.
- **Results never carry a column named like a p-value** (`p.value`, `pvalue`, ...). The effects
  column is `p`.
- **Seeds.** Every analysis spec carries `seed` (integer). Monte Carlo layers (MI, bootstrap) draw
  from per-chunk L'Ecuyer streams: chunk `k` uses the `k`-th `parallel::nextRNGStream` of the stream
  that `sr_rng_preamble(seed)` sets.
- **Event types added by P1c**, each with its reducer registered in the file that owns it:
  `sap.analysis_set`, `sap.analysis_removed`, `sap.family_set`, `sap.rule_set`, `sap.locked`
  (sap/sap.R); `run.completed` (routes/run.R); `diag.completed` (diagnostics/checks.R);
  `analysis.added`, `sap.rule_fired` (sap/deviations.R).
- **State keys:** `state$analyses[[id]]` (every analysis, pre-specified or not), `state$sap$families`,
  `state$sap$rules`, `state$sap$locked`, `state$sap$lock`, `state$runs[[id]]`, `state$diag[[id]]`.
- **Analysis record** (one shape everywhere):
  `list(id, route, route_version, table, label, family, spec, parent, trigger, addresses,
  prespecified, reason)`.
  - `id` matches `^[a-z][a-z0-9_]{0,40}$`.
  - `label` ∈ `primary | secondary | sensitivity` before lock; `exploratory | post_hoc |
    sensitivity` after lock.
  - `family`: a multiplicity-family id or `NA`. `parent`: the analysis this one is a variant of, or
    `NA`. `trigger`: a diagnostic check id or `NA`. `addresses`: DQ finding ids (chr, may be empty).
  - `prespecified`: TRUE for every record set before the lock.
- **Diagnostic verdicts are effect sizes or graphical thresholds**, never a p-value alone. A test
  statistic may be *reported*; it never decides.
- **Defaults are robust by design** (spec principle 2): modified Poisson always uses a robust (or
  cluster) SE; Cox uses `cluster()` when `layers$clustered`; RMST is pre-specified alongside HR.

## Review Focus
1. **Hostile column names reach emitted code** (a name containing a backtick, quote, `);`,
   newline or non-ASCII, e.g. `` x`); system('calc'); (` ``).
   - Expected: the code parses, `.sr_emit_check` passes, no extra call appears in the parse tree,
     and the fit equals the same fit on cleanly renamed columns to 1e-10.
   - Pinned in Task 1 (quoting) and Task 6 (end to end through `sr_run`).
2. **A run is attempted before the SAP is locked, or after the data changed.**
   - Expected: `sr_run` stops with "the SAP is not locked" before extracting anything, and with
     "table '<t>' changed after the SAP lock" when the store hash differs from the lock's. No
     `run.completed` event and no cache file is written.
   - Pinned in Task 4.
3. **The RMST horizon τ lies beyond follow-up in one group** (survfit would silently extrapolate the
   last value).
   - Expected: the run fails with a message naming τ and the shortest group follow-up; the spec
     cannot be locked with a non-positive or non-finite τ.
   - Pinned in Task 8.
4. **MI changes the design across imputations** (RCS knots or factor levels computed per imputed
   dataset; a level absent in one imputation).
   - Expected: knots and levels are fixed from the observed data before `mice`, every completed
     dataset yields the same coefficient names in the same order, and pooling refuses mismatched
     names.
   - Pinned in Task 9.
5. **The project folder is moved (or the drive letter changes) after a run.**
   - Expected: reopening at the new path and running the same analysis is a cache hit
     (`cached = TRUE`) with an identical value; editing one byte of the extract makes the worker
     stop with "extract changed".
   - Pinned in Task 4.

Further cases a reviewer should probe, each pinned in its task: BCa when `z0` is infinite or the
acceleration is NA (Task 10); a diagnostic failure with and without a SAP rule (Task 13); a
`keep_sensitivity` decision without an addressing sensitivity analysis (Task 3); separation in a
logistic fit (Task 11).

## File Structure
```
app/global.R                   + the new files below, in this order
app/R/core/libsnap.R           .sr_core_pkgs() gains the analysis packages
app/R/core/exec.R              sr_exec_cached() gains cache_inputs
app/R/routes/emit.R            quoting, helper inlining, emitted-code checker
app/R/routes/result.R          effects frame, sr_result_check, Table 2 / non-collapsible flags
app/R/routes/terms.R           spec columns, term builder (factor/RCS/quadratic/interaction), outcome coding
app/R/sap/sap.R                SAP events, lock, refusals, sap hash
app/R/routes/run.R             sr_extract, sr_run, sr_result, run.completed
app/R/routes/table1.R          Table 1 route (SMD with CI, no p-values)
app/R/routes/se.R              .h_vcov (model / robust HC / cluster) and .h_coef_table
app/R/routes/lm.R              linear regression route
app/R/routes/marginal.R        g-computation marginal RD/RR (delta method), NNT, Wilson CI helpers
app/R/routes/glm.R             logistic and modified-Poisson routes (+ Firth option)
app/R/routes/cox.R             Cox route with KM figure data
app/R/routes/rmst.R            RMST route with horizon check
app/R/impute/pool.R            Rubin's rules with Barnard-Rubin df and FMI
app/R/impute/impute.R          mice code emission, auto-m (von Hippel), missingness summary
app/R/resample/boot.R          unit/cluster bootstrap, percentile/BCa/normal CIs, MCSE
app/R/diagnostics/checks.R     diagnostic registry, emitted check code, verdicts, sr_diag_run
app/R/diagnostics/plots.R      ggplot builders from figure data + sr_figure_save (ragg)
app/R/sap/deviations.R         post-lock analyses, rule firing, deviations, multiplicity families
app/R/resample/coverage.R      coverage-simulation cells (in-process)
tests/testthat/helper-data.R   + .fx_unblind(p), .fx_ready_project(df, table, roles, env)
tests/testthat/test-*.R        one per module (+ test-textbook.R, test-coverage-smoke.R)
tools/coverage_sim.R           full CI-coverage simulation (manual; results in docs/validation.md)
docs/analysis.md               analysis guide
docs/validation.md             coverage + textbook record
```

---

### Task 1: Emitted-code quoting and checking, and the result contract

**Files:**
- Create: `app/R/routes/emit.R`, `app/R/routes/result.R`, `tests/testthat/test-emit.R`,
  `tests/testthat/test-result.R`
- Modify: `app/R/core/libsnap.R` (`.sr_core_pkgs()` adds `survival, mice, sandwich, boot, Hmisc,
  logistf, ggplot2, ragg`); `app/global.R`

**Interfaces:**
- Produces (emit.R):
  - `.sr_q(x) -> chr`: backtick-quoted name for one non-NA, non-empty string; error otherwise.
  - `.sr_lit(x) -> chr`: a single-line R literal that `eval(parse())`s back to `identical(x)` for
    atomic vectors (NA, Inf, -0, 17 significant digits, strings with quotes/newlines/non-ASCII).
  - `.sr_inline(fn, name) -> chr`: `"<name> <- <deparse(fn)>"`, with `name` matching
    `^\.h_[a-z0-9_]+$`, the function's environment ignored; deterministic text.
  - `.sr_emit_check(code, kind = c("analysis", "diagnostic"), prologue = FALSE) -> invisible(TRUE)`:
    parses `code`, walks every call, and stops naming the offending call when a Global Constraints
    rule is broken. `prologue = TRUE` additionally allows `readBin`, `readRDS`, `file.size` and
    `openssl::sha256`.
  - `.sr_value_plain(x) -> lgl`: TRUE when `x` is recursively only NULL, atomic vectors, factors,
    data.frames, matrices and lists (no functions, environments, formulas, calls or S4).
- Produces (result.R):
  - `SR_EFFECT_COLS <- c("term", "measure", "est", "lo", "hi", "p", "scale", "level", "mcse",
    "role", "conditional", "ci_method", "n", "events")`.
  - `sr_effects_empty() -> data.frame` with those columns and 0 rows. `scale` ∈ `ratio | difference`;
    `role` ∈ `primary | covariate | descriptive`; `conditional` lgl (TRUE for an adjusted OR/HR);
    `measure` ∈ `MD, OR, RR, RD, HR, RMST, RMST_diff, NNT, SMD, coef, risk`.
  - `sr_result_check(presented, route) -> presented`: requires `list(tables, figures, fragments,
    effects)`; `effects` has exactly `SR_EFFECT_COLS`; every `role == "primary"` row has finite
    `est`, `lo`, `hi` with `lo <= est <= hi` (describe-family routes exempt). It adds
    `presented$flags` = data.frame(term, flag, detail):
    - `table2` for every `covariate` row of an `explain-causal` route (spec M1.3);
    - `non_collapsible` for every `conditional` OR/HR row ("conditional; see the marginal RD/RR").

- [ ] **Step 1: Write `test-emit.R`.**
  - For each of `"a b"`, `` "x`); system('calc'); (`" ``, `"new\nline"`, `"\\"`,
    `intToUtf8(0xE9)`: `names(eval(parse(text = paste0("list(", .sr_q(x), " = 1)"))))` equals `x`,
    and `all.names(parse(text = .sr_q(x))[[1]])` has length 1.
  - `.sr_lit()` round-trips `c(1/3, NA, Inf, -0)`, `NA_character_`, `"a\"b\n"`, `TRUE`, `5L`.
  - `.sr_inline(function(x) x + 1, ".h_add")` is identical on two calls, parses, and defines a
    working function.
  - `.sr_emit_check` stops for `system("x")`, `base::system("x")`, `utils::download.file("u")`,
    `stats::shapiro.test(x)` (analysis), `foo::bar()`, `eval(quote(1))`, `do.call("system", list())`,
    and `readRDS("f")` with `prologue = FALSE`; it passes `stats::lm(y ~ x, data = d)` and, with
    `kind = "diagnostic"`, `survival::cox.zph(f)`; it stops for
    `if (survival::cox.zph(f)$table[1, 3] < 0.05) 1` with `kind = "diagnostic"`.
  - `.sr_value_plain(list(a = 1, b = data.frame(x = 1)))` TRUE; `.sr_value_plain(list(f = lm))` and
    `.sr_value_plain(y ~ x)` FALSE.
- [ ] **Step 2: Write `test-result.R`.** With a fake `explain-causal` regression route:
  - a missing effects column, `lo > est`, or an NA `hi` on a primary row → error naming the row;
  - a describe-family route accepts NA CIs on `descriptive` rows;
  - a covariate row gets `table2`; a conditional HR gets `non_collapsible`; a marginal RD
    (`conditional = FALSE`) gets no flag.
- [ ] **Step 3: Run** `"$RS" tools/run_tests.R emit` and `... result`. Expected: FAIL (functions not
  found).
- [ ] **Step 4: Implement** `emit.R` and `result.R` (walk the parse tree recursively over
  `as.list(call)`; a namespaced call has `call[[1]]` of the form `` `::`(pkg, fn) ``).
- [ ] **Step 5: Run** both filters, the whole suite and the lint. Expected: PASS, `LINT OK`.
- [ ] **Step 6: Commit** `routes: emitted-code quoting/checking and the result contract`.

### Task 2: Spec columns, term builder and outcome coding

**Files:** Create `app/R/routes/terms.R`, `tests/testthat/test-terms.R`. Modify `app/global.R`.

**Interfaces:**
- Consumes: `.sr_q`, `.sr_lit`, `.sr_emit_check` (Task 1).
- Spec fields shared by the model routes (each route's `spec_schema` declares the ones it needs):
  - `outcome` (chr); `event_value` (the outcome value that is the event; required for
    logistic/modpois unless the outcome is 0/1 or logical);
  - `time`, `status`, `status_event` (Cox/RMST: the status value meaning "event"; lung uses 2);
  - `exposure` (chr), `exposure_ref` (reference level);
  - `terms`: list of `list(var, type = linear|factor|rcs|quad, ref, knots)`; `knots` is 3–7 or a
    numeric vector of explicit locations;
  - `interactions`: list of length-2 chr vectors naming the exposure or linear/factor term vars;
  - `cluster` (chr or NULL), `se` ∈ `model | robust | cluster`;
  - `layers`, `choices` (as compose.R), `mi`, `boot`, `seed` (int).
- Produces:
  - `sr_spec_cols(spec) -> chr`: unique source columns the spec reads, in the order outcome, time,
    status, exposure, term vars, cluster, strata, `spec$mi$aux`.
  - `sr_terms_code(spec) -> list(setup, design, rhs, coef_map)`:
    - `setup`: code run once on `sr_data` (observed values) that fixes RCS knots
      (`Hmisc::rcspline.eval(x[!is.na(x)], nk = k, knots.only = TRUE)` unless explicit) and factor
      levels (sorted unique non-NA, `ref` first) into `.sr_fixed`;
    - `design`: code defining `.h_design(d)`, which returns the outcome columns plus derived columns
      `t<i>` (linear or factor with fixed levels), `t<i>`, `t<i>_s1..` (RCS basis via
      `Hmisc::rcspline.eval(x, knots = .sr_fixed$...)`, `inclx = TRUE`), `t<i>`, `t<i>_q` (quadratic);
    - `rhs`: right-hand side over the derived names, with `t<a>:t<b>` for interactions;
    - `coef_map`: data.frame(coef, source_var, kind) mapping coefficients back to user names.
  - `sr_outcome_code(spec, kind = c("binary", "continuous", "surv")) -> chr`: code adding `.y` (0/1
    integer via `== <.sr_lit(event_value)>`, or numeric) or `.time` / `.event` to the design frame.

- [ ] **Step 1: Write `test-terms.R`.**
  - `sr_spec_cols` on a Cox spec with cluster and MI aux returns the documented order, no duplicates.
  - On `survival::lung`, an RCS(4) term on `age`: the knots equal
    `Hmisc::rcspline.eval(lung$age, nk = 4, knots.only = TRUE)`; the design has `t1`, `t1_s1`,
    `t1_s2`; `lm(.y ~ <rhs>)` equals a direct fit on `Hmisc::rcspline.eval(age, knots, inclx = TRUE)`
    columns to 1e-10.
  - A factor term with `ref = "2"` puts level `"2"` first; `.h_design` on a subset lacking a level
    keeps all levels.
  - `event_value = 2` on lung `status` gives `.y == (status == 2)`.
  - A hostile name in `terms` produces code that passes `.sr_emit_check(kind = "analysis")`.
  - An interaction involving a `quad` or `rcs` term is refused ("interactions are between the
    exposure and linear or factor terms in P1c").
- [ ] **Step 2: Run** `"$RS" tools/run_tests.R terms`. Expected: FAIL.
- [ ] **Step 3: Implement** `terms.R`.
- [ ] **Step 4: Run** the filter and the whole suite. Expected: PASS.
- [ ] **Step 5: Commit** `routes: spec columns, fixed-knot term builder and outcome coding`.

### Task 3: SAP drafting and the lock

**Files:**
- Create: `app/R/sap/sap.R`, `tests/testthat/test-sap.R`
- Modify: `app/global.R`; `tests/testthat/helper-data.R` (add `.fx_unblind`, `.fx_ready_project`);
  `tests/testthat/test-dq-structure.R` (the throwaway-reducer helper `.st_unblind` at ~43–51, its
  uses at ~172, 266, 312, 357, 366, and the test "the throwaway sap.locked reducer is gone" at
  ~384–385); `tests/testthat/test-profile.R` (~320–326 and ~433–438)

**Interfaces:**
- Consumes: `sr_route_get`, `sr_spec_validate`, `sr_compose_check`, `sr_readiness`, `sr_action`,
  `sr_store_manifest`, `sr_store_hash`, `sr_libsnap_hash`, `sr_mem_estimate`, `sr_project_append`.
- Produces:
  - `SR_DIAG_IDS <- c("ph", "influence", "separation", "few_clusters", "inf_censoring",
    "linearity", "heteroscedasticity")` (Task 11 keeps the same ids).
  - `sr_sap_add(project, id, route, spec, table, label = c("primary", "secondary", "sensitivity"),
    family = NA, parent = NA, addresses = character(), version = NULL) -> project`
    (`sap.analysis_set`). Validates the id, the route, `sr_spec_validate`, `sr_compose_check`
    (refused or unmet → error listing them), that `table` exists and that `parent` exists when given.
    Re-adding an id replaces it. After lock: "the SAP is locked; use sr_analysis_add".
  - `sr_sap_remove(project, id) -> project` (`sap.analysis_removed`), pre-lock only.
  - `sr_sap_family(project, id, members, method = c("holm", "hochberg", "hommel", "bonferroni",
    "BH", "BY", "none")) -> project` (`sap.family_set`). Members must be primary or secondary; a
    sensitivity member is refused ("sensitivity analyses are not an inferential family", spec M1.4).
    An analysis is in at most one family.
  - `sr_sap_rule(project, id, analysis, check, action = c("add_sensitivity", "replace_primary"),
    alternative) -> project` (`sap.rule_set`). `check` ∈ `SR_DIAG_IDS`; `alternative` must be a
    pre-specified sensitivity analysis whose `parent` is `analysis`.
  - `sr_sap_hash(project) -> chr`: SHA-256 of `sr_canonical_json(list(analyses = <pre-specified
    records ordered by id>, families, rules))`.
  - `sr_sap_check(project) -> data.frame(item, status, detail)` — what the lock would refuse:
    - no `primary` analysis → block;
    - per analysed table, each `block` row of `sr_readiness` → block (detail prefixed with the table);
    - each `keep_sensitivity` decision on an analysed table whose finding id is in no sensitivity
      analysis's `addresses` → block ("finding <id> was kept for a sensitivity analysis, but none
      addresses it");
    - `sr_mem_estimate(...)$fits` FALSE for any analysis → block ("too large for an in-memory fit;
      big-data engines arrive in P1e");
    - readiness `warn` rows → warn (do not refuse).
  - `sr_sap_lock(project) -> project` (`sap.locked`): stops listing every block row; payload
    `list(sap_hash, tables = list(<t> = list(store_hash)), libsnap_hash, locked_at)`. The reducer
    sets `state$sap$locked <- TRUE` and `state$sap$lock <- payload`.
  - Reducers for the five `sap.*` event types, registered in this file.
- Produces (helper-data.R):
  - `.fx_unblind(p) -> p`: `p$state$sap <- list(locked = TRUE)` in memory, no event.
  - `.fx_ready_project(df, table = "t", roles, env = parent.frame()) -> project`: writes `df` to a
    CSV, ingests it as `table`, `sr_roles_set(roles)`, `sr_dq_run`, then decides every warn/block
    finding with its suggested action, except `keep_sensitivity`, which it overrides to `recheck`
    with reason `"test fixture"`. Stops printing the readiness table if any row is still `block`.

- [ ] **Step 1: Replace the throwaway reducers.** In `test-dq-structure.R` and `test-profile.R`,
  every `.st_unblind(...)` / throwaway `sap.locked` reducer becomes `.fx_unblind(p)`; delete the
  "throwaway sap.locked reducer is gone" test (with the real reducer registered, the old
  `rm()`-on-defer would delete it). Run `"$RS" tools/run_tests.R dq-structure` and `... profile`.
  Expected: PASS.
- [ ] **Step 2: Write `test-sap.R`** on `.fx_ready_project` (a lung-derived frame) with a registered
  stub route (purpose `explain-causal`, schema `outcome` chr required):
  - `sr_sap_add` → `state$analyses$a1` has `prespecified = TRUE`, the label and the table;
  - an invalid spec, an unknown table, or a compose-refused layer stack (`framework = "bayesian"`
    with `resampling = "bootstrap"`) is refused with the reason;
  - `sr_sap_family` with a sensitivity member is refused; a second family claiming the same analysis
    is refused;
  - `sr_sap_rule` with an alternative whose `parent` differs is refused;
  - `sr_sap_hash` does not depend on the order of independent adds and changes when a spec changes;
  - lock with no primary → error mentioning "primary";
  - a `keep_sensitivity` decision (via `sr_action_decide`) without an addressing analysis → lock
    refused naming the finding; adding a sensitivity analysis with `addresses = <id>` → lock succeeds;
  - after lock: `sr_blind(p)` is FALSE; `state$sap$lock$tables$t$store_hash` equals
    `sr_store_hash(sr_store_manifest(p, "t"))`; `sr_sap_add` and `sr_sap_remove` are refused;
  - close and reopen the project: `state$sap` and `state$analyses` replay `identical()`.
- [ ] **Step 3: Run** `"$RS" tools/run_tests.R sap`. Expected: FAIL.
- [ ] **Step 4: Implement** `sap.R` and the two fixtures.
- [ ] **Step 5: Run** `sap`, then the whole suite (blind/profile/DQ tests must pass with the real
  reducer registered). Expected: PASS.
- [ ] **Step 6: Commit** `sap: SAP drafting events, lock with readiness/sensitivity refusals`.

### Task 4: Extract and run engine

**Files:**
- Create: `app/R/routes/run.R`, `tests/testthat/test-run.R`
- Modify: `app/R/core/exec.R`; `tests/testthat/test-exec.R`; `app/global.R`

**Interfaces:**
- Consumes: Tasks 1–3; `sr_store_view`, `sr_duck_query`, `sr_mem_estimate`, `sr_content_hash`.
- Produces:
  - `sr_exec_cached(project, key_hash, code, inputs = list(), seed, ..., cache_inputs = inputs)`:
    the cache validity hash is `.sr_inputs_hash(cache_inputs)` instead of `inputs`. Default
    behaviour is unchanged.
  - `sr_extract(project, table, cols) -> list(rel, sha, n, cols)`: materialises `cols` of `table`
    via DuckDB with `ORDER BY` every column (byte-stable under `preserve_insertion_order = false`) as
    a data.frame with the store's types, written as `serialize(df, NULL, version = 3)` bytes to
    `data/extracts/<sha>.rds` (`rel` relative to the project dir). An existing file with the same
    sha is reused.
  - `.sr_run_prologue(ex) -> chr`: fixed template that reads `file.path(sr_project_dir, <rel>)` with
    `readBin`, stops with `"extract changed"` unless `openssl::sha256(bytes)` equals `<sha>`, and
    binds `sr_data <- unserialize(bytes)`. Checked with `.sr_emit_check(prologue = TRUE)`.
  - `sr_run(project, analysis_id, timeout = 3600) -> project`. In order:
    1. the analysis exists; `sr_blind(project)` → stop "the SAP is not locked";
    2. the table's current store hash equals `state$sap$lock$tables[[t]]$store_hash`, else stop
       "table '<t>' changed after the SAP lock";
    3. route, `sr_spec_validate`, `sr_compose_check` (refused/unmet → stop);
    4. `sr_mem_estimate(project, table, sr_spec_cols(spec), method)$fits` FALSE → stop;
    5. `ex <- sr_extract(...)`; `body <- sr_compile(route, spec)`; `.sr_emit_check(body, "analysis")`;
    6. `key <- sr_content_hash(ex$sha, list(route = "<id>@<version>", spec = spec), spec$seed)`;
    7. `res <- sr_exec_cached(project, key, paste(prologue, body, sep = "\n"), inputs =
       list(sr_project_dir = project$paths$dir), seed = spec$seed, cache_inputs = list(extract =
       ex$sha), timeout = timeout)`;
    8. on ok: `.sr_value_plain(res$value)` else fail "not plain data";
       `sr_result_check(route$present(res$value), route)`;
    9. append `run.completed` with `list(analysis, key, extract_sha, ok, cached, error, elapsed,
       lib_changed)`, where `lib_changed` = `sr_libsnap_hash() != lock$libsnap_hash`.
    A failed fit still appends `run.completed` with `ok = FALSE` and returns the project.
  - `sr_result(project, analysis_id)`: the presented result of the latest ok run, or an error
    "analysis '<id>' has not run successfully".
  - Reducer `run.completed` → `state$runs[[analysis]]`.

- [ ] **Step 1: Extend `test-exec.R`.** With `cache_inputs = list(k = 1)` and two different
  `inputs`, the second call is a hit; the default still misses on changed inputs.
- [ ] **Step 2: Write `test-run.R`** with a stub route whose body is
  `list(n = nrow(sr_data), m = mean(sr_data[[<.sr_q(outcome)>]]))` and whose present returns a
  descriptive effects row:
  - before lock: stops with "the SAP is not locked"; `data/extracts` and the fits cache are empty;
  - after lock: `state$runs$a1$ok` is TRUE and `sr_result(p, "a1")` holds the right mean;
  - extract bytes are identical across two `sr_extract` calls and across a reopened project;
  - after re-ingesting changed data into the same table: stops with "changed after the SAP lock"
    and appends no `run.completed`;
  - **Review Focus 5:** copy the project folder to a new tempdir, open it there, run again →
    `cached` TRUE with an identical value; flip one byte of the extract and clear the fits cache →
    `ok = FALSE` with an error containing "extract changed";
  - a body that errors → `ok = FALSE`, error recorded, project returned;
  - a body returning a function → `ok = FALSE` with "not plain data".
- [ ] **Step 3: Run** `"$RS" tools/run_tests.R run` and `... exec`. Expected: FAIL (new tests).
- [ ] **Step 4: Implement** the `exec.R` change and `run.R`.
- [ ] **Step 5: Run** both filters and the whole suite. Expected: PASS.
- [ ] **Step 6: Commit** `routes: hashed column extract and the locked, cached run engine`.

### Task 5: Table 1 route

**Files:** Create `app/R/routes/table1.R`, `tests/testthat/test-table1.R`. Modify `app/global.R`.

**Interfaces:**
- Consumes: Tasks 1, 2, 4.
- Produces: route `table1@1`, family `describe`, purpose `describe`; schema `by` (chr, required),
  `vars` (chr, required), `nonnormal` (chr, optional: summarised by median [IQR]), `seed`. Estimand:
  population "analysed sample", summary "baseline characteristics by group".
  - Continuous: mean (SD) or median [IQR]; categorical: n (%) per level; missing n per var.
  - SMD: continuous `(m1 - m0) / sqrt((s1^2 + s0^2) / 2)`; binary
    `(p1 - p0) / sqrt((p1(1 - p1) + p0(1 - p0)) / 2)`; > 2 levels: Yang–Dalton multivariate SMD.
    Two-level CI: `d ± 1.96 sqrt((n1 + n0) / (n1 n0) + d^2 / (2 (n1 + n0)))`; the multi-level SMD
    has CI `NA`, `ci_method = "none"`.
  - All effects rows are `role = "descriptive"`. With > 2 groups: SMD of each group versus the first
    level of `by`.
  - **No p-values anywhere** (spec C). The effects `p` column is `NA`.

- [ ] **Step 1: Write `test-table1.R`** on a seeded synthetic frame:
  - the continuous SMD equals the hand formula to 1e-12 and its CI the documented approximation;
  - binary and multi-level SMDs equal hand computations (Yang–Dalton via its matrix formula in the
    test);
  - no column of any returned table matches `(?i)^p(\.|_)?val` or `^p$`, except effects `p`, all NA;
  - a three-level `by` gives two SMD columns;
  - end to end through `sr_run` on a locked `.fx_ready_project`.
- [ ] **Step 2: Run** `"$RS" tools/run_tests.R table1`. Expected: FAIL.
- [ ] **Step 3: Implement** `table1.R` (compile emits base-R summaries; present formats with
  `sr_fmt_num` / `sr_fmt_ci`).
- [ ] **Step 4: Run** the filter, the whole suite and the lint. Expected: PASS, `LINT OK`.
- [ ] **Step 5: Commit** `routes: Table 1 with SMD and CI, no p-values`.

### Task 6: Linear regression route and SE helpers

**Files:** Create `app/R/routes/se.R`, `app/R/routes/lm.R`, `tests/testthat/test-lm.R`. Modify
`app/global.R`.

**Interfaces:**
- Consumes: Tasks 1, 2, 4.
- Produces:
  - `.sr_h_vcov(fit, se = c("model", "robust", "cluster"), cluster = NULL)`, inlined as `.h_vcov`:
    model → `stats::vcov`; robust → `sandwich::vcovHC(fit, type = "HC3")` for lm, `"HC0"` for glm;
    cluster → `sandwich::vcovCL(fit, cluster = cluster, type = "HC1")`.
  - `.sr_h_coef_table(est, V, df = Inf, level = 0.95)`, inlined as `.h_coef_table`: data.frame(term,
    est, se, lo, hi, p) with t (finite df) or z intervals and two-sided `p`.
  - Route `lm@1`, family `regression`, purpose `explain-causal`, and a twin `lm_describe@1` (purpose
    `describe`) sharing compile/present. Schema: `outcome`, `exposure`, `terms`, `interactions`, `se`,
    `cluster`, `marginal` (lgl, default FALSE; Task 14 uses it), `standardize_over`, `layers`, `seed`.
    Estimand: population "complete cases" or "all analysed (MI)", variables `c(exposure, outcome)`,
    intercurrent "not applicable", summary "adjusted mean difference".
  - Effects: the exposure row(s) `role = "primary"`, measure `MD`, scale `difference`; others
    `role = "covariate"`, measure `coef`. `se = "cluster"` requires `cluster` and
    `layers$clustered = TRUE` (validation).
  - Figure data: fitted, residuals, standardised residuals, leverage, Cook's D (for Task 12).

- [ ] **Step 1: Write `test-lm.R`.**
  - On `mtcars` (`mpg ~ am + wt + hp`, exposure `am`): model-SE est/lo/hi equal `confint(lm())` to
    1e-10; robust equals `sandwich::vcovHC(type = "HC3")`; cluster on `cyl` equals
    `sandwich::vcovCL(cluster = ~cyl, type = "HC1")`.
  - Covariate rows carry `table2` after `sr_result_check`.
  - **Review Focus 1:** outcome named `` "x`); system('calc'); (`" `` and exposure `"a b"`, through
    `.fx_ready_project` → lock → `sr_run`: `ok = TRUE`; est equals the fit on renamed columns to
    1e-10; the emitted code's `all.names()` contains no `system`.
  - `se = "cluster"` without `cluster` is refused at `sr_sap_add`.
- [ ] **Step 2: Run** `"$RS" tools/run_tests.R lm`. Expected: FAIL.
- [ ] **Step 3: Implement** `se.R` and `lm.R`.
- [ ] **Step 4: Run** the filter, the whole suite and the lint. Expected: PASS, `LINT OK`.
- [ ] **Step 5: Commit** `routes: linear regression with model/robust/cluster SE`.

### Task 7: Logistic, modified Poisson and g-computation

**Files:** Create `app/R/routes/marginal.R`, `app/R/routes/glm.R`, `tests/testthat/test-marginal.R`,
`tests/testthat/test-glm.R`. Modify `app/global.R`.

**Interfaces:**
- Consumes: Tasks 1, 2, 4, 6 (`.h_vcov`, `.h_coef_table`).
- Produces (marginal.R, each inlined as `.h_<name>`):
  - `.sr_h_gcomp(fit, data, exposure_col, levels, V, over = NULL) -> list(p1, p0, rd, rd_se,
    log_rr, log_rr_se)`: standardised means with the exposure set to each level over `over`
    (default `data`); gradients `colMeans(mu.eta(eta_a) * X_a)`; delta-method variances `g' V g`;
    the log RR gradient is `g1/p1 - g0/p0`. Works for any glm or lm (identity link → mean difference).
  - `.sr_h_nnt(rd, lo, hi) -> list(nnt, lo, hi, label)` (Altman 1998): CI excluding 0 → `NNTB` or
    `NNTH` with bounds `1/hi .. 1/lo`; CI crossing 0 → label
    `"NNTB <1/|hi|> to infinity to NNTH <1/|lo|>"`.
  - `.sr_h_wilson(x, n, level = 0.95) -> c(est, lo, hi)`.
- Produces (glm.R):
  - Route `logistic@1` (purpose `explain-causal`; twin `logistic_describe@1`). Schema adds
    `event_value`, `marginal` (lgl, default TRUE), `standardize_over` ∈ `analysis | all`,
    `penalty` ∈ `none | firth` (Firth via `logistf::logistf` with profile-penalised-likelihood CIs;
    no marginal effects under Firth). Effects: conditional OR of the exposure (`conditional = TRUE`),
    marginal RD and RR (`conditional = FALSE`), NNT from the RD; crude risk per exposure level with
    Wilson CI as `descriptive` rows.
  - Route `modpois@1`: `glm(family = poisson(link = "log"))` on the 0/1 outcome; `se` must be
    `robust` or `cluster` (`model` refused: "modified Poisson needs a robust or cluster SE",
    Zou 2004). Effect RR (collapsible, so no `non_collapsible` flag) plus marginal RD via `.h_gcomp`.
  - `standardize_over = "all"`: also loads the rows dropped only for a missing *outcome*
    (covariates complete) and standardises over analysed + those rows (NHEFS: fit on 1566,
    standardise over 1629). Any other missingness under `cc` is dropped from both.

- [ ] **Step 1: Write `test-marginal.R`.**
  - On a seeded logistic sample (n = 2000, binary exposure, two covariates): `p1`, `p0`, RD and RR
    equal a hand computation with `predict(type = "response")` on counterfactual copies to 1e-12;
    the delta-method RD SE is within 5% of a seeded 2000-rep nonparametric bootstrap SE.
  - `.h_nnt(0.1, 0.05, 0.15)` → NNTB 10 with bounds (6.67, 20); `.h_nnt(0.05, -0.02, 0.12)` →
    label containing "NNTB 8.3 to infinity to NNTH 50".
  - `.h_wilson(0, 10)` → (0, 0, 0.2775) to 1e-4; `.h_wilson(5, 10)` → (0.5, 0.2366, 0.7634).
- [ ] **Step 2: Write `test-glm.R`.**
  - logistic OR and CI equal `glm(binomial)` + `confint.default` to 1e-10 (model SE);
  - modpois RR equals `exp(coef)` and the robust CI equals `sandwich::vcovHC(type = "HC0")`;
    `se = "model"` is refused;
  - `penalty = "firth"` equals `logistf::logistf` estimates and profile CIs to 1e-8;
  - after `sr_result_check`, the conditional OR row has `non_collapsible`; RR and RD rows do not;
  - `event_value = "dead"` on a character outcome codes correctly; no `event_value` on a non-0/1
    outcome is refused at validation.
- [ ] **Step 3: Run** `"$RS" tools/run_tests.R marginal` and `... glm`. Expected: FAIL.
- [ ] **Step 4: Implement** `marginal.R` and `glm.R`.
- [ ] **Step 5: Run** both filters, the whole suite and the lint. Expected: PASS, `LINT OK`.
- [ ] **Step 6: Commit** `routes: logistic and modified Poisson with g-computation RD/RR and NNT`.

### Task 8: Cox, Kaplan–Meier and RMST

**Files:** Create `app/R/routes/cox.R`, `app/R/routes/rmst.R`, `tests/testthat/test-cox.R`,
`tests/testthat/test-rmst.R`. Modify `app/global.R`.

**Interfaces:**
- Consumes: Tasks 1, 2, 4.
- Produces:
  - Route `cox@1` (purpose `explain-causal`; twin `cox_describe@1`). Schema: `time`, `status`,
    `status_event`, `exposure`, `terms`, `interactions`, `strata` (chr, optional), `cluster`,
    `ties` ∈ `efron | breslow` (default efron), `layers`, `seed`. Code:
    `survival::coxph(survival::Surv(.time, .event) ~ <rhs> [+ strata(..)] [+ cluster(..)], ties = ..)`.
    `layers$clustered = TRUE` requires `cluster` and always uses `cluster()` (robust SE). Effects:
    HR rows (`conditional = TRUE` when adjusted), with n and events.
  - KM figure data (both routes): `survival::survfit(Surv(.time, .event) ~ exposure)` as
    data.frame(strata, time, surv, lower, upper, n.risk, n.event, n.censor), plus a risk table at
    5–7 times from `pretty()` of the time range.
  - Route `rmst@1`, family `survival`, purpose `explain-causal`. Schema adds `tau` (num, required,
    finite, > 0; validated). Code: `summary(survfit(...), rmean = tau)$table` per exposure level;
    effect `RMST_diff` = level minus reference with SE `sqrt(se1^2 + se0^2)`, scale `difference`;
    each group's RMST as `descriptive` rows.
  - **Horizon check (Review Focus 3):** the emitted code stops with
    `"tau = <tau> exceeds the follow-up of group '<g>' (max time <m>)"` when `tau` exceeds any group's
    maximum observed time, and adds a `warn` note when fewer than 10 subjects are at risk at `tau` in
    any group.

- [ ] **Step 1: Write `test-cox.R`.** On `survival::lung` (`status_event = 2`, exposure `sex`):
  - coef −0.5310, SE 0.1672, HR 0.588 (0.424–0.816) to the printed digits, and equal to direct
    `coxph(Surv(time, status) ~ sex, lung)` to 1e-8;
  - `cluster = "inst"` with `layers$clustered = TRUE` (NA `inst` rows dropped by cc): robust SE
    equals direct `coxph(... + cluster(inst))` to 1e-8;
  - `strata = "ph.ecog"`: HR equals direct `coxph(... + strata(ph.ecog))`;
  - `survival::gbsg`, `Surv(rfstime, status) ~ hormon + age + meno + size + grade + nodes + pgr +
    er`: every HR equals direct `coxph` to 1e-8;
  - KM figure data equals `summary(survfit())` at the risk-table times.
- [ ] **Step 2: Write `test-rmst.R`.** lung by sex, `tau = 365`:
  - RMST 241.4951 (SE 10.35823) for sex 1 and 297.4654 (SE 10.79132) for sex 2, to 1e-4;
    difference 55.9703, SE 14.9581, CI `55.9703 ± 1.959964 × 14.9581`;
  - `tau = 5000` → `ok = FALSE` with an error naming tau and the group;
  - `tau = 0`, `-1`, `Inf` → refused at `sr_sap_add`.
- [ ] **Step 3: Run** `"$RS" tools/run_tests.R cox` and `... rmst`. Expected: FAIL.
- [ ] **Step 4: Implement** `cox.R` and `rmst.R`.
- [ ] **Step 5: Run** both filters, the whole suite and the lint. Expected: PASS, `LINT OK`.
- [ ] **Step 6: Commit** `routes: Cox with KM data, and RMST with a horizon check`.

### Task 9: Multiple imputation and Rubin pooling

**Files:** Create `app/R/impute/pool.R`, `app/R/impute/impute.R`, `tests/testthat/test-pool.R`,
`tests/testthat/test-impute.R`. Modify `app/global.R`; the compile functions of `lm`, `logistic`,
`modpois`, `cox`, `rmst` (dispatch to the MI wrapper when `layers$missing == "mi"`).

**Interfaces:**
- Consumes: Tasks 2, 6–8.
- Produces:
  - `.sr_h_rubin(est_list, vcov_list, dfcom) -> data.frame(term, est, se, lo, hi, p, df, riv,
    lambda, fmi, mcse)`, inlined as `.h_rubin`:
    `Qbar`, `Ubar`, `B`, `T = Ubar + (1 + 1/m) B`; `riv = (1 + 1/m) B / Ubar`;
    `lambda = (1 + 1/m) B / T`; `df_old = (m - 1) / lambda^2`;
    `df_obs = (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)`;
    `df = df_old * df_obs / (df_old + df_obs)` (Barnard–Rubin; `df_old` when `dfcom` is Inf);
    `fmi = (riv + 2 / (df + 3)) / (riv + 1)` (as `mice::pool`); `mcse = sqrt(B / m)`.
    Stops with "design differs across imputations" when coefficient names differ.
  - Pooling scale: log OR / log RR / log HR (exponentiated after pooling); identity for MD, RD and
    RMST difference. Marginal RD/RR under MI: g-computation per imputation, then pooled (RD on
    identity, RR on log).
  - `sr_mi_code(spec, fit_code) -> chr`, the emitted wrapper:
    1. `setup` from `sr_terms_code` on `sr_data` (knots and levels fixed **before** `mice`);
    2. imputation data: raw analysis columns, factors with fixed levels, the outcome; for survival
       the event indicator plus `mice::nelsonaalen(d, .time, .event)` (time itself not a predictor);
       `spec$mi$aux` as predictors only;
    3. `mice::mice(..., m, maxit = spec$mi$maxit (default 10), printFlag = FALSE)` under the
       preamble's stream; methods `pmm` numeric, `logreg` binary, `polyreg` unordered factor;
    4. per completed dataset: `.h_design` → fit → est/vcov;
    5. `.h_rubin` with `dfcom = n - p` (lm) or `Inf` (glm/Cox).
    Derived terms (RCS, quadratic, interactions) are computed **after** imputation from imputed raw
    values with the fixed knots ("impute, then transform"), stated as an assumption in the fragment
    inputs.
  - `spec$mi$m`: an integer ≥ 2, or `"auto"`: pilot `m = 20`, then
    `fmi_up = plogis(qlogis(fmi_max) + 1.96 * sqrt(2 / 20))` and
    `m = min(200, max(20, ceiling(1 + 0.5 * (fmi_up / 0.05)^2)))` (von Hippel 2020), where `fmi_max`
    is the largest FMI among primary-role terms; the final run reimputes with that `m`. The value
    returns `m_pilot`, `fmi_max`, `m_final`, `m_capped`.
  - `sr_missing_summary(project, table, cols) -> list(columns = data.frame(column, n_missing,
    pct_missing), patterns)`: the 10 most frequent missingness patterns (descriptive only).

- [ ] **Step 1: Write `test-pool.R`.** `imp <- mice::mice(mice::nhanes, m = 5, seed = 1,
  printFlag = FALSE)`, `fit <- with(imp, lm(chl ~ age + bmi))`: `.h_rubin` on the per-imputation
  `coef`/`vcov` equals `summary(mice::pool(fit))` estimate and std.error to 1e-10, df to 1e-8 and
  `pool(fit)$pooled$fmi` to 1e-10; mismatched names stop with "design differs across imputations".
- [ ] **Step 2: Write `test-impute.R`.**
  - Through `sr_run` on an `nhanes`-derived project (`lm`, MI, `m = 5`, seed 1): pooled est/SE equal
    a direct `mice` run under the same `sr_rng_preamble(1)` to 1e-10;
  - **Review Focus 4:** RCS(3) on `bmi` plus a factor `hyp` with a 2-case level: the knots in the
    value equal the observed-data knots, and every imputation's coefficient names are identical
    (the value exposes them);
  - `m = "auto"` returns `m_pilot = 20` and `m_final` equal to the formula applied to the returned
    `fmi_max` (recomputed in the test);
  - a Cox MI run lists the Nelson–Aalen column among the imputation predictors;
  - `sr_missing_summary` counts equal `colSums(is.na())`.
- [ ] **Step 3: Run** `"$RS" tools/run_tests.R pool` and `... impute`. Expected: FAIL.
- [ ] **Step 4: Implement** `pool.R`, `impute.R` and the route dispatch.
- [ ] **Step 5: Run** both filters, the whole suite and the lint. Expected: PASS, `LINT OK`.
- [ ] **Step 6: Commit** `impute: mice with fixed design, Rubin/Barnard-Rubin pooling, auto m`.

### Task 10: Bootstrap layer

**Files:** Create `app/R/resample/boot.R`, `tests/testthat/test-boot.R`. Modify `app/global.R`; the
model routes' compile functions (wrap when `layers$resampling == "bootstrap"`).

**Interfaces:**
- Consumes: Tasks 2, 6–9; `spec$boot = list(B = 2000, type = percentile|bca|normal)`;
  `spec$choices$mi_x_boot` ∈ `boot_then_impute | impute_then_boot` (compose.R requires it).
- Produces (each inlined as `.h_<name>`):
  - `.sr_h_boot(data, stat, B, cluster = NULL, chunk = 100L) -> matrix (B × k)`: resamples rows, or
    whole clusters when `cluster` is given; replicate chunk `j` runs under the `j`-th
    `parallel::nextRNGStream` of the stream current at entry, so the matrix depends only on `seed`
    and `chunk`, never on worker count (P1d).
  - `.sr_h_boot_ci(t0, t, type, jack = NULL, level = 0.95) -> list(lo, hi, type_used,
    fallback_reason, mcse_lo, mcse_hi)`:
    - percentile and normal: as `boot::boot.ci(type = "perc" / "norm")` (normal is bias-corrected);
    - BCa: `z0 = qnorm(mean(t < t0))`; acceleration `a = sum(d^3) / (6 * sum(d^2)^1.5)` with
      `d = mean(jack) - jack`; adjusted percentiles as `boot.ci(type = "bca")`;
      **falls back to percentile** with `fallback_reason` when `z0` is infinite, `a` is NA, or an
      adjusted index falls outside `1..B`;
    - endpoint MCSE: split the replicates into 10 consecutive batches, take the endpoint in each,
      `sd / sqrt(10)`.
  - Jackknife for BCa: over units (or clusters) when there are ≤ 1000, else over 1000 seeded groups
    (grouped jackknife), recorded in the value.
  - MI × bootstrap: `impute_then_boot` = bootstrap within each completed dataset, then Rubin with
    the bootstrap variances; `boot_then_impute` = impute within each bootstrap sample (`spec$mi$m`,
    default 5), percentile CI of the replicate means (Schomaker & Heumann 2018). Bootstrap effects
    carry `ci_method = "boot_<type>"` and `mcse = max(mcse_lo, mcse_hi)`.

- [ ] **Step 1: Write `test-boot.R`.**
  - Mean of a seeded exponential sample (n = 50, B = 2000): percentile, normal and BCa endpoints equal
    `boot::boot.ci(type = c("perc", "norm", "bca"))` on a `boot` object carrying the **same** replicate
    matrix and the same jackknife influence values (`L = boot::empinf(type = "jack")`) to 1e-8;
  - a statistic constant across replicates (`z0 = Inf`) → percentile with a fallback reason;
  - `B = 20` on a skewed statistic with an out-of-range adjusted index → fallback;
  - the replicate matrix is identical across two calls with the same seed and chunk;
  - cluster bootstrap resamples whole clusters (each replicate contains every row of each drawn
    cluster);
  - endpoint MCSE is positive and the ratio MCSE(B = 4000) / MCSE(B = 1000) lies in 0.3–0.8;
  - through `sr_run`: `lm` with `resampling = "bootstrap"`, `type = "bca"`, and an MI × bootstrap
    run under each `mi_x_boot` choice both return finite CIs and record the choice.
- [ ] **Step 2: Run** `"$RS" tools/run_tests.R boot`. Expected: FAIL.
- [ ] **Step 3: Implement** `boot.R` and the route wrapping.
- [ ] **Step 4: Run** the filter, the whole suite and the lint. Expected: PASS, `LINT OK`.
- [ ] **Step 5: Commit** `resample: unit/cluster bootstrap with percentile/BCa/normal CIs and MCSE`.

### Task 11: Diagnostics with effect-size verdicts

**Files:** Create `app/R/diagnostics/checks.R`, `tests/testthat/test-diagnostics.R`. Modify
`app/global.R`; `app/R/sap/sap.R` (`SR_DIAG_IDS` derives from the registry, same ids).

**Interfaces:**
- Consumes: Tasks 1, 2, 4, 6–8; `.sr_emit_check(kind = "diagnostic")`.
- Produces:
  - `sr_diag_checks() -> data.frame(id, routes, metric, warn, fail, sensitivity)` with these pinned
    rows (the statistic in brackets is reported, never used for the verdict):

| id | routes | metric | warn | fail | sensitivity node on fail |
|---|---|---|---|---|---|
| `ph` | cox | \|log HR(early) − log HR(late)\| for the exposure, split at the median event time [cox.zph] | ≥ log 1.25 | ≥ log 1.5 | `rmst` with the pre-specified `tau`, else the 75th centile of event times |
| `influence` | lm, logistic, modpois | max Cook's D | > 0.5 | > 1 | same route without the rows with Cook's D > 1 |
| `influence` | cox | max \|scaled dfbeta\| of the exposure | ≥ 0.5 | ≥ 1 | same route without the flagged rows |
| `separation` | logistic | refit with `maxit = 100, epsilon = 1e-12`: some \|β\| grows by > 1 and some fitted p is within 1e-8 of 0 or 1 | — | yes | `logistic` with `penalty = "firth"` |
| `few_clusters` | any with `cluster` | number of clusters | < 50 | < 30 | same route, cluster bootstrap (B = 2000, percentile) |
| `inf_censoring` | cox, rmst | max \|SMD\| of baseline terms, censored before median follow-up vs not | > 0.1 | > 0.2 | none in P1c: a `limitation` note (IPCW is P9) |
| `linearity` | lm, logistic, modpois, cox | max \|linear − RCS(4) partial prediction\| over the 5th–95th centile, in outcome SD (lm) or on the linear-predictor scale | > 0.1 | > 0.25 | same route with that term as RCS(4) |
| `heteroscedasticity` | lm with `se = "model"` | robust SE / model SE of the exposure | > 1.2 | > 1.5 | same route with `se = "robust"` |

  - `sr_diag_compile(check_id, analysis) -> chr`: emitted code (kind `diagnostic`) returning
    `list(value, verdict ∈ ok|warn|fail, detail, plot_data, reported)`.
  - `sr_diag_run(project, analysis_id, checks = NULL) -> project`: needs an ok run of the analysis;
    runs each applicable check via `sr_exec_cached` (same extract and prologue as Task 4, key over
    `list(check, route, spec)`), and appends `diag.completed` with `list(analysis, check, verdict,
    value, warn, fail, detail, key)`; reducer → `state$diag[[analysis]][[check]]`. It does **not**
    add analyses (Task 13 does).

- [ ] **Step 1: Write `test-diagnostics.R`** with seeded generators defined in the test:
  - exposure HR 3 before t = 1 and 0.8 after → `ph` fail; constant HR 2 → ok;
  - a planted point with Cook's D > 1 in an lm → `influence` fail;
  - a perfectly separating covariate → `separation` fail; overlapping data → ok;
  - 12 clusters → `few_clusters` fail; 60 → ok;
  - errors with sd ∝ x² and `se = "model"` → `heteroscedasticity` fail; homoscedastic → ok;
  - a quadratic truth fitted linearly → `linearity` fail;
  - n = 200000 with HR 1.02 early vs 1.00 late → `ph` ok while the reported cox.zph statistic has
    p < 0.05 (the effect size decides);
  - every emitted diagnostic passes `.sr_emit_check(kind = "diagnostic")`; no emitted analysis body
    from Tasks 5–10 contains any diagnostic call.
- [ ] **Step 2: Run** `"$RS" tools/run_tests.R diagnostics`. Expected: FAIL.
- [ ] **Step 3: Implement** `checks.R`.
- [ ] **Step 4: Run** the filter, the whole suite and the lint. Expected: PASS, `LINT OK`.
- [ ] **Step 5: Commit** `diagnostics: effect-size verdicts with reported tests and plot data`.

### Task 12: Figures

**Files:** Create `app/R/diagnostics/plots.R`, `tests/testthat/test-plots.R`. Modify `app/global.R`.

**Interfaces:**
- Consumes: figure data from Tasks 5–8 and `plot_data` from Task 11.
- Produces:
  - `sr_plot(data, kind) -> ggplot`, `kind` ∈ `km` (with a number-at-risk strip), `forest` (log axis
    for `scale == "ratio"`, reference line at 1 or 0), `resid_panel` (residual vs fitted, QQ,
    scale-location, leverage/Cook's via facets over one long frame), `schoenfeld`, `loglog`, `smd`
    (Table 1 dot plot with ±0.1 lines), `linearity` (linear vs RCS curves).
  - `sr_figure_save(plot, path, width_mm = 170, height_mm = 120, dpi = 300) -> list(path, sha)`:
    `ragg::agg_png`, white background, default family with an Arial fallback; atomic write.
  - Plots read only the data frames given, never model objects or the store.

- [ ] **Step 1: Write `test-plots.R`.** For each kind, a fixture frame → `sr_plot` returns a
  `ggplot`; `sr_figure_save` writes a file starting with bytes `89 50 4E 47`; IHDR width/height equal
  `round(width_mm / 25.4 * dpi)` × `round(height_mm / 25.4 * dpi)`; saving twice gives the same sha;
  the forest plot of ratio rows uses a log10 x scale.
- [ ] **Step 2: Run** `"$RS" tools/run_tests.R plots`. Expected: FAIL.
- [ ] **Step 3: Implement** `plots.R`.
- [ ] **Step 4: Run** the filter and the whole suite. Expected: PASS.
- [ ] **Step 5: Commit** `diagnostics: ggplot figures and deterministic ragg export`.

### Task 13: Sensitivity nodes, deviations and multiplicity

**Files:** Create `app/R/sap/deviations.R`, `tests/testthat/test-deviations.R`. Modify `app/global.R`.

**Interfaces:**
- Consumes: Tasks 3, 4, 11.
- Produces:
  - `sr_analysis_add(project, id, route, spec, table, label = c("exploratory", "post_hoc",
    "sensitivity"), reason, parent = NA, trigger = NA) -> project` (`analysis.added`): post-lock
    only; `reason` required; `prespecified = FALSE`.
  - `sr_diag_apply(project, analysis_id) -> project`, for each `fail` in `state$diag[[analysis_id]]`:
    - a matching SAP rule (`analysis`, `check`): append `sap.rule_fired`; for `replace_primary` its
      reducer relabels the alternative `primary` and the original `sensitivity` (both stay
      `prespecified = TRUE`); for `add_sensitivity` nothing is added (already pre-specified);
    - no rule: `analysis.added` with id `<analysis>_<check>`, label `sensitivity`,
      `prespecified = FALSE`, `parent = analysis_id`, `trigger = check`, spec from the check's
      `sensitivity` column, `reason = "<check> verdict fail: <detail>"`;
    - never changes the primary without a rule (spec principle 2, M1.1). An `inf_censoring` fail
      records a `limitation` under `state$diag` instead.
  - `sr_deviations(project) -> data.frame(analysis, label, kind, reason, trigger)`, `kind` ∈
    `post_hoc | exploratory | unplanned_sensitivity | rule_fired | lib_changed`.
  - `sr_family_adjust(project, family_id) -> data.frame(analysis, term, p, p_adj, method)`: the
    primary-role effects row of each member's latest ok run, adjusted with `stats::p.adjust(method)`.
    Members not yet run → error naming them.

- [ ] **Step 1: Write `test-deviations.R`** on a locked project with the Task 11 non-PH generator
  (copied into the test):
  - no rule: `sr_diag_run` + `sr_diag_apply` → primary unchanged; a new analysis `<id>_ph`
    (label sensitivity, `prespecified = FALSE`, trigger `ph`); `sr_deviations` lists it as
    `unplanned_sensitivity`;
  - with a pre-lock `replace_primary` rule to a pre-specified RMST alternative: after apply, RMST is
    `primary` and Cox `sensitivity`; `sr_deviations` shows `rule_fired` and no `unplanned_sensitivity`;
  - `sr_analysis_add` before lock → refused; after lock without reason → refused;
  - `sr_family_adjust` on members with `p` 0.01 and 0.04: `holm` → (0.02, 0.04), `BH` → (0.02, 0.04),
    each equal to `p.adjust`;
  - a run with a different library hash (set `state$sap$lock$libsnap_hash` in memory) shows
    `lib_changed`.
- [ ] **Step 2: Run** `"$RS" tools/run_tests.R deviations`. Expected: FAIL.
- [ ] **Step 3: Implement** `deviations.R`.
- [ ] **Step 4: Run** the filter, the whole suite and the lint. Expected: PASS, `LINT OK`.
- [ ] **Step 5: Commit** `sap: sensitivity nodes from diagnostics, deviations, multiplicity families`.

### Task 14: Textbook reproductions

**Files:** Create `tests/testthat/test-textbook.R`. Modify `app/R/routes/lm.R` if the `marginal`
option needs finishing.

**Interfaces:**
- Consumes: the public path only: `.fx_ready_project` → `sr_sap_add` → `sr_sap_lock` → `sr_run` →
  `sr_result`.

- [ ] **Step 1: Write `test-textbook.R`.**
  - **lung:** HR 0.588 (0.424–0.816) and the RMST difference 55.9703 through the public path;
    `sr_fmt_ci` reads `0.59 (0.42, 0.82)`.
  - **GBSG:** the Task 8 multivariable Cox equivalence through the public path.
  - **mice::nhanes:** `lm(chl ~ age + bmi)` under MI (`m = 5`, seed 1) equals `mice::pool` (Task 9).
  - **NHEFS** (Hernán & Robins ch. 13, standardisation), `skip_if(Sys.getenv("SR_NHEFS_CSV") == "")`:
    `lm` with outcome `wt82_71`, exposure `qsmk`, terms `sex`, `race`, `education`, `exercise`,
    `active` (factor), `age`, `smokeintensity`, `smokeyrs`, `wt71` (quad), interaction
    `qsmk × smokeintensity`; `marginal = TRUE`, `standardize_over = "all"` (fit on 1566, standardise
    over 1629); bootstrap percentile B = 2000, seed 1. Expected: |est − 3.52| < 0.01; CI endpoints
    within 0.15 of (2.6, 4.5).
- [ ] **Step 2: Run** `"$RS" tools/run_tests.R textbook`. Expected: PASS for lung, GBSG, nhanes;
  NHEFS skipped unless the env var is set. Run once with `SR_NHEFS_CSV` pointing at a copy of the
  book's CSV kept outside the repo, and note the values for `docs/validation.md` (Task 15).
- [ ] **Step 3: Commit** `tests: textbook reproductions (lung, GBSG, nhanes, NHEFS when supplied)`.

### Task 15: CI-coverage simulation

**Files:** Create `app/R/resample/coverage.R`, `tools/coverage_sim.R`,
`tests/testthat/test-coverage-smoke.R`, `docs/validation.md`. Modify `app/global.R`.

**Interfaces:**
- Consumes: the route compile functions, evaluated **in-process** with `sr_data` bound and the RNG
  preamble applied (no callr, no project).
- Produces:
  - `sr_coverage_cell(cell, reps, n = 200L, seed = 1L, B = NULL) -> list(cell, reps, coverage, mcse,
    pass, bias, mean_width)`, with `mcse = sqrt(coverage * (1 - coverage) / reps)` and
    `pass = abs(coverage - 0.95) < 3 * mcse`. Rep `r` uses the `r`-th L'Ecuyer stream of `seed`.
  - Cells (true parameter known from the generator):

| cell | generator | analysis | reps |
|---|---|---|---|
| `cc` | linear, MCAR 20% in a covariate | lm, complete case | 1000 |
| `mi_mar` | linear, MAR 30% in a covariate (depends on outcome and another covariate) | lm, MI m = 20 | 500 |
| `boot` | logistic, no missing | marginal RD, bootstrap percentile B = 500 | 500 |
| `boot_mi` | linear, MAR 30% | lm, MI m = 10 × bootstrap B = 200, `boot_then_impute` | 500 |
| `cluster_robust` | linear, 50 clusters, ICC 0.2 | lm, `se = "cluster"` | 1000 |
| `cluster_boot` | linear, 50 clusters, ICC 0.2 | lm, cluster bootstrap percentile B = 500 | 500 |

  - `tools/coverage_sim.R [cells] [reps_scale]` runs the cells and prints a table; it writes nothing
    into the repo.

- [ ] **Step 1: Write `test-coverage-smoke.R`.** Each cell with `reps = 20` (bootstrap cells
  `B = 50`) returns the documented fields with `coverage` in [0, 1], and the same result twice with
  the same seed. It does not assert coverage.
- [ ] **Step 2: Run** `"$RS" tools/run_tests.R coverage`. Expected: FAIL.
- [ ] **Step 3: Implement** `coverage.R` and `tools/coverage_sim.R`.
- [ ] **Step 4: Run** the smoke test and the whole suite. Expected: PASS.
- [ ] **Step 5: Run the full simulation once** with `"$RS" tools/coverage_sim.R` in the background
  (expect hours). Expected: every cell `pass = TRUE`. A failing cell is a finding: investigate with
  superpowers:systematic-debugging; never widen the tolerance.
- [ ] **Step 6: Write `docs/validation.md`**: the coverage table (cell, reps, coverage, MCSE, pass),
  the textbook values with their sources, the date, R and package versions — no paths or machine
  names.
- [ ] **Step 7: Commit** `validation: CI-coverage simulation and recorded results`.

### Task 16: Analysis guide and memory

**Files:** Create `docs/analysis.md`. Modify `memory.md`.

- [ ] **Step 1: Write `docs/analysis.md`**: the SAP workflow (add → family → rule → lock), the run
  engine (extract, prologue, cache key, moved projects), each route's spec fields and estimand, MI
  and bootstrap options, the Task 11 diagnostics table, deviations, and the "Out of scope" rulings.
- [ ] **Step 2: Update `memory.md`**: P1c status with the final test count; **Next:** the P1d plan
  (router + writer + freeze + UI), then the new **P1e scale** plan; gotchas: the SAP lock lives in
  P1c and every run needs it; `cache_inputs` keeps the cache valid across moved projects; emitted
  analysis code never names a diagnostic and diagnostics live outside `routes/`; NHEFS needs
  `SR_NHEFS_CSV`.
- [ ] **Step 3: Run** the whole suite and the lint. Expected: PASS, `LINT OK`.
- [ ] **Step 4: Commit** `docs: analysis guide and memory update for P1c`.

---

## Self-review (done while writing)
- **Spec coverage.**
  - P1 item 4 (lock the SAP) → Task 3; labels and the automatic deviation list → Tasks 3 and 13.
  - P1 item 5: Table 1 → Task 5; linear / logistic / modified Poisson / Cox → Tasks 6–8; RCS →
    Task 2; robust/cluster SE → Tasks 6–8; pre-specified RMST → Task 8; complete case or mice +
    Rubin → Task 9; effect sizes + CIs → the Task 1 contract and every route; bootstrap → Task 10;
    diagnostics with plots → sensitivity nodes → Tasks 11–13.
  - P1 Exit: NHEFS / lung / GBSG → Task 14 (GBSG published *performance* values → P6, ruled in Out
    of scope); MI + bootstrap coverage → Task 15. Planted-defect recall was P1b's.
  - B: PH, influence, separation, linearity, heteroscedasticity, clustering → Tasks 11–12.
    Overdispersion, zero-inflation, DHARMa, rootograms and PO belong to count/ordinal families,
    which are not in P1 item 5.
  - C+: MD, OR, RR, RD, marginal RD/RR, NNT, HR, RMST difference, SMD → Tasks 5–8.
  - C++: percentile/BCa with logged fallback, cluster, MI-boot, recorded seed/B/method, coverage by
    simulation → Tasks 10 and 15. Studentised → P5 (Out of scope); wild, parametric and permutation
    belong to later families.
  - D: mice, Nelson–Aalen, von Hippel m, Rubin on the correct scale, MI-boot → Tasks 9–10. D1/D2/D3
    are not needed by single-parameter P1c effects.
  - M1.1 → `.sr_emit_check` + lint + Task 13; M1.2 → every route's estimand; M1.3 → Task 1 flags and
    describe twins; M1.4 → Tasks 3 and 13; M1.5 → `sr_compose_check` at add and run; M1.6 →
    Global Constraints, Tasks 9–10, numfmt; M1.7 (NNT, Wilson, τ in the SAP) → Tasks 7–8.
- **Type consistency.** The analysis record (Global Constraints) is produced by Tasks 3 and 13 and
  consumed by Tasks 4, 11 and 13. `SR_EFFECT_COLS` (Task 1) is what every `present` returns and what
  `sr_family_adjust` reads (`p`, `role`). `.h_vcov` / `.h_coef_table` (Task 6) are reused by Tasks
  7–9; `.h_gcomp` (Task 7) by Tasks 9 and 14; `.h_rubin` (Task 9) by Task 10. `SR_DIAG_IDS` is
  pinned in Task 3 and replaced in Task 11 by the registry with the same ids. `cache_inputs` is
  added in Task 4 and used by Tasks 4 and 11. The File Structure order matches task order, so each
  file's dependencies load first.
- **Proportion.** Signatures, pinned values, formulas the signatures do not determine (Rubin/BR df,
  von Hippel m, BCa fallback, NNT) and test assertions; no function bodies.
