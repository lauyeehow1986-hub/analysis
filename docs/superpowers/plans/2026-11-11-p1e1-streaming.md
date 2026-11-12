# P1e.1 Streaming regression and SQL Table 1 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Run `lm@1`, `logistic@1`, `logistic_describe@1`, `modpois@1` and `table1@1` exactly on analysis tables of
any row count, through a streaming engine chosen and frozen at the SAP lock.

**Architecture:** A new `app/R/scale/` layer resolves the engine at the lock (`auto|memory|stream`). It writes a
Parquet extract with a `.sr_row` column, and it emits inlined R code. That code reads the extract in fixed chunks
through a locked-down in-memory DuckDB, fits by TSQR (lm) or TSQR-IRLS (glm), and streams the sandwich, marginal
and Table 1 aggregates. The route ids, estimands, presenters and value lists stay the same. `compile()` branches on
the locked `engine` field.

**Tech Stack:** R 4.5.x, duckdb (R package), DBI, openssl, Hmisc, sandwich (for the reference values in tests
only), testthat, shiny `testServer`.

**Spec:** `docs/superpowers/specs/2026-11-11-p1e1-streaming-regression-design.md`. Read it, and its section 12
"Plan rulings", before any task.

## Global Constraints

- Rscript: `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"`. Run tests with `NOT_CRAN=true`. Never run long code
  through `Rscript -e`; write a script file to the scratchpad.
- Run one test file with:
  `Rscript -e "testthat::test_file('tests/testthat/<file>.R', load_package='none')"`.
  `helper-load.R` sources the app. If that form fails, copy the invocation the existing tests use.
  Full suite: `Rscript -e "testthat::test_dir('tests/testthat')"`.
- Exported functions use `sr_`, internal ones `.sr_`, and inlined helpers `.h_`. snake_case. One concern per file.
- Read options with `sr_opt("x", default)`. That is `getOption("sr.x", Sys.getenv("SR_X", default))`.
- Generated `.R` and `.ps1` files are **ASCII only**, and so are all emitted code strings.
- **Air-gap.** No network calls. Never call `py_available(initialize=TRUE)`. No URL literal in any emitted code or
  SQL. Never use `read_json` or the sqlite extension. Every DuckDB connection in emitted code goes through
  `.h_st_open`.
- **No test-gated switching.** `compile()` never reads data or a p-value. `tests/testthat/test-lint.R` must stay
  green (`LINT OK`).
- **Events are authoritative.** Every new fact is an event with a reducer (`sr_reducer_register`).
- **Commits.** Trailer `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`. Before every commit, grep
  `git diff --cached` for the Windows username, `Users\`, emails and hostnames (the controller gives the exact
  pattern); it must print nothing. The public repo carries no personal details. Do not push.
- **Synthetic data only**, made in the test files or in `samples/*.R`.
- `app/global.R` sources `app/R/scale/*.R` after `app/R/routes/*.R`.
- **Tolerances** (spec section 10):
  - estimates, model SEs, CIs and marginal effects: `all.equal(tolerance = 1e-8)`;
  - robust and cluster SEs: `1e-7`;
  - Table 1 cells: identical strings;
  - knots: `1e-12`.
- Exact texts from the spec are quoted in the tasks below. Use them verbatim.

## Review Focus

1. **Old projects.** A project locked before P1e reopens with the same SAP hash. Its run keys and its exported
   `analysis.R` are unchanged, so no rerun is needed and the export still passes the key check. Pinned in Task 2
   (hash fixture) and Task 11 (key unchanged for a spec without `engine`).
2. **A move to `locked16` after a `memory` lock.** The run must refuse with the switch hint and never switch
   silently. Pinned in Task 11 (`SR_RAM_BUDGET_GB` lowered after the lock).
3. **Text levels with non-ASCII characters or mixed case.** Stream factor levels must equal the in-memory ones,
   and the emitted code must stay ASCII (`.sr_lit` escapes). Pinned in Task 5 and Task 15.
4. **A cluster spanning chunk boundaries and the last chunk.** U_g must be flushed exactly once. Pinned in Task 6
   (chunk 7, a cluster of 9 rows straddling chunks, the last cluster ending on the last row).
5. **Columns that are all NA, and an outcome stored as text with `event_value`.** These need the same refusal or
   behaviour as in memory. Pinned in Task 15 (edge-case block).

---

## Rulings (decided while reading the code; recorded in spec section 12)

- **R1 Levels and knots** are computed over the rows left after `exclude_rows` only, not over the complete-case
  set. That matches the in-memory `sr_terms_code()` setup, which sets `.sr_fixed` on the full post-exclusion
  column. Spec section 6.3's "after `<keep>`" means after the exclusion.
- **R2 glm warnings.** In memory, `stats::glm` runs under `suppressWarnings`. The stream therefore reproduces the
  value fields, not warning texts:
  - `converged`;
  - `near_separation` (any mu < 1e-08 or > 1 - 1e-08);
  - `risk_above_one` (any mu > 1).
  It uses the same thresholds as `routes/glm.R`.
- **R3 lm under stream.**
  - `fig = NULL`, as the MI path does.
  - `sigma = sqrt(RSS / df_resid)` with `df_resid = n - rank`.
  - `r2` and `adj_r2` come from a streamed total sum of squares (Chan's merge of mean and M2 per chunk).
  - The cluster `df` rule `min(df_resid, G - 1)` is unchanged.
- **R4 diagnostics.** `sr_diag_run` refuses an analysis whose effective engine is stream, with "diagnostics for
  a streaming analysis arrive in P1e.3".
- **R5 run metadata.**
  - `run.completed` gains `engine`, `extract_kind` and `chunk`. The reducer keeps `ok_engine`,
    `ok_extract_kind` and `ok_chunk`. Old events read as `"memory"`, `"rds"` and NULL.
  - `.sr_script_block`, freeze, srproj_export and archive_view take the extract extension from
    `ok_extract_kind`.
- **R6 The run spec** is built by one helper, `.sr_run_spec(project, id, engine = NULL, chunk = NULL)`, used
  by `sr_run`, `.sr_script_block` and `sr_diag_run`.
  - For the effective engine `memory` it **removes** `engine` and `stream_chunk` from the spec. A pre-P1e spec
    therefore compiles and hashes byte-for-byte as before (Review Focus 1).
  - For `stream` it sets `engine = "stream"` and `stream_chunk`.
  - Compile treats a missing, `"auto"` or `"memory"` engine as memory.
- **R7 Chunk size is a spec field** (`stream_chunk`, an optional number in the five schemas), because
  `sr_compile` validates against the schema and the run key hashes the spec, not the code.
  - `.sr_run_spec` fills it from `sr_opt("stream_chunk_rows", 250000)` when it is absent.
  - Compile emits it as an integer literal.
  - It must be a whole number >= 1, else the spec is invalid: "stream_chunk must be a whole number >= 1".
- **R8 Complete cases are decided in R, per chunk,** with the same emitted snippets as in memory:
  `.h_design`, the outcome code, `mg$pre`, and `complete.cases(des[used])`. SQL only drops `exclude_rows`, which
  is the exactness-by-construction rule. Checks that look at the whole outcome column run on the accumulated
  counts after the first pass, with the in-memory texts.
- **R9 Observed risk** counts (n and events per exposure level) are accumulated in R during the first main pass,
  over the same rows `.sr_glm_obs_code` uses, rather than by SQL `GROUP BY`. The Wilson code is shared.
- **R10 Alias tolerance.** lm uses `qr(., tol = 1e-7)`. glm uses `min(1e-7, 1e-8 / 1000)` = `1e-11`, the
  tolerance `glm.fit` passes to `Cdqrls`. Aliasing is re-decided on every IRLS pass, as `glm.fit` does.
- **R11 glm robust and cluster scores** follow `sandwich::estfun.glm` exactly:
  `s_i = x_i * (y_i - mu_i) / mu.eta(eta_i) * w_i`. Here `mu_i` and `eta_i` are at the final beta_k, and `w_i`
  holds the working weights at beta_{k-1}, the pass whose R is kept. The SE pass computes both etas. The bread is
  `chol2inv(R)` with dispersion 1.
- **R12 TSQR merge** uses LINPACK `qr(rbind(M, B), tol = 0)`. It stores `qr.R(q)[, order(q$pivot)]`, so a column
  that is all zero in a chunk (a factor level absent from it) cannot permute the result. Only `crossprod(M)` is
  invariant. The final triangular R, the rank and the pivot come from `.h_st_alias`.
- **R13 Exclusion in SQL** registers `exclude_rows` as an in-memory table
  (`duckdb::duckdb_register(con, "sr_excl", data.frame(r = <rows>))`). It then filters with
  `".sr_row" NOT IN (SELECT r FROM sr_excl)`. There is no 1000-position branch.
- **R14 Table 1 threads.** The Table 1 compile passes the literal `1L` to `.h_st_open`'s `threads` instead of
  `sr_duck_threads`, so no `SET` follows the lockdown.
- **R15 Progress file.** It lives at `.sr_progress_path(project, job)` =
  `file.path(project$paths$dir, "data", "tmp", paste0("progress-", job, ".json"))`, passed to the worker as the
  input `sr_progress_file`, outside the cache key. The text is `{"pass": k, "max": m}`.
  - m = fit passes (lm 1; glm 26) + (se != "model") + (marginal requested and se != "robust").
  - For robust SEs the marginal sums share the SE pass.
- **R16 The badge** comes from `state$runs[[id]]$ok_engine`, the event-sourced value, not from the result value.
  `present()` stays unchanged. The value still carries `engine = "stream"`, `chunk`, `passes` and `iter`.
- **R17 Engine Methods fragment.** `sr_present(route, value, spec)` (new, in `routes/result.R`) calls
  `route$present` and, when `value$engine == "stream"`, appends `fragments$engine`, the spec section 7 sentence
  with digits. `sr_ph_lift` turns the numbers into placeholders. Every `route$present(` call site uses
  `sr_present`.
- **R18 SAP hash after the lock** reads `lock$engines`, which is frozen. Before the lock it resolves the engines
  now, and a resolution error counts as not stream.

---

## File structure

| File | Responsibility |
|---|---|
| `app/R/scale/engine.R` (new) | `SR_ENGINES`, `.sr_engine_refusal`, `sr_engine_resolve`, `.sr_sap_engines`, `.sr_engine_effective`, `sr_engine_switch` + reducer, `.sr_engine_fragment` |
| `app/R/scale/stream_extract.R` (new) | `sr_extract_stream`, `.sr_extract_stream_need` |
| `app/R/scale/stream_io.R` (new) | `.h_st_open`, `.h_st_excl`, `.h_st_pass`, `.h_st_progress`, `.h_st_distinct`, `.h_st_knots`, `.sr_run_prologue_stream` |
| `app/R/scale/stream_fit.R` (new) | `.h_st_tsqr`, `.h_st_alias`, `.h_st_lm_finish`, `.h_st_wls_solve` |
| `app/R/scale/stream_vcov.R` (new) | `.h_st_meat_add`, `.h_st_cluster_step`, `.h_st_vcov_finish` |
| `app/R/scale/stream_marginal.R` (new) | `.h_st_marg_add`, `.h_gcomp_finish` (moved from `marginal.R`, shared) |
| `app/R/scale/stream_compile.R` (new) | `.sr_lm_compile_stream`, `.sr_glm_compile_stream` (emit the whole stream body) |
| `app/R/scale/table1_sql.R` (new) | `.h_t1_sql_main`, `.sr_t1_compile_stream` |
| `tests/testthat/helper-scale.R` (new) | `.sc_frame`, `.sc_parquet`, `.sc_con`, `.sc_locked` |
| `tests/testthat/test-scale-*.R` (new) | one file per task, named below |

---

### Task 1: Engine field, refusals and resolution

**Files:**
- Create: `app/R/scale/engine.R`, `tests/testthat/test-scale-engine.R`, `tests/testthat/helper-scale.R`
- Modify:
  - the schemas and cross-field validation in `app/R/routes/lm.R`, `app/R/routes/glm.R` (three routes),
    `app/R/routes/table1.R`, `app/R/routes/cox.R` and `app/R/routes/rmst.R`;
  - `app/R/profile/profile.R:395-445` (`sr_mem_estimate`);
  - `app/R/sap/sap.R:34-42` (`.sr_mem_method`);
  - `app/global.R`;
  - `tests/testthat/test-profile.R` (the `"bigglm"` expectations become `"stream"`).

**Interfaces:**
- Produces:
  - `SR_ENGINES <- c("auto", "memory", "stream")`.
  - `.sr_engine_refusal(route_id, spec)` returns `NULL` or one reason string. The strings are:
    - `"multiple imputation at scale arrives in P1e.3"` (`layers$missing == "mi"`);
    - `"the bag of little bootstraps arrives in P1e.3"` (`layers$resampling == "bootstrap"`);
    - `"the Firth penalty at scale arrives in P1e.3"` (`penalty == "firth"`);
    - `"the streaming engine for Cox / RMST arrives in P1e.2"` (route id starts with `cox` or `rmst`).
  - `sr_engine_resolve(project, analysis_id, spec = NULL)` returns `list(engine, requested, reason, estimate,
    block)`.
    - `engine` is in `c("memory", "stream")`, or `NA` when blocked.
    - `block` is `NULL` or a string.
    - `spec` defaults to the state spec of `analysis_id`.
  - `sr_mem_estimate(..., method = c("glm", "lm", "cox", "ml", "table1"))`. Its non-fitting route is `"stream"`
    for lm, glm and table1.
  - `.sr_mem_method("table1")` returns `"table1"`.
- Schema fields on lm, logistic, logistic_describe, modpois and table1:
  - `engine = list(type = "chr", required = FALSE)`;
  - `stream_chunk = list(type = "num", required = FALSE)`.
  Cox and RMST get `engine` only.

- [ ] **Step 1: Write the failing tests** in `test-scale-engine.R`:
  - `test_that("engine field validates against SR_ENGINES")`:
    - `sr_spec_validate(<lm spec + engine = "fast">, lm route)$ok` is FALSE, and an error names `engine`;
    - `"auto"`, `"memory"` and `"stream"` are each ok.
  - `test_that("cox and rmst refuse the stream engine at validation")`: the errors contain
    `"the streaming engine for Cox / RMST arrives in P1e.2"`, and `engine = "memory"` is ok.
  - `test_that("stream refusals name their phase")`: with `engine = "stream"`, each of mi, bootstrap and firth
    makes `sr_spec_validate` fail with the exact strings above. With `engine = "auto"` the spec stays valid.
  - `test_that("stream_chunk must be a whole number >= 1")`: 0, 2.5 and -1 fail with
    `"stream_chunk must be a whole number >= 1"`; 7 is ok.
  - `test_that("sr_mem_estimate table1 counts data bytes only")`:
    - `need_gb` for `method = "table1"` equals `data_gb`, i.e. `need_gb * 1024^3 == rows * sum(bytes)` for a
      known two-column fixture;
    - `override_rows = 1e10` gives `route == "stream"`.
  - `test_that("auto resolves to memory when it fits and to stream when it does not")`:
    - use `.sc_locked()` before the lock (helper below);
    - `withr::local_envvar(SR_RAM_BUDGET_GB = "0.0001")` gives `engine == "stream"`, and `reason` starts with
      `"auto -> stream: does not fit in memory"`;
    - the default gives `engine == "memory"`, and `reason` starts with `"auto -> memory: fits in memory"`.
  - `test_that("explicit stream on data that fits resolves to stream")`.
  - `test_that("memory that does not fit is a block, not a switch")`: `block` contains `"does not fit in memory"`
    and `engine` is NA.
- [ ] **Step 2: Write `helper-scale.R`.**
  - `.sc_frame(n = 20000L, seed = 1L)` returns a data.frame with these columns:
    - `id` (1..n);
    - `pid`, a cluster id with clusters of 1-9 rows, so clusters cross chunk boundaries;
    - `y` (0/1, logistic truth), `yc` (continuous);
    - `x1` (normal), `x2` (gamma, tie-free);
    - `xt`, numeric with heavy ties: `round(runif(n) * 5)`;
    - `grp`, text with levels `"A"`, `"b"`, `"C"` and `"é"`, the last spelled as `"\u00e9"` so the source stays
      ASCII;
    - `late`, text whose level `"z"` appears only in the last 5 rows;
    - `sex` (`"M"`/`"F"`), the exposure.
  - `.sc_locked(spec, route = "logistic", env = parent.frame())` returns a locked project built with
    `.fx_ready_project` and `sr_sap_add`, then `sr_sap_lock(.fx_feasible(...))`. Copy `.run_locked` in
    `helper-run.R`.
  - `.sc_parquet(df, env)` writes `df` with `.sr_row = seq_len(nrow(df))` to a temp parquet and returns the path
    (used by Tasks 5-8 and 10).
  - `.sc_con(path, env)` returns `.h_st_open(path, "2GB", 2L, tempdir())` and closes it on defer.
- [ ] **Step 3: Run the tests** and see them fail: `test-scale-engine.R` fails on "could not find function"
  errors.
- [ ] **Step 4: Implement** `engine.R` (`SR_ENGINES`, `.sr_engine_refusal`, `sr_engine_resolve`) and the schema
  and validation changes.
  - The `.sr_engine_refusal` check runs in each route's existing cross-field validator when
    `identical(spec$engine, "stream")`.
  - In `sr_engine_resolve`, `reason` is `paste0(requested, " -> ", engine, ": ", estimate$reason)`.
  - The block texts are:
    - memory that does not fit: `paste0("memory engine requested but the analysis does not fit in memory: ",
      estimate$reason)`;
    - a §4.2 refusal when `auto` resolves to stream: `.sr_engine_refusal(...)`.
  - `sr_mem_estimate` with `"table1"`: `need <- data_gb`; route `"in_memory"` or `"stream"`. Update the
    roxygen.
  - Add the `scale/` source line to `global.R`.
- [ ] **Step 5: Run the tests** `test-scale-engine.R`, `test-profile.R`, `test-lm.R`, `test-glm.R`,
  `test-table1.R`, `test-cox.R`, `test-rmst.R` and `test-spec.R`. Expected: all PASS.
- [ ] **Step 6: Commit** `feat(scale): engine field, stream refusals and engine resolution`.

### Task 2: Lock checklist rows, lock record and SAP hash part

**Files:**
- Modify:
  - `app/R/sap/sap.R`: `sr_sap_check` at 462-477, `sr_sap_hash` at 387-407, `sr_sap_lock` at 526-541, and the
    lock reducer at 124;
  - `app/R/sap/deviations.R`: the `analysis.added` reducer at 29 and `sr_analysis_add` at 92-140;
  - `app/R/scale/engine.R`.
- Create: `tests/testthat/test-scale-lock.R`, `tests/testthat/fixtures/pre-p1e-lock/` (see Step 1).

**Interfaces:**
- Consumes: `sr_engine_resolve`, `.sr_engine_refusal` (Task 1).
- Produces:
  - `.sr_sap_engines(project)`: the named list `id -> list(engine, requested, reason)` for the pre-specified
    analyses. After the lock it is `lock$engines`. Before the lock it resolves the engines now, and a resolution
    error gives `engine = NA`.
  - `state$sap$lock$engines`.
  - `state$analyses[[id]]$engine_resolved`, a `list(engine, requested, reason)` set from the `analysis.added`
    payload field `engine`.
  - `sr_sap_hash(project)` adds the part `engines = <sorted stream ids -> "stream">` only when that list is
    non-empty.

- [ ] **Step 1: Write the failing tests** in `test-scale-lock.R`:
  - `test_that("checklist shows an engine row per analysis")`:
    - a fitting analysis gives a row with status `ok` whose text matches `"^a1: in memory \\(auto -> memory: "`;
    - under `SR_RAM_BUDGET_GB = "0.0001"` the text matches `"^a1: stream engine \\(auto -> stream: "`;
    - the old text `"big-data engines arrive in P1e"` appears nowhere.
  - `test_that("memory request that does not fit blocks the lock")`: status `block`, and `sr_sap_lock` errors.
  - `test_that("auto resolving to stream with mi blocks, naming P1e.3")`.
  - `test_that("lock records engines and the hash part only for stream")`:
    - two locks of identical projects, one with `engine = "stream"`, have different `sap_hash`;
    - `p$state$sap$lock$engines$a1$engine == "stream"`;
    - an all-memory project has no `engines` part in the hash; the next test pins that its hash is unchanged.
  - `test_that("a project locked before P1e keeps its SAP hash")`:
    - build the fixture once, by a script `tests/testthat/fixtures/make_pre_p1e_lock.R` run against `main`
      (commit 457c54f or later, before this branch). The fixture is the project's `events.jsonl` plus the
      synthetic table and its stored `sap_hash`;
    - the test opens it and asserts `identical(sr_sap_hash(p), p$state$sap$lock$sap_hash)`.
  - `test_that("analysis added after the lock records its engine and leaves the hash")`: after `sr_analysis_add`,
    `p$state$analyses$a9$engine_resolved$engine` is set, and `sr_sap_hash(p)` is unchanged.
- [ ] **Step 2: Run the tests** and see them fail.
- [ ] **Step 3: Implement.**
  - The engine rows replace the "fits in memory" rows in `sr_sap_check`:
    - `ok` text: `sprintf("%s: %s (%s)", id, if (engine == "stream") "stream engine" else "in memory", reason)`;
    - `block`: the `block` string, prefixed `"<id>: "`;
    - the estimate-error rows keep today's behaviour.
  - The lock payload gains `engines`, and the reducer stores it.
  - In `sr_sap_hash`, add the `engines` part as a sorted named character vector, omitted when empty.
  - `sr_analysis_add` calls `sr_engine_resolve` and puts `engine = list(engine, requested, reason)` in the
    payload. A blocked resolution refuses the add with the block text.
- [ ] **Step 4: Run the tests** `test-scale-lock.R`, `test-sap.R`, `test-deviations.R` and
  `test-writer-sapdoc.R`. Expected: PASS.
- [ ] **Step 5: Commit** `feat(sap): engine rows in the lock checklist, frozen engines and hash part`.

### Task 3: Engine switch (computational deviation)

**Files:**
- Modify: `app/R/scale/engine.R`, `app/R/sap/deviations.R:415-460` (`sr_deviations`).
- Create: `tests/testthat/test-scale-switch.R`.

**Interfaces:**
- Consumes: `sr_engine_resolve`, `.sr_engine_refusal`, `.sr_sap_engines`.
- Produces:
  - `.sr_engine_effective(project, id)` returns `"memory"` or `"stream"`, from the first of these that is set:
    1. `state$engines[[id]]$to`;
    2. `lock$engines[[id]]$engine`;
    3. `analyses[[id]]$engine_resolved$engine`;
    4. otherwise `"memory"`.
  - `sr_engine_switch(project, analysis_id, engine, reason)` returns the project after appending
    `engine.switched` with payload `list(analysis, from, to, reason)`. Its reducer sets
    `state$engines[[id]] <- list(from, to, reason)`.

- [ ] **Step 1: Write the failing tests:**
  - `test_that("switch writes the event, lists a computational deviation, keeps the hash")`:
    - lock an in-memory analysis, record the hash `h`, then `sr_engine_switch(p, "a1", "stream", "moved to
      locked16")`;
    - the last event type is `"engine.switched"`;
    - `.sr_engine_effective(p, "a1") == "stream"`;
    - `sr_deviations(p)` has a row with `kind == "computational"` and `reason == "engine changed from memory to
      stream: moved to locked16; estimator unchanged"`;
    - `sr_sap_hash(p) == h`.
  - `test_that("switch refusals")`, each an `expect_error` with a regexp:
    - read-only (`"read-only"`);
    - unknown analysis (`"unknown analysis"`);
    - `engine = "auto"` (`"engine must be \"memory\" or \"stream\""`);
    - the same engine as the current one (`"already uses the"`);
    - memory when it does not fit (`"does not fit in memory"`);
    - stream with mi (`"P1e.3"`);
    - an empty reason (`"a reason is required"`).
- [ ] **Step 2: Run the tests** and see them fail.
- [ ] **Step 3: Implement** the switch and its reducer. In `sr_deviations`, add one `computational` row per
  `state$engines` entry, with trigger `NA`.
- [ ] **Step 4: Run the tests** `test-scale-switch.R` and `test-deviations.R`. Expected: PASS.
- [ ] **Step 5: Commit** `feat(scale): sr_engine_switch computational deviation`.

### Task 4: The stream extract

**Files:**
- Create: `app/R/scale/stream_extract.R`, `tests/testthat/test-scale-extract.R`.
- Modify: `app/R/routes/run.R:137-176`. Factor the `ORDER BY` clause of `.sr_extract_frame` into
  `.sr_extract_order_sql(con, cols)`, used by both extracts.

**Interfaces:**
- Produces:
  - `sr_extract_stream(project, table, cols, drive_info = .sr_drive_info)` returns
    `list(rel, sha, n, cols, kind = "parquet", bytes)`. `rel` is `"data/extracts/<sha>.parquet"`.
  - `.sr_extract_stream_need(rows, bytes_per_row)` returns bytes: `rows * bytes_per_row * 1.35 + 1024^3`.

- [ ] **Step 1: Write the failing tests:**
  - `test_that(".sr_row equals the .sr_extract_frame position")`:
    - on a ready project with ties and NAs in the sort columns, read the parquet back with `duckdb` and order it
      by `.sr_row`;
    - `identical(df_parquet[cols], .sr_extract_frame(...)[cols])`, after dropping row names;
    - `df_parquet$.sr_row == seq_len(n)`.
  - `test_that("the name is the streaming SHA-256 and a rerun reuses it")`:
    - `ex$sha == as.character(openssl::sha256(file(<full path>)))`;
    - a second call returns the same `rel`;
    - no `.tmp-*.parquet` remains in `data/extracts`.
  - `test_that("space refusal before writing")`:
    - `drive_info = function(path) list(free_bytes = 1, remote = FALSE, warnings = character())`;
    - `expect_error(..., "not enough free space for the stream extract: about [0-9.]+ GB needed, [0-9.]+ GB
      free")`;
    - the extracts directory is unchanged.
  - `test_that("a COPY error leaves no temp file")`: inject one with
    `testthat::local_mocked_bindings(.sr_extract_copy = function(...) stop("boom"))`, then expect error "boom"
    and no `.tmp-*` file.
- [ ] **Step 2: Run the tests** and see them fail.
- [ ] **Step 3: Implement.**
  - The COPY statement is exactly spec section 5, run on `sr_duck(project)` through a small internal
    `.sr_extract_copy(con, sql)`, so the test can mock it.
  - Bytes per row come from `.sr_mem_bytes` over the profile columns, as in `sr_mem_estimate`.
  - Use `tryCatch(..., finally = unlink(tmp))` after the rename, then check.
  - Rename with `file.rename`. When the target exists, delete the temp file.
- [ ] **Step 4: Run the tests** `test-scale-extract.R` and `test-run.R`. Expected: PASS.
- [ ] **Step 5: Commit** `feat(scale): parquet stream extract with .sr_row and space check`.

### Task 5: Stream IO, frozen terms and the stream prologue

**Files:**
- Create: `app/R/scale/stream_io.R`, `tests/testthat/test-scale-io.R`.
- Modify:
  - `app/R/routes/terms.R`: `sr_terms_code(spec, engine = "memory")`. With `"stream"`, `setup` becomes the
    stream setup, and `design`, `rhs` and `coef_map` stay identical;
  - `app/R/routes/run.R`: add `.sr_run_prologue_stream(ex)` next to `.sr_run_prologue`.

**Interfaces:**
- Produces, all inlinable with `.sr_inline` and free of StatRouter calls:
  - `.h_st_open(path, mem, threads, temp)` returns a DBI connection. It applies these, in order:
    1. `SET extension_directory = '<temp>/ext-empty'`, after creating that empty directory;
    2. `autoinstall_known_extensions = false`;
    3. `autoload_known_extensions = false`;
    4. `allow_community_extensions = false`;
    5. `memory_limit = '<mem>'`;
    6. `threads = <threads>`;
    7. `temp_directory = '<temp>'`;
    8. `preserve_insertion_order = true`;
    9. `lock_configuration = true`, last.
    It then re-reads the autoload and autoinstall settings and stops with "DuckDB lockdown did not take effect"
    if they are not false. The order and texts mirror `.sr_duck_setup` in `io/duck.R:48-70`.
  - `.h_st_excl(con, rows)` registers `sr_excl` (ruling R13) and returns the SQL predicate: `"TRUE"` when
    `rows` is empty, else `"\".sr_row\" NOT IN (SELECT r FROM sr_excl)"`.
  - `.h_st_pass(con, sql, chunk, init, step, order = TRUE)` folds `step(state, df)` over `dbFetch(n = chunk)`.
    With `order = TRUE` it stops with `"extract read out of order"` unless `.sr_row` is strictly increasing
    within and across chunks. It returns `list(state, rows)`.
  - `.h_st_progress(file, k, m)` writes `{"pass": k, "max": m}`. A failed write is ignored.
  - `.h_st_distinct(con, src, col, where)` returns the vector of distinct non-NA values.
  - `.h_st_knots(con, src, col, nk, where, max_rows)` returns `.h_knots(...)`, following spec section 6.3 with
    m < 100 / quantile_cont / ties. The Hmisc probabilities for nk 3-7 come from `Hmisc::rcspline.eval`'s source
    and are emitted as literals. Above `max_rows` it stops with `"too many tied values to place knots for <col>
    at scale; set explicit knots in the SAP"`.
  - `.sr_run_prologue_stream(ex)` returns the code string. It checks the rel, computes the streaming SHA,
    stops with `"extract changed"`, and opens `.sr_con`. It also sets
    `sr_src <- "read_parquet('<normalized path>')"` and `on.exit(DBI::dbDisconnect(.sr_con, shutdown = TRUE),
    add = TRUE)`. It must pass `.sr_emit_check(code, "analysis", prologue = TRUE)`.

- [ ] **Step 1: Write the failing tests:**
  - `test_that("stream connection is locked down")`:
    - on `.sc_con(.sc_parquet(.sc_frame(200)))`, `current_setting('autoload_known_extensions')` is FALSE, and so
      are autoinstall and `allow_community_extensions`;
    - `extension_directory` is an empty directory;
    - `preserve_insertion_order` is TRUE;
    - `expect_error(DBI::dbExecute(con, "SET threads = 1"))`.
  - `test_that("pass reads in file order and detects disorder")`:
    - the sum of `.sr_row` over `.h_st_pass(..., chunk = 7)` equals `n*(n+1)/2`, and `rows == n`;
    - a parquet written with `.sr_row` shuffled gives `expect_error(..., "extract read out of order")`.
  - `test_that("stream levels equal .h_levels, including non-ASCII and mixed case")`: for `grp` and `late`, the
    output of `.h_st_distinct` piped through `.h_levels(., ref)` is `identical` to `.h_levels(df[[col]], ref)`.
  - `test_that("stream knots equal Hmisc on the full column")`, `tolerance = 1e-12` for nk in 3:5, on:
    - `x2`, which is tie-free;
    - `xt`, which is tie-heavy and takes the `rep(value, count)` path;
    - a 60-row column (m < 100).
  - `test_that("knots refused above stream_knot_rows when tied")`: with `max_rows = 10`, the error matches
    `"too many tied values to place knots for xt at scale"`.
  - `test_that("stream prologue checks the extract")`:
    - the code parses, is ASCII and holds no `"http"`;
    - evaluating it after flipping one byte of the parquet gives `"extract changed"`;
    - a rel ending `.rds` errors with `"data/extracts/<sha>.parquet"`.
  - `test_that("stream terms setup keeps design identical")`: for a spec with factor, rcs, quad and interaction
    terms, `sr_terms_code(spec, "stream")$design` is `identical` to `sr_terms_code(spec)$design`.
- [ ] **Step 2: Run the tests** and see them fail.
- [ ] **Step 3: Implement.**
  - The stream setup emits `.sr_excl` exactly as `.sr_exclude_code` does, then
    `.sr_where <- .h_st_excl(.sr_con, .sr_excl)`.
  - Per factor term: `.sr_fixed$levels[[id]] <- .h_levels(.h_st_distinct(.sr_con, sr_src, var, .sr_where),
    ref)`.
  - Per rcs term without explicit knots: `.sr_fixed$knots[[id]] <- .h_st_knots(..., <stream_knot_rows
    literal>)`.
  - The knot row limit is `sr_opt("stream_knot_rows", 5e7)`, read at compile. It is a code literal and is not
    in the key, because it only decides refusal and never changes a number.
- [ ] **Step 4: Run the tests** `test-scale-io.R`, `test-terms.R` and `test-emit.R`. Expected: PASS.
- [ ] **Step 5: Commit** `feat(scale): locked-down stream IO, frozen terms and stream prologue`.

### Task 6: Streaming lm fit and its SEs

**Files:**
- Create: `app/R/scale/stream_fit.R`, `app/R/scale/stream_vcov.R`, `tests/testthat/test-scale-lm-core.R`.

**Interfaces:**
- Produces:
  - `.h_st_tsqr(M, B)` follows ruling R12. With `M = NULL` it starts from `B`.
  - `.h_st_alias(M, p, tol)` returns `list(R, keep, rank, pivot)`:
    - `q <- qr(M[, 1:p, drop = FALSE], tol = tol)`;
    - `keep` is the kept column indices in the original order;
    - `R` is the `rank x rank` upper triangle for `keep`, in pivot order.
  - `.h_st_lm_finish(M, p, nms, n, sst)` returns `list(beta (named, NA for aliased), R, keep, rank, rss,
    df_resid, sigma, r2, adj_r2, aliased)`.
  - `.h_st_meat_add(S, X, r)` returns `S + crossprod(X * r)`.
  - `.h_st_cluster_step(st, X, r, g)` carries `U` for the open cluster across chunks. It adds `tcrossprod(U)`
    when g changes and stops with `"extract read out of order"` if a closed cluster id reappears (tracked in a
    hashed environment). `.h_st_cluster_flush(st)` closes the last cluster.
  - `.h_st_vcov_finish(se, B, meat, n, k, G)` returns V. It applies HC3 (meat already weighted), HC0, or HC1
    with the factor `G/(G-1) * (n-1)/(n-k)`.

- [ ] **Step 1: Write the failing tests.** On `.sc_frame(2000)`, use the design matrix `X` of
  `yc ~ x1 + grp + x2` from `model.matrix`, plus one duplicated column for aliasing. Then:
  - `test_that("TSQR lm equals lm for chunk sizes 7, 1000 and > n")`:
    - for each chunk, fold `.h_st_tsqr` over row blocks of `cbind(X, y)` and call `.h_st_lm_finish`;
    - `beta` equals `coef(lm.fit)` at 1e-8, with the same NA pattern;
    - `sigma` and `r2` equal `summary(lm)`.
  - `test_that("all-zero column in a chunk does not permute")`:
    - rows 1-50 have level `C` absent, which makes a zero column in that block;
    - the result equals the full-data result.
  - `test_that("HC3 by leverage equals sandwich::vcovHC HC3")`:
    - one pass computes `h_i = colSums(backsolve(R, t(X[, keep]), transpose = TRUE)^2)`, then the meat;
    - V equals `sandwich::vcovHC(fit, "HC3")[keep, keep]` at 1e-7.
  - `test_that("cluster HC1 equals vcovCL, clusters crossing chunks and the last row")`:
    - sort by `pid`, then use chunk 7;
    - V equals `sandwich::vcovCL(fit, cluster = ~pid, type = "HC1")` at 1e-7;
    - also assert the count of flushed clusters equals `length(unique(pid))`, which pins Review Focus 4.
- [ ] **Step 2: Run the tests** and see them fail.
- [ ] **Step 3: Implement** the helpers. The model V for lm is `sigma^2 * chol2inv(R)`, mapped back to `keep`
  order.
- [ ] **Step 4: Run the tests** `test-scale-lm-core.R`. Expected: PASS.
- [ ] **Step 5: Commit** `feat(scale): TSQR lm, aliasing and streamed sandwich meats`.

### Task 7: Streaming IRLS for logistic and modified Poisson

**Files:**
- Modify: `app/R/scale/stream_fit.R`.
- Create: `tests/testthat/test-scale-glm-core.R`.

**Interfaces:**
- Consumes: `.h_st_tsqr`, `.h_st_alias`, `.h_st_meat_add`, `.h_st_cluster_step`, `.h_st_vcov_finish` (Task 6).
- Produces: `.h_st_irls(passfn, family, p, nms, maxit = 25L, epsilon = 1e-8)`.
  - `passfn(beta_or_NULL, wls_fun)` runs one data pass. Each chunk calls `wls_fun(X, y)`, which updates the TSQR
    of `[sqrt(w) X | sqrt(w) z]` and the deviance at the current mu. Under `NULL` the current mu is `mustart`
    from `family$initialize`; otherwise `eta = X beta`.
  - It returns `list(beta, beta_prev, R, keep, rank, iter, converged, deviance, near_separation,
    risk_above_one, aliased, passes)`.
  - The rule is spec section 6.4: convergence is decided on pass k+1. The fit keeps beta_k and the R of pass k.
    Aliasing uses tol `1e-11` (R10).
  - A non-finite deviance stops with `"the streaming fit met a non-finite deviance; refit in memory"`.
  - `near_separation` and `risk_above_one` are evaluated on the final mu, as `routes/glm.R` does (R2).
  - `.h_st_glm_scores(X, y, eta, eta_prev, family)` returns the R11 scores.

- [ ] **Step 1: Write the failing tests.** On `.sc_frame(5000)`, use `y ~ x1 + grp + rcs-like x2^2`, and drive
  `passfn` over an in-memory data.frame split into chunks of 7, 1000 and > n:
  - `test_that("streamed IRLS equals glm binomial")`: coefficients at 1e-8; `iter` equals `fit$iter`;
    `converged` is TRUE; model V `chol2inv(R)` equals `vcov(fit)` at 1e-8.
  - `test_that("streamed IRLS equals glm poisson log on a binary outcome")`: the same assertions for
    `poisson(link = "log")`.
  - `test_that("glm HC0 and cluster HC1 equal sandwich")`: `vcovHC(fit, "HC0")` and `vcovCL(fit, ~pid, type =
    "HC1")` at 1e-7.
  - `test_that("non-convergence and separation flags match glm")`:
    - `maxit = 2` gives `converged` FALSE, equal to `glm(..., control = glm.control(maxit = 2))$converged`;
    - a perfectly separated predictor gives `near_separation` TRUE in both.
  - `test_that("aliased column is reported like glm")`: `aliased` equals `names(coef(fit))[is.na(coef(fit))]`.
- [ ] **Step 2: Run the tests** and see them fail.
- [ ] **Step 3: Implement `.h_st_irls`.** The pass loop:

```r
# pass 1: mu = mustart, devold = dev(mustart); WLS -> beta_1 (R_1)
# pass k+1 (k >= 1): eta = X beta_k; dev_k = dev(mu(beta_k)); WLS -> beta_{k+1} (R_{k+1})
#   if |dev_k - dev_{k-1}| / (|dev_k| + 0.1) < epsilon: keep beta_k, R_k; iter = k; stop
#   (dev_0 = devold); if k == maxit: converged = FALSE, keep beta_k, R_k
```

- [ ] **Step 4: Run the tests** `test-scale-glm-core.R`. Expected: PASS.
- [ ] **Step 5: Commit** `feat(scale): TSQR-IRLS following glm.fit, streamed glm sandwich`.

### Task 8: Streamed marginal effects and observed risk

**Files:**
- Create: `app/R/scale/stream_marginal.R`, `tests/testthat/test-scale-marginal.R`.
- Modify: `app/R/routes/marginal.R`. Move the post-processing of `.sr_h_gcomp` (from the means and gradients to
  `p1`, `p0`, `rd`, `rd_se`, `log_rr` and `log_rr_se`) into `.h_gcomp_finish(p, grad, V, levels)`, which both
  paths inline. `.sr_h_gcomp` keeps its signature and output.

**Interfaces:**
- Produces:
  - `.h_st_marg_add(st, Xa_list, beta, keep, family)`. `Xa_list` holds, per exposure level a, the design rows
    with the exposure set to a. It accumulates `sum mu(eta_a)`, `sum mu.eta(eta_a) * x_a` and `n`.
  - `.h_gcomp_finish(p, grad, V, levels)` returns the output of `.sr_h_gcomp`.
  - `.h_st_obs_add(st, exposure, y)` accumulates n and events per level (R9). `.h_st_obs_finish(st, levels)`
    returns the table that `.sr_glm_obs_code` builds, using `.h_wilson`.

- [ ] **Step 1: Write the failing tests:**
  - `test_that(".sr_h_gcomp unchanged after the refactor")`: `test-marginal.R` passes unmodified (run it).
  - `test_that("streamed g-computation equals .sr_h_gcomp")`:
    - logistic and modpois on `.sc_frame(5000)`, chunk 7;
    - "analysis" set and "all" set;
    - every field equals at 1e-8, the NNT included (via `.sr_h_nnt`).
  - `test_that("streamed observed risk equals the in-memory table")`: `identical` after `round(., 12)`.
- [ ] **Step 2: Run the tests** and see them fail.
- [ ] **Step 3: Implement** the helpers and the refactor.
- [ ] **Step 4: Run the tests** `test-scale-marginal.R` and `test-marginal.R`. Expected: PASS.
- [ ] **Step 5: Commit** `feat(scale): streamed marginal effects and observed risk`.

### Task 9: Stream compile branches for lm and glm

**Files:**
- Create: `app/R/scale/stream_compile.R`, `tests/testthat/test-scale-compile.R`.
- Modify: `app/R/routes/lm.R:99-151` and `app/R/routes/glm.R` (`.sr_glm_compile_fn`). When
  `identical(spec$engine, "stream")`, return `.sr_lm_compile_stream(spec)` or `.sr_glm_compile_stream(spec,
  kind)`. Everything else is unchanged.

**Interfaces:**
- Consumes: Tasks 5-8 helpers, `sr_terms_code(spec, "stream")`, `sr_outcome_code`, `.sr_marg_code`,
  `.sr_h_coef_table`.
- Produces: the compiled stream body. Evaluated after `.sr_run_prologue_stream`, it returns a value list with
  exactly the in-memory names. Use the field lists in `routes/lm.R` and `routes/glm.R`, with R3's `fig = NULL`,
  plus `engine = "stream"`, `chunk`, `passes` and `iter` (`iter` is `NA` for lm).
- The emitted code:
  - reads `SELECT <cols>, ".sr_row" FROM <sr_src> WHERE <.sr_where>` with `.h_st_pass(..., order = TRUE)`;
  - in each chunk, binds the chunk as `sr_data` inside the step function and runs the in-memory snippets (R8);
  - runs the cluster pass with `ORDER BY "<cl>", ".sr_row"` and `order = FALSE`;
  - writes progress with `.h_st_progress(sr_progress_file, k, m)` (R15). If `sr_progress_file` does not exist,
    the emitted code uses `if (exists("sr_progress_file")) ...`, so the exported script runs without it;
  - holds every in-memory stop text, evaluated on the accumulated counts. These are: no complete rows, the events
    check, `df.residual < 1`, `"se = cluster needs at least two clusters (found ...)"`, and NA cluster ids.
- `stream_chunk` is emitted with `as.integer` as a literal. A missing `stream_chunk` in a stream spec is a compile
  error: `"a stream spec needs stream_chunk (set by the run)"`.

- [ ] **Step 1: Write the failing tests:**
  - `test_that("stream body is lint-clean, ASCII and URL-free")`: for lm, logistic and modpois × model, robust
    and cluster, `.sr_emit_check(body, "analysis")` passes, the body is ASCII, has no `"http"` and no `"p.value"`
    / `"p_value"` branching, and parses.
  - `test_that("memory compile is byte-identical with and without engine")`: for each route,
    `identical(sr_compile(r, spec), sr_compile(r, modifyList(spec, list(engine = "memory"))))`, and also
    with `engine = "auto"`.
  - `test_that("stream body evaluates to the in-memory value")`:
    - for logistic with factor + rcs + cluster SE and chunk 1000, write `.sc_parquet` and evaluate
      `prologue_stream + body` in a new env with `sr_project_dir`, `sr_duck_memory = "2GB"`,
      `sr_duck_threads = 2L` and `sr_duck_temp = tempdir()`;
    - evaluate the memory prologue and body on the rds of the same rows;
    - `coef` matches at 1e-7 (cluster), and `n`, `n_total`, `events`, `aliased`, `levels` and `n_clusters` are
      identical.
- [ ] **Step 2: Run the tests** and see them fail.
- [ ] **Step 3: Implement** the two emitters. Reuse `.sr_inline` for every `.h_` helper, as
  `.sr_glm_compile_fn` does.
- [ ] **Step 4: Run the tests** `test-scale-compile.R`, `test-lm.R`, `test-glm.R` and `test-lint.R`. Expected:
  PASS and `LINT OK`.
- [ ] **Step 5: Commit** `feat(routes): stream compile branch for lm, logistic and modpois`.

### Task 10: SQL Table 1

**Files:**
- Create: `app/R/scale/table1_sql.R`, `tests/testthat/test-scale-table1.R`.
- Modify: `app/R/routes/table1.R:166-196`. When `identical(spec$engine, "stream")`, return
  `.sr_t1_compile_stream(spec)`.

**Interfaces:**
- Produces:
  - `.h_t1_sql_main(con, src, by, vars, nonnormal)` returns a list `identical` in names and structure to
    `.h_t1_main(dat, by, vars, nonnormal)`. It follows spec section 6.7.
    - Column types come from `DESCRIBE SELECT * FROM <src>`.
    - Numeric means a DuckDB type in `TINYINT SMALLINT INTEGER BIGINT HUGEINT UTINYINT USMALLINT UINTEGER UBIGINT
      FLOAT DOUBLE DECIMAL(...)`.
    - `DATE`, `TIME` and `TIMESTAMP*` are refused with the in-memory text.
  - `.sr_t1_compile_stream(spec)` passes threads `1L` (R14).

- [ ] **Step 1: Write the failing tests:**
  - `test_that("SQL Table 1 equals .h_t1_main")`:
    - the variables are continuous (`x1`), nonnormal (`x2`), two-level (`sex`), multi-level (`grp`,
      non-ASCII), a two-valued numeric column, an all-NA numeric column, and a `by` with missing values;
    - every numeric element equals at 1e-10, with the same names and kinds;
    - `.sr_t1_present` of both values gives `identical` tables, i.e. identical strings.
  - `test_that("SQL Table 1 refusals match in-memory texts")`: more than 50 levels, a date column, and fewer than
    two groups each give the same message as `.h_t1_main`, compared with `expect_identical(conditionMessage(e1),
    conditionMessage(e2))`.
- [ ] **Step 2: Run the tests** and see them fail.
- [ ] **Step 3: Implement.**
  - The SD is two-pass: `avg` first, then `sum((x - m)^2) / (n - 1)`, with `m` passed as a literal from the
    first query.
  - Quantiles use `quantile_cont(x, [0.25, 0.5, 0.75])`.
  - Levels use `SELECT DISTINCT`, then the in-memory R ordering code.
- [ ] **Step 4: Run the tests** `test-scale-table1.R` and `test-table1.R`. Expected: PASS.
- [ ] **Step 5: Commit** `feat(scale): exact SQL Table 1`.

### Task 11: Run dispatch, run metadata, progress and the diagnostics refusal

**Files:**
- Modify:
  - `app/R/routes/run.R`: `sr_run` at 242-321 and the `run.completed` reducer at about 95-115;
  - `app/R/scale/engine.R` (`.sr_run_spec`, `.sr_progress_path`);
  - `app/R/routes/result.R` (`sr_present`, R17);
  - every `route$present(` call site (find them with `grep -rn "\$present(" app/R`);
  - `app/R/diagnostics/checks.R:593-655`.
- Create: `tests/testthat/test-scale-run.R`.

**Interfaces:**
- Consumes: `.sr_engine_effective`, `sr_extract_stream`, `.sr_run_prologue_stream`, `sr_engine_resolve`.
- Produces:
  - `.sr_run_spec(project, id, engine = NULL, chunk = NULL)` returns the spec (R6, R7).
  - `.sr_progress_path(project, job)` (R15).
  - `sr_present(route, value, spec)` (R17).
  - `run.completed` gains `engine`, `extract_kind` and `chunk`. The reducer adds `ok_engine`, `ok_extract_kind`
    and `ok_chunk`.
- `sr_run`, under effective memory, keeps the estimate check but changes its stop text to
  `"analysis '<id>' does not fit in memory: <reason>; it can be switched to the streaming engine with
  sr_engine_switch()"`.
- `sr_run`, under stream:
  - skips the memory check;
  - writes the extract with `sr_extract_stream`;
  - runs `paste(.sr_run_prologue_stream(ex), body, sep = "\n")`;
  - passes `inputs = list(sr_project_dir, sr_duck_memory = paste0(caps$duckdb_memory_gb, "GB"), sr_duck_threads =
    caps$duckdb_threads, sr_duck_temp = <project data/tmp>, sr_progress_file)`;
  - keeps `cache_inputs = list(extract = ex$sha)`.

- [ ] **Step 1: Write the failing tests:**
  - `test_that("stream run equals memory run through sr_run")`:
    - two `.sc_locked` projects on the same frame with logistic, factor and robust SEs: one `engine = "memory"`,
      one `engine = "stream"` with `options(sr.stream_chunk_rows = 1000)`;
    - `sr_present(...)$effects[, c("est", "lo", "hi")]` matches at 1e-7;
    - `p$state$runs$a1$ok_engine == "stream"`, `ok_extract_kind == "parquet"` and `ok_chunk == 1000`.
  - `test_that("second stream run is a cache hit with an identical value")`: `cached` is TRUE, and the value
    `identical(serialize(v1, NULL), serialize(v2, NULL))`.
  - `test_that("old spec without engine keeps its run key")`: for a memory analysis whose state spec has no
    `engine`, the `run.started` key equals `sr_content_hash(ex$sha, list(route = key, spec = <state spec>),
    ...)`, computed without `.sr_run_spec`. This pins Review Focus 1.
  - `test_that("memory lock refuses after the budget shrinks, with the switch hint")`: lock with the default
    budget, then `withr::local_envvar(SR_RAM_BUDGET_GB = "0.0001")`; `sr_run` errors with `"it can be switched to
    the streaming engine with sr_engine_switch\\(\\)"`, and no `run.started` is appended. This pins Review
    Focus 2.
  - `test_that("progress file reports passes")`: after a stream run, `.sr_progress_path(p, job)` exists and parses
    to `pass == max` or `pass <= max`.
  - `test_that("stream engine adds the Methods fragment")`: `sr_present` for a stream value has
    `fragments$engine` matching `"fitted by streaming iteratively reweighted least squares"`.
  - `test_that("diagnostics refuse a stream analysis")`: `sr_diag_run` errors with `"diagnostics for a streaming
    analysis arrive in P1e.3"`.
- [ ] **Step 2: Run the tests** and see them fail.
- [ ] **Step 3: Implement.**
  - `.sr_engine_fragment(value, family)` lives in `engine.R`. It uses the spec section 7 texts verbatim, with
    `format(value$n, big.mark = ",")` and `value$chunk` as digits. Table 1 uses its own sentence.
- [ ] **Step 4: Run the tests** `test-scale-run.R`, `test-run.R`, `test-diagnostics.R`, `test-result.R` and
  `test-analyses-job.R`. Expected: PASS.
- [ ] **Step 5: Commit** `feat(run): dispatch on the effective engine, run metadata and progress`.

### Task 12: Script export and the archive for parquet extracts

**Files:**
- Modify:
  - `app/R/writer/script.R:54-86` (`.sr_script_block`);
  - `app/R/archive/freeze.R:123,167`;
  - `app/R/archive/srproj_export.R:78,131`;
  - `app/R/ui/archive_view.R:27`.
- Create: `tests/testthat/test-scale-archive.R`.

**Interfaces:**
- Consumes: `.sr_run_spec(project, id, engine = r$ok_engine, chunk = r$ok_chunk)`, `ok_extract_kind`.
- `.sr_script_block` builds the spec with `.sr_run_spec`, recomputes the key from it, and chooses the prologue by
  `ok_extract_kind`. The stream block defines defaults:
  - `sr_duck_memory <- Sys.getenv("SR_DUCK_MEMORY", "4GB")`;
  - `sr_duck_threads <- 2L`;
  - `sr_duck_temp <- tempdir()`.
  They go in the `local()` before the prologue, ASCII.
- The header comment adds: `"# Streaming analyses need the duckdb package; the parquet extracts are read from
  data/extracts/."`

- [ ] **Step 1: Write the failing tests:**
  - `test_that("analysis.R reproduces a stream run")`:
    - after a stream `sr_run`, `sr_analysis_script(p, "a1")` parses and is ASCII;
    - sourcing it in a fresh `callr::r` with `SR_PROJECT_DIR` set gives `sr_results$a1$coef`, equal to the
      stored value at 1e-12.
  - `test_that("memory export unchanged for an old run")`: the script text for a memory run equals the text built
    with the pre-change logic, i.e. it has `.rds` and no `duckdb`.
  - `test_that("freeze lists the parquet extract")`: the freeze manifest has the entry
    `data/extracts/<sha>.parquet` with kind `"extract"`.
  - `test_that("full, lean and auto export then import round-trip the parquet")`: after import, the extract
    exists and its SHA matches; for lean, use the existing lean rules.
  - `test_that("archive panel accepts a parquet extract")`: the extract check in `archive_view` reports ok.
- [ ] **Step 2: Run the tests** and see them fail.
- [ ] **Step 3: Implement.** Replace each hardcoded `".rds"` with `paste0(".", r$ok_extract_kind %||% "rds")`,
  through one helper `.sr_extract_rel(sha, kind)` in `run.R`.
- [ ] **Step 4: Run the tests** `test-scale-archive.R`, `test-writer-script.R`, `test-archive-*.R` and
  `test-writer-export.R`. Expected: PASS.
- [ ] **Step 5: Commit** `feat(archive): parquet extracts in export, freeze and the Archive panel`.

### Task 13: SAP_v1 Engine column and deviation text in the manuscript

**Files:**
- Modify: `app/R/writer/sapdoc.R` (the analyses table, about lines 166 and 301) and `app/R/writer/fragments.R`
  (only if the deviations sentence needs the computational kind).
- Create: `tests/testthat/test-scale-writer.R`.

**Interfaces:**
- Consumes: `lock$engines` (locked), `.sr_sap_engines(project)` (draft), `sr_deviations` (Task 3), and
  `sr_present` (Task 11).
- The SAP_v1 analyses table gains a column `Engine` with the values `"In memory"` and `"Streaming"`. The
  resolution reason goes in a footnote per analysis.

- [ ] **Step 1: Write the failing tests:**
  - `test_that("SAP_v1 shows the Engine column")`: the docx table text contains `"Engine"` and `"Streaming"` for a
    stream lock, and the footnote holds `"auto -> stream"`.
  - `test_that("manuscript Methods carry the engine sentence through placeholders")`:
    - the Methods paragraph of a stream analysis contains `"fitted by streaming iteratively reweighted least
      squares"`;
    - its template text holds no digit, because the numbers are `{{E..}}` tokens (as `writer/fragments.R`
      requires);
    - the claim guard passes.
  - `test_that("engine switch appears in the deviations text")`.
- [ ] **Step 2: Run the tests** and see them fail.
- [ ] **Step 3: Implement.**
- [ ] **Step 4: Run the tests** `test-scale-writer.R`, `test-writer-sapdoc.R`, `test-writer-docx.R` and
  `test-writer-guard.R`. Expected: PASS.
- [ ] **Step 5: Commit** `feat(writer): engine column in SAP_v1 and the engine Methods sentence`.

### Task 14: UI - Engine field, badge, progress and the switch dialog

**Files:**
- Modify:
  - `app/R/ui/spec_form.R` (`SR_FORM_KINDS`, line 18);
  - `app/R/ui/mod_sap.R` (the checklist rows come from `sr_sap_check`; check that they render);
  - `app/R/ui/analyses_view.R`;
  - `app/R/ui/analyses_job.R`;
  - the Analyses module (the `mod_analyses` file).
- Create: `tests/testthat/test-scale-ui.R`.

**Interfaces:**
- Consumes: `sr_engine_switch`, `.sr_progress_path`, `state$runs[[id]]$ok_engine`.
- Produces:
  - `.sr_an_engine_badge(p, id)` returns `"Streaming"` or `""`.
  - `.sr_an_progress_text(file)` returns `"pass 4 of at most 29"`, or `""` when the file is missing or
    unreadable.
  - The form field `engine` has choices `c("Auto" = "auto", "In memory" = "memory", "Streaming" = "stream")`.
    Cox and RMST show only Auto and In memory.

- [ ] **Step 1: Write the failing tests** with `testServer` and the pure helpers:
  - `test_that("Engine select appears for the five routes and is limited for cox/rmst")`.
  - `test_that("badge reads ok_engine")`.
  - `test_that("progress line reads progress.json")`, written by hand as `{"pass": 4, "max": 29}`.
  - `test_that("switch dialog requires a reason and calls sr_engine_switch")`:
    - an empty reason shows the error and appends no event;
    - a reason appends `engine.switched`.
  - `test_that("switch action shown after a memory refusal")`: the run error text contains
    `"sr_engine_switch"`, and the action is visible.
- [ ] **Step 2: Run the tests** and see them fail.
- [ ] **Step 3: Implement**, following the existing module patterns (`helper-ui.R`).
- [ ] **Step 4: Run the tests** `test-scale-ui.R`, `test-ui-spec-form.R`, `test-mod-sap.R`,
  `test-mod-analyses.R`, `test-analyses-view.R` and `test-app-wiring.R`. Expected: PASS.
- [ ] **Step 5: Commit** `feat(ui): engine field, streaming badge, progress line and switch dialog`.

### Task 15: Equivalence suite, edge cases and the scale tools

**Files:**
- Create:
  - `tests/testthat/test-scale-equiv.R`;
  - `tools/scale_equiv.R`, which takes the argument `--rows` (default 1e6) and writes a CSV and a markdown
    summary to the scratchpad or `tools/out/`, git-ignored;
  - `tools/scale_fit.R`, which takes `--rows` (default 2e7) and `--patients` (default 2e6) and samples peak RSS
    with the `callr` watcher from `tools/scale_test.R`.

**Interfaces:**
- Consumes: everything above, through `sr_run` and `sr_present` only.

- [ ] **Step 1: Write the matrix test.** `test_that` blocks are generated in a loop with names
  `"equiv <route> <se> <terms> chunk <k>"`:
  - routes {lm, logistic, modpois} × se {model, robust, cluster} × terms {linear, factor, rcs, quad,
    factor:linear};
  - chunks {7, 1000, 25000} on `.sc_frame(20000)`;
  - `exclude_rows = c(3, 500, 19999)` on one case per route;
  - marginal "analysis" and "all" on logistic and modpois, and observed risk;
  - Table 1 as in Task 10.
  - The tolerances are the Global Constraints ones.
  - To keep the suite fast, chunk 7 runs only on the linear and factor terms with cluster SEs. Every other cell
    runs at chunk 1000.
- [ ] **Step 2: Write the edge-case block**, each comparing the memory and stream error message or value:
  - no complete case;
  - a single cluster;
  - an all-0 and an all-1 outcome;
  - an aliased column;
  - maxit non-convergence: the `converged` field;
  - separation: `near_separation`;
  - an NA cluster id;
  - `exclude_rows` naming every row;
  - an all-NA column;
  - a text outcome with `event_value`;
  - the non-ASCII factor `grp` as the exposure.
  This pins Review Focus 3 and 5.
- [ ] **Step 3: Run** `test-scale-equiv.R`. Expected: PASS. Fix any failure in the owning task's helper and
  re-run that task's tests.
- [ ] **Step 4: Run** `Rscript tools/scale_equiv.R --rows 1000000` in the background. Expected: every comparison
  within tolerance, printed as `SCALE EQUIV PASS`.
- [ ] **Step 5: Run** the full suite with `NOT_CRAN=true`. Expected: all PASS and `LINT OK`.
- [ ] **Step 6: Commit** `test(scale): memory vs stream equivalence matrix, edge cases and scale tools`.

### Task 16: Docs and the browser walk

**Files:**
- Create: `docs/scale.md`, the user guide to the engines, the switch, limits, the P1e.2 and P1e.3 refusals, and
  the options `stream_chunk_rows` and `stream_knot_rows`.
- Modify:
  - `docs/analysis.md`: an "Analysis at scale" section;
  - `docs/validation.md`: the 1M-row results from Task 15 and the 20M-row smoke row, marked "pending (laptop)"
    until it is run;
  - `memory.md`:
    - the P1d UI 3 rows become "merged (PR 10, PR 11)";
    - P1e.1 is "built on p1e1-streaming";
    - record the gotchas found;
  - `docs/roadmap.md`: the P1e.1 status.

- [ ] **Step 1: Write the docs.** Use no personal paths; use `%USERPROFILE%` and `<repo root>`.
- [ ] **Step 2: Do the browser walk** in spec section 11 on synthetic data, with `SR_RAM_BUDGET_GB` forced low.
  Check that the SAP shows the stream row and locks, that the run shows progress and the badge, that the result
  matches an in-memory run of the same spec, and that the docx Methods carries the sentence. Record the outcome in
  `docs/validation.md`.
- [ ] **Step 3: Run** `tools/scale_fit.R` on the laptop if the session runs there. Otherwise leave the row
  "pending (laptop)" and say so in the hand-off.
- [ ] **Step 4: Commit** `docs: P1e.1 scale guide, validation results and memory`.
