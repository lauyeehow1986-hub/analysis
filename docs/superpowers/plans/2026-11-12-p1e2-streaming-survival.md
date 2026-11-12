# P1e.2 Streaming Cox, piecewise-exponential and KM/RMST Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Run `cox@1`, `cox_describe@1` and `rmst@1` exactly on analysis tables of any row count through the
P1e.1 streaming engine, and add the opt-in piecewise-exponential (PEM) Cox method on both engines.

**Architecture:** The stream body reads the Parquet extract in chunks, sorted by stratum and descending time,
through P1e.1's locked-down DuckDB. The exact Cox replicates `survival:::coxph.fit`: Newton passes accumulate
the log-likelihood, score and information per chunk, with tie blocks and strata carried across chunk
boundaries. The cluster SE adds a pass that writes an event-time table and a pass that sums the score
residuals per cluster. KM and RMST are rebuilt from streamed count tables. PEM splits rows into intervals, in
R for memory and in SQL for stream, then fits a Poisson GLM; the stream fit is P1e.1's IRLS. The memory code
of the existing specs stays byte-identical.

**Tech Stack:** R 4.5.x, survival 3.8.3 (reference values and the memory path), duckdb, DBI, testthat.

**Spec:** `docs/superpowers/specs/2026-11-12-p1e2-streaming-survival-design.md`. Read it, then the Rulings
below, before any task. Where a ruling differs from the spec, the ruling wins; S20 and S21 are the two
deliberate deviations.

## Global Constraints

- Rscript is `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"`. Run tests with `NOT_CRAN=true`.
  - Never run long code through `Rscript -e`; write a script file to the scratchpad.
  - Run one file with `Rscript -e "testthat::test_file('tests/testthat/<file>.R', load_package='none')"`.
- Naming: exported functions `sr_`, internal `.sr_`, inlined helpers `.h_` (stream ones `.h_st_`), snake_case.
  One concern per file. Read options with `sr_opt("x", default)`.
- `.R` and `.ps1` files and every emitted code string are **ASCII only**. Write non-ASCII test levels as
  `"\u00e9"`.
- **Air-gap.**
  - No network calls, and no URL literal in emitted code or SQL.
  - Every DuckDB connection in emitted code goes through `.h_st_open`.
  - Emitted code passes `.sr_emit_check`. Writing to a data file is forbidden there, and P1e.2 adds no
    allow-list entry (S20).
- **No test-gated switching.** `compile()` never reads data or a p-value. `tests/testthat/test-lint.R` stays
  green.
- **Memory code is frozen.** For a spec with no `cox_method` or with `cox_method = "exact"`, and the engine
  memory, `sr_compile` output is byte-identical to main. The sha256 pins of Task 1 enforce this.
- **Exact texts** quoted in this plan are used verbatim.
- **Tolerances** use `expect_equal(..., tolerance = t)`, relative to max(1, |x|):

  | Quantity | Tolerance |
  |---|---|
  | Cox coef, SE, CI, p (model and cluster), stream vs memory | 1e-8 |
  | KM surv, lower, upper | 1e-12 |
  | RMST, its SE, differences | 1e-10 |
  | PEM stream vs PEM memory | 1e-8 |
  | n, n_total, events, n_clusters, aliased, levels, warnings, km/km_risk counts, cuts | identical |
  | chunk sizes 7, 50 and 1e5 against each other | 1e-12 |

- **Commits.**
  - Trailer: `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.
  - Before every commit, grep `git diff --cached` for the Windows username, `Users\`, emails and hostnames
    (the controller gives the exact pattern). It must print nothing.
  - Do not push.
- **Synthetic data only**, made in test files.

## Review Focus

1. **A tie block or a stratum that straddles a chunk boundary.** Pending rows must be held back and replayed
   once, with S0, S1, W, A and B carried. At chunk 7 the result equals chunk 1e5 to 1e-12. Pinned in Task 3
   (block tests) and Task 5 (chunk sweep with a stratum ending exactly on a boundary).
2. **Near-tie times** (two times 1e-10 apart) that `aeqSurv` merges. Without the map the risk sets differ
   from coxph's. Pinned in Task 2 (map vs `aeqSurv`) and Task 5 (fit vs coxph).
3. **A factor level absent from the analysed rows.** coxph keeps it and gives an NA coefficient (`aliased`).
   The stream must keep the column, not drop it the way `.h_st_mm_cols` does. Pinned in Task 5.
4. **Monotone likelihood and an aliased column.** The warnings must be coxph's exact strings, including the
   trailing space, and `aliased` must match. Pinned in Task 4 and Task 5.
5. **An old Cox project reopened after P1e.2.** It must keep the same SAP hash and the same compiled code, and
   be treated as `cox_method = "exact"` with no rerun. Pinned in Task 1 (pins, hash fixture) and Task 10.

---

## Rulings

S1 to S9 are probe facts (survival 3.8.3). S10 to S21 are design rulings made while writing this plan.

- **S1 Centring and scaling** (coxph.fit `nocenter = c(-1, 0, 1)`).
  - A column whose analysed values all lie in {-1, 0, 1} is neither centred nor scaled.
  - Every other column j is centred by its mean m_j and scaled by s_j = n / sum|x - m_j|, with s_j = 1 when
    the sum is 0.
  - **Coordinates.** The stream computes in centred, unscaled coordinates. Means and the {-1,0,1} flags come
    from pass 0. sum|x - m| comes from the first Newton pass (beta = 0).
  - **Scaled step.** Only `.h_st_chol2` / `.h_st_chsolve2` / `.h_st_chinv2` run in scaled coordinates:
    U_s = U * s, I_s = I * outer(s, s), step = step_s * s.
- **S2 Factor levels.** coxph keeps unused levels. The stream design is `model.matrix(~ <tc$rhs>)` without the
  intercept column, built with the full `.sr_fixed$levels`. A level absent from the rows is an all-zero
  column, which cholesky2 declares singular, so its coefficient is NA.
- **S3 Near ties** (`aeqSurv`).
  - tol = sqrt(.Machine$double.eps).
  - y = the sorted unique finite times, dy = diff(y), tied = dy <= tol | dy / mean(abs(y)) <= tol.
  - cuts = y[c(TRUE, !tied)], and each t maps to cuts[findInterval(t, cuts)].
- **S4 Control** (`coxph.control()` defaults, checked with `formals`): eps = 1e-9,
  toler.chol = .Machine$double.eps^0.75, iter.max = 20, toler.inf = sqrt(eps).
- **S5 Newton loop** (coxfit6).
  - **Start.** Evaluate at beta = 0, giving loglik0, U and I. Then cholesky2(I), and step = chsolve2(I, U).
  - **Each iteration** (iter 1..20): newbeta = beta + step; evaluate at newbeta; cholesky2; notfinite = any
    non-finite U, I or loglik.
  - **Converged** when notfinite == 0 and |1 - lbest / newlk| <= eps. This test runs before the reject test.
    On convergence beta = newbeta, and loglik, U and I are those at newbeta.
  - **Reject** when notfinite or newlk < lbest: halving += 1, newbeta = (newbeta + halving * beta) /
    (halving + 1). The next iteration evaluates that newbeta; step is unchanged.
  - **Accept:** halving = 0, lbest = newlk, beta = newbeta, step = chsolve2(I, U), newbeta = beta + step.
  - **Exhaustion** (20 iterations without convergence): evaluate once more at beta (the best), flag 1000.
  - There is no clamp on beta.
- **S6 cholesky2.** It works on the scaled I. thr = toler.chol * max(diag). A pivot that is non-finite or
  below thr gets diagonal 0, and its column is skipped. chsolve2 gives 0 for singular coordinates, and chinv2
  gives zero rows and columns there.
- **S7 Post-processing** (coxph.fit, in unscaled coordinates).
  - V = chinv2(I) at the final beta. `which.sing = diag(V) == 0` gives an NA coefficient.
  - infs = abs(U %*% V).
  - Flag 1000: warning "Ran out of iterations and did not converge". If, in addition, max(abs(eta)) > 500
    or any infs is non-finite, the warning "one or more coefficients may be infinite" follows.
  - Otherwise: infs = !is.finite(U) | (infs > eps & infs > toler.inf * abs(coef)). If any, the warning is
    `paste0("Loglik converged before variable ", paste(which(infs), collapse = ","), "; coefficient may be infinite. ")`.
    It ends in a space; compare with `conditionMessage`. The indices count design columns, singular ones
    included.
- **S8 Efron information.**
  - I = sum_l (S2 - (l/d) E2) / D_l - sum_l a_l a_l' / D_l^2, with a_l = S1 - (l/d) E1 and
    D_l = S0 - (l/d) E0, for l = 0..d-1.
  - The risk set at t is every row with time >= t, censored rows included.
  - **Breslow** is the same algebra with every (l/d) term set to 0.
- **S9 Score residual** (coxscore2).
  - resid_i = delta_i (x_i - xbar_mean(t_i)) - r_i [x_i Lam(t_i) - B(t_i)] + delta_i r_i [x_i c1 - c2(t_i)].
  - **Efron, per block with d events:** h_l = 1 / D_l, xbar_l = (S1 - (l/d) E1) / D_l, dLam = sum h_l,
    dB = sum h_l xbar_l, xbar_mean = sum xbar_l / d, c1 = sum (l/d) h_l, c2 = sum (l/d) h_l xbar_l.
  - **Breslow:** c1 = 0, c2 = 0, xbar_mean = S1 / S0, dLam = d / S0, dB = d S1 / S0^2.
  - Lam(t) and B(t) are cumulative over the event times <= t in the stratum, and include the row's own block.
  - **Robust variance:** D = rowsum(resid %*% V, cluster), V_robust = D'D, with no small-sample factor. In
    code, `.h_st_cluster_step(st, resid %*% V, 1, cluster)` and then `.h_st_cluster_flush`.
- **S10 Reading order.** Each pass reads:

  ```
  SELECT <cols>, DENSE_RANK() OVER (ORDER BY <strata cols>) AS ".sr_stratum", ".sr_row"
  FROM sr_src WHERE (<.sr_where>)
  ORDER BY <strata cols>, <time col> DESC, ".sr_row"
  ```

  - With no strata, `.sr_stratum` is the literal `1`.
  - `.h_st_pass(..., order = FALSE)` is used, because the SQL carries its own ORDER BY.
  - The time guard runs after the R8 snippets. Within one `.sr_stratum`, `.time` must be non-increasing, with
    the last stratum and time carried across chunks. Otherwise the chunk stops with "the streamed Cox needs
    .time to keep the order of the time column".
- **S11 Pass 0** collects:
  - n, events and n_total;
  - the {-1,0,1} flags and the column sums of X;
  - the KM counts keyed by (KM group, raw time) (S13);
  - the distinct strata present.

  The near-tie map is built after pass 0 from the distinct raw times of the KM count table (S3 applied in R).
  Later chunks map `.time` with `.h_st_timefix_apply`. The map only moves times down and is monotone, so the
  DESC order and the contiguity of tie blocks survive.
- **S12 W equals Lam (amended after the Task 3 review).** The deferred S2 weight of a block is
  w1 = sum_l 1/D_l, which is the hazard increment dLam, for both Breslow and Efron.
  - **Do not use `Lam*A - B`.** It cancels catastrophically once max|eta| exceeds about 10 (I came out 0 or
    negative at beta = 4 and 6, p = 1, x sd 2).
  - **Positive-weight form.** Carry per stratum S2c = sum of r x x' over rows already read. Per chunk
    segment, W = sum of w1 over its complete blocks, and each row gets wrow = the reverse cumulative sum of w1
    from its own block to the segment end (0 for pending-block rows). Then
    I += W * S2c + crossprod(Xr, (rr * wrow) * Xr), and then S2c += crossprod(Xr, rr * Xr). S2c resets at a
    stratum change; there is no stratum-close term.
  - Task 6's residual term x_i Lam(t_i) - B(t_i) has the same shape; compare it at a looser tolerance when
    max|eta| is large.
- **S13 KM group** (as in `.sr_cox_compile`).
  - **Factor exposure (`t1f`):** its levels in order, with absent levels dropped (`droplevels`).
  - **Numeric exposure:** counts are keyed by value while the number of distinct values seen is <= 8. Once a
    ninth value appears, the table collapses to one group "All" and stays collapsed. Labels are
    `levels(factor(values))`.
  - **`km_risk` times:** `pretty(range(raw time), n = 6)`, keeping those <= max raw time. n.risk at tt is the
    count of mapped times >= tt.
  - **RMST:** the KM figure uses the map built over all its rows. Each group's RMST uses a map built from that
    group's own distinct times, because `.h_rmst` calls survfit on one group.
- **S14 RMST distinct exposures.** The distinct exposure values are kept exactly, so the ".nlv" stop text
  (count > 8) is identical. RMST refuses `exclude_rows` and clustering already, so neither is handled.
- **S15 Progress.**
  - Exact Cox: m = 1 (pass 0) + 22 (init + 20 iterations + the exhaustion pass) + 2 when clustered. It is
    lowered to the passes run when the fit converges. This corrects the spec's "1 + 21".
  - PEM: m = 1 + 26 + (se != "model").
  - RMST: m = 1.
- **S16 PEM cuts and baseline.**
  - **Cuts.** quantile: `c(0, unique(stats::quantile(<analysed event times, raw>, probs = (1:(K-1))/K,
    type = 7, names = FALSE)), Inf)`. explicit: `c(0, at[at < max(time)], Inf)`.
  - **Splitting.** Interval j is (c[j-1], c[j]]. pt = pmin(.time, hi) - lo and ev = .event * (.time <= hi),
    both computed in R. The near-tie map does not apply to the fit, but the KM data still use it.
  - **Baseline.** One indicator per present (stratum, interval) combination. A combination is present when
    the stratum's max time is above c[j-1]. Indicators come before the covariates, and their order is free.
  - **Value.** `aliased` lists covariates only, on both engines.
  - **SE.** The model SE uses dispersion 1. The cluster SE is `.h_st_vcov_finish("cluster", ...)`, the
    modpois path (vcovCL HC1), on the expanded rows.
- **S17 Engine mapping.**
  - `.sr_engine_kind` gains `cox = "cox"`, `cox_describe = "cox"` and `rmst = "rmst"`.
  - `.sr_engine_fragment(value, kind, requested)`:
    - kind "cox": `how` is "Newton-Raphson iteration on the partial likelihood", or "iteratively reweighted
      least squares" when `value$method == "pem"`.
    - kind "rmst" returns `paste0("The restricted mean survival times were computed exactly from Kaplan-Meier
      counts accumulated over all ", num(value$n), " rows in chunks of ", num(value$chunk), " rows.")`.
- **S18 Value names.** The exact Cox stream value has the memory names (coef, aliased, n, n_total, events,
  level, se_type, n_clusters, ties, strata, levels, warnings, km_by, km, km_risk), then engine, chunk, passes
  and iter.
  - PEM inserts `method = "pem"`, `cuts` and `k` after `km_risk`, on both engines.
  - RMST adds engine, chunk and passes.
- **S19 Non-finite stop.** If the loglik at the final beta is not finite, the stream stops with "the streamed
  Cox fit gave a non-finite log-likelihood at every step-halving".
- **S20 (deviation from spec 4.4 and 8) Event-time table.** The table is a DuckDB TEMP table `sr_et` in the run
  connection, not `<job>.tmp-et.parquet`.
  - It is created from the first chunk. Each chunk then runs `duckdb::duckdb_register(con, "sr_et_chunk", df)`,
    an INSERT and an unregister.
  - Cumulative Lam and B come from a view with `SUM(...) OVER (PARTITION BY stratum ORDER BY time ROWS
    UNBOUNDED PRECEDING)`.
  - There is no file write, so there is no emit allow-list entry and no `.h_st_et_write`.
  - If this is wrong, a huge event-time table might not spill under the memory limit. The connection's temp
    directory is set by `.h_st_open`.
- **S21 (deviation from spec 4.2) Near-tie map.** It is built in R from the pass-0 distinct times (S11), not by
  a SQL `LAG` candidate query. It is exact by construction, and simpler.
- **S22 Cluster pass SQL.**

  ```
  SELECT s.<cols>, s.".sr_stratum", s.".sr_row", et.* EXCLUDE (stratum, time)
  FROM (<S10 select without ORDER BY>) s
  ASOF LEFT JOIN sr_et_cum et ON s.".sr_stratum" = et.stratum AND s.<time col> >= et.time
  ORDER BY <quoted cluster col>, s.".sr_row"
  ```

  - The join uses raw time. That is exact because the map moves each time down to the largest cut <= it.
  - A row with no event time at or before it gets NA terms, which are treated as 0.

---

## File structure

| File | Responsibility |
|---|---|
| `app/R/scale/stream_surv.R` (new) | `.h_st_timefix_map`, `.h_st_timefix_apply`, `.h_st_km_add`, `.h_st_km_counts`, `.h_st_km_finish`, `.h_st_rmst` |
| `app/R/scale/stream_cox.R` (new) | `.h_st_chol2`, `.h_st_chsolve2`, `.h_st_chinv2`, `.h_st_cox_new`, `.h_st_cox_step`, `.h_st_cox_close`, `.h_st_time_guard`, `.h_st_coxfit`, `.h_st_cox_warn`, `.h_st_et_step`, `.h_st_cox_resid` |
| `app/R/scale/stream_fit.R` | `.h_st_irls` gains an offset (Task 9) |
| `app/R/scale/stream_cox_compile.R` (new) | `.sr_cox_compile_stream`, `.sr_rmst_compile_stream`, `.sr_pem_compile_stream`, `.sr_cox_sql` |
| `app/R/routes/cox.R` | fields and validation, stream and PEM branches, `.sr_cox_pem_compile`, the estimand summary, the PEM fragments |
| `app/R/routes/rmst.R` | schema fields and the stream branch |
| `app/R/scale/engine.R` | refusal line removed, `.sr_engine_kind`, `.sr_engine_fragment`, header |
| `app/R/sap/` (SAP text) | the method sentence (3.2) |
| `app/R/ui/spec_form.R` | Method and cut-point fields |
| `tests/testthat/helper-scale.R` | survival routes in `.ssc_route`, `.ssc_body` branch, `.ssc_surv_frame`, `.ssc_surv_expect` |
| `tests/testthat/test-scale-surv-*.R` (new) | one file per task, named below |

---

### Task 1: Pins, spec fields, validation, engine mapping and UI

**Files:**
- Create: `tests/testthat/test-scale-surv-pins.R`, `tests/testthat/test-scale-surv-fields.R`.
- Modify:
  - `app/R/routes/cox.R`: `.sr_cox_schema` (389-404), `.sr_cox_check` (67-87), `.sr_cox_estimand` (89-104).
  - `app/R/routes/rmst.R`: the schema (322-333).
  - `app/R/scale/engine.R`: `.sr_engine_refusal` (14-24, remove the cox/rmst branch at 16-18), the header at
    3-4, `.sr_engine_kind` (203-209), `.sr_engine_fragment` (225-243).
  - The SAP method text: find where the SAP describes a Cox analysis (`grep -rn "partial likelihood\|coxph"
    app/R/sap`).
  - `app/R/ui/spec_form.R`: `SR_FORM_KINDS` (18-33), `SR_FORM_EXTRAS` (70+), `.SR_FORM_LABELS`.
  - Existing tests that assert the P1e.2 refusal text (`grep -rn "arrives in P1e.2" tests`).

**Interfaces:**
- Produces:
  - Schema fields on cox and cox_describe:
    - `stream_chunk = list(type = "num", required = FALSE)`;
    - `cox_method = list(type = "chr", required = FALSE, values = c("exact", "pem"))`;
    - `pem_cuts = list(type = "list", required = FALSE)`.
  - rmst gains `stream_chunk`.
  - `.sr_cox_method_of(spec)` returns `"exact"` or `"pem"` (NULL gives "exact").
  - `.sr_pem_cuts_of(spec)` returns `spec$pem_cuts`, or `list(rule = "quantile", k = 10)` when it is NULL.
  - `.sr_engine_kind("cox")`, `("cox_describe")` and `("rmst")` give "cox", "cox" and "rmst".

- [ ] **Step 1: Write the pins on main first.**
  - In a scratch checkout of `main`, compile these specs with `sr_compile(route, spec)` and record
    `.sr_sha256(charToRaw(code))`:
    - cox: model SE, cluster, two strata, Breslow, an interaction, rcs, MI and bootstrap;
    - cox_describe: one spec;
    - rmst: plain and MI.
  - Write `test-scale-surv-pins.R` with `test_that("memory code of survival specs is unchanged")`. It asserts
    each hash literal. Build the specs inside the test file with fixed values, so the pins depend on nothing
    else.
- [ ] **Step 2: Write the failing tests** in `test-scale-surv-fields.R`:
  - `test_that("cox and rmst accept the stream engine")`: `sr_spec_validate` is ok with `engine = "stream"`,
    and no error contains "arrives in P1e.2".
  - `test_that("pem_cuts needs cox_method pem")`: it fails with "pem_cuts needs cox_method = \"pem\"".
  - `test_that("explicit cuts must be increasing finite and positive")`: `at` = c(2, 1), c(1, 1), c(0, 1),
    c(1, Inf) and "a" each fail with "pem_cuts$at must be increasing, finite numbers above zero"; c(1, 2.5) is
    ok.
  - `test_that("quantile k is a whole number in 2..50")`: k = 1, 51 and 2.5 fail with "pem_cuts$k must be a
    whole number from 2 to 50"; an unknown rule fails with "pem_cuts$rule must be \"quantile\" or
    \"explicit\"".
  - `test_that("pem refuses MI and bootstrap on both engines")`: the error is "PEM with multiple imputation /
    bootstrap arrives in P1e.3" for engine memory and stream.
  - `test_that("pem estimand summary names the piecewise baseline")`: the summary equals "hazard ratio from a
    piecewise-constant baseline hazard (piecewise-exponential model)". The exact summaries are unchanged.
  - `test_that("SAP text names the method")`: exact contains "Cox proportional hazards model (partial
    likelihood, Efron/Breslow ties)". PEM with k = 10 contains "piecewise-exponential Poisson model with 10
    intervals (rule quantile)". PEM with explicit `at = c(1, 2)` contains "(rule explicit: 1, 2)".
  - `test_that("engine kind and fragment cover survival")`: `.sr_engine_fragment(list(n = 10, chunk = 5),
    "cox")` contains "Newton-Raphson iteration on the partial likelihood". With `method = "pem"` it contains
    "iteratively reweighted least squares". The "rmst" kind gives the S17 sentence.
  - `test_that("the cox form offers method and cut fields")`: `.sr_form_engine_choices` offers "stream" for
    cox and rmst. The form kinds contain `cox_method` (enum) and the `pem_cuts` extras. The labels are
    "Cox (exact partial likelihood)", "Piecewise-exponential (Poisson)", "K intervals at event-time quantiles"
    and "explicit".
- [ ] **Step 3: Run** both files. The fields file FAILS; the pins PASS on the branch before any edit.
- [ ] **Step 4: Implement.**
  - The validation lines go in `.sr_cox_check`, after the existing checks.
  - Do not touch any line `.sr_cox_compile` emits.
  - In `.sr_cox_estimand`, only `summary` changes, and only for PEM.
  - The SAP text is built from `.sr_cox_method_of` and `.sr_pem_cuts_of`. For an exact spec its output stays
    byte-identical to main.
  - UI:
    - "Method" select, `to = "cox_method"`;
    - "Cut points" radio, `to = c("pem_cuts", "rule")`;
    - K numeric, `to = c("pem_cuts", "k")`;
    - explicit text, `to = c("pem_cuts", "at")`, parsed with `as.numeric(strsplit(x, ",")[[1]])`.
    Follow the existing `SR_FORM_EXTRAS` entries.
- [ ] **Step 5: Run** the two new files, plus `test-cox.R`, `test-rmst.R`, `test-scale-engine.R`,
  `test-scale-lock.R`, `test-spec.R`, `test-spec-form*.R` and `test-sap*.R`. Expected: all PASS.
- [ ] **Step 6: Commit** `feat(cox): cox_method and pem_cuts fields, survival stream engine allowed`.

### Task 2: Near-tie map, KM counts and RMST port

**Files:**
- Create: `app/R/scale/stream_surv.R`, `tests/testthat/test-scale-surv-km.R`.

**Interfaces:**
- Produces (all inlined as `.h_st_*` by `.sr_inline`, so each is self-contained and calls base/stats only):
  - `.h_st_timefix_map(times)` takes the sorted unique finite raw times and returns `list(from, to)` for the
    times S3 moves; both are empty when none move.
  - `.h_st_timefix_apply(t, map)` returns `t` with each `map$from` value replaced by its `map$to` (via
    `match`).
  - `.h_st_km_add(st, time, event, group)` accumulates the per-chunk `rowsum` of `cbind(d = event, c = 1 -
    event)` keyed by (group, raw time) and collapses the parts every 16 chunks. `st` starts as `NULL`.
  - `.h_st_km_counts(st, map = NULL)` returns a data.frame(g, t, d, c) with keys collapsed, the map applied to
    `t`, re-summed by `rowsum`, and the rows sorted by g, then t.
  - `.h_st_km_finish(counts, levels, trange)` returns `list(km, risk)` with exactly the columns, types and row
    order of `.sr_h_km`. `levels` is the group order and `trange` the raw time range.
  - `.h_st_rmst(counts1, tau)` takes the counts of one group, already mapped by that group's own map, and
    returns `c(rmst = , se = )`.

- [ ] **Step 1: Write the failing tests:**
  - `test_that("timefix map equals aeqSurv")`: for times with pairs 1e-10 apart, a triple chain and exact
    duplicates, `.h_st_timefix_apply(t, .h_st_timefix_map(sort(unique(t))))` is identical to
    `as.numeric(survival::aeqSurv(survival::Surv(t, rep(1, n)))[, 1])`. With no near ties the map is empty.
  - `test_that("km finish equals .sr_h_km")`: on 2000 rows with integer-time ties, near ties, and censoring at
    the last time:
    - one group, two groups, and a factor with an absent level;
    - counts fed in chunks of 7 and 1e5;
    - `km` and `risk` are identical in names, strata, time, n.risk, n.event and n.censor, with
      surv/lower/upper at 1e-12.
    - Include a group whose last time has d = n.
  - `test_that("km collapses after 16 chunks without changing counts")`: chunk 7 over 500 rows equals chunk
    1e5.
  - `test_that("rmst port equals survmean")`: for tau inside the follow-up, tau equal to an event time and tau
    equal to the max time, `.h_st_rmst` equals `.sr_h_rmst` at 1e-10, with near ties present.
- [ ] **Step 2: Run** `test-scale-surv-km.R`. Expected: FAIL, could not find function.
- [ ] **Step 3: Implement** `stream_surv.R`.
  - `.h_st_km_finish` follows survfit with `conf.type = "log"`: Greenwood std.err, lower and upper =
    exp(log(surv) -/+ qnorm(0.975) * std.err), upper capped at 1. For the rows where d = n (surv drops to
    0), print survfit's std.err, lower and upper on the test fixture first, then reproduce those values
    (the test pins them).
  - Risk rows follow `summary(fit, times = tt, extend = TRUE)` for each group.
  - `.h_st_rmst` ports the `rmean` branch of `survival:::survmean`. Read it with
    `deparse(survival:::survmean)`, including its `n.risk - n.event == 0` handling.
- [ ] **Step 4: Run** `test-scale-surv-km.R`. Expected: PASS.
- [ ] **Step 5: Commit** `feat(scale): streamed KM counts, near-tie map and RMST port`.

### Task 3: Cholesky port and the Cox chunk step

**Files:**
- Create: `app/R/scale/stream_cox.R` (first part), `tests/testthat/test-scale-surv-coxstep.R`.

**Interfaces:**
- Produces:
  - `.h_st_chol2(M, toler)` returns `list(L, D, sing)`, an LDL' of a symmetric matrix as in S6. `sing` is
    logical per column.
  - `.h_st_chsolve2(ch, b)` returns the solution, with 0 at singular coordinates.
  - `.h_st_chinv2(ch)` returns the inverse, with zero rows and columns at singular coordinates.
  - `.h_st_cox_new(p, ties)` returns the per-pass state: loglik, U, I, carries S0, S1, Lam, A, B, the current
    stratum, the pending block rows, and `sumabs` (S1).
  - `.h_st_cox_step(st, time, event, X, eta, strat)` returns `st` for one chunk. Its inputs are the mapped
    time (DESC within each stratum), the 0/1 event, the centred X matrix, `eta = X %*% beta` and the
    stratum id. It completes every tie block that ends inside the chunk and holds the last block back.
  - `.h_st_cox_close(st)` flushes the pending block, closes the stratum, and returns `list(loglik, U, I)`.
- An evaluation helper for tests, `cox_eval(beta)`, runs the step over a data.frame in chunks. Its reference
  is `survival::coxph(..., init = beta, control = coxph.control(iter.max = 0))`, whose `loglik[1]` and
  `vcov` give the loglik and I^-1 at beta.

- [ ] **Step 1: Write the failing tests:**
  - `test_that("chol2 matches solve on a full-rank matrix and flags a singular column")`:
    - for a random SPD 4x4, `.h_st_chsolve2(.h_st_chol2(M, 1e-9^0.75), b)` equals `solve(M, b)` at 1e-12;
    - with column 3 = column 1 + column 2, `sing` is c(FALSE, FALSE, TRUE, FALSE), and chinv2 has zero row
      and column 3.
  - `test_that("cox step loglik, score and information equal coxph at beta")`: at a non-zero beta, compare
    `loglik` with `coxph(..., init = beta, iter.max = 0)$loglik[1]` at 1e-10, and solve(I) with its `vcov` at
    1e-10. Cover:
    - ties efron and breslow;
    - integer times with heavy ties, and tie blocks straddling chunk 7;
    - two strata, one ending exactly at a chunk boundary;
    - an all-censored stratum;
    - a single event;
    - a covariate around 2000 (centred).
    Chunk 7 and chunk 1e5 agree at 1e-12.
  - `test_that("the deferred S2 identity holds")`: the I from the step equals a direct O(n^2) reference that
    sums d S2(t_k)/S0 over event times, at 1e-12 on 200 rows.
- [ ] **Step 2: Run** `test-scale-surv-coxstep.R`. Expected: FAIL.
- [ ] **Step 3: Implement.**
  - Use the S8/S12 algebra, vectorised over the chunk's complete blocks.
  - Within a block, rows sorted DESC are in the risk set together. S0 and S1 at the block are the totals
    *including* the block. Compute them with `cumsum` over rows, taking the value at each block's last row.
  - Efron weights per block of d events: sum_l 1/D_l, sum_l (l/d)/D_l, sum_l 1/D_l^2, sum_l (l/d)/D_l^2,
    sum_l (l/d)^2/D_l^2, and sum_l log D_l. Loop over l only for blocks with d > 1; d is small.
  - The E2 term is `crossprod(X_ev, (r_ev * w_E2[block]) * X_ev)` with w_E2 = sum_l (l/d)/D_l.
  - The mean-outer-product terms: `crossprod(S1m, q1 * S1m)`, the S1/E1 cross term with q2, and E1 with q3.
- [ ] **Step 4: Run** the file. Expected: PASS.
- [ ] **Step 5: Commit** `feat(scale): streamed Cox block math with chunk carries`.

### Task 4: Newton driver and warnings

**Files:**
- Modify: `app/R/scale/stream_cox.R`.
- Create: `tests/testthat/test-scale-surv-coxfit.R`.

**Interfaces:**
- Consumes: Task 3.
- Produces:
  - `.h_st_coxfit(passfn, p, nms, scale, maxit = 20L, eps = 1e-9)` returns `list(coef, V, U, loglik, iter,
    flag, sing, passes, max_eta)`. `passfn(beta)` runs one pass and returns `list(loglik, U, I, max_eta)` in
    centred, unscaled coordinates. `scale` is the S1 vector. The S5 loop runs with the scaled Cholesky.
  - `.h_st_cox_warn(fit, eps = 1e-9)` returns the character vector of S7 warnings.

- [ ] **Step 1: Write the failing tests** (the passes run in memory through Task 3's `cox_eval`):
  - `test_that("coxfit equals coxph coef and vcov")`: on the Task 3 fixtures, `coef` and `V` equal
    `coef(coxph)` and `vcov(coxph)` at 1e-8, and `iter` equals `fit$iter`.
  - `test_that("aliased column gives NA as coxph")`: with x3 = x1 + x2, `is.na(coef)` equals
    `is.na(coef(coxph))`.
  - `test_that("monotone likelihood warns with coxph's text")`: x perfectly separates the events, so the
    warnings are identical to coxph's (captured with `withCallingHandlers`), trailing space included.
  - `test_that("no convergence warns")`: with maxit = 2 on a slow fixture, the warning is "Ran out of
    iterations and did not converge", and the coef equals `coxph(..., control = coxph.control(iter.max = 2))`.
  - `test_that("non-finite final loglik stops")`: a stub `passfn` returning NaN after the first pass gives the
    S19 text.
- [ ] **Step 2: Run** the file. Expected: FAIL.
- [ ] **Step 3: Implement** S5-S7 exactly. `passes` counts calls of `passfn`.
- [ ] **Step 4: Run** the file. Expected: PASS.
- [ ] **Step 5: Commit** `feat(scale): streamed Cox Newton driver with coxph warnings`.

### Task 5: Stream compile for exact Cox (model SE) and the survival test harness

**Files:**
- Create: `app/R/scale/stream_cox_compile.R`, `tests/testthat/test-scale-surv-cox.R`.
- Modify:
  - `app/R/routes/cox.R`: the first line of `.sr_cox_compile`, after the checks:
    `if (identical(spec[["engine"]], "stream")) return(.sr_cox_compile_stream(spec))`, the predicate
    `routes/glm.R:156` uses.
  - `tests/testthat/helper-scale.R`.

**Interfaces:**
- Consumes: Tasks 2-4. From P1e.1: `.h_st_open`, `.h_st_excl`, `.h_st_pass`, `.h_st_progress`,
  `.sr_run_prologue_stream`, and the `.sr_glm_compile_stream` head pattern (`.sr_base`, `.prep`, `.pg_tick`,
  the `.A` env).
- Produces:
  - `.sr_cox_sql(spec, cols)` returns the S10 SELECT text with quoted identifiers. Task 6 reuses it without
    the ORDER BY.
  - `.sr_cox_compile_stream(spec)` returns one code string. It branches to `.sr_pem_compile_stream` when
    `.sr_cox_method_of(spec) == "pem"`, a stub that stops until Task 9.
  - Test helpers in `helper-scale.R`:
    - `.ssc_route` registers cox, cox_describe and rmst;
    - `.ssc_body` dispatches cox/rmst ids to `sr_compile`;
    - `.ssc_surv_frame(n, seed)` adds to `.sc_frame` a `time` (integer ties plus near ties), a `status`
      (about 30% censored), `st1`/`st2` strata, and `yr` (about 2000);
    - `.ssc_surv_spec(id, ...)` builds a spec;
    - `.ssc_surv_expect(st, mem, info)` compares by the tolerance table and the S18 names.

- [ ] **Step 1: Write the failing tests** in `test-scale-surv-cox.R`, each comparing `.ssc_stream` (chunks 7,
  50 and 1e5) with `.ssc_memory` through `.ssc_surv_expect`:
  - `test_that("stream cox equals memory: efron and breslow")`, for both `cox` and `cox_describe`;
  - `test_that("strata: one, two, all-censored stratum, stratum ending on a chunk boundary")`;
  - `test_that("terms: interaction with the exposure, rcs, factor exposure with an absent level")`, where
    `aliased` is identical and contains the absent level;
  - `test_that("rows: small and large exclude_rows")`, the large one over 1000 rows;
  - `test_that("near ties, single event, large-mean covariate")`;
  - `test_that("monotone likelihood warnings are identical")`;
  - `test_that("km and km_risk equal memory, factor and >8-valued numeric exposure")`, the latter
    `km_by == "overall"`;
  - `test_that("time order guard")`: `.h_st_time_guard(st, time, strat)` (an inlined helper in
    `stream_cox.R`, returning the updated carry) passes on c(5, 3, 3, 1) and stops with the S10 text on
    c(5, 3, 4), and on a chunk starting above the previous chunk's last time in the same stratum;
  - `test_that("errors match memory")`: no complete rows, no events, and a `factor ... has new level` case
    give identical messages;
  - `test_that("value names and progress")`: names are as S18, `passes` <= 23, and the progress file's final
    `max` equals `passes`;
  - `test_that("emitted stream code passes the emit check and is ASCII")`.
- [ ] **Step 2: Run** the file. Expected: FAIL.
- [ ] **Step 3: Implement** `.sr_cox_compile_stream`.
  - The emitted code inlines the Task 2-4 helpers. The R8 snippets per chunk are `tc$design`,
    `.h_design(chunk)`, `sr_outcome_code(spec, "surv")`, the strata/cluster copies and the complete-cases
    filter, the same lines as memory.
  - Then: the time guard, the timefix apply, centring by the pass-0 means, eta, and `.h_st_cox_step`.
  - KM group per S13. The value is per S18.
  - Exclude rows with `.h_st_excl` as in P1e.1.
- [ ] **Step 4: Run** `test-scale-surv-cox.R`, `test-scale-surv-pins.R`, `test-cox.R` and `test-lint.R`.
  Expected: PASS.
- [ ] **Step 5: Commit** `feat(scale): streamed exact Cox with model SE`.

### Task 6: Cluster SE for exact Cox

**Files:**
- Modify: `app/R/scale/stream_cox.R`, `app/R/scale/stream_cox_compile.R`.
- Create: `tests/testthat/test-scale-surv-coxcl.R`.

**Interfaces:**
- Consumes: Task 5, and P1e.1's `.h_st_cluster_step` / `.h_st_cluster_flush`.
- Produces:
  - `.h_st_et_step(st, ...)` returns the per-chunk rows of the event-time table, data.frame(stratum, time,
    dlam, c1, xbm_1..p, db_1..p, c2_1..p), from a pass at the final beta. It reuses Task 3's block
    computation. Factor the block weights into one internal function that both call.
  - `.h_st_cox_resid(X, r, event, et)` returns the S9 score-residual matrix. `et` holds the joined columns,
    with NA set to 0.

- [ ] **Step 1: Write the failing tests:**
  - `test_that("stream cluster SE equals coxph robust")`: efron and breslow, two strata, clusters spanning
    strata and chunk boundaries, chunks 7 and 1e5. SE, CI and p are at 1e-8, and `n_clusters` is identical.
  - `test_that("score residuals equal residuals.coxph score")`: `.h_st_cox_resid` on a 300-row fixture with
    near ties equals `residuals(fit, type = "score")` at 1e-10.
  - `test_that("one cluster stops with the memory text")`.
  - `test_that("no file is written")`: after a clustered run the job temp directory holds no new file, and
    the emit check passes.
- [ ] **Step 2: Run** the file. Expected: FAIL.
- [ ] **Step 3: Implement** S20 and S22.
  - Create `sr_et` with `CREATE TEMP TABLE sr_et AS SELECT * FROM sr_et_chunk` on the first chunk, and
    `INSERT INTO sr_et SELECT * FROM sr_et_chunk` after that.
  - The view `sr_et_cum` holds the cumulative `lam` and `b_j` plus the block columns.
  - The cluster pass reads S22 and recomputes eta and the R8 filter per chunk. resid %*% V goes to
    `.h_st_cluster_step`.
  - Progress: two extra passes.
- [ ] **Step 4: Run** the file and `test-scale-surv-cox.R`. Expected: PASS.
- [ ] **Step 5: Commit** `feat(scale): streamed Cox cluster-robust SE`.

### Task 7: Stream compile for RMST

**Files:**
- Modify: `app/R/scale/stream_cox_compile.R`, and `app/R/routes/rmst.R` (the stream branch at the top of
  `.sr_rmst_compile`, after the checks).
- Create: `tests/testthat/test-scale-surv-rmst.R`.

**Interfaces:**
- Consumes: Task 2 and the Task 5 harness.
- Produces: `.sr_rmst_compile_stream(spec)`. It runs one counting pass of KM counts keyed by (exposure value,
  raw time) plus the exact distinct exposures (S14), then emits the memory groups, tau and value code on the
  rebuilt counts.

- [ ] **Step 1: Write the failing tests** (stream vs memory, chunks 7 and 1e5):
  - `test_that("stream rmst equals memory")`: groups `rmst` and `se` at 1e-10; n, events, at_risk and
    max_time identical; KM at 1e-12; with near ties.
  - `test_that("rmst with exposure_ref and a text exposure")`.
  - `test_that("tau above a group's follow-up stops with the memory text")`.
  - `test_that("fewer than 10 at risk gives the memory note")`: compare `sr_present` fragments.
  - `test_that("more than 8 exposure values stops with the memory text")`.
  - `test_that("rmst value names")`: memory names plus engine, chunk and passes; `passes == 1`.
- [ ] **Step 2: Run** the file. Expected: FAIL.
- [ ] **Step 3: Implement.** `at_risk` and `max_time` use raw times; each group's RMST uses that group's own
  map (S13).
- [ ] **Step 4: Run** the file, `test-rmst.R` and `test-scale-surv-pins.R`. Expected: PASS.
- [ ] **Step 5: Commit** `feat(scale): streamed RMST`.

### Task 8: Memory PEM and its Methods text

**Files:**
- Modify: `app/R/routes/cox.R`:
  - `.sr_cox_compile`: after the boot/MI returns, `if (.sr_cox_method_of(spec) == "pem") return(.sr_cox_pem_compile(spec))`;
  - `.sr_cox_present`: the PEM fragments.
- Create: `tests/testthat/test-cox-pem.R`.

**Interfaces:**
- Produces:
  - `.sr_pem_cut_code(spec)` returns the emitted lines that compute `.cuts` from the analysed event times
    (S16). It is shared with Task 9.
  - `.sr_cox_pem_compile(spec)` emits these steps:
    - the same setup and R8 lines as exact Cox;
    - the time <= 0 stop: "the piecewise-exponential model needs every time above zero (found <n> rows with
      time <= 0)";
    - `survival::survSplit(Surv(.time, .event) ~ ., data = des, cut = .cuts[-c(1, length(.cuts))],
      episode = ".int")`;
    - `.pt = .time - tstart`;
    - `stats::glm(.event ~ 0 + <baseline> + <tc$rhs> + offset(log(.pt)), family = poisson)`, with baseline
      `factor(.int)` or `interaction(factor(.int), <strata>)`;
    - SE through `.sr_h_vcov` (model, or cluster with `cluster = .cl`), as modpois does;
    - the coef table over the covariate columns only;
    - the S18 value, with KM from the unsplit `des`.
  - Fragments:
    - `frag$pem_method` is `sprintf("Hazard ratios were estimated with a piecewise-exponential (Poisson) model
      with %d intervals of constant baseline hazard (cut points at %s); this approximates the Cox model.", k,
      <cuts joined by ", ">)`;
    - `frag$cox_note` names "piecewise-exponential Poisson regression" in place of "Cox proportional hazards
      regression" for PEM.

- [ ] **Step 1: Write the failing tests:**
  - `test_that("memory pem equals a hand-built survSplit + glm")`: on a strata fixture, coef and SE are at
    1e-10, against a hand-written reference in the test.
  - `test_that("quantile cuts and explicit cuts, explicit cut >= max time dropped")`: `cuts` is identical to
    the S16 formula, and `k` is `length(cuts) - 1`.
  - `test_that("pem cluster SE equals sandwich::vcovCL HC1")`.
  - `test_that("time <= 0 stops")`: exact text with n.
  - `test_that("Weibull sanity: PEM K=10 log HR within 0.02 of coxph")`: 20000 rows, Weibull shape 1.5, true
    log HR 0.5.
  - `test_that("pem fragment names K and the cuts")`.
- [ ] **Step 2: Run** the file. Expected: FAIL.
- [ ] **Step 3: Implement.** The exact-Cox emitted lines stay untouched (the pins guard them).
- [ ] **Step 4: Run** the file, `test-cox.R` and `test-scale-surv-pins.R`. Expected: PASS.
- [ ] **Step 5: Commit** `feat(cox): piecewise-exponential method on the memory engine`.

### Task 9: Stream PEM

**Files:**
- Modify: `app/R/scale/stream_cox_compile.R` (replace the Task 5 stub), `app/R/scale/stream_fit.R`
  (`.h_st_irls` at 167+, the offset).
- Create: `tests/testthat/test-scale-surv-pem.R`.

**Interfaces:**
- Consumes: Task 8 (`.sr_pem_cut_code`), and P1e.1's `.h_st_irls(passfn, family, p, nms, maxit, epsilon,
  select)`, `.h_st_glm_scores`, `.h_st_cluster_step` / `.h_st_cluster_flush` / `.h_st_vcov_finish`.
- Produces: `.sr_pem_compile_stream(spec)`.
  - Pass 0 collects the analysed event times (raw), n, events, the levels, the per-stratum max time, the KM
    counts and the time <= 0 count.
  - The fit passes read the S10 row query cross-joined with
    `(VALUES (1, 0.0, c1), (2, c1, c2), ...) AS sr_int(j, lo, hi)` and filtered by `time > lo`. The VALUES
    literals are formatted with `.sr_lit` digits (`format(x, digits = 17)`); Inf is `'Infinity'::DOUBLE`.
  - The per chunk work in R: the R8 snippets, pt and ev (S16), the baseline indicator columns for the present
    combos, then the covariates and the offset `log(pt)` into the IRLS `wls` callback.
  - `.h_st_irls` has no offset today. Add an optional third argument to its `wls` callback,
    `wls(X, y, offset = NULL)`, following `glm.fit`: the first pass starts from `linkfun(mustart)` without the
    offset; after that eta = X b + offset, and the working response is z = eta - offset + (y - mu) / mu.eta.
    Existing glm calls pass no offset, and their results must not change by a bit (`test-scale-compile.R`).

- [ ] **Step 1: Write the failing tests** (stream vs memory PEM, chunks 7, 50 and 1e5):
  - `test_that("stream pem equals memory pem: quantile and explicit")`: coef, SE and CI at 1e-8; cuts and k
    identical.
  - `test_that("stream pem with strata and cluster")`.
  - `test_that("stream pem time <= 0 stops with the memory text")`.
  - `test_that("streamed IRLS with an offset equals glm with offset")`: a Poisson fixture with
    `offset(log(pt))`, coef at 1e-8 against `stats::glm`.
- [ ] **Step 2: Run** the file. Expected: FAIL.
- [ ] **Step 3: Implement.**
- [ ] **Step 4: Run** the file, `test-scale-compile.R` and `test-scale-fit.R`. Expected: PASS.
- [ ] **Step 5: Commit** `feat(scale): streamed piecewise-exponential Cox`.

### Task 10: Run, archive, writer and deviations end to end

**Files:**
- Create: `tests/testthat/test-scale-surv-run.R`.
- Modify: only what the tests show is missing in run, archive or writer.

**Interfaces:**
- Consumes: everything above; P1e.1's `test-scale-run.R` and `test-scale-archive.R` patterns.

- [ ] **Step 1: Write the failing tests:**
  - `test_that("streamed cox runs end to end")`: lock with `engine = "stream"`, `sr_run`, the writer, then the
    archive replay with the Parquet extract. The Methods text has the S17 cox sentence, lifted to
    placeholders, and the claim guard passes.
  - `test_that("streamed cox with pem runs end to end")`: the Methods text has the `pem_method` fragment.
  - `test_that("streamed rmst runs end to end")`.
  - `test_that("auto resolves survival routes by size")`: `SR_RAM_BUDGET_GB = "0.0001"` gives stream.
  - `test_that("engine switch on cox is computational, cox_method amendment is methodological")`.
  - `test_that("old cox project reopens unchanged")`: a project locked with a pre-P1e.2 spec (no
    `cox_method`) has the same SAP hash and run key as on main, so no rerun is flagged. Build it as P1e.1's
    `pre-p1e-lock` fixture was.
  - `test_that("diagnostics refuse streamed cox with the P1e.3 text")`.
- [ ] **Step 2: Run** the file. Expected: FAIL or PASS per test; fix what fails.
- [ ] **Step 3: Implement** the missing wiring.
- [ ] **Step 4: Run** the full suite `Rscript -e "testthat::test_dir('tests/testthat')"` in the background.
  Expected: all PASS, `LINT OK`.
- [ ] **Step 5: Commit** `test(scale): streamed survival run, archive and deviations end to end`.

### Task 11: Docs and memory

**Files:**
- Modify: `docs/scale.md` (section 6), `docs/validation.md`, `docs/roadmap.md` (the P1e.2 row), the analysis
  guide (Cox method choice; find it with `grep -rln "Cox" docs/guide*`), `memory.md`.

- [ ] **Step 1: Write**:
  - **scale.md §6:** the streamed Cox, cluster, KM/RMST and PEM, with the pass counts.
  - **validation.md:** the tolerance table and the Weibull sanity result, stated as "not an equivalence
    claim".
  - **roadmap.md:** P1e.2 done.
  - **The guide:** when to choose PEM (fixed cuts, an approximation, faster on stream), and that a change
    after the lock is a methodological deviation.
  - **memory.md:** rulings S15, S20 and S21, and the gotchas found.
- [ ] **Step 2: Check** every claim against the code. Grep the docs for "arrives in P1e.2" and remove stale
  text.
- [ ] **Step 3: Commit** `docs: P1e.2 streaming survival`.
