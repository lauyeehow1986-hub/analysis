# P1e.2 Streaming Cox, piecewise-exponential and KM/RMST - design

## 1. Purpose and scope

P1e.1 made the regression routes and Table 1 run exactly on tables of any row count. The survival
routes still refuse the streaming engine with "the streaming engine for Cox / RMST arrives in P1e.2"
(`scale/engine.R`, `.sr_engine_refusal`). P1e.2 removes that refusal and adds the streaming engine to
the survival routes.

**In scope:**
- **`cox@1` and its describe twin `cox_describe@1`, exact, on the streaming engine.**
  - Ties: Efron and Breslow.
  - Model terms: strata (one or more columns), and the full term set (linear, factor, RCS, quad,
    interactions).
  - `exclude_rows`: small and large.
  - SE: model-based, or cluster-robust (`layers$clustered`).
  - The Kaplan-Meier figure data (`km`, `km_risk`).
- **A new pre-specified estimator `cox_method = "pem"`**, the piecewise-exponential (Poisson) model,
  on both engines.
- **`rmst@1` on the streaming engine**, including the tau checks and the few-at-risk note.
- **Plumbing:** the spec fields, UI fields, SAP and Methods text, writer fragments and docs that the
  above needs.

**Out of scope (refusals unchanged):**
- **Kept for P1e.3:**
  - diagnostics for a streaming analysis (ruling R4 of P1e.1, "diagnostics for a streaming analysis
    arrive in P1e.3");
  - MI, bootstrap and Firth.
- **Still refused:** clustered RMST, as in memory (R23b).
- **Not done:** thinning the KM table.
- **Deferred to the end of P1e:** the laptop 20M-row smoke.

**Exact** means the same thing as in P1e.1. On the same rows, the presented result of the streaming
engine equals the in-memory route's result within the tolerances of section 9. Peak memory is one
chunk of rows plus p x p matrices, plus the KM counts, which have one row per distinct time (as
survfit's output has).

### Decisions made with the user (2026-11-12)

- **Cox method: both.**
  - The default is exact streamed Cox, matching `survival::coxph` to about 1e-8.
  - `cox_method = "pem"` is an opt-in alternative.
- **PEM is a pre-specified method, not an engine.** It is a spec field in the SAP hash, frozen at the
  lock, and it runs on both engines, so memory-PEM and stream-PEM are equivalent. The consequences:
  - Changing `cox_method` or `pem_cuts` after the lock is a methodological deviation.
  - `sr_engine_switch` stays computational for PEM too.
- **Feature scope: full fit parity** with the in-memory Cox and RMST routes, listed above. Anything
  not covered is refused at the lock with a reason.
- **PEM cut points: a rule fixed at the lock.** The default is `list(rule = "quantile", k = 10)`:
  K intervals at the event-time quantiles, with the cut values computed by the emitted code and
  reported. The alternative is `list(rule = "explicit", at = c(...))`.
- **Approach: an R chunk loop over a time-sorted read.**
  - Rejected: DuckDB window sums. Efron is awkward in SQL, and the parallel sums are not
    reproducible.
  - Rejected: covariate-pattern collapse. It is exact only for all-discrete terms.

## 2. Rules carried over

All P1e.1 rules hold:

- **Code-first.** The inlined `.h_st_*` helpers run in the code `compile()` emits, so `analysis.R`
  runs with only `duckdb`, `survival` and the extract.
- **No test-gated switching.**
- **Air-gap.** The locked-down `.h_st_open`.
- **Events.** Event-sourced state.
- **Text and naming.** ASCII-only generated code, and the `sr_` / `.sr_` / `.h_` prefixes.
- **Byte-identical memory code.**
  - The in-memory emitted code of `cox@1`, `cox_describe@1` and `rmst@1` for every existing spec
    stays byte-identical. Its sha256 pins are taken on `main` before any change and kept in a new
    `tests/testthat/test-scale-surv-pins.R`.
  - PEM is a new memory branch, taken only when `cox_method = "pem"`.
- **The R8 rule.** Each chunk of the stream goes through the in-memory snippets (`.h_design`,
  `sr_outcome_code(spec, "surv")`, the strata/cluster copies and the complete-case filter). A row is
  therefore analysed on the stream exactly when it is analysed in memory.

## 3. Spec fields and the SAP

### 3.1 Fields

| field | routes | values | default |
|---|---|---|---|
| `engine` | cox, cox_describe, rmst | auto / memory / stream (P1e.1) | auto |
| `stream_chunk` | same | whole number >= 1 (set by the run) | profile |
| `cox_method` | cox, cox_describe | `"exact"` / `"pem"` | `"exact"` |
| `pem_cuts` | cox, cox_describe, when `cox_method = "pem"` | `list(rule = "quantile", k = K)` with K a whole number in 2..50, or `list(rule = "explicit", at = <increasing finite numbers > 0>)` | `list(rule = "quantile", k = 10)` |

Validation, in the route's cross-field validator (each case stops with its reason):

- `pem_cuts` without `cox_method = "pem"` is refused.
- An `explicit` rule with unsorted, duplicated, non-finite or non-positive cuts is refused.
- `cox_method = "pem"` with MI or bootstrap is refused on both engines in P1e.2 ("PEM with
  multiple imputation / bootstrap arrives in P1e.3"), which keeps the new branch small.

**Backward compatibility.** A spec without `cox_method` means `"exact"`, so projects locked before
P1e.2 resolve and run unchanged.

### 3.2 SAP, estimand and deviations

- Both fields enter the SAP hash. The SAP text names the method:
  - exact: "Cox proportional hazards model (partial likelihood, Efron/Breslow ties)";
  - pem: "piecewise-exponential Poisson model with K intervals (rule ...)".
- The estimand stays a conditional HR. For PEM, the `summary` says "hazard ratio from a
  piecewise-constant baseline hazard (piecewise-exponential model)".
- **Deviations.**
  - A change of `cox_method` or `pem_cuts` after the lock goes through the normal SAP amendment, as
    a methodological deviation.
  - `sr_engine_switch` keeps its P1e.1 meaning, a computational deviation, for both methods.
- **`.sr_engine_refusal`.**
  - The Cox/RMST line is removed.
  - The MI, bootstrap and Firth lines keep their P1e.3 texts.
  - `.sr_mem_method()` (`sap/sap.R`) already maps cox and rmst to `"cox"`. Its estimate is reused
    unchanged, so `"auto"` resolves Cox and RMST from the size estimate like the other routes.

### 3.3 UI

- **Cox analysis form:**
  - a "Method" select: "Cox (exact partial likelihood)" / "Piecewise-exponential (Poisson)";
  - when PEM is chosen, a "Cut points" control: "K intervals at event-time quantiles", with a
    numeric K, or "explicit", with a text field of comma-separated times.
- **Engine select:** the P1e.1 select, now shown for Cox and RMST as well.

## 4. Exact streamed Cox

### 4.1 Reading order

Every pass reads the same query, in chunks of `stream_chunk` rows (`.h_st_pass`):

```
SELECT <needed cols>, ".sr_row" FROM <sr_src> WHERE (<.sr_where>)
ORDER BY <strata cols, in spec order>, <time col> DESC, ".sr_row"
```

- **Sort order.** The `ORDER BY` is always present: correctness never depends on the file order of
  the extract, including under the hash anti-join of a large `exclude_rows`. The rows of one stratum
  are contiguous, and the rows with one time (a tie block) are contiguous within it.
- **Time guard.** After the R8 snippets, the chunk checks that `.time` is non-increasing within each
  stratum, carried across chunks. Otherwise it stops: "the streamed Cox needs .time to keep the
  order of the time column".

### 4.2 Pass 0 (setup)

One pass over the analysed rows collects:

- n, events and n_total;
- the factor levels present, which give the design columns (P1e.1's `.h_st_mm_cols`) and the
  `factor ... has new level` behaviour;
- the column sums of X, giving the means coxph centres X by;
- the KM counts (section 6).

It also builds the **near-tie map**:

- **The rule.** coxph's `Surv` uses `timefix = TRUE`, which calls `survival::aeqSurv` and merges
  distinct times that differ by less than its tolerance.
- **Candidates.** One SQL query with `LAG` over the distinct values of the time column (all rows)
  finds the candidate times within tolerance of a neighbour. The result is usually empty.
- **Applying it.** Pass 0 keeps the candidates present among the analysed rows and applies
  `aeqSurv`'s exact rule to them. Every later chunk then replaces each mapped time before use. The
  KM and RMST passes apply the same map, as survfit does.
- **Why candidates are enough.** `aeqSurv` compares adjacent sorted unique values. Any analysed pair
  it would merge consists of two candidates, so the map is exact.

### 4.3 Newton passes

The iteration replicates `survival:::coxph.fit` / `coxfit6`:

- **Start and limits:** start at beta = 0; `iter.max = 20`; `eps = 1e-9`.
- **Convergence:** when `abs(1 - loglik_new / loglik_old) <= eps`.
- **Step-halving:** when the log-likelihood falls or is not finite.
- **Aliasing:** solved by a port of `cholesky2` / `chsolve2` with `toler.chol =
  .Machine$double.eps^0.75`. Columns it declares singular get coefficient NA, as coxph reports them
  (`aliased`).
- **Passes:** each iteration is one pass at the current beta, returning the log-likelihood, the
  score U and the information I. There are at most 21 passes.

**Breslow, per stratum.** The pass walks the rows in descending time and carries:

- S0 = sum of r;
- S1 = sum of r x;

where r = exp(eta) on the centred X. Per tie block with d events:

- loglik += sum over the events of (eta - log S0);
- U += sum over the events of (x - S1/S0);
- the information takes d (S2/S0 - xbar xbar').

The S2 part is never stored per event time:

- **The identity.** sum_k w_k S2(t_k), with w_k = d_k / S0(t_k), equals sum_i r_i W(t_i) x_i x_i',
  where W(t_i) = W_stratum - W_after(t_i) is the sum of w over the event times <= t_i.
- **Per chunk.** Two `crossprod` calls: `crossprod(X, r * X)` into the stratum's A, and
  `crossprod(X, (r * W_after) * X)` into its B.
- **Closing a stratum.** When the stratum ends, I += W_stratum * A - B, and A and B reset.
- **Cost.** Memory stays O(chunk x p + p^2). The cancellation in W_stratum - W_after is benign
  (absolute error about 1e-16 W_stratum), and a test pins it (section 9).
- **The xbar term.** sum_k d_k xbar_k xbar_k' is a per-block outer product, done as
  `crossprod(M, d * M)` over the chunk's completed blocks, with M = S1/S0.

**Efron.** For a block with d events, E0 = sum of r over the events, E1 = sum of r x, and E2 = sum of
r x x'. For l = 0..d-1, D_l = S0 - (l/d) E0.

- loglik: sum over the events of eta, minus sum_l log D_l.
- U: x_events - sum_l (S1 - (l/d) E1) / D_l.
- I: the S2 part takes the per-block weight sum_l 1/D_l, by the same deferred identity.
- The E2 part takes the weight -sum_l (l/d)/D_l. It comes from
  `crossprod(X_ev, (r * c_block) * X_ev)` over the event rows.
- The mean-outer-product part expands into the four sums of S1 S1', S1 E1', E1 S1' and E1 E1',
  weighted by sum_l 1/D_l^2, sum_l (l/d)/D_l^2 and sum_l (l/d)^2/D_l^2, all vectorised over the
  chunk's blocks.

**Chunk boundaries.** A tie block that straddles a chunk boundary is held back, as its pending rows,
until the block completes. A stratum that straddles one carries S0, S1, W, A and B.

**Warnings, with coxph's texts.**
- No convergence: "Ran out of iterations and did not converge".
- Monotone likelihood: the `coxph.fit` check (`infs <- abs(u %*% var)` against `eps` and
  `sqrt(eps) * abs(coef)`) gives "Loglik converged before variable <k>; coefficient may be
  infinite."
- The run collects these into `warnings` as the in-memory `withCallingHandlers` does.

### 4.4 Standard errors

- **Model:** V = I^-1 at the final beta, from the last Newton pass. `.h_coef_table(coef, V, df =
  Inf)`, as in memory.
- **Cluster** (`layers$clustered`): two extra passes.
  1. **Event-time pass.** At the final beta, a pass writes a per-event-time table. For each stratum
     and event time it holds:
     - the cumulative hazard increment sum `Lam`;
     - the p cumulative sums `B` of `xbar * dLambda`;
     - the Efron tie-block terms needed for the dying rows' partial increments.

     The table is written to `<job>.tmp-et.parquet` in the job's temp directory, which is covered by
     P1e.1's 24 h `.tmp-*.parquet` sweep. It is the only data file the emitted code writes. The emit
     check gets a scoped allow-list entry for the one write call inside `.h_st_et_write`, in the
     same way `.h_st_progress` was scoped in PR 14.
  2. **Cluster pass.** The query is:

     ```
     SELECT ..., et.* FROM <src> ASOF LEFT JOIN <et>
       ON stratum match AND time >= et.time
     WHERE (<.sr_where>)
     ORDER BY <cluster col>, ".sr_row"
     ```

     Each chunk recomputes eta and the R8 filter. It then computes the score residuals as
     `survival`'s `coxscore2` does, for Breslow and for Efron. P1e.1's `.h_st_cluster_step` /
     `.h_st_cluster_flush` sum them per cluster into D = rowsum(resid) %*% V, and the variance is
     D'D, which is coxph's robust variance with `cluster()`.

  `n_clusters` and the "at least two clusters" stop are as in memory.

### 4.5 Value and progress

- **Value.** The value list has exactly the in-memory names: coef, aliased, n, n_total, events,
  level, se_type, n_clusters, ties, strata, levels, warnings, km_by, km and km_risk. To these it adds
  `engine = "stream"`, `chunk`, `passes` and `iter`.
- **Progress.** Progress `{"pass": k, "max": m}` (R15) uses m = 1 + 21, plus 2 when clustered. m is
  lowered to the passes actually run when the fit converges.

## 5. PEM (`cox_method = "pem"`), both engines

- **Cuts.**
  - `quantile`: the cuts are `unique(stats::quantile(<analysed event times>, probs = (1:(K-1))/K,
    type = 7, names = FALSE))`, with 0 below and Inf above.
  - `explicit`: the cuts are `c(0, at, Inf)`, with values >= max time dropped. The Methods report
    the cuts used.
- **Intervals.** They follow `survival::survSplit`'s convention, (c[j-1], c[j]]. A row enters
  interval j when time > c[j-1]. Its person-time is min(time, c[j]) - c[j-1], and its event is
  status when time <= c[j], else 0.
- **Time stop.** Any analysed time <= 0 stops both engines with "the piecewise-exponential model
  needs every time above zero (found <n> rows with time <= 0)".
- **Model.** Poisson on the split rows with `offset(log(pt))`. The baseline is a factor `.int`, or
  `interaction(.int, strata)` when strata are given, without an intercept. The fitted terms are
  those of the exact Cox, and the baseline coefficients are not presented.
- **SE.** Model-based, or cluster-robust with `layers$clustered`. Each uses the same in-memory and
  stream functions as `modpois@1`. A subject is a cluster only when the spec says so, so the split
  rows of one subject are not a cluster by default: the Poisson likelihood of the split rows is the
  PEM likelihood.
- **Memory engine.** It runs `survival::survSplit` and then `stats::glm(family = poisson)`.
- **Stream engine.**
  - Pass 0 collects the analysed event times, one double each, for the quantile rule, plus n,
    events and the factor levels.
  - The split happens in SQL: the row query is cross-joined with a `VALUES` table of
    (j, lo, hi) and filtered by `time > lo`, giving pt and ev. The expanded rows are streamed, never
    stored.
  - The fit is P1e.1's streamed Poisson IRLS (`.h_st_irls`) with the offset. Cluster SE uses P1e.1's
    cluster pass on the expanded rows, sorted by cluster and then `.sr_row`.
- **Value.** It has the exact-Cox names plus `method = "pem"`, `cuts` and `k`. The KM data are the
  same as for exact Cox.

## 6. KM data and RMST on stream

- **KM counts.** Pass 0 accumulates (group, time, n.event, n.censor) over the analysed rows, after
  the near-tie map:
  - one `rowsum` per chunk;
  - merged into a running list, collapsed by one `rowsum` every 16 chunks and at the end.

  The group is the KM group of the in-memory `.h_km` call: the exposure factor, or a numeric
  exposure with <= 8 distinct values, or else overall.
- **KM rebuild.** `.h_st_km_finish` reproduces `survival::survfit` with `conf.type = "log"` and
  `conf.int = 0.95`:
  - time: the sorted distinct times;
  - n.risk: the reverse cumulative sum of (n.event + n.censor);
  - surv: the cumulative product of (1 - d/n);
  - std.err: Greenwood, sqrt(cumsum(d / (n (n - d)))), with survfit's handling of d = n;
  - lower and upper: exp(log(surv) -/+ z std.err), capped at 1.

  The `pretty()` at-risk table uses the analysed time range, as `.h_km` does.
- **RMST (`rmst@1`).**
  - One counting pass per run collects the KM counts by exposure level over the complete cases of
    time, status and exposure, together with each level's max time and the n at risk at tau.
  - `.h_st_rmst` ports `survival`'s `survmean` for `rmean` and `se(rmean)` at the common tau.
  - The tau stop, its text, the few-at-risk note, the RMST differences, their SE and z, and the
    value names are the in-memory ones, plus `engine`, `chunk` and `passes`.

## 7. Errors and refusals

- **Same texts as in memory:**
  - no complete rows;
  - no events;
  - fewer than 2 clusters;
  - the tau stop;
  - the `factor ... has new level` stop.
- **Stream-only stops:**
  - the time-order guard (4.1);
  - a non-finite chunk value after step-halving is exhausted: "the streamed Cox fit gave a
    non-finite log-likelihood at every step-halving".
- **Refused under stream (unchanged):** diagnostics, MI, bootstrap and Firth, with their P1e.3
  reasons.
- **Refused on both engines:** PEM with MI or bootstrap (3.1).

## 8. Components (files)

- **`app/R/scale/stream_cox.R`** (new): the inlined `.h_st_cox_*` helpers.
  - The setup accumulators and the near-tie map.
  - The Breslow and Efron block math.
  - The chunk step with carries.
  - The `cholesky2` / `chsolve2` port.
  - The Newton driver `.h_st_coxfit`.
  - The warnings.
  - The event-time writer `.h_st_et_write`.
  - The score-residual step.
- **`app/R/scale/stream_surv.R`** (new): the shared counting.
  - The KM counts (`.h_st_km_add` and `.h_st_km_finish`).
  - The RMST port (`.h_st_rmst`).
  - The near-tie SQL helper.
- **`app/R/scale/stream_cox_compile.R`** (new): `.sr_cox_compile_stream(spec)`,
  `.sr_rmst_compile_stream(spec)` and `.sr_pem_compile_stream(spec)`, which emit the code.
- **`app/R/routes/cox.R`:**
  - `cox_method` and `pem_cuts` validation;
  - the stream branch at the top of `.sr_cox_compile`;
  - the new memory PEM branch `.sr_cox_pem_compile(spec)`;
  - the estimand summary;
  - the Methods text for PEM.

  The existing memory code text is untouched.
- **`app/R/routes/rmst.R`:** the stream branch only.
- **`app/R/scale/engine.R`:** the refusal line removed, and the header comment updated.
- **`app/R/core/emit.R`:** the scoped allow-list entry for `.h_st_et_write`.
- **`app/R/writer/`:** the `pem_method` fragment (K, the cut values and the approximation sentence).
  The P1e.1 stream fragment is reused for Cox and RMST.
- **UI:** the Cox form fields of 3.3.
- **Docs:** `docs/scale.md` section 6, `docs/validation.md`, `docs/roadmap.md` (P1e.2 row), the
  analysis guide (Cox method choice), and `memory.md`.

## 9. Testing and tolerances

**Equivalence**: the stream result vs memory on the same spec and data. Tolerances are relative to
`max(1, |x|)`.

| quantity | tolerance |
|---|---|
| Cox coef, SE, CI, p (model and cluster SE), exact vs `coxph` | 1e-8 |
| KM `surv`, `lower`, `upper`; RMST, its SE and differences | 1e-12 (KM), 1e-10 (RMST) |
| PEM stream vs PEM memory (coef, SE, CI) | 1e-8 |
| n, n_total, events, n_clusters, aliased, levels, warnings, km/km_risk counts, cuts | identical |
| the same stream spec at chunk sizes 7, 50 and 1e5 | 1e-12 to each other |

**Fixtures** (synthetic, from `samples/`, never real data):

- **Ties:** heavy integer-time ties, Efron and Breslow.
- **Chunk boundaries:** a tie block straddling a chunk boundary (chunk 7), and a stratum ending
  exactly on one.
- **Strata:** one and two strata columns, and an all-censored stratum.
- **Terms:** an exposure x covariate interaction, RCS terms, and a factor exposure with a level
  absent from the fitted rows.
- **Rows:** a small and a large `exclude_rows` (the hash anti-join).
- **Edge cases:**
  - an aliased column;
  - a monotone likelihood (the infinite-coefficient warning);
  - near-tie times within `aeqSurv`'s tolerance;
  - a single event;
  - centring with a large-mean covariate (year about 2000);
  - clusters spanning strata, and one cluster (the stop).
- **RMST:** tau above one group's follow-up (the stop text), and fewer than 10 at risk (the note).
- **PEM:** both cut rules, an explicit cut at or above max time, strata, cluster, and a time <= 0
  (the stop).

**Code checks:**

- The memory sha256 pins for the existing cox, cox_describe and rmst specs are unchanged.
- The stream code passes `.sr_emit_check`, including the new scoped write.
- Generated code is ASCII only.
- The existing p-value lint passes.

**Engine checks:**

- The Cox/RMST refusal is gone.
- An engine switch is a computational deviation.
- A `cox_method` amendment is methodological.
- `"auto"` resolves survival routes by size.

**Validation-doc sanity check (not an equivalence claim):** on a simulated Weibull dataset, the PEM
(K = 10) log HR lies within 0.02 of the exact Cox log HR.

**Run and archive:** a streamed Cox and RMST each run end to end through `sr_run`, the writer and
the archive replay with the Parquet extract, as P1e.1's `test-scale-run.R` / `test-scale-archive.R`
do.
