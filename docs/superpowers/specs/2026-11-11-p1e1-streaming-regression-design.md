# P1e.1 Streaming regression and SQL Table 1 - design

## 1. Purpose and scope

P1 analyses currently refuse data that does not fit in memory. The lock checklist blocks every
oversized analysis with "big-data engines arrive in P1e" (`sap/sap.R`), and `sr_run` stops with "does
not fit in memory" (`routes/run.R`). P1e.1 makes the regression routes and Table 1 run **exactly** on
analysis tables of any row count.

- **Exact** means this. On the same rows, the presented result (coefficients, SEs, CIs, marginal
  effects, Table 1 cells) equals the in-memory route's result within numerical tolerance (section 10).
- **Peak memory** is one chunk of rows plus p x p matrices.

The target users have three kinds of table:

- per-patient cohorts over about 5M rows, which do not fit on `locked16`;
- per-encounter or long tables of tens of millions of rows, clustered by patient;
- smaller tables that already fit, which are unchanged.

**In scope:**
- `lm@1`, `logistic@1`, `logistic_describe@1` and `modpois@1`. Terms are linear, factor, RCS, quad
  and interactions, with `exclude_rows`. SEs are model, robust and cluster. Marginal RD/RR/NNT and
  the observed-risk table are included.
- `table1@1`.
- The engine choice at the lock, the run dispatch, the computational deviation, the Methods and SAP
  text, the UI fields, and the archive support for the new extract kind.

**Out of scope:** these are refused under the streaming engine, each with the named reason.
- Cox and RMST go to **P1e.2**: streamed exact Cox, piecewise-exponential, and KM/RMST in SQL.
- MI, bootstrap, the Firth penalty, covariate-pattern aggregation as a fast path, sampling for ML
  and mixed models, and bag of little bootstraps go to **P1e.3**.
- The 100 GB laptop run, the locked16 run and the P1 exit sign-off go to **P1e.4**.

### Decisions made with the user (2026-11-11)

- **Phase order.** P1e comes before P2, as `memory.md` "Next" says. The spec's P1 validation list
  counts the scale fits as P1 exit criteria.
- **Real data** covers every case above, so build to the main design spec (section J, "Analysis at
  scale").
- **Engine.** Use inlined streaming IRLS whose WLS steps combine chunks by TSQR. `biglm::bigglm`
  was rejected: it has no cluster SE and is fragile with factor levels. IRLS in DuckDB SQL was
  rejected: it uses normal equations, is not bit-reproducible, and would need the term bases
  rewritten in SQL.
- **Pattern aggregation is a P1e.3 fast path, not the engine.** It is exact only for
  categorical-only models without clustering. The routes support continuous terms, splines and
  cluster SEs, and modified Poisson needs a robust or cluster SE.
- **Engine is a locked spec field, not a new route.** The estimand and estimator are unchanged, so
  route ids, estimands and presenters are reused. `compile()` branches on the locked field the way it
  already branches on `se`.
- **Extract is Parquet.** An `.rds` extract would load the table into R, and querying the store
  directly would break lean archives and reviewer replay.

## 2. Rules carried over

- **Code-first.** `compile(spec)` returns the R code that runs and is exported. Helpers are inlined
  (`.sr_inline`), so `analysis.R` runs without StatRouter; it needs only `duckdb` and the extract.
- **No test-gated switching.** The engine is chosen by a deterministic rule on the locked table
  (section 4) and frozen at the lock. `compile()` never reads data or a p-value, and the lint rules
  apply unchanged.
- **Air-gap.**
  - The worker's DuckDB connection is in-memory and reads only the extract file.
  - It applies the same lockdown as `io/duck.R`: no extension autoload or autoinstall, an empty
    `extension_directory`, and no URL literal in any SQL.
  - Never `read_json`; never the sqlite extension.
- **Events are authoritative.** Every new fact is an event with a reducer: the resolved engine in
  the lock payload, and `engine.switched`.
- **Options** are read with `sr_opt` (`getOption("sr.x", Sys.getenv("SR_X", default))`).
- Generated `.R` and `.ps1` files are ASCII only. Exported functions use `sr_`, internal ones
  `.sr_`, and inlined helpers `.h_`.
- **Backward compatibility.** A spec without `engine` means `"auto"`. Projects locked before P1e keep
  their SAP hash unchanged (section 4.3).

## 3. Files

New `app/R/scale/`, one concern per file:

| File | Holds |
|---|---|
| `scale/engine.R` | `SR_ENGINES`, `sr_engine_resolve`, `sr_engine_switch` and its reducer, lock-checklist rows |
| `scale/stream_extract.R` | `sr_extract_stream`, the size estimate and free-space check |
| `scale/stream_io.R` | inlined `.h_st_open` (locked-down connection), `.h_st_pass` (chunk loop), `.h_st_fixed` (levels and knots from SQL) |
| `scale/stream_fit.R` | inlined `.h_st_lm`, `.h_st_glm` (TSQR-IRLS), `.h_st_alias` |
| `scale/stream_vcov.R` | inlined `.h_st_vcov` (model, robust and cluster meat passes) |
| `scale/stream_marginal.R` | inlined `.h_st_marginal`, `.h_st_obsrisk` |
| `scale/table1_sql.R` | inlined `.h_t1_sql_main` |

Modified files:

| File | Change |
|---|---|
| `routes/lm.R`, `routes/glm.R`, `routes/table1.R` | schema field `engine`; the compile branch emits the stream body; refusals |
| `routes/cox.R`, `routes/rmst.R` | schema field `engine` accepting `auto` and `memory`; `stream` refused (P1e.2) |
| `routes/run.R` | dispatch on the resolved engine; `.sr_run_prologue_stream`; worker inputs |
| `profile/profile.R` | `sr_mem_estimate` method `"table1"` (data bytes only) |
| `sap/sap.R` | the engine rows replace the "fits in memory" rows; the lock records the engines; hash part |
| `sap/deviations.R` | `sr_deviations` lists `engine.switched` |
| `archive/freeze.R`, `archive/srproj_export.R`, `ui/archive_view.R` | extract kind `.parquet` |
| `writer/fragments.R`, `writer/sapdoc.R` | the engine Methods fragment; the engine column in SAP_v1 |
| `ui/spec_form.R`, `ui/mod_sap.R`, `ui/analyses_view.R`, `ui/analyses_job.R` | Engine field, checklist rows, badge, progress line |
| `docs/scale.md` (new), `docs/analysis.md`, `docs/validation.md`, `memory.md` | guide, section, results, status |
| `tools/scale_equiv.R`, `tools/scale_fit.R` (new) | the 1M-row equivalence check; the 20M-row smoke |

`app/global.R` sources `scale/` after `routes/`.

## 4. Engine choice and the lock

### 4.1 Field and rule

- `SR_ENGINES <- c("auto", "memory", "stream")`.
  - The `lm`, `logistic`, `logistic_describe`, `modpois` and `table1` schemas take
    `engine = list(type = "chr", required = FALSE)`, validated against `SR_ENGINES`.
  - `cox` and `rmst` accept `auto` and `memory`. A `stream` there is refused at validation with
    "the streaming engine for Cox / RMST arrives in P1e.2".
- **`sr_engine_resolve(project, analysis)`** returns `list(engine, requested, reason, estimate)`.
  - `requested` is `spec$engine %||% "auto"`.
  - `estimate` is `sr_mem_estimate(project, table, cols, method)`. The method is `"table1"` for
    Table 1, otherwise `.sr_mem_method(route)`.
  - `auto` resolves to `memory` when `estimate$fits`, else to `stream`.
  - `stream` is allowed whether or not the data fits.
  - `memory` when it does not fit is a **block** (the lock checklist), never a silent switch.
  - `reason` is `estimate$reason`, prefixed with the resolution, e.g. "auto -> stream: does not fit
    in memory: about 12.1 GB needed ...".
- `sr_mem_estimate(method = "table1")` sets `need = data bytes`, because Table 1 builds no model
  matrix. The route stays `"in_memory"` or becomes `"stream"`. The other methods are unchanged,
  except that the non-fitting route value for `lm` and `glm` becomes `"stream"` instead of
  `"bigglm"`. Tests that expect `"bigglm"` are updated.

### 4.2 Refusals under `stream`

These are checked by the route's cross-field validation when `engine = "stream"`. When the engine is
`auto`, they are checked by the lock checklist once `auto` resolves to `stream`.

| Spec | Reason text names |
|---|---|
| `layers$missing = "mi"` | P1e.3 (MI at scale) |
| `layers$resampling = "bootstrap"` | P1e.3 (bag of little bootstraps) |
| `penalty = "firth"` | P1e.3 |
| route `cox*` / `rmst*` | P1e.2 |

### 4.3 Lock checklist, lock record and hash

- The "fits in memory" rows in `sr_sap_check` become **"engine"** rows, one per analysis:
  - `ok`, e.g. "a3: stream engine (auto -> stream: does not fit in memory: ...)" or
    "a1: in memory (fits ...)";
  - `block` for a `memory` request that does not fit, for a §4.2 refusal (naming its P1e.x), and
    for an estimate error (as today).
- **The lock payload** gains `engines = list(<analysis id> = list(engine, requested, reason))` for
  every pre-specified analysis. The reducer keeps it in `state$sap$lock$engines`.
- **`sr_sap_hash`** gains a part `engines`: the sorted ids whose resolved engine is `stream`, with
  that engine. **The part is omitted when that list is empty.** A project locked before P1e can only
  have in-memory analyses, so its hash is unchanged. An analysis added after the lock
  (`sr_analysis_add`) resolves its engine when it is added, records it in its `analysis.added`
  payload, and never enters the hash.

### 4.4 Computational deviation

**`sr_engine_switch(project, analysis_id, engine, reason)`** appends `engine.switched` with
`list(analysis, from, to, reason)`.

- It refuses:
  - a read-only project;
  - an unknown analysis;
  - `to` not in `c("memory", "stream")`, or `to` equal to the current engine;
  - `to = "memory"` when the current estimate does not fit;
  - `to = "stream"` when a §4.2 refusal applies;
  - an empty reason.
- The reducer sets `state$engines[[id]]`, which overrides the lock's engine for that analysis.
- `sr_deviations` lists it as kind `"computational"`, with the text "engine changed from <from> to
  <to>: <reason>; estimator unchanged".
- The SAP hash is unchanged, because the lock record is not rewritten.
- `sr_run` uses the effective engine: the switch if present, else the lock's.

When an effective `memory` analysis does not fit at run time (the caps changed or the project moved
machines), `sr_run` stops with "analysis '<id>' does not fit in memory: <reason>; it can be switched
to the streaming engine with sr_engine_switch()".

## 5. The stream extract

**`sr_extract_stream(project, table, cols)`** returns
`list(rel, sha, n, cols, kind = "parquet", bytes)`.

- **Content.**
  - The columns are `cols` (unique, in order) plus `.sr_row`, the 1-based position under
    `ORDER BY <every col>`. That is the order `.sr_extract_frame` uses, so `.sr_row` equals the row
    position that `spec$exclude_rows` refers to.
  - It is written by DuckDB `COPY (SELECT ... , row_number() OVER (ORDER BY <cols>) AS ".sr_row" FROM
    <view> ORDER BY ".sr_row") TO '<tmp>' (FORMAT parquet, COMPRESSION zstd, ROW_GROUP_SIZE 122880)`.
- **Write path.**
  - Write to `data/extracts/.tmp-<random>.parquet`, compute the streaming SHA-256 of the file
    (`openssl::sha256(file(<tmp>))`), then rename to `data/extracts/<sha>.parquet`.
  - If that name already exists, the temp file is deleted and the existing file is kept.
  - On any error the temp file is removed. A file named by a SHA is therefore always complete.
- **Space check before writing.**
  - Estimate: `n_rows * sum(bytes per column) * 0.35`, plus the DuckDB sort spill
    (`n_rows * sum(bytes) * 1.0`), plus 1 GB of headroom.
  - Free space comes from `.sr_drive_info` for the project's data drive.
  - If the estimate exceeds the free space, it refuses before writing, with both numbers in GB.
- **Byte determinism is not assumed.** The same data can yield a different SHA under another DuckDB
  version. That only gives a cache miss: the run's identity is the extract SHA, and the result is
  the same.
- `sr_run` writes the stream extract on every run and does not cache by (store hash, cols). That
  costs one `COPY` per run, and reuse can be added later if it proves slow.

## 6. The streaming run

### 6.1 Prologue and connection

- **`.sr_run_prologue_stream(ex)`** emits code that:
  - checks `ex$rel` is `data/extracts/<sha>.parquet`;
  - computes the file's streaming SHA-256 and stops with "extract changed" unless it equals `<sha>`;
  - opens `.sr_con <- .h_st_open(file.path(sr_project_dir, <rel>), sr_duck_memory, sr_duck_threads,
    sr_duck_temp)`;
  - defines `sr_src` as the quoted `read_parquet('<path>')` expression.
- It never binds `sr_data`.
- `.sr_emit_check(code, "analysis", prologue = TRUE)` applies.
- **Worker inputs.**
  - `sr_duck_memory`, `sr_duck_threads` and `sr_duck_temp` come from `sr_profile_caps` and the
    project's `data/tmp`, passed like `sr_project_dir`.
  - They are inputs, not literals, because they change speed, not results. They are kept out of the
    cache key.
- **`.h_st_open(path, mem, threads, temp)`** opens `duckdb::duckdb()` (in memory) and applies the
  `io/duck.R` lockdown statements (inlined text, not a call into StatRouter). It then sets
  `memory_limit`, `threads`, `temp_directory`, and `preserve_insertion_order = true`.
- The emitted code closes the connection in `on.exit`.

### 6.2 Chunk loop

**`.h_st_pass(con, sql, chunk, init, step)`** runs `DBI::dbSendQuery(con, sql)`, then
`dbFetch(n = chunk)` until empty, folding `state <- step(state, df)`. It returns `state`.

- The main passes read `SELECT <cols>, ".sr_row" FROM sr_src WHERE <keep>` in **file order**
  (`preserve_insertion_order = true`, a single file, written sorted).
- Every chunk checks that `.sr_row` is strictly increasing, both within the chunk and from the last
  row of the previous chunk. Otherwise it stops with "extract read out of order".
- The cluster pass reads `ORDER BY <cluster>, ".sr_row"`. It sorts once and spills to `sr_duck_temp`.
- `<keep>` drops `spec$exclude_rows` (`".sr_row" NOT IN (...)`, chunked into a temp table when
  there are more than 1000 positions) and applies the route's complete-case rule over the model
  columns and the cluster column, exactly as the in-memory setup does.
- **The chunk size is a literal** in the compiled code: `sr_opt("stream_chunk_rows", 250000)`,
  integer >= 1, read at compile. It changes the summation and TSQR order, so it belongs to the code
  and to the cache key.
- After each pass the worker writes `progress.json` (`{"pass": k, "max": m}`) to its temp dir for the
  Analyses panel. Here m is the most passes the fit can take: 1 for terms, up to 26 for IRLS
  (`maxit` + 1), and 1 each for the SE and marginal passes when they run. A failed write is ignored.

### 6.3 Frozen terms

`.h_st_fixed` reduces the data in SQL and then calls the **same** inlined helpers the in-memory
setup uses, so levels and knots are identical by construction:

- **Factor term:** `SELECT DISTINCT <col> FROM sr_src WHERE <keep>`, then
  `.h_levels(distinct_values, ref)`. `.h_levels` sorts the values itself.
- **RCS term** with a number of knots:
  - Let m be the count of non-missing values after `<keep>`.
  - If m < 100, fetch that column and call `.h_knots(x, nk)`. Hmisc's small-sample rule then applies
    unchanged.
  - Otherwise the knots are `quantile_cont(<col>, <Hmisc's probabilities for nk>)`. These equal R's
    type-7 `quantile`, which is what `Hmisc::rcspline.eval` computes for m >= 100.
  - If those quantiles are not all distinct, Hmisc's tie handling applies. The worker then fetches
    `SELECT <col>, count(*) GROUP BY <col>` and calls `.h_knots(rep(value, count), nk)`, provided
    m <= `sr_opt("stream_knot_rows", 5e7)`. Above that, the fit is refused with "too many tied values
    to place knots for <col> at scale; set explicit knots in the SAP".
  - Explicit knots stay literals.
  - The equivalence test pins knots to `Hmisc::rcspline.eval` on the full column (section 10).
- **Quad term:** none needed.
- The term builder (`routes/terms.R` "build") is applied unchanged to each chunk, so every chunk's
  design matrix has the same columns.

### 6.4 Fitting

- **lm, one pass.**
  - Each chunk contributes `M <- qr.R(qr(rbind(M, cbind(X, y))))`, starting from an empty
    (p+1) x (p+1) matrix. This is TSQR.
  - Then `R = M[1:p, 1:p]`, `c = M[1:p, p+1]`, and `RSS = M[p+1, p+1]^2`.
- **glm (logistic and modpois): IRLS with TSQR WLS steps.**
  - **Initialisation:** `mustart` from `family$initialize`, with unit prior weights.
  - **Pass k:** given `mu` (from `mustart`, or from `eta = X beta_{k-1}`):
    - compute `w = mu.eta(eta)^2 / variance(mu)` and `z = eta + (y - mu) / mu.eta(eta)`;
    - accumulate TSQR of `[sqrt(w) X | sqrt(w) z]` to get `beta_k`;
    - accumulate the deviance at the same `mu`, which is D(beta_{k-1}).
  - **Convergence** is `glm.fit`'s rule: `|D_k - D_{k-1}| / (|D_k| + 0.1) < 1e-8`, maxit 25. It is
    decided when pass k+1 reports D(beta_k). The fit then keeps `beta_k` and the `R` of pass k (the
    weights at beta_{k-1}), as `glm.fit` does, and discards pass k+1's WLS.
  - **Warnings** use `glm.fit`'s texts: non-convergence, and fitted probabilities numerically 0 or 1
    (`mu < 10 * .Machine$double.eps` or `mu > 1 - 10 * .Machine$double.eps`, counted during the
    pass).
  - `iter` is reported as `glm.fit` does.
- **Aliasing (`.h_st_alias`).** Apply `qr(R, tol = 1e-7)` (LINPACK, the rule `lm` and `glm` use) to
  the final R. Its pivot and rank give the aliased columns. The fit is then solved on the kept
  columns, and `aliased` lists the dropped names in the value list, as the in-memory code does.
- **The value list** has the same names and shapes as the in-memory compiled code, so `present()`
  is reused unchanged (`coef`, `aliased`, `n`, `n_total`, ...).

### 6.5 Standard errors (`.h_st_vcov`)

| `se` | Computation |
|---|---|
| `model`, lm | `sigma2 * chol2inv(R)`, with `sigma2 = RSS / (n - p)` |
| `model`, glm | `chol2inv(R)` from the kept pass (dispersion 1) |
| `robust`, lm (HC3) | **one pass:** `h_i = ||backsolve(R, x_i, transpose = TRUE)||^2`; meat `sum x_i x_i' e_i^2 / (1 - h_i)^2`; `V = B meat B` with `B = chol2inv(R)` |
| `robust`, glm (HC0) | **one pass:** scores `s_i = x_i (y_i - mu_i)` at `beta` (canonical links); `V = B (sum s_i s_i') B` |
| `cluster` (HC1) | **one sorted pass:** `U_g = sum over the cluster's rows of s_i` (lm: `x_i e_i`), carried across chunk boundaries; `V = G/(G-1) * (n-1)/(n-k) * B (sum U_g U_g') B` |

This reproduces `routes/se.R` (`sandwich::vcovHC` / `vcovCL` as called there). The equivalence
test is the authority. A single cluster is refused with the in-memory route's text. An NA cluster
id is removed by the complete-case rule, as today.

### 6.6 Marginal effects and observed risk

- **`.h_st_marginal`, one pass** over the standardisation set (`"analysis"`: the fitted rows;
  `"all"`: also the rows lost only to a missing outcome).
  - For each exposure level a, accumulate `sum mu(eta_a)` and `sum mu.eta(eta_a) * x_a`, where
    `x_a` is the row with the exposure set to a.
  - Dividing by n gives `routes/marginal.R`'s mean and analytic gradient (`colMeans(X * mu_eta(eta))`).
  - RD, log RR, their delta-method SEs `g'Vg` with the chosen V, and the NNT are then computed by
    the same inlined code as `marginal.R`.
  - When the SE pass is not sorted (`model` and `robust`), the marginal pass is merged into it.
- **`.h_st_obsrisk`** is one SQL `GROUP BY exposure` that counts events and n. It uses the same
  Wilson-interval code.

### 6.7 SQL Table 1 (`.h_t1_sql_main`)

It produces the same value list as `.h_t1_main(sr_data, by, vars, nonnormal)`.

- **Rows:** a missing `by` is excluded and counted; "Overall" is the same set of rows.
- **Classification:** a numeric column with more than 2 distinct non-NA values is continuous
  (`COUNT(DISTINCT)`). A column with more than 50 levels is refused, and so is a date or time column.
  Texts are identical to the in-memory route.
- **Groups and levels:** `SELECT DISTINCT`, then the same R ordering code (`sort(method = "radix")`).
  Both paths read the same stored text, so the order is the same.
- **Continuous columns**, per group:
  - n, missing count, mean, and SD by a two-pass query: the mean first, then
    `sum((x - mean)^2) / (n - 1)`, as R's `var` does.
  - median and IQR from `quantile_cont` (R type 7).
- **Categorical columns**, per group: level counts.
- **SMDs**, including Yang-Dalton and its pseudo-inverse rule, are computed in R from these
  aggregates by the inlined helpers of `routes/table1.R`.
- **Threads:** the Table 1 queries run with `SET threads = 1`, so the summation order is fixed.

## 7. Methods, SAP and the result

- **Writer fragment `engine`** for a stream analysis. Numbers go through the placeholder registry,
  never literal prose.
  - Regression: "The analysis dataset ({{N}} rows) exceeded the in-memory budget, so the models were
    fitted by streaming iteratively reweighted least squares over fixed chunks of {{chunk}} rows.
    This gives the same maximum-likelihood estimates and standard errors as standard software within
    numerical precision."
  - lm: the same sentence with "least squares".
  - Table 1: "Descriptive statistics were computed exactly by SQL aggregation over all {{N}} rows."
  - `engine.switched` adds the deviation sentence through the existing deviations text.
- **SAP_v1.docx:** the analyses table gains an "Engine" column (In memory or Streaming, with the
  resolution reason in a footnote).
- **The result value** carries `engine = "stream"`, `chunk`, `passes` and `iter`. `present()` passes
  them to the Analyses panel. They are not part of the presented effect tables.

## 8. UI

- **SAP spec form (`SR_FORM_KINDS`):** an Engine select with Auto (default), In memory and Streaming,
  for the five routes. Cox and RMST show Auto and In memory only.
- **Lock checklist:** the engine rows (section 4.3).
- **Analyses panel:**
  - an engine badge ("Streaming") on the result;
  - during a run, a progress line read from the worker's `progress.json` ("pass 4 of at most 29");
  - a "Switch engine" action that opens a dialog with a required reason and calls
    `sr_engine_switch`. It is shown only when the run refused for memory, or when the user opens it
    from the result menu.
- The existing process-tree Cancel applies. A cancelled or killed stream fit restarts from pass 1.
  There is no per-pass checkpoint.

## 9. Failure handling summary

| Failure | Behaviour |
|---|---|
| Not enough space for the extract | Refuse before writing; needed and free GB |
| Disk full or kill during `COPY` | Temp file removed; no `<sha>.parquet` written |
| Extract bytes changed | "extract changed" |
| Rows read out of order | "extract read out of order" |
| DuckDB error in the worker | Scrubbed by `.sr_run_scrub` (no absolute path) and recorded in `run.completed` |
| Non-convergence or separation | `glm.fit`'s warning texts, recorded like the in-memory warnings |
| `memory` does not fit at run | Stop, with the `sr_engine_switch` hint |
| A §4.2 combination | Refused at validation or at the lock, naming P1e.2 or P1e.3 |

## 10. Testing and Review Focus

**Equivalence.** `tests/testthat/test-scale-equiv.R` runs `sr_run` with `engine = "memory"` and with
`engine = "stream"` on the same locked synthetic table and compares the presented results.

- **Matrix:**
  - {lm, logistic, modpois} x {model, robust, cluster} x terms {linear, factor, rcs, quad,
    factor x linear interaction}, with `exclude_rows` on one case each;
  - marginal RD/RR/NNT for "analysis" and "all";
  - observed risk;
  - Table 1 with continuous, nonnormal, two-level and multi-level variables and a missing `by`.
- **Tolerances:**
  - estimates, model SEs, CIs and marginal effects: `all.equal(tolerance = 1e-8)`;
  - robust and cluster SEs: 1e-7;
  - Table 1 cells: identical strings;
  - knots: equal to `Hmisc::rcspline.eval` on the full column, 1e-12, including a tie-heavy column
    and one with fewer than 100 values.
- **Data:** about 20k rows. Chunk sizes 7, 1000, and larger than n, so clusters and factor levels
  cross chunk boundaries, and one factor level appears only in the last chunk.
- `tools/scale_equiv.R` runs the same matrix at **1M rows** (the main spec's P1 check). Its results
  go in `docs/validation.md`.

**Edge cases.** Each must match the in-memory route's behaviour or text:
- no complete case;
- a single cluster;
- an all-0 and an all-1 outcome;
- an aliased column;
- non-convergence (maxit) and separation;
- an NA cluster id;
- `exclude_rows` naming every row.

**Other tests.**
- **Lock:**
  - `auto` resolution both ways;
  - an explicit `stream` on data that fits;
  - a `memory` request that does not fit (blocked);
  - each §4.2 row;
  - the `engines` hash part present for stream and omitted when empty;
  - a pre-P1e lock fixture keeps its hash;
  - an analysis added after the lock records its engine.
- **Deviation:** `sr_engine_switch` writes the event, the deviations list shows it, the hash does
  not change, and each refusal holds.
- **Reproducibility:** two stream runs give byte-identical serialised results, and the second is a
  cache hit. A changed extract file stops with "extract changed". Rows shuffled after the write give
  "extract read out of order".
- **Air-gap:** the stream connection has autoload and autoinstall off and an empty
  `extension_directory`. The emitted code has no URL literal. The stream bodies are lint-clean:
  no p-value, ASCII only.
- **Archive:** freeze lists `<sha>.parquet` as an extract. Full, lean and auto export and then
  import round-trip it. The Archive panel's extract check accepts it.
- **Extract:**
  - the space refusal (with a stub drive-info);
  - no temp file left after an injected `COPY` error;
  - `.sr_row` equals the `.sr_extract_frame` position.
- **UI:** `testServer` for the Engine field, the switch dialog, the badge and the progress line.

**Scale smoke.** `tools/scale_fit.R`, on the laptop, manual:
- about 20M encounter rows, about 2M patients;
- logistic with a factor, an RCS term and a cluster SE by patient, plus Table 1;
- records wall time, passes, peak RSS (sampled by the `callr` watcher as in `tools/scale_test.R`;
  it must stay under the DuckDB cap + 2 GB + one chunk) and spill.

### Review Focus

These are inputs no task test pins down unless named:

1. **Old projects.** A project locked before P1e reopens with the same SAP hash and runs unchanged.
2. **A move to `locked16` after a `memory` lock.** The run refuses with the switch hint. It must
   never switch silently.
3. **Text levels with non-ASCII characters or mixed case.** Stream factor levels must equal the
   in-memory ones in the user's locale, because both call `.h_levels`.
4. **A cluster spanning chunk boundaries and the last chunk.** U_g must be flushed exactly once.
5. **Columns that are all NA, and an outcome stored as text with `event_value`.** The same refusal
   or behaviour as in memory.

## 11. Acceptance

- The full suite passes with `NOT_CRAN=true`, and `LINT OK`.
- `tools/scale_equiv.R` at 1M rows: every comparison within tolerance, recorded in
  `docs/validation.md`.
- `tools/scale_fit.R` at about 20M rows on the laptop: PASS, with the numbers recorded.
- A browser walk on synthetic data covering:
  - a project whose table exceeds the budget (forced by `SR_RAM_BUDGET_GB` or a profile override in
    the test);
  - the SAP shows the stream engine row and locks;
  - the run shows progress and the badge;
  - the result matches an in-memory run of the same spec;
  - the docx Methods carries the engine sentence.

## 12. Plan rulings

These were decided while writing the implementation plan
(`docs/superpowers/plans/2026-11-11-p1e1-streaming.md`), after reading the code. Where one conflicts
with the text above, the ruling wins.

- **R1** (section 6.3) Levels and knots are computed over the rows left after `exclude_rows` only,
  not over the complete-case set, as the in-memory setup does. "After `<keep>`" in 6.3 means after
  the exclusion.
- **R2** glm warnings are reproduced as the value fields `converged`, `near_separation` and
  `risk_above_one`, not as warning texts (in memory they are suppressed).
- **R3** lm under stream: `fig = NULL`; `sigma = sqrt(RSS / (n - rank))`; `r2` and `adj_r2` from a
  streamed total sum of squares.
- **R4** `sr_diag_run` refuses a stream analysis: "diagnostics for a streaming analysis arrive in
  P1e.3".
- **R5** `run.completed` gains `engine`, `extract_kind` and `chunk`; old events read as memory, rds
  and NULL. Export, freeze and archive take the extract extension from `ok_extract_kind`.
- **R6** One helper `.sr_run_spec()` builds the run spec. For memory it removes `engine` and
  `stream_chunk`, so a pre-P1e spec compiles and hashes byte-for-byte as before.
- **R7** (section 6.2) The chunk size is the optional spec field `stream_chunk`, filled by the run
  from `sr_opt("stream_chunk_rows", 250000)`. Compile reads it from the spec, not from options,
  because the run key hashes the spec.
- **R8** (section 6.2) Complete cases are decided in R per chunk with the in-memory snippets. SQL
  drops only `exclude_rows`. Whole-column checks run on the accumulated counts with the in-memory
  texts.
- **R9** (section 6.6) Observed risk is accumulated in R during the first main pass over the rows
  `.sr_glm_obs_code` uses, not by SQL `GROUP BY`.
- **R10** The alias tolerance is `1e-7` for lm and `1e-11` for glm (the `Cdqrls` tolerance
  `glm.fit` uses), re-decided on every IRLS pass.
- **R11** glm robust and cluster scores follow `sandwich::estfun.glm`: mu and eta at the final beta,
  working weights at the previous one.
- **R12** The TSQR merge uses LINPACK `qr(rbind(M, B), tol = 0)` and stores the unpivoted R, so a
  column that is zero in one chunk cannot permute the result.
- **R13** (section 6.2) `exclude_rows` is always registered as a temp table `sr_excl`; there is no
  1000-position branch.
- **R14** The Table 1 compile passes the literal `1L` threads to `.h_st_open`, so no `SET` follows
  the lockdown.
- **R15** (section 6.2) The progress file is `<project>/data/tmp/progress-<job>.json`, passed to the
  worker as `sr_progress_file`, outside the cache key. For robust SEs the marginal sums share the SE
  pass.
- **R16** The Streaming badge comes from the event-sourced `ok_engine`, not from the result value.
- **R17** (section 7) A new `sr_present()` wraps `route$present` and appends the engine Methods
  fragment for a stream value; every `present` call site uses it.
- **R18** After the lock the SAP hash reads the frozen `lock$engines`. Before the lock it resolves
  the engines now, and a resolution error counts as not stream.

### Rulings made during the build (added at Task 16)

- **R19** (Task 9) A non-binary numeric exposure with the marginal effect on needs the exact count of
  distinct values for the in-memory "N distinct values" stop or note. The exact count needs a sort, so the
  stream run makes one **extra sorted distinct-count pass** that R15's maximum did not include. The progress
  maximum is raised when the pass happens, so the progress file never shows a pass above its maximum.
- **R20** (Task 11) The Methods engine sentence depends on how the engine was chosen. When the engine was
  resolved from `auto`, the sentence uses the wording of section 7 ("exceeded the in-memory budget"). When
  the analysis asked for `stream` explicitly, the sentence says "as requested" instead, because the budget
  claim would be false for a table that fits. Numbers are plain digits (no thousands mark) so the writer can
  lift them into placeholders.
- **R21** (Task 10) The Table 1 fragment's "over all N rows" uses N = the overall n plus the rows excluded
  for a missing group, which matches the two counts in `table1_note`. The Table 1 mean is an exact
  (Shewchuk) sum accumulated in R with one correctly rounded division, so the strings equal the in-memory
  strings; the loop costs about 2.4 microseconds per value per continuous variable (a known limit at 1e8
  rows and more; see docs/scale.md).
- **R22** (Task 5) Knots use the SQL quantile only when Hmisc's default rule applies unchanged: at least 100
  values, more than nk + 2 distinct values, distinct quantiles, and no end value holding 5% or more of the
  rows (Hmisc's fractied rule, which section 6.3 did not mention). Otherwise the (value, count) table is
  fetched and `.h_knots` runs on it. A column with 5% or more at an end and more than `stream_knot_rows`
  (default 5e7) values is **refused**. A NaN is missing, as in memory.
- **R23** (Task 7) `glm.fit` step-halving (non-finite deviance or invalid mu) is **replaced by a stop**:
  "the streaming fit met a non-finite working value; refit in memory". With the clipped canonical links the
  only way to reach it is a Poisson eta overflow.
- **R24** (Task 15) The `exclude_rows` predicate is a hybrid, replacing R13. Up to 1000 rows are literals in a
  `NOT IN (...)`, which DuckDB pushes into the Parquet scan and which keeps the order of `.sr_row`. Above
  1000 the rows are registered as `sr_excl` and the predicate is a hash anti-join, which DuckDB plans as a
  parallel join whose output is **not** in `.sr_row` order over several row groups (found by
  `tools/scale_equiv.R` at 1M rows: "extract read out of order"). Reads that need file order therefore add
  `ORDER BY ".sr_row"` for that predicate. The cost of the 1e8-row ORDER BY spill is not measured.
