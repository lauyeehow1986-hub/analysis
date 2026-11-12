# Analysis at scale: the streaming engine (P1e.1)

StatRouter fits `lm`, `logistic`, `modpois` and `table1` analyses either **in memory** (the data is read
into R) or by **streaming**: the data is written once to a Parquet extract and fitted over fixed-size
chunks, so R never holds the table. The two engines give the same estimates and standard errors within
numerical precision; the streaming one exists for tables that do not fit the machine's RAM budget.

This is a research and governance tool, not for clinical or diagnostic use. Outputs need review by a
qualified statistician. The design is
[docs/superpowers/specs/2026-11-11-p1e1-streaming-regression-design.md](superpowers/specs/2026-11-11-p1e1-streaming-regression-design.md);
section 12 of it holds the plan rulings that win over the body.

## 1. The engine field

Every analysis of the six routes `lm`, `lm_describe`, `logistic`, `logistic_describe`, `modpois` and `table1` has an
`engine` field. Its values are `auto` (the default), `memory` and `stream`. In the SAP form it is the Engine
select: Auto, In memory, Streaming. `cox` and `rmst` accept `auto` and `memory` only (section 6).

| Value | Meaning |
|---|---|
| `auto` | Stream when the analysis does not fit in memory, otherwise in memory. |
| `memory` | In memory. If the data does not fit, the lock checklist **blocks**; nothing is switched silently. |
| `stream` | Always stream, whether or not the data would fit. |

### How `auto` resolves, and when it is frozen

`sr_engine_resolve(project, analysis)` calls `sr_mem_estimate()`: the data bytes plus three times the model
matrix must be at most 0.8 of the hardware profile's RAM budget. Table 1 builds no model matrix, so its need
is the data bytes alone.

- Before the lock, the SAP checklist shows one **engine** row per analysis, for example "a3: stream engine
  (auto -> stream: does not fit in memory: ...)" or "a1: in memory (fits ...)".
- **At the lock the resolved engine is frozen** into the lock record (`lock$engines`: engine, requested and
  reason, per pre-specified analysis). After the lock the engine never re-resolves: a project moved to a
  smaller machine keeps its locked engine.
- The SAP hash gains an `engines` part naming the analyses whose resolved engine is `stream`. The part is
  **omitted when no analysis streams**, so a project locked before P1e (which can only have in-memory
  analyses) keeps its SAP hash and **reopens and runs unchanged**. The SAP_v1.docx of such a project has no
  engine table.
- An analysis added after the lock resolves its engine when it is added and records it; it never enters the
  SAP hash.
- When a locked `memory` analysis does not fit at run time (the caps changed or the project moved), `sr_run`
  stops with "analysis '<id>' does not fit in memory: ...; it can be switched to the streaming engine with
  sr_engine_switch()". It never switches by itself.

## 2. Switching the engine after the lock

`sr_engine_switch(project, analysis_id, engine, reason)` records an `engine.switched` event. It is refused
until the SAP is locked (before the lock, edit the analysis engine instead). It is a
**computational deviation**, not a change to the pre-specified method: the estimator is unchanged, the SAP
hash is unchanged, and the Deviations card and the Methods text list it ("engine changed from <from> to <to>:
<reason>; estimator unchanged").

It refuses a read-only project, an unknown analysis, a target that equals the current engine, an empty
reason, `memory` when the data does not fit, and `stream` when a refusal of section 6 applies. The effective
engine of a run is the switch if there is one, else the locked engine. In the Analyses panel the "Switch
engine" action opens a dialog with a required reason.

## 3. What you see in the Analyses panel

- During a streaming run the panel shows a **progress line** read from the worker's progress file
  (`<project>/data/tmp/progress-<job>.json`): "pass 4 of at most 29". The file is not atomic and old files
  stay in `data/tmp`; the panel tolerates both. The maximum can rise during a run (see the distinct-count
  pass in the spec's section 12), but the line never shows a pass above its maximum.
- A finished streaming result carries the **Streaming** badge, read from the event-sourced run record (not
  from the result value), so it survives reopening.
- Cancel is the usual process-tree Cancel. A cancelled or killed streaming fit restarts from pass 1; there is
  no per-pass checkpoint.

## 4. The Methods sentence

A streamed analysis adds an engine sentence to the Methods text (the numbers go through the placeholder
registry):

- Resolved from `auto`: "The analysis dataset (N rows) exceeded the in-memory budget, so the models were
  fitted by streaming iteratively reweighted least squares over fixed chunks of C rows. This gives the same
  maximum-likelihood estimates and standard errors as standard software within numerical precision."
  (`lm`: "least squares".)
- Asked for explicitly (`stream`): "The analysis was run with the streaming engine, as requested, reading the
  data in chunks of C rows, ..." because the budget claim would be false for a table that fits.
- Table 1: "Descriptive statistics were computed exactly by SQL aggregation over all N rows." N is the overall
  n plus the rows excluded for a missing group.
- An `engine.switched` event adds the deviation sentence.

## 5. Options and environment

| Name | Default | Effect |
|---|---|---|
| `stream_chunk_rows` (option `sr.stream_chunk_rows`) | 250000 | Rows per chunk. It is copied into the run spec as `stream_chunk` and is part of the stream run key, so changing it reruns streaming analyses that did not fix a chunk in their spec. |
| `stream_knot_rows` (option `sr.stream_knot_rows`) | 5e7 | The largest (value, count) table the knot step may expand (section 7). |
| `SR_RAM_BUDGET_GB` (option `sr.ram_budget_gb`) | the hardware profile's budget | Overrides the RAM budget `auto` compares against. Tests and the browser walk use it to force `stream`. A value that is not a number > 0 is ignored with a warning (once per value) and the profile's budget is used. |

Memory and disk:

- **Memory.** R holds one chunk plus the accumulated p x p matrices, so it does not grow with the number of
  rows. DuckDB is capped by the hardware profile (14 GB on `laptop`, 6 GB on `locked16`). The scale check
  passes when the peak RSS stays below the DuckDB cap plus 2 GB.
- **Disk.** The extract is written to `<project>/data/extracts/<sha>.parquet` (zstd, about 0.35 of the raw
  bytes) and the DuckDB sort spill goes under `data/tmp`. Before writing, the run estimates both plus 1 GB
  of headroom and **refuses, naming both numbers**, if the data drive has less free space.
- A stream run writes the extract on every run; the identity of the run is the extract's SHA-256, which is
  also what `sr_freeze` records and verifies (a flipped byte gives "SHA-256 differs from the manifest").

## 6. Limits and refusals

| Spec | Refused with |
|---|---|
| route `cox` or `rmst` and `engine = "stream"` | "the streaming engine for Cox / RMST arrives in P1e.2". With `auto` they resolve to memory; if the table does not fit, the lock checklist blocks. |
| `layers$missing = "mi"` | P1e.3 (multiple imputation at scale) |
| `layers$resampling = "bootstrap"` | P1e.3 (bag of little bootstraps) |
| `penalty = "firth"` | P1e.3 |
| diagnostics of a streaming analysis | "diagnostics for a streaming analysis arrive in P1e.3" |

When `engine = "stream"` the refusal comes from the route's validation. When the engine is `auto` and
resolves to `stream`, the lock checklist blocks with the same reason. Effect-size diagnostics, MI and the
bootstrap therefore stay in memory until P1e.2 and P1e.3 are built.

## 7. Behaviour that differs from in memory

- **Exact-sum Table 1 mean.** The streaming Table 1 is computed by SQL aggregation, and the mean is an exact
  (Shewchuk) sum accumulated in R with one correctly rounded division, so the strings are **identical** to
  the in-memory Table 1. The cost is an interpreted loop over every value: about **2.4 microseconds per
  value per continuous variable** (2.10 s at 1e6 values, 23.8 s at 1e7). That is about 24 s at 1e7 rows,
  about 4 minutes at 1e8 and about 40 minutes at 1e9, per continuous variable, on top of the SQL passes. It is
  linear and memory stays flat. This is a **known limit**: a Table 1 with ten continuous variables at 1e9 rows
  would spend about 6.6 hours in this loop. A vectorised or SQL-side exact sum is the remedy if 1e8 rows and
  more matter.
- **Step-halving is replaced by a stop.** `glm.fit` halves the step when the deviance is not finite. The
  streaming fit instead stops with "the streaming fit met a non-finite working value; refit in memory" (or
  "... non-finite deviance ..."). With the canonical links this is reachable only through a Poisson eta
  overflow. Refit that analysis in memory (`sr_engine_switch`), if it fits.
- **Complete separation** is reported as in memory: `converged = FALSE` and `near_separation = TRUE`; the
  warnings are fields of the result, not warning texts.
- **Knots of an RCS term.** Knots are the quantiles Hmisc chooses. The SQL quantile is used when Hmisc's
  default rule applies unchanged. If an end value holds 5% or more of the rows, Hmisc's rule differs, so the
  (value, count) table is fetched and `Hmisc` runs on it. That is **refused above `stream_knot_rows` values**
  (a column with at least 5% of its rows at its minimum or maximum and more than `stream_knot_rows` rows is refused).
  Give explicit knots in the term, or stream fewer rows, to avoid it.
- **Exact distinct count.** A non-binary numeric exposure with the marginal effect on needs an exact distinct
  count, which is one extra sorted pass.
- **`exclude_rows`.** Up to 1000 excluded rows go into the SQL as literals (pushed into the Parquet scan, so
  the read stays in `.sr_row` order). Above 1000 the rows are registered as a table and joined by an ordered
  hash anti-join with an explicit `ORDER BY ".sr_row"`. DuckDB's parallel anti-join does not keep the
  order over several row groups, which the streaming sums and the cluster flush depend on.
- **Error texts.** An infinite value in a chunk stops both engines but with different texts (memory: "NA/NaN/Inf
  in 'x'"; stream lm: "NA/NaN/Inf in foreign function call (arg 1)"; stream glm: the non-finite working value
  text above).
- **Order of summation.** Sums are accumulated chunk by chunk, so results match memory to about 1e-11 (the
  largest difference at 1M rows was 1.34e-11), not bit for bit.

## 8. How it was checked

See [validation.md](validation.md) section 4: the 1M-row memory-versus-stream equivalence, the 20M-row smoke
(logistic with a cluster SE), and the browser walk. Tools: `tools/scale_equiv.R` (`--rows`, `--chunk`,
`--out`) and `tools/scale_fit.R` (`--rows`, `--patients`, `--chunk`, `--routes`).
