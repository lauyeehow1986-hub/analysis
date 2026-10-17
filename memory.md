# memory.md — StatRouter project memory

This file records status, decisions and gotchas, with short dated entries. The **spec** holds the
design; this file holds *what has happened* and *what we learned*.

## Status
| Date | Milestone |
|---|---|
| 2026-10-03 | Design approved after 4 rounds of critique and research; spec committed (`docs/specs/2026-10-03-statrouter-design.md`) |
| 2026-10-03 | Repo initialised and pushed to github.com/lauyeehow1986-hub/analysis (public) |
| 2026-10-03 | Spec approved; implementation plans written: `docs/superpowers/plans/2026-10-03-p0-feasibility-spike.md`, `docs/superpowers/plans/2026-10-03-p1a-foundation.md` |
| 2026-10-03 | P1a foundation done (branch `p1a-foundation`, Tasks 1-12): core (util, audit, events, project, hardware, libsnap, hash, lint, exec), design spec/compose, writer numfmt, and the bslib shell with the project create/open module. final-review fixes (I1 torn/failed audit writes, I2 parse_json only, I3 events/audit cross-check on open, M1 atomic rewrites). 2295 tests pass, `LINT OK` |
| 2026-10-10 | P1b data layer done (branch `p1b-data`, Tasks 1-23): DuckDB ingest to a Parquet store (chunked, resumable, external mode, pre-flight), dictionaries (CSV, REDCap CSV / ODM), join and cohort builder, profiling, the DQ check library (6 domains), Data Action Suggestor, recipe, readiness panel and the Data tab; `docs/data.md` user guide; `tools/scale_test.R`. Full suite 6558 pass, 0 fail, 2 skip (no fst package, no sas7bdat fixture), `LINT OK (41 files)`. Scale smoke 2M x 20 on the laptop and with `--profile=locked16`: PASS (peak RSS 1.07 GB / 0.83 GB) |
| 2026-10-18 | P1c analysis done (branch `p1c-analysis`, Tasks 1-16): emitted-code quoting and the result contract, SAP drafting, families, rules and the lock (moved from P1d), the hashed extract and cached run engine, routes table1 / lm / logistic / modpois / cox / rmst with g-computation marginal effects, MI (mice + Rubin, von Hippel auto-m), the bootstrap layer (percentile / BCa / normal, cluster, MI x boot), effect-size diagnostics with figures, sensitivity nodes, deviations and multiplicity; `docs/analysis.md`, `docs/validation.md`. Textbook reproductions (lung, GBSG, nhanes; NHEFS 3.52 kg when `SR_NHEFS_CSV` is set) and the CI-coverage simulation (6 cells, all pass). Final review fixed: `sr_sap_add` compiles every spec and checks binary event coding before the lock. Full suite 9967 pass, 0 fail, 3 skip (no fst, no sas7bdat fixture, NHEFS unset), `LINT OK (60 files)` |

**Next:**
1. User reviews the plans and picks the execution method.
2. Execute P0 spike on the laptop, then the user runs `run_spike.bat` on the locked PC; record the go/no-go.
   - **User hand-off (locked16 PC):** run the FULL scale test (`Rscript tools/scale_test.R 20000000 60 <dir on the data drive> --profile=locked16`, about 8 GB CSV, needs the free space the pre-flight reports) next to the P0 Task 8 hand-off (run `sr_spike.zip` on the locked PC). Paste the printed report back; expect `PASS` (peak RSS under 8 GB = DuckDB cap 6 + 2). Only a 2M x 20 smoke has been run (on the laptop).
3. ~~Execute P1a in a worktree (branch `p1a-foundation`)~~ done 2026-10-03; review and merge the branch.
4. ~~P1b data layer~~ done 2026-10-10 (branch `p1b-data`); review and merge the branch.
5. ~~P1c analysis~~ done 2026-10-18 (branch `p1c-analysis`); review and merge the branch.
6. **Next: write the P1d plan** (router + writer + freeze + UI), then the new **P1e scale** plan
   (big-data engines: SQL Table 1, bigglm, aggregated Poisson, piecewise-exponential Cox), and execute
   them in that order.
   - P1d also owns: the SAP_v1.docx and lock UI, Methods/Results prose from the P1c fragment inputs,
     chunk-level checkpoint/cancel and parallel workers for the bootstrap and MI (ExtendedTask).
   - Open P1c limitations to carry: interactions accept only linear and factor terms (no interaction
     with a quadratic or spline term); the censoring check ignores strata and uses Efron ties; a
     logical (Yes/No) outcome given a text `event_value` such as "Yes" is not caught at add time and fails
     at run with "no events" (refuse it at `sr_sap_add` in P1d); `sr_analysis_add` has no event-value
     check (the run refuses instead).

## Decisions (with the user)
| Topic | Decision |
|---|---|
| Name / prefix | StatRouter, `sr_` |
| Router | Pluggable behind TypeSafe `POST /v1/systemone`: `rules`, `jev` (cloud, needs key), `local` (Von 1.3), `clm` (CLM-8B Q8_0 via llama.cpp on the laptop GPU). Strands Decider 2B is a later option |
| Modes | `airgap` (real data, no network) and `connected` (de-identified data only; Jev needs consent and a payload preview) |
| Hardware | Switch between `locked16` and `laptop`: auto-detected, can be overridden |
| Users | Expert + Guided. Guided output is watermarked until a statistician signs off |
| Output | Word docx with bookmarked numbers, "refresh numbers" in the user's edited manuscript, plus `analysis.R` and a lockfile |
| Scope | All families (catalogues A–N). Prediction/ML is phased first |
| Class imbalance | User decides; the app recommends no resampling or class weights. SMOTE and similar run inside each fold, with intercept + slope recalibration and a side-by-side comparison panel |
| TabPFN 2.5–3.5 | Off by default; one-click toggle with a non-commercial licence acknowledgement per project. TabPFN v2 and TabICLv2 are on by default |
| Post hoc | Games–Howell by default; Tukey only if justified a priori in the SAP; Dunnett; emmeans/multcomp simultaneous CIs; full set of p-value adjustments; no Nemenyi |
| Risk calculator | Single-file offline HTML + `model_spec.json` (JSON Schema) + reference predictors in R, Python and JS + test vectors + signed manifest. Export needs four-way agreement |
| Large data | DuckDB streams into a project Parquet store; external-data mode for 100 GB sources; cohort builder |

## Research findings worth remembering (2026-10-03)
- **Model licences**
  - TabPFN v2: Prior Labs licence (commercial use allowed).
  - Non-commercial: TabPFN 2.5 / 2.6 / 3 / 3.5 (3.5 is evaluation-only) and LimiX-2.
  - Open: TabICLv2 (BSD-3); TabDPT and Mitra (Apache-2.0).
- **Package status**
  - Removed from CRAN: fastshap (2026-05), coxed (2025-07).
  - Not on CRAN: mlr3proba (r-universe only), catboost (GitHub only).
  - CoxBoost is back on CRAN (2026-09).
- **No existing R tool produces a standalone offline JavaScript calculator.**
  - DynNom and shinyPredict need a running Shiny server; shinylive fails when opened from `file://`.
  - m2cgen has had no release since 0.10.0 in 2022; use it only as a cross-check.
  - orbital 0.5.1 (2026-05) outputs R, SQL and JSON, but not JS.
- **Calculator export pitfalls**
  - xgboost stores thresholds and leaf values as float32, so the JS must compare with `Math.fround`.
  - LightGBM splits on `<=`.
  - Ordinal-model sign conventions differ between `MASS::polr` and `rms::orm`.
  - Cox exports must record the centring constant; `coxph` centres on the means.
- **Regulation**
  - HSA GL-04 (software medical devices) has been updated with a Change Management Program (industry
    briefing 2026-05-20).
  - A calculator labelled "research use only" is generally outside device scope. Using it for
    individual patient care may make it a SaMD.
- **Still to re-verify:** see "Open items to re-verify" at the end of the spec.

## Gotchas (from sibling projects; apply here)
- **Stray home repo:** `%USERPROFILE%\.git` captures any folder that has no repo of its own. Always
  check `git rev-parse --show-toplevel`.
- **Session sandbox:** a Claude session started from the structured_deid worktree has an edit guard
  that blocks the Write tool here. Start sessions in this repo instead.
- **`Rscript -e`** segfaults on long multi-line code; use a script file.
- **PowerShell 5.1**
  - Variable names are case-insensitive.
  - `Set-Content -Encoding UTF8` writes a BOM that breaks Rscript; write ASCII.
- **MAX_PATH:** build bundles in a short directory (e.g. `C:\sds_build`) and prune `include/`
  subtrees.
- **Antivirus** quarantines unsigned executables (e.g. llama.cpp builds). Keep them optional, or
  exclude them from the bundle.
- **Locked PC**
  - There is no compiler, so use only precompiled packages (rstanarm, not brms).
  - There is no cairo/GTK, so use ragg/systemfonts with an Arial fallback.
  - No webshot, chromote or DiagrammeR PNG export.
- **Shiny app.R + autoload off:** with `options(shiny.autoload.r = FALSE)` Shiny runs app.R with the
  app dir as the working directory but does NOT source `global.R`; `app.R` sources it itself
  (guarded by `exists("sr_root")`). `tools/run_app.R` also sets the option and serves 127.0.0.1 only.
- **Tests that launch the app:** `testthat::test_dir` does not set `NOT_CRAN`, so `skip_on_cran()`
  silently skips; `tools/run_tests.R` sets it. Poll the child with curl handles that bypass proxies
  (`proxy = "", noproxy = "*"`), as a corporate proxy swallows loopback requests.
- **Shiny sessions share one R process:** the project lock is per process (host + pid), so two
  sessions opening the same project both get write access. Fine for the one-user desktop app.
- **ASCII source and badges:** build the middle-dot separator with `intToUtf8(0xB7)`; never type it.
- **Bundle dependency lists** must include the full transitive closure, or packages fail on the
  target machine.
- **Base R has no fsync;** appends use flush() (survives process crash, not power loss/unplug) --
  torn-tail repair on both logs is the mitigation (spec says fsync).
- **TLS on the dev laptop:** AVG Web-Shield can intercept HTTPS (MITM), and `uv` needs
  `--system-certs`.
- **DuckDB is locked down at connect time (air-gap).** Probe on 2026-10-04: duckdb 1.4.3 bundles
  `parquet` and `core_functions` but **not `json`**. `httpfs` sits installed in the dev laptop's user
  cache, and `INSTALL` / `LOAD` still succeed after `lock_configuration`. So every connection opens
  with `SET extension_directory` pointing at an empty `data/tmp/duckdb_ext` folder (owned by the
  project), sets `autoinstall_known_extensions=false`, `autoload_known_extensions=false` and
  `allow_community_extensions=false`, then sets `lock_configuration=true`. All app SQL goes through
  `sr_duck_exec` / `sr_duck_query`, which refuse any statement starting with `INSTALL`, `FORCE INSTALL`,
  `LOAD`, `SET`, `RESET`, `PRAGMA` or `ATTACH` (except the connection's own setup) and any string
  literal matching `'[A-Za-z][A-Za-z0-9+.-]*://`. Never read JSON with DuckDB `read_json` (use
  `jsonlite::stream_in` on a `file()` connection) and never use DuckDB's sqlite extension (use RSQLite).
  Paths are never URLs: `sr_sniff` refuses `^[A-Za-z][A-Za-z0-9+.-]*://` before any read.
- **Locked connection = no `SET threads`:** the reproducible sample (`REPEATABLE`) is deterministic only
  on one thread, so it runs on its own short-lived single-threaded connection with the same lockdown.
- **gz ingest is single-pass:** a gzip stream cannot be entered in the middle, so a `.gz` source is read
  in one pass and is not resumable (bz2 / xz / zip are staged to `source.csv` first, then read normally).
- **Quoted newlines force single-pass:** a CSV with line breaks inside quoted fields cannot be cut at a
  record boundary without reading from the start, so it is read in one pass and is not resumable.
- **Measuring RSS of a process blocked in DuckDB:** a `later::later()` poll cannot fire while R is inside
  a DuckDB call, so `tools/scale_test.R` samples the parent from a separate `callr::r_bg` watcher via
  `ps::ps_memory_info()` and cross-checks with the OS peak working set (`peak_wset`).
- **The SAP lock lives in P1c and every run needs it.** `sr_run` refuses an unlocked SAP; tests that
  run an analysis must add, lock, then run (see the `.fx_ready_project` fixture).
- **`cache_inputs` keeps the run cache valid across moved projects:** the cache identity is the extract
  SHA, route key + spec, seed, library snapshot and hardware fingerprint, never the project path; a
  project moved to a machine with a different library or hardware fingerprint recomputes.
- **Emitted analysis code never names a diagnostic,** and diagnostics live in `app/R/diagnostics/`,
  outside `routes/`; the lint fails the build otherwise (no test-gated switching).
- **`exclude_rows` positions** (the influence sensitivity) refer to the row order of the sorted extract
  (`sr_extract` orders rows by the selected columns), not to store order.
- **NHEFS needs `SR_NHEFS_CSV`:** the textbook test reads the CSV from that path and skips when it is
  unset; the dataset is never committed. Recorded values are in `docs/validation.md`.
- **Coverage simulation:** `Rscript tools/coverage_sim.R [cells] [reps_scale]` takes about 40 minutes
  on the laptop with parallel workers (`SR_COVERAGE_WORKERS`; results do not depend on the worker
  count) and hours serially. A failing cell is a finding; never widen the tolerance.
- **Claude Code subagents and docs:** a hook can refuse a subagent's Write of a `.md` deliverable as a
  "report file"; write such docs from the main session.
- **Claude Code Bash tool and big heredocs:** a very large single heredoc can be rejected with
  "unexpected EOF while looking for matching quote". Write files in small `cat >> file <<'EOF'` chunks
  and keep commands such as `grep -P` out of the same call.

## DQ thresholds (calibrated on the synthetic cardiac twins, `tools/dq_calibrate.R`)
- **unit_mix min share 0.3% -> 0.155%** (`SR_DQ_UNIT_MIX_MIN_SHARE`; min n stays 3). Recall of the planted
  `weight_kg` pounds, seeds fired / 20, before -> after: n=2000 4 -> 10; n=50000 0 -> 20; n=500000 0 -> 20
  (n=5000 seeds 1-5: 2 of 5 -> 5 of 5). Clean-twin firing: 0 false positives at every n; the only warn+ findings
  on the clean twin are the by-design missing_col / missing_dependence on death_date and statin_start_date.
  n=2000 recall for weight_kg stays below 1.0: most pounds weights (x2.2) fall inside the kg soft range, so only
  0-8 of 2000 are detectable, and some seeds have fewer than 3. The window for the share is (0.15%, 0.16%]:
  3 of 2000 must stay silent (Task 14 test), 8 of 5000 must fire. The planted glucose 60s now also raise a
  unit_mix finding (as mg/dL) at n=2000, next to outlier.
- Grid run in full: 20 seeds x n in {2000, 50000, 500000}, recall 1.0 for every other planted row.

## Reuse sources
- **structured_deidentification:** `project.R`, `hashchain.R`, `engine_py.R`, `run_engine.py`,
  `sdc*.R`, `report.R`, `tools/*.ps1`.
- **AndyFishing:** the `run.bat` pattern and `stage_ollama.ps1`.
- **sts_reverse_engineer:** the `sts_coefficients.json` + `sts_model.py/.R` pattern for spec-driven
  predictors.
- **elso:** the REDCap ODM XML parser.
