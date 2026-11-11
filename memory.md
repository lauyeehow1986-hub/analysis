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
| 2026-10-18 | P1c.1 study feasibility verdict done (branch `p1c1-feasibility`, Tasks 1-6): the outcome-blind engine (`app/R/sap/feas_engine.R`: SEs, design effect, h, MDE, tiers, blockers, warnings), the `feasibility.assessed` / `feasibility.decided` events, assess / decide / report / fragment API (`app/R/sap/feasibility.R`), the lock gate (`sr_sap_check` feasibility rows, the verdict frozen into `sr_sap_hash`, `sr_sap_remove` and `sr_run` honour a stop). Validation: the predicted null-effect SE is within 10% of the empirical SD in 11/11 cells at 1000 reps (with and without clustering), and on NHEFS the marginal-value prediction is conservative (predicted MD SE 0.472 vs actual 0.463); `tools/feas_sim.R`, `docs/validation.md` section 3. `docs/analysis.md` section 4. Full suite 10691 pass, 0 fail, 4 skip (no fst, no sas7bdat fixture, two NHEFS tests with `SR_NHEFS_CSV` unset), `LINT OK (63 files)` |
| 2026-11-07 | P1d.1 writer done (branch `p1d-writer`, Tasks 1-12): the placeholder registry and document model (`app/R/writer/placeholders.R`, `fragments.R`, `tables.R`, `figures.R`, `model.R`), small-cell suppression (`sdc_out.R`, one `suppress_k` per document), the claim guard G1-G9 (`guard.R`), the STROBE checklist (`checklist.R`), references (`cite.R`), the Word renderer with journal presets (`docx.R`), the analysis script and lockfile (`script.R`), the SAP and feasibility Word files (`sapdoc.R`), and `sr_export` (out/exports/export-NNN, with a `doc.exported` event per file). Guide `docs/writer.md`; `docs/analysis.md` section 13. Lint `LINT OK (76 files)`. Full suite: run by the controller at the P1d.1 review; its counts are not recorded here |
| 2026-11-08 | P1d.2 router done (branch `p1d-router`, Tasks 1-11): the PICO record (`pico.set`), the decision-node graph with input hashes and staleness, the metadata-only state builder with column-name masking and the token budget, the System One request / response contract, the abstain rule, the rules backend and the Jev cloud backend (gates: mode, key, DPA ack, session consent, payload approval; shadow mode, predictions never applied), the P1 nodes (design, purpose, guideline, outcome / exposure column, outcome type, primary route, covariate timing), the decision table, the shadow report and golden-set export, the SAP lock gate and router hash part, and the writer study-design paragraph, decision table and STROBE items 3-5. Guide `docs/router.md`. Full suite 13355 pass, 0 fail, 4 skip (no fst, no sas7bdat fixture, two NHEFS tests with `SR_NHEFS_CSV` unset), 1 warning (shiny built under a newer R), `LINT OK (89 files)`; after the final-review fixes (PICO in the design and purpose hashes, decisions frozen at the lock, vanished instances, mask punctuation, shadow same_input) 13483 pass, 0 fail, 4 skip, 1 warning, `LINT OK (89 files)`. The first full run failed 2 tests (test-writer-checklist): the cached writer template outlives `test-spec`, which empties the route registry, so `.fx_writer_project` now re-registers the routes |
| 2026-11-08 | P1d.3 freeze done (branch `p1d-freeze`, Tasks 1-7): `sr_freeze` (gated, manifest with SHA-256 of store, sources, extracts, fits, exports and code; pinned events prefix and audit tip), the library baseline gate in `sr_run` with `sr_rebaseline_diff` / `sr_rebaseline`, the `.srproj` export (full / lean / auto, owner names scrubbed) and all-or-nothing import with a pure-R tar header scanner, `sr_freeze_diff` and `sr_freeze_results_diff`; `docs/archive.md`. P1 exit item 7 passes end to end (`test-archive-e2e.R`). Archives are not signed (out of scope). Full suite 13859 pass, 1 fail (the flaky 5 s timing assert in `test-archive-tar.R`, 10.9 s under load; see the gotcha), 4 skip (no fst, no sas7bdat fixture, NHEFS unset, one more textbook skip), `LINT OK (95 files)` |
| 2026-11-09 | P1d UI sub-project 1 (Design + SAP panels) done (branch `p1d-ui-sap`, Tasks 1-9): the Design panel (`app/R/ui/mod_design.R`: PICO, run rules, decision table, shadow summary and feed export; the Ask Jev card only outside air-gap mode) and the SAP panel (`mod_sap.R`: analyses with the typed spec form, feasibility assess and decide, families and rules, the lock checklist, the typed-word lock, SAP_v1.docx and the feasibility report), wired into `app/app.R` after Data; the empty Profile tab is gone. Shared helpers `.sr_ui_*` in `app/R/ui/util.R`; the spec form is driven by `SR_FORM_KINDS` (`spec_form.R`). Tests run per file (lint, ui-util, ui-spec-form, mod-design, mod-sap, app-launch) all pass; the full suite was NOT run for this task (controller ruling). Browser walk on synthetic cardiac data (import, roles, PICO, rules, add/edit/add analyses, feasibility, DQ decisions, lock, SAP_v1.docx) found the stale-checklist issue below |
| 2026-11-11 | P1d UI sub-project 2 (Analyses panel) done (branch `p1d-ui-analyses`, Tasks 1-9): `run.started` / `run.cancelled` events and the run history in the reducer, `sr_run_cancel`, `sr_diag_apply_plan`, the Analyses panel (`app/R/ui/mod_analyses.R`, `analyses_job.R`, `analyses_view.R`: Runs queue in a mirai worker with a process-tree Cancel, Result with diagnostics and Apply, Primary vs sensitivity, Multiplicity, Deviations and run history), wired after SAP with an app-wide busy flag; `docs/analysis.md` section 14. Suite counts and the acceptance walk: see the Task 9 commit message |
| 2026-11-11 | P1d UI sub-project 3 (Write, Archive, Import, Settings) built on branch `p1d-ui-write-archive`, Tasks 1-11: `sr_export_verify` / `sr_export_zip` / `sr_opt_source`, the shared job layer `app/R/ui/job.R`, the busy hub in `app/app.R`, the Write panel (`mod_write.R`, `write_view.R`), the Archive panel (`mod_archive.R`, `archive_view.R`, `archive_job.R`), the Import card on the Project tab (`mod_import.R`) and the Settings panel (`mod_settings.R`, `settings_view.R`); guides `docs/writer.md` section 10, `docs/archive.md` sections 10-11, `docs/settings.md`. Acceptance walk (Task 12) passed, steps 1-5 (see the walk note below); **pending:** the final review; not yet merged |

**Next:**
1. User reviews the plans and picks the execution method.
2. Execute P0 spike on the laptop, then the user runs `run_spike.bat` on the locked PC; record the go/no-go.
   - **User hand-off (locked16 PC):** run the FULL scale test (`Rscript tools/scale_test.R 20000000 60 <dir on the data drive> --profile=locked16`, about 8 GB CSV, needs the free space the pre-flight reports) next to the P0 Task 8 hand-off (run `sr_spike.zip` on the locked PC). Paste the printed report back; expect `PASS` (peak RSS under 8 GB = DuckDB cap 6 + 2). Only a 2M x 20 smoke has been run (on the laptop).
3. ~~Execute P1a in a worktree (branch `p1a-foundation`)~~ done 2026-10-03; review and merge the branch.
4. ~~P1b data layer~~ done 2026-10-10 (branch `p1b-data`); review and merge the branch.
5. ~~P1c analysis~~ done 2026-10-18 (branch `p1c-analysis`), merged to `main` (PR #1).
6. ~~P1c.1 study feasibility verdict~~ done 2026-10-18 (branch `p1c1-feasibility`; plan
   `docs/superpowers/plans/2026-10-18-p1c1-feasibility.md`); review and merge the branch. The SAP lock
   requires its verdict.
7. **P1d is split into four plans: writer, router, freeze, UI.** The writer (P1d.1) is done (plan
   `docs/superpowers/plans/2026-10-18-p1d-writer.md`, branch `p1d-writer`, guide `docs/writer.md`). The
   router (P1d.2) is done (plan `docs/superpowers/plans/2026-11-08-p1d-router.md`, branch `p1d-router`,
   guide `docs/router.md`).
   The freeze (P1d.3) is done (plan `docs/superpowers/plans/2026-11-08-p1d-freeze.md`, branch `p1d-freeze`,
   guide `docs/archive.md`).
   **P1d UI sub-project 1 (Design + SAP) is done 2026-11-09 (branch `p1d-ui-sap`). Next: sub-projects (2) and (3), each with its own spec, plan and branch:**
   (2) Analyses (run, long jobs, post-lock analyses, results), done 2026-11-11 (branch `p1d-ui-analyses`); (3) Write + Archive + Settings, built 2026-11-11 (branch `p1d-ui-write-archive`; pending the acceptance walk and final review).
   Together they own the payload preview, the consent dialog and the freeze, export and import buttons. The empty Profile tab is dropped (the Data panel covers it). Writer and router open items (carried into the UI, P2 and P3 work):
     - The router has only the Design panel; the preview and consent dialogs come later. Only Jev is registered (the `local` and `clm` ids are reserved for P3), and
       the abstain thresholds (`SR_ROUTER_ASK_COST`, `SR_ROUTER_DANGER_COST`) are placeholders until P3.
     - The journal presets are `verified: false` (all four: default, bmj, lancet, jama). Check each against
       the journal's author guide. The jama preset narrows the side margins for its 178 mm figures.
     - The bibliographic data of the method references copied from the plan (Task 5) must be re-checked
       against the publisher records before release (including schulz2010, the CONSORT statement, added by
       the router writer task).
     - "Refresh numbers" in an edited docx is P2. The writer stores only the first-use bookmarks (`sr_E<n>`)
       and `placeholders.json`; later uses of a token are not refreshed (R30).
     - STROBE items 1a, 1b and 2 stay amber (author items); items 3-5 are scored from the router decisions and the PICO.
   Then the **P1e scale** plan
   (big-data engines: SQL Table 1, bigglm, aggregated Poisson, piecewise-exponential Cox), and execute
   them in that order.
   - P1d also owns: the SAP_v1.docx and lock UI, the feasibility panel and report docx, the Methods
     sentence from the `sr_feasibility_fragment` inputs and the claim-guard rule that an exploratory
     analysis needs hypothesis-generating language, Methods/Results prose from the P1c fragment inputs,
     chunk-level checkpoint/cancel and parallel workers for the bootstrap and MI (ExtendedTask).
   - Open P1c limitations to carry: interactions accept only linear and factor terms (no interaction
     with a quadratic or spline term); the censoring check ignores strata and uses Efron ties; a
     logical (Yes/No) outcome given a text `event_value` such as "Yes" is still accepted by `sr_sap_add`,
     but since P1c.1 it surfaces as a "no events" blocker in the feasibility verdict before the lock
     (see the R45 gotcha below); `sr_analysis_add` has no event-value check (the run refuses instead).
   - P1c.1 follow-ups F1-F15 done 2026-10-18 (branch `p1c1-followups`). Declined: a locale-independent
     level sort. The engine follows the run's (locale) order of `.h_levels` on purpose, and changing it
     would change the emitted analysis code (a test pins engine levels == run levels).
   - Open feasibility follow-ups (from the p1c1-followups review):
     - No engine version is stored with an assessment, so P1c.1-era assessments stay current after
       engine changes.

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
- **Feasibility SE simulation:** `Rscript tools/feas_sim.R [cells] [reps_scale]` runs the 11 cells at
  1000 replications in about 95 s with 6 parallel workers (`SR_FEAS_SIM_WORKERS`; results do not depend
  on the worker count), and a few times that serially. A failing cell is a finding; never widen the 10%
  tolerance or change a formula away from spec catalogue O. Recorded results: `docs/validation.md`
  section 3.
- **Every new lock needs `.fx_feasible` in tests:** `sr_sap_lock` refuses a primary analysis with no
  current feasibility decision, so a test that locks calls `sr_sap_lock(.fx_feasible(p))` (it assesses
  and decides every primary; a clustered analysis gets a test ICC).
- **Feasibility is blind, so expected events are scaled, not counted:** the events among the complete
  cases are `events_total * n_cc / n_obs`. Counting them would cross the outcome with the missingness
  of the other columns. The permutation test (outcome shuffled among non-missing rows) must leave the
  assessment unchanged.
- **The feasibility element of the SAP hash is omitted when empty,** so locks made before P1c.1 keep
  their hash. A stop on any pre-specified analysis (secondary included) blocks the lock; missing,
  stale, no-decision and revise checks apply to primaries only. A stop is replaced only with a reason,
  and that is allowed even when the assessment is stale.
- **Feasibility `n_cc` counts the model columns only** (outcome, time, status, exposure, term
  variables, cluster, strata). That is the compiled run's own complete-case filter. Only a strata or
  cluster field outside the route's schema (strata on lm / glm, either on rmst), which the run
  ignores, still drops rows in the engine (conservative). `mi$aux`, `by`, `vars` and `nonnormal` never
  drop a row (kind "none" still counts every spec column).
- **Clustering is all or nothing on lm / glm:** `layers$clustered = TRUE` or a `cluster` column needs
  `se = "cluster"`, and `se = "cluster"` needs both (`.sr_cluster_se_check`, `routes/se.R`), as cox
  does with `cluster()`. So the engine's design effect and "fewer than 2 clusters" blocker apply
  exactly when the run clusters. A clustered bootstrap therefore sets `se = "cluster"` too.
- **R45 now surfaces before the lock:** an `event_value` that codes no row (for example "yes" on a
  column the store holds as logical) is a "no events" blocker in the feasibility verdict, not a failure
  at the run.
- **Claude Code subagents and docs:** a hook can refuse a subagent's Write of a `.md` deliverable as a
  "report file"; write such docs from the main session.
- **Claude Code Bash tool and big heredocs:** a very large single heredoc can be rejected with
  "unexpected EOF while looking for matching quote". Write files in small `cat >> file <<'EOF'` chunks
  and keep commands such as `grep -P` out of the same call.
- **Regex backslashes in shell-edited R code:** `sed` replacement text and other shell-quoted strings
  can halve backslashes, so a test pattern `"stale \\(...\\)"` became `"stale \(...\)"`, an R parse
  error. Make such edits with the Edit / Write tool, or use `[(]` / `fixed = TRUE` in the pattern.
- **Full RED counts:** testthat stops after 10 failures. Run
  `TESTTHAT_MAX_FAILS=Inf Rscript tools/run_tests.R <filter>` to see them all.
- **Every lock now needs a routed project:** the SAP lock gate requires a decided study design and purpose.
  `.fx_feasible(p)` calls `.fx_routed(p)` first (a PICO, the design, the purpose, then the rules); pass
  `routed = FALSE` only for a test of the gate itself.
- **The router never applies a prediction (P1) and is a no-op after the lock:** `sr_router_run` returns the
  project unchanged once the SAP is locked, `pico.set` and `router.decided` are refused, and a prediction is
  only ever read to decide whether an override needs a reason. A backend failing a gate (mode, key, DPA ack,
  consent) is skipped with no event; only a missing payload approval is logged, as a failed prediction.
- **The shadow export keeps the last prediction per (key, backend) and refuses to overwrite;** it never holds
  the key (the sentinel `sk-test-SENTINEL-0000` must not appear in events, audit, errors or an export).
- **The lock freezes the router's view:** `sr_sap_lock` stores the current decisions in the `sap.locked`
  payload (`router$decisions`, not an input to the hash), and once locked every reader of "current" (choice,
  decision table, hash part, gate, writer, shadow) reads that snapshot, so roles or a DQ re-run after the
  lock cannot make a locked decision stale or move `sr_sap_hash`. A pre-P1d lock (no snapshot) keeps the
  live view. A test that edits a locked fixture's decisions must edit `state$sap$lock$router$decisions`.
- **`sr_ph_resolve(x, reg)` reads the registry before forcing `x`:** build the paragraph into a variable
  first; `sr_ph_resolve(.sr_wr_m_design(p, reg), reg)` fails with "unknown placeholder {{L1}}".
- **After a freeze, `sr_run` refuses a changed library** (`sr.cran_snapshot` changes the hash): tests that change the library after `sr_freeze` must `sr_rebaseline` first.
- **A `.srproj` is the project at the freeze:** `events.jsonl` and `audit.log` are cut after `freeze.created`, so later runs and exports are not in it.
- **R's internal tar** is the only archiver on the locked PC (`utils::zip` needs an external program); members are limited to 8 GiB and names to 255 bytes. `utils::tar` writes the local `uname` into each header; `.sr_srproj_scrub_owner` blanks it and rewrites the checksums. The history is not scrubbed: the `actor` of every event and audit entry is `sr_opt("user")`, which falls back to the OS user name, so set `sr.user` / `SR_USER` to a role or pseudonym before recording a project that will be shared; `sr_srproj_export` warns when the exported history holds the OS user name (tests set `sr.user = "sr-test"` in `helper-load.R`).
- **`utils:::untar2` reads names differently from GNU tar**: a GNU `L` long name is the whole padded block up to its last non-zero byte (not the declared size), name and prefix fields run to their last non-zero byte, and a long name passes over a folder entry to the next file. The scanner reads names exactly that way and refuses any case where the readings could differ, and import extracts only the scanner-approved files (`untar(files = ...)`). Parity tests must call `utils::untar(..., tar = "internal")`: the default may be the external `tar.exe`, which agrees with GNU, not with untar2. untar2 also blanks a whole `C:...` path (a bad drive regex), so an escape test uses an absolute path without a drive letter.
- **Tar scanner rules (`.sr_tar_entries` / `.sr_tar_check`):** only regular files and folders pass; links, devices and pax `x` / `g` headers are refused before extraction; names are checked for traversal, Windows device names, trailing dots, case-insensitive duplicates and 255 bytes; GNU `L` long names are followed, and a non-ustar header with a prefix field is ambiguous and refused.
- **Replay loses the names of `libsnap$pkgs`:** after an event replay the stored snapshot's `pkgs` is a list, not a named character vector (the `.sr_libsnap_json` quirk). The baseline gate therefore compares hashes only and never recomputes one from a stored snapshot; `.sr_libsnap_items` normalises for diffs.
- **Old manifests have an unnamed `libsnap$pkgs`:** a freeze written before the names fix cannot be compared package by package; `sr_freeze_diff` then compares R and the snapshot date and reports one `packages` row (`unknown`) when the hashes differ.
- **No absolute-time asserts:** a 5 s bound on `.sr_tar_check` of 50000 entries flaked under load (10.9 s). `.sr_tar_check` and `.sr_archive_safe_path` are now vectorised and the test checks scaling (time(40000) / max(time(10000), 0.05) < 8), which load does not break; write performance tests that way.


### P1d UI (Design + SAP) gotchas and rulings
- **Helpers.** `.sr_ui_*` (`app/R/ui/util.R`) hold the shared pieces: `.sr_ui_try`, `.sr_ui_actor` (the busy-guarded
  `act`), the DT wrapper, badges and the export lister. `.sr_data_try` stays as a wrapper of `.sr_ui_try`.
- **New route field.** Add its kind to `SR_FORM_KINDS` (`app/R/ui/spec_form.R`); the SAP spec form is built from it.
- **Mode.** The air-gap / cloud mode is read once at module start, so a mode change needs a new session. In
  air-gap mode there is no Ask Jev card.
- **Liveness.** Panels never cache project state; `project` is the reactiveVal and every panel re-reads it.
  Shiny keeps a modal's input values after the modal closes and resends only CHANGED values, so a re-opened
  modal can carry the last session's values. Rule: every modal field is reset or re-set on open from the row
  being edited; checked in the walk (edit a1, edit a2, then Add showed clean defaults).
  Stateful renderUI (forms holding typed input) use a value-compared flag (`.sr_sap_flag`, invalidates only when
  the derived value changes), never a bare renderUI on `project()`; do not let DT selections reset on a project change.
- **Test mocks.** In sourced app code `testthat::local_mocked_bindings(.env = globalenv())` errors; use the
  globalenv assign-and-restore helper `.ms_mock`.
- **Retry after a failed SAP document.** Core `sr_sap_docx` refuses a retry when `SAP_v1.docx` exists on disk
  without a `doc.exported` event; there is no UI recovery (delete the file by hand).
- **Environmental warning.** "shiny built under R 4.5.3" in the test output is environmental, not a defect.
- **Layout.** Data, Design and SAP are NOT in `fillable` (fillable squashed and overlapped the Design cards);
  the placeholder panels and Project stay fillable.
- **Open: stale lock checklist (not reproduced).** In the browser walk, after DQ runs and decisions made in the
  Data tab, the SAP lock checklist kept "no DQ run recorded for this table" (2 blocks) while the project on
  disk had no block; reopening the project refreshed it. The review could not reproduce it server-side: the
  real app under testServer updates the checklist and the Lock button count after a DQ run
  (`test-app-wiring.R`; every write goes through `.sr_ui_actor` -> `project(res)` and the panel observes
  `project()`). Open suspicion: a second browser session on the same project, or flush timing. It fails safe
  (blocks, never allows).
- **Checklist is lazy.** `chk` in `mod_sap.R` is a plain reactive, so `sr_sap_check` runs only when a visible SAP
  output reads it, not on every project change in another tab (the observe-based flag made Data-tab actions
  slow). testServer has no hidden tabs, so `test-app-wiring.R` pins the mechanism by source, not by call count.
- **Execution deviations (decided during the build).** Editing controls are disabled by re-rendering them with
  `disabled`, not with shinyjs (so testServer can assert it from output HTML). The shadow export uses
  `.sr_ui_try`, not `act`, because it writes a file and returns a path. `.sr_design_gates` repeats the four gate
  checks of `.sr_jev_available` so all four show at once; the core gate stays authoritative at send time.
- **Observed, not changed.** A failed Project "Create new" (directory not empty) closes the open project and
  hides Data / Design / SAP; "Open existing" recovers. The Lock dialog's Lock button was enabled before the
  word was typed (`sr_sap_lock` takes no word; the panel server checks `identical(word, "LOCK")`). Accepting a leakage `drop_var` DQ decision adds a recipe
  step, which the checklist shows as a warn until applied.

### P1d UI (Analyses) gotchas and rulings
- **No mirai daemons, so a killed worker is never reused.** `.sr_an_spawn` starts an ephemeral mirai per
  analysis (no `mirai::daemons()`), so Cancel can kill the worker and nothing is left to reuse or clean up.
  Do not set daemons for this panel: `ps_kill` on a persistent daemon would kill the daemon.
- **Kill order and the PID guard.** The worker writes `"<pid> <create time>"` to its job file (via
  `<job file>.part` + rename). `.sr_an_kill` refuses when the PID is gone or the create time differs by more
  than 0.01 s (a reused PID). It then suspends the worker, snapshots and kills the callr children, then
  kills the worker (the old child-first order is a race: the worker can start another child). On Windows
  `ps_kill` is TerminateProcess, with no grace. If anything survives it resumes the survivors and raises.
- **Cancel race semantics.** `run.completed` wins: a cancel that arrives after the fit finished is refused by
  `sr_run_cancel` as stale, writes no `run.cancelled` and says "finished before it could be cancelled". Cancel
  during the diagnostics step leaves the fit standing (its `run.completed` is already recorded) and loses
  only the diagnostics. A worker killed before `run.started` gives "Cancelled before <id> started.".
- **Interrupted is derived.** Status `running` with no live job in this session is shown as `interrupted`;
  there is no event for it. Run selected appends a new `run.started`.
- **`run.completed` now carries `job`** (and `run.started` carries `job` and `extract_sha`). Old projects
  without `run.started` replay unchanged.
- **Busy flags.** The busy hub in `app/app.R` holds one flag per job module (data, analyses, archive, import).
  `busy_for(except)` is TRUE when any flag other than `except` is set: Analyses gets `busy_for("analyses")` (so
  its own running state never blocks its Run buttons), Data `busy_for("data")`, and Design, SAP, Write and
  Project get `busy_for()`. The refusal text is `.sr_ui_busy_text`.
- **Lock handover.** Before each job the session closes DuckDB, then the project, and keeps a read-only copy;
  after it settles it re-opens (only the same folder, only over the read-only copy). A job that settles while
  another project is open settles on a temporary handle and never replaces the open project. The worker
  closes DuckDB before the project (the Windows file lock).
- **Tests.** The real-process tests (`test-analyses-job.R`, the Cancel tests in `test-mod-analyses.R`) are
  skipped unless `NOT_CRAN=true`. `sr_opt("run_sync")` makes the panel run `.sr_an_job` inline (tests). A killed
  worker rejects the ExtendedTask promise, so Shiny logs a harmless "connection reset" style warning.
- **Observed in the acceptance walk (not changed).** The bootstrap with B = 2000 on 400 rows fits in about 4 s, so a Cancel walk needs a much larger B (60000 took 96 s). While a run is going the project is released to the worker, so the SAP panel shows its read-only banner and disabled controls (the busy text appears only on Data actions). Card 3 lists a sensitivity analysis of a different measure (an RMST difference under a Cox HR) as `ok` with no estimate, because it shows only same-measure rows.
- **Known gap.** `.sr_data_spawn` still has the early-error gap `.sr_an_spawn` fixed (its temp files are not
  always unlinked when the worker fails before the job starts).

### P1d UI (Write, Archive, Import, Settings) state, deviations and gotchas
State: all four panels are built, documented and walked (Task 12 passed, one fix); the final review is still to do.

Acceptance walk (2026-11-11, port 7790, synthetic writer-fixture sample built from `.fx_writer_template()`): (1) project opened with SAP locked and analyses run; (2) Write gates green, Check ran (claim guard found nothing), Preview showed `<5` cells, Export gave export-001, the zip served (PK, 609 KB), Verify ok, one changed byte turned Verify red (`changed: ...analysis.R`) and restoring it turned it green; (3) FREEZE dialog froze submission-v1, quick verify ok, Verify with data cancelled once then ok, diff showed no differences, `.srproj` exported on auto (full); (4) import into another folder: verify ok, the imported freeze files and manifest hash are identical to the original (the import adds only srproj.json); (5) digits 3 changed the preview (1.70 to 1.703), a dummy key showed as set and never appeared in the page HTML, a wrong word was refused and CONNECT switched to connected, one Set back to airgap cleared the key, and after a restart every setting was at its default. Fix: the Write Preview button and its output shared the id `preview` (Shiny warns), the output is now `preview_body`, with a test. Notes: the app started from `tools/run_app.R` by hand because `preview_start` reads the launch file of the primary worktree; the browser pane typing is slow, so wait before reading a table; parent folders of an export or import destination must already exist.

Deviations from the spec (decided during the work):
- **D1.** The Write helpers are `.sr_wp_*` (in `write_view.R`), not `.sr_wr_*`: the writer core already owns `.sr_wr_`.
- **D2.** Import is its own module `mod_import_*` (`mod_import.R`) on the Project tab; `mod_project_server` still returns the project reactiveVal, not `list(project, busy)`.
- **D3.** `mod_settings_server` returns `list(version)`, a counter that goes up after each successful set or reset (Write, Archive and the badges watch it).
- **D4.** The options snapshot `.sr_job_opts()` (the `sr.<name>` options of the Settings catalogue that are set; always without `sr.jev_key` and the test-only options; the actor is `sr.user`) goes to every worker: the analyses worker, `.sr_data_spawn` and the Archive and Import jobs.

Gotchas:
- **A cancelled import after the worker wrote `srproj.imported` is kept.** `settle()` checks the last event of the target; it opens the project and says "Import already finished; kept". Only a killed or never-started worker gets the cleanup.
- **Export cleanup uses a keep list.** `.sr_ar_export_cleanup(..., keep)` never deletes a path that existed before the job (the archive, `.partial`, the staging folder), so a pre-existing `archive/.partial` survives a cancel.
- **Test option leaks.** An option set inside a `testServer` leaks into later test files; use `withr::local_options(..., .local_envir = <the test env>)` (a `test_that`-level frame), not a bare `options()` or a `defer` that clears the baseline (for example `sr.user`, `sr.cran_snapshot`).
- **A killed real mirai job leaves a rejected-task warning** (a harmless "connection reset" style warning in the console); the panel still settles.
- **`settle()` releases busy in `on.exit`.** Whatever fails while settling, the job env is emptied and `active(FALSE)` runs, so the app is never left blocked.
- **Settings are process-wide, not per tab (M6, documented, not re-architected).** A Set is `options(sr.<name>)` in the one R process, so every browser tab or Shiny session that process serves shares it and the last Set or Reset wins; nothing is on disk and a restart forgets it. `docs/settings.md` says so; a per-session store would need every `sr_opt()` caller to take a session context.

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
