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

**Next:**
1. User reviews the plans and picks the execution method.
2. Execute P0 spike on the laptop, then the user runs `run_spike.bat` on the locked PC; record the go/no-go.
3. ~~Execute P1a in a worktree (branch `p1a-foundation`)~~ done 2026-10-03; review and merge the branch.
4. Next: write the plans for P1b (data: import/DuckDB/profiling/DQ/suggestor), P1c (analysis routes) and P1d (router + writer + freeze + UI), then execute them in that order.

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
