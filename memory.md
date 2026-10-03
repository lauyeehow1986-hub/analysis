# memory.md — StatRouter project memory

This file records status, decisions and gotchas, with short dated entries. The **spec** holds the
design; this file holds *what has happened* and *what we learned*.

## Status
| Date | Milestone |
|---|---|
| 2026-10-03 | Design approved after 4 rounds of critique and research; spec committed (`docs/specs/2026-10-03-statrouter-design.md`) |
| 2026-10-03 | Repo initialised and pushed to github.com/lauyeehow1986-hub/analysis (public) |

**Next:**
1. User reviews the spec.
2. Run `writing-plans` for **P0** (feasibility spike on the locked-down PC) and **P1** (MVP).
3. Create a worktree and implement.

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
- **Bundle dependency lists** must include the full transitive closure, or packages fail on the
  target machine.
- **TLS on the dev laptop:** AVG Web-Shield can intercept HTTPS (MITM), and `uv` needs
  `--system-certs`.

## Reuse sources
- **structured_deidentification:** `project.R`, `hashchain.R`, `engine_py.R`, `run_engine.py`,
  `sdc*.R`, `report.R`, `tools/*.ps1`.
- **AndyFishing:** the `run.bat` pattern and `stage_ollama.ps1`.
- **sts_reverse_engineer:** the `sts_coefficients.json` + `sts_model.py/.R` pattern for spec-driven
  predictors.
- **elso:** the REDCap ODM XML parser.
