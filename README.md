# StatRouter

Portable, air-gap-capable R Shiny application for healthcare data analysis. A pluggable
System One router (rules, then local or cloud typed-judgment models, then the user) selects
pre-specified, assumption-checked analyses. It produces manuscript-ready Word output
(Methods, Results, tables, figures, checklist, reproducible `analysis.R`), archives the
project for reviewer responses, and builds interactive risk calculators with a developer
handover package.

Status: **design stage.** See [docs/specs/2026-10-03-statrouter-design.md](docs/specs/2026-10-03-statrouter-design.md).

Research and governance tool. Outputs require review by a qualified statistician; calculators
are research-use only unless separately validated and regulated. Never commit real patient
data — only synthetic samples.
