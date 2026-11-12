# StatRouter roadmap

The design is [specs/2026-10-03-statrouter-design.md](specs/2026-10-03-statrouter-design.md). Running
state and decisions are in [../memory.md](../memory.md). This page lists phase status only.

| Phase | Status |
|---|---|
| P0 feasibility spike | done |
| P1a foundation | done |
| P1b data layer | done |
| P1c analysis, P1c.1 feasibility and cluster SE | done |
| P1d writer, router, freeze, UI (Design + SAP, Analyses, Write + Archive + Import + Settings) | done and merged (UI 3: PR 10, PR 11) |
| P1e.1 streaming regression and SQL Table 1 (lm, logistic, modpois, table1) | done and merged (PR 12). Guide [scale.md](scale.md); evidence in [validation.md](validation.md) section 4. 20M-row smoke row for the laptop profile: pending (laptop) |
| P1e.2 streaming Cox and RMST (piecewise-exponential) | next |
| P1e.3 MI, bootstrap (bag of little bootstraps), Firth and diagnostics at scale | next, after P1e.2 |

Later phases (P2 onward) are in the design spec.
