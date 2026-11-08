# P1d UI, sub-project 1: Design and SAP panels - design

**Status:** approved in conversation 2026-11-08, written for review.
**Parent spec:** [docs/specs/2026-10-03-statrouter-design.md](../../specs/2026-10-03-statrouter-design.md)
(sections Router, Archive / SAP lock, Phases P1).
**Core guides it drives:** [docs/router.md](../../router.md), [docs/analysis.md](../../analysis.md),
[docs/writer.md](../../writer.md).

## 1. Purpose and scope

P1d UI is the last piece of the P1 MVP: after it, a user goes from a raw file to a locked SAP, a run,
a Word manuscript and a frozen `.srproj` inside the app, in Expert mode. The core behind every panel is
built and tested; the UI is not. P1d UI is split into three sub-projects, each with its own spec,
plan and branch, built in this order:

1. **Design + SAP** (this spec): the research question, the router decision table with the Jev payload
   preview and consent, the SAP analysis builder, feasibility, multiplicity families and rules, the lock
   checklist, the lock, and the SAP and feasibility documents.
2. **Analyses**: running locked analyses (ExtendedTask + mirai, chunked checkpoint, cancel and parallel
   workers for the bootstrap and MI), post-lock exploratory and post hoc analyses, results and
   diagnostics.
3. **Write + Archive + Settings**: the manuscript export, freeze / export / import / re-baseline, and the
   mode, profile and router-backend settings.

Out of scope here: everything in sub-projects 2 and 3, Guided mode (P4), and Reviewer mode (P2; its tab
stays a placeholder). Two items `memory.md` assigned to P1d are already done in the writer and need no
UI work: the claim-guard rule that an exploratory analysis needs hypothesis-generating language (G7) and
the feasibility Methods sentence.

### Decisions made with the user

| Topic | Decision |
|---|---|
| Split | Three sub-projects as above; Design + SAP first. |
| Analysis builder | A structured form generated from each route's `spec_schema`, prefilled from the router's decided choices and the confirmed roles; nothing is saved until the user saves. |
| Jev approval | One batch dialog listing every pending payload with its SHA and a mask toggle; the user ticks which to send. The DPA/IT acknowledgement and session consent are one-time dialogs shown when first needed. |
| Structure | Two modules (`mod_design`, `mod_sap`), a pure form builder (`ui/spec_form.R`) and a shared `act()` helper (`ui/util.R`) moved out of `mod_data`. |
| Profile tab | Removed: the Data panel already shows roles, DQ checks, suggestions, the recipe, the flow and readiness. |
| Lock | A dialog that needs the typed word `LOCK`; locking writes `SAP_v1.docx` at once. |
| Files | Documents are written into the project's `out/` folder; the UI shows the path with Open folder and Download buttons. |

## 2. Components

```
app/app.R               Design and SAP tabs replace their placeholders; the Profile tab is removed
app/R/ui/util.R         shared: the act() factory, card and table helpers, the read-only banner
app/R/ui/spec_form.R    pure: route schema + field-kind map -> form description; inputs -> spec
app/R/ui/mod_design.R   cards: research question, decision table, Ask Jev, shadow summary
app/R/ui/mod_sap.R      cards: analyses, feasibility, families and rules, lock checklist, lock, documents
app/R/ui/mod_data.R     uses ui/util.R; returns its import-busy reactive
app/global.R            loads ui/util.R and ui/spec_form.R before the modules
```

Both new tabs are hidden until a project is open, like the Data tab.

### 2.1 `ui/util.R`

- `.sr_ui_actor(project, busy = function() FALSE)` returns `act(label, fn)`, which is the current
  `mod_data` `act()` lifted out unchanged in behaviour: it refuses with a notification when no project is
  open, when `busy()` is TRUE ("wait for the import to finish") or when the project is read-only; it runs
  `fn(project)`, reports core errors and warnings as notifications, and writes a returned project back to
  the shared reactive. It returns the result invisibly so dialogs can show a refusal in place.
- `mod_data_server` returns a list holding its import-busy reactive; `app.R` passes it to `mod_design`
  and `mod_sap`, so no panel edits the project while an import runs.
- The card helper (`.sr_data_card`) and the DT helper (`.sr_data_dt`) move here under `.sr_ui_` names.
- `.sr_ui_readonly_banner(project)` gives one banner explaining why editing is off: read-only project
  (locked by another session) or locked SAP.

### 2.2 `ui/spec_form.R` (no Shiny server code)

Route schemas are shallow (`chr`, `num`, `lgl`, `list`, optional `values` and `default`); they do not say
that a string is a column or what shape `terms` has. The builder pairs each schema with a field-kind map:

| Kind | Fields | Input |
|---|---|---|
| `column` | `outcome`, `exposure`, `cluster`, `time`, `status`, `by`, `strata` (and any single-column field of a P1 route) | select over the table's columns |
| `columns` | `vars`, `nonnormal` | multi-select over the table's columns |
| `terms` | `terms` | editable rows: column, type (`linear`, `factor`, `rcs`, `quad`), knots (rcs only: empty, 3-7, or 3-7 locations) |
| `interactions` | `interactions` | pairs of variables, offered only among linear and factor terms and the exposure |
| `layers` | `layers` | the six fixed selects of docs/analysis.md section 2; values P1 cannot run (weighting, matching, Bayesian) are listed but disabled with the reason |
| `enum` / `number` / `logical` | other `chr` with `values`, `num`, `lgl` | select, numeric input, checkbox; schema default applied |
| `hidden` | `exclude_rows` | never shown (set only by the influence sensitivity) |

The exact kind of every field of every registered route (including route-specific fields such as event
values, ties, tau, marginal-effect options, MI and bootstrap settings) is fixed in the plan from the
route files; a field with no kind makes the builder refuse that route with "the form cannot build field
'<f>' of route '<r>' yet". It never drops a field silently.

Functions:

- `sr_spec_form(route_id, table, project)` returns a form description: one entry per field with kind,
  label, choices, default, the prefilled value and its source (`"router"`, `"roles"`, `"default"` or
  `NA`). Fixed: `purpose` is the route's own and not editable.
- `sr_spec_collect(route_id, values)` turns the form values into a spec list (dropping empty optional
  fields) without validating it; validation stays in the core (`sr_spec_validate`, `sr_sap_add`).
- Prefill order: a decided router choice (purpose, outcome, exposure, primary route), then the confirmed
  role of the table (`sr_roles`), then the schema default. Stale or undecided router choices are not used.

## 3. Design panel (`mod_design`)

**Card 1, Research question.** The eight PICO fields: `population` and `outcome` required, `aim` a select
(`describe`, `predict`, `explain-causal`), the others free text up to 2000 characters. Save calls
`sr_pico_set` once. After the lock the fields show the locked values, read-only.

**Card 2, Decision table.** The `sr_router_table` view, one row per instance, with status badges
(`decided`, `stale`, `pending`, `waiting`, `override_required`).

- *Run rules* calls `sr_router_run(project)` with no backends: rules decide what they can, and no network
  code runs.
- Selecting a row opens an editor with the node's options, the suggestion, any model prediction and its
  confidence (labelled "shadow only, not applied") and a reason box. Save calls `sr_router_decide`.
  Choosing other than the suggestion is an override and needs a reason; `override_required` rows always
  need one.
- Stale rows keep their old choice with a "decide again" prompt; `waiting` rows name the upstream decision
  they need.
- After the lock the table shows the lock snapshot (docs/router.md section 9), read-only.

**Card 3, Ask Jev.** Rendered only when `sr_mode()` is `"connected"`; in air-gap mode the card and its
server observers do not exist. A status line shows gates 1-4 (mode, key, DPA/IT acknowledgement, session
consent), each ticked or with its closed message.

- *Acknowledge*: a one-time dialog per project with a reason box; calls `sr_router_jev_ack`.
- *Consent*: a dialog the first time in this R session; calls `sr_router_consent(project, TRUE)`; a
  *Withdraw consent* button calls it with `FALSE`.
- *Ask Jev*: one dialog lists every pending question's exact payload from `sr_router_preview` with its
  SHA, and one column-name mask toggle that rebuilds the payloads. The user ticks payloads; *Send* calls
  `sr_router_run(project, backends = "jev", approve = <ticked SHAs>)`. Unticked payloads are not sent; a
  payload that changed after preview fails gate 5 and is logged as a failed prediction by the core.

**Card 4, Shadow summary.** The read-only `sr_router_shadow_summary` table and an *Export* button that
writes the golden-set JSON lines with `sr_router_shadow_export` into the project's `out/` folder (the core
refuses to overwrite).

## 4. SAP panel (`mod_sap`)

**Card 1, Analyses.** A table of SAP analyses (id, label, route, table, purpose, feasibility status) with
*Add*, *Edit* and *Remove*.

- The builder dialog takes id (`^[a-z][a-z0-9_]{0,39}$`), label (`primary`, `secondary`, `sensitivity`),
  table, route, and parent (sensitivity only; existing analyses), then the spec form of section 2.2.
  Each prefilled field shows its source. Changing the table or route rebuilds the form.
- *Save* calls `sr_sap_add`; a refusal is shown in the dialog in full, with the inputs kept.
- *Remove* asks for confirmation and calls `sr_sap_remove` (the core refuses a stopped analysis).

**Card 2, Feasibility.** One row per primary analysis from `sr_feasibility_report`: verdict, stale flag,
decision.

- *Assess* opens a form: the minimum important difference (value, a measure select limited to the
  measures the analysis reports, source), and the assumptions that apply: `r2_y` for a continuous outcome
  and `icc` for a clustered analysis (required there), each with a justification. Calls
  `sr_feasibility_assess`; refusals show in the dialog.
- *Decide*: `proceed`, `exploratory`, `revise` or `stop`, with a reason; `stop` asks for confirmation
  because it blocks every run. Calls `sr_feasibility_decide`.
- *Feasibility report* writes the docx with `sr_feasibility_docx` (card 6 shows it).

**Card 3, Families and rules.**

- Family: id, a multi-select of analyses, an adjustment method (`holm`, `hochberg`, `hommel`,
  `bonferroni`, `BH`, `BY`, `none`); calls `sr_sap_family`.
- Rule: id, analysis, check, action (`add_sensitivity`, `replace_primary`), and an alternative offered
  only from the sensitivity analyses whose parent is that analysis; calls `sr_sap_rule`.

**Card 4, Lock checklist.** The live `sr_sap_check` table (item, status, detail) coloured by status,
re-read after every change. The number of blocks shows on the Lock button.

**Card 5, Lock.** The button is disabled while any block remains. The dialog lists the open warnings,
the analyses and the library snapshot hash, and needs the typed word `LOCK` (exact, case-sensitive).
Confirm calls `sr_sap_lock`, then `sr_sap_docx` to write `SAP_v1.docx`. If the docx step fails, the lock
stands (it is an event) and the panel shows the error with a *Write SAP_v1.docx* retry button. After the
lock every editing control on the panel is disabled and the panel shows the lock time and SAP hash.

**Card 6, Documents.** Buttons for `SAP_draft.docx` (before the lock) and the feasibility report, each
showing its path under `out/` with *Open folder* (local `shell.exec` / `utils::browseURL` on the folder)
and *Download* (`downloadHandler` serving the written file). After the lock, `SAP_v1.docx` appears here;
it is never re-exported (enforced by the core).

## 5. State, errors and safety

- **One source of state.** The project returned by the core is the only state; every edit goes through
  `act()`, and every view re-renders from the replayed project. No panel caches decisions, analyses,
  feasibility or the lock. Reopening a project or recovering from a crash shows what `events.jsonl` says.
- **Errors.** `act()` shows core error messages verbatim. Refusals raised by a dialog's action show inside
  that dialog with the inputs kept. Each core call appends one event or none, so an error never leaves a
  half-saved state.
- **Read-only.** A read-only project and a locked SAP disable every editing control and show one banner.
  The core refuses these edits as well, so safety never rests on disabled buttons.
- **Air-gap.** In air-gap mode the Jev card is not rendered and no server code calls a Jev function. A
  test runs `mod_design` in air-gap mode with a `jev_transport` that fails the test if called.
- **Import running.** While a Data-panel import runs, Design and SAP edits are refused through the shared
  busy reactive.
- **ASCII.** All new `.R` files are ASCII-only; non-ASCII UI text (such as the middle dot) is built from
  code points, as `app.R` already does.

## 6. Testing

- **Pure (`test-ui-spec-form.R`):** for every registered route, the form builds; a valid fill collected
  by `sr_spec_collect` passes `sr_spec_validate`; a route with an unknown field is refused; prefill picks
  a decided router choice over a role over a default and ignores stale choices; `exclude_rows` never
  appears.
- **`testServer` (`test-mod-design.R`, `test-mod-sap.R`)** on synthetic projects built by the test
  helpers from `samples/` data:
  - PICO save; *Run rules*; decide and override (no reason refused); stale rows shown after an upstream
    change;
  - the Jev batch flow through the `jev_transport` test hook: only ticked SHAs are sent; the air-gap test
    above;
  - the SAP flow add -> assess -> decide -> family -> rule -> check -> lock; a wrong typed word does not
    lock; after the lock `SAP_v1.docx` exists and every edit is refused;
  - edits refused while busy and on a read-only project.
- **Regression:** the existing `test-mod-data.R` and `test-mod-project.R` pass unchanged after the
  `act()` move.
- **Manual check** at the end: the app runs in the in-app browser and one synthetic project is walked from
  PICO to lock.

## 7. Review focus

1. A route whose schema gains a field the form does not know is refused, never built without it.
2. Nothing reaches the network in air-gap mode, and in connected mode only ticked payloads are sent.
3. A locked SAP and a read-only project cannot be edited from any control on either panel.
4. A refused save keeps the user's inputs and shows the core's message.
5. A failed `SAP_v1.docx` write after a successful lock is reported and can be retried.
