# P1d UI, sub-project 2: Analyses panel (run and results) - design

## 1. Purpose and scope

P1d UI is split into three sub-projects (see `2026-11-08-p1d-ui-design-sap-design.md` section 1). Sub-project 1
(Design + SAP) is done. This spec covers sub-project 2, cut down with the user to **run and results**:

- running the analyses of a locked SAP (one, or all that are not yet ok) in a background worker, with
  progress, cancel and recovery after a crash;
- the diagnostics of each run, and applying them (rules fired, sensitivity analyses added, limitations);
- the results: per-analysis result and diagnostics, primary vs sensitivity, multiplicity, deviations and
  the run history.

Out of scope: post-lock exploratory and post hoc analyses (`sr_analysis_add` by hand; a later
sub-project), chunk checkpoint / resume inside a bootstrap or MI fit (a rerun starts from scratch),
within-fit progress percentages, parallel runs, and everything in sub-project 3 (Write + Archive +
Settings).

### Decisions made with the user

| Topic | Decision |
|---|---|
| Scope | Run + results only; post-lock analyses later. |
| Cancel | Kill the worker's whole process tree (the callr fit included) and record `run.cancelled`; a rerun starts from scratch. No checkpoint / resume. |
| Run model | Run selected, or Run all (the analyses not `ok`), sequentially in SAP order, one worker at a time. |
| Crash | `sr_run` records `run.started`; on reopen a start with no end shows as "interrupted" with Run again. |
| Diagnostics | Run chains the fit and its diagnostics; Apply is a separate button with a preview. |
| Results | Effects, route tables, figures, fragments, flags, failed-run error, library-changed warning, diagnostics; family-adjusted p-values; deviations; primary vs sensitivity side by side. |
| Architecture | Approach A: a mirai worker owns each step and calls the unchanged `sr_run` / `sr_diag_run`; queue and cancel live in the main session. |

## 2. Rules carried over from sub-project 1

- Models never compute or write numbers. The UI only formats (rounds) numbers taken from the result
  contract and the event state.
- Panels never cache project state; `events.jsonl` is authoritative and every card re-derives from the
  replayed project. The only session state about a run is the live job handle.
- A locked or read-only project, or a busy app, cannot be changed from any control: controls are
  rendered disabled and the server refuses as well. Refusals show the core message verbatim.
- Cards that hold user input or selection use value-compared flags (`.sr_ui_flag`), not a bare
  re-render on `project()`.
- No network calls. Options through `sr_opt`. UI helpers `.sr_ui_*`; panel helpers `.sr_an_*`.
  Generated `.R` files are ASCII only.

## 3. Core additions (`app/R/routes/run.R`, `app/R/sap/deviations.R`)

### 3.1 `run.started`

`sr_run` appends `run.started` after every refusal check has passed and immediately before
`sr_exec_cached`. Payload: `analysis`, `key`, `extract_sha`, `job` (a caller-supplied job id, new
argument `job = NULL`; when NULL a random id is generated). A refusal still writes nothing.

### 3.2 `sr_run_cancel(project, analysis_id, job, reason = "cancelled by the user")`

Appends `run.cancelled` (`analysis`, `job`, `reason`). Refused (nothing written) for a read-only project,
an unknown analysis, or when the analysis's latest run event is not a `run.started` with the same `job`
(a stale Cancel, or a run that already completed).

### 3.3 Reducer

`state$runs[[id]]$status` is one of `running` (latest event `run.started`), `ok` / `failed` (latest
`run.completed`, by `ok`), `cancelled` (latest `run.cancelled`). `ok_key` and `ok_extract_sha` keep
pointing at the last good run: starting, failing or cancelling never removes a previous good result, and
`sr_result` keeps working on it. `state$runs[[id]]$history` lists every attempt (job, started,
ended, outcome) for the run-history card. Existing projects without `run.started` replay unchanged
(status from `run.completed` alone).

"Interrupted" is not an event. It is derived when the panel reads the project: status `running` and no
live job in this session owns that `job`. Run again appends a new `run.started`, so the crashed attempt
stays in the log.

`sr_deviations` gains no kind: cancelled and interrupted attempts are run history, not SAP deviations.

### 3.4 `sr_diag_apply_plan(project, analysis_id)`

The planning half of `sr_diag_apply`, split out unchanged: a pure function returning a data.frame
(`type` = rule / add / limitation, `check`, `target` (rule id or new analysis id), `route`, `reason`).
Same refusals as `sr_diag_apply`, including the taken-id refusal. `sr_diag_apply` calls it and
executes exactly its rows, so the preview and the action cannot disagree.

## 4. The panel (`app/R/ui/mod_analyses.R`)

Wired into `app/app.R` after the SAP tab. Signature
`mod_analyses_server(id, project, busy)`; it also returns its own `running` reactive so `app.R` can
combine the app-wide busy flag as import OR run and pass it to Data, Design and SAP.

### 4.1 Card 1, Runs

A table, one row per analysis in SAP order (primary, secondary, sensitivity; a sensitivity analysis
indented under its parent): id, label, route, status (not run / running / ok / failed / cancelled /
interrupted), last run time, elapsed, cached, diagnostics summary ("2 ok, 1 warn, 1 fail").
Selection is held by analysis id.

Buttons: **Run selected** (re-runs an `ok` analysis too; the fits cache makes it fast and it still
records events), **Run all** (queues every analysis whose status is not `ok`), **Cancel**.
A progress line shows "Running 3 of 7: `cox_primary`, fitting (2 m 14 s)" then "..., diagnostics".
While a run is going a "running" badge shows on the Analyses navbar item.

Run is enabled only when the SAP is locked, the project is not read-only, the app is not busy and the
project is not stopped (`sr_project_stopped`). Each is re-checked on the server.

### 4.2 The queue and the worker

- The queue is a list of analysis ids in the main session. For each id the panel releases the project
  lock and starts a mirai `ExtendedTask` (spawner `.sr_an_spawn`, the same shape as `.sr_data_spawn`).
  The worker sources the app, opens the project, writes its PID to a job file, calls
  `sr_run(p, id, job = job)` and, when that run is `ok`, `sr_diag_run(p, id)`, then closes the
  project. The step ("fitting" / "diagnostics") is written to a progress file.
- After each analysis the panel re-opens the project (re-taking the lock) and moves to the next id.
- A failed fit (`ok = FALSE`) is recorded by `sr_run`; the queue continues. A sensitivity analysis
  whose parent failed still runs.
- A refusal (from `sr_run` or `sr_diag_run`, or a worker error outside them, or "locked by another
  session") stops the queue and is shown verbatim.
- **Cancel** kills the worker PID and all its descendants with `ps` (the callr fit included), waits
  for them to exit, re-takes the project lock, calls `sr_run_cancel` for the current job and drops the
  rest of the queue. When the run completed first, `sr_run_cancel` is refused for a stale job and the
  panel reports "finished before it could be cancelled" (a notice, not an error).
- The run keeps going while the user is on another tab. If the browser closes, the worker still
  finishes and records its events. If the R process dies, the daemons die with it, and the reopened
  project shows the analysis as interrupted.

### 4.3 Card 2, Result of the selected analysis

Everything is read through `sr_result()` and `project$state`.

- Header: status, elapsed, cached; a warning when the run's library snapshot differs from the lock's
  (`lib_changed`); a failed run's `error` text verbatim. A not-ok analysis with an earlier good run
  shows that good result, labelled with its time.
- Effects: primary rows first, then covariate and descriptive; reporting flags (`table2`,
  `non_collapsible`) inline on their rows.
- Route tables: each table in `presented$tables`.
- Figures: each entry of `presented$figures`, drawn from its plot data with the diagnostics plotting
  helpers (`app/R/diagnostics/plots.R`).
- Fragments: the route's prose with `{{E1}}`-style placeholders shown as tokens.
- Diagnostics: one row per check (verdict, value, warn and fail thresholds, detail), with a figure
  where the check carries plot data. Below them, the **Apply diagnostics** preview: each row of
  `sr_diag_apply_plan` in words ("fire rule `r1` (replace_primary -> `cox_primary_rmst`)", "add
  sensitivity `cox_primary_ph` (rmst)", "record limitation: ..."). The Apply button is gated like Run
  and calls `sr_diag_apply`; analyses it adds appear in the Runs table as not run. A plan with no rows
  says "nothing to apply".

### 4.4 Card 3, Primary vs sensitivity

One block per primary or secondary analysis: its primary-role estimate (measure, est, CI) and, for
each of its sensitivity analyses (pre-specified and unplanned), the rows of the same measure.
A rule-fired replacement is marked "now primary". A sensitivity analysis that has not run
successfully shows "not run" / its status, never a blank.

### 4.5 Card 4, Multiplicity

One table per family from `sr_family_adjust`. A family whose members have not all run successfully
shows "waiting for: a, b" instead of an error.

### 4.6 Card 5, Deviations and run history

The `sr_deviations` table, then the run history: every cancelled and interrupted attempt (analysis,
job, started, ended or "interrupted", reason).

## 5. Failure handling summary

| Condition | Behaviour |
|---|---|
| Cancel races completion | `run.completed` wins; `sr_run_cancel` refused as stale; notice shown. |
| Browser closed mid-run | Worker finishes and records; reopen shows the result. |
| R process / machine dies | Reopen shows interrupted, Run again offered. |
| Lock handover race | Worker reports "locked by another session"; shown as a refusal; queue stops. |
| `sr_run` refusal | Queue stops; core message verbatim. |
| Fit fails | Recorded `ok = FALSE`; queue continues; error shown in Card 2. |
| Worker error outside the core | Refusal with the message; queue stops. |

## 6. Scale

Fits already go through `sr_exec_cached` with the memory pre-check. Cards read only the event state and
small cached RDS files, so nothing in the panel scales with row count. No card re-runs `sr_sap_check`
or a DuckDB scan on a project change (sub-project 1 deferred minor M7).

## 7. Testing and Review Focus

- Core: reducer statuses and history; `run.started` written only after the refusal checks;
  `sr_run_cancel` refusals (read-only, unknown, stale job, completed run); old projects replay
  unchanged; `sr_diag_apply` executes exactly `sr_diag_apply_plan` on every check fixture.
- Panel (`testServer`, spawner replaced by a synchronous stub as in the `mod_data` tests): queue order
  and skipping `ok`; failed fit continues; refusal stops; Cancel drops the rest; gating for locked /
  read-only / busy / stopped; interrupted on reopen; Apply preview matches the action; the app-wide
  busy flag blocks Data, Design and SAP edits during a run.
- One real end-to-end test (skipped on CRAN): a long-sleeping job through mirai is cancelled and its
  callr child process no longer exists.

Review Focus (each pinned by a named test):

1. Cancel racing completion: the completed result stands and no `run.cancelled` is written.
2. A crashed run shows as interrupted on reopen and Run again works.
3. Cancel leaves no orphan callr process.
4. A refusal stops the queue and shows the core text verbatim.
5. A sensitivity analysis added by Apply appears in the Runs table and runs.

## 8. Acceptance

1. On a locked synthetic project, Run all fits every analysis and its diagnostics, and Card 2 shows the
   effects, tables, figures, fragments and verdicts of each.
2. Cancel during a bootstrap analysis stops it within seconds, records `run.cancelled`, leaves no
   worker or callr process, and a rerun succeeds.
3. Killing the R session mid-run and reopening shows the analysis as interrupted; Run again completes.
4. Apply on a failing check adds the sensitivity analysis (or fires the rule), and running it fills the
   primary vs sensitivity card.
5. Family-adjusted p-values and the deviations list match `sr_family_adjust` and `sr_deviations`.
6. During a run no Data, Design or SAP control changes the project.
