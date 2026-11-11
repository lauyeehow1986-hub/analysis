# P1d UI, sub-project 3: Write, Archive and Settings panels - design

## 1. Purpose and scope

P1d UI is split into three sub-projects (see `2026-11-08-p1d-ui-design-sap-design.md` section 1). Sub-projects 1
(Design + SAP) and 2 (Analyses) are done. This spec covers sub-project 3, which puts the finished writer and
archive cores behind panels and adds a Settings panel:

- **Write**: options, live gates, a manuscript check without writing files, a read-only preview, the STROBE
  checklist, export, the export history with download and per-export verify.
- **Archive**: freeze, the freezes list with verify, diffs, `.srproj` export and re-baseline.
- **Project tab**: `.srproj` import.
- **Settings**: every user-facing option, edited for this session only.

With this sub-project the P1 MVP exit item 7 ("freeze -> .srproj -> reimport on another path with an identical
hash") can be walked in the app.

Out of scope: the Reviewer panel (stays a placeholder), journal templates beyond the existing presets (all
`verified = FALSE`), writing settings to disk, a file browser (paths are typed), editing a manuscript in the
app, and post-lock exploratory analyses.

### Decisions made with the user

| Topic | Decision |
|---|---|
| Scope | Write + Archive + Settings in one sub-project. |
| Write depth | Options, gates, Check (guard findings, no files), preview of Methods/Results and the main table, export, history, per-export Verify, STROBE checklist. |
| Download | Per manuscript export: Download .zip and Open folder. SAP and feasibility exports: a single-file .docx download. |
| Import | On the Project tab; runs in the background; opens the project on success. The Write and Archive tabs stay hidden until a project is open. |
| `.srproj` export | Saved to a typed path (ends in `.srproj`, must not exist); choose the freeze (latest by default) and data auto / full / lean; the panel shows what auto picks and the size. |
| Freeze | Gate checklist with live reasons; label prefilled and editable; a dialog that needs the typed word `FREEZE`. |
| Slow operations | Verify with data, `.srproj` export and `.srproj` import run in a background worker with Cancel. Freeze, diff, re-baseline and quick verify stay synchronous. |
| Settings | Edit all user-facing settings, mode and Jev key included; session only (R options), reset on restart; nothing written to disk; the key is never saved or shown. |
| Architecture | Approach 1: extract the shared job layer from `ui/analyses_job.R` into `ui/job.R`; Analyses keeps its run settle logic and behaves exactly as before. |

## 2. Rules carried over

- Models never compute or write numbers. The panels only format numbers taken from core results; the preview
  renders the writer's model blocks and never builds prose of its own.
- `events.jsonl` is authoritative. Panels re-derive from the replayed project; the only session state is a live
  job handle, the Write model cache (section 4.3) and the Settings panel's own edits.
- A read-only project or a busy app cannot be changed from any control: controls are rendered disabled and the
  server refuses as well. Refusals show the core message verbatim.
- Cards that hold user input use value-compared flags (`.sr_ui_flag`), not a bare re-render on `project()`.
- No network calls. Options through `sr_opt`. Panel helpers: `.sr_wr_*` (Write), `.sr_ar_*` (Archive),
  `.sr_set_*` (Settings), `.sr_job_*` (shared job layer). Generated `.R` files are ASCII only.

## 3. Files

```
app/R/ui/job.R            shared background job: spawn, job file, cancel file, tree kill, read, options snapshot
app/R/ui/analyses_job.R   keeps the run settle; calls ui/job.R (behaviour unchanged)
app/R/ui/write_view.R     pure: gates, guard table, preview blocks, history rows, verify tables, checklist rows
app/R/ui/mod_write.R      cards: Options + gates, Check, Preview, Checklist, Export + history
app/R/ui/archive_view.R   pure: freeze gates, freezes table, diff and results-diff tables, re-baseline diff
app/R/ui/mod_archive.R    cards: Freeze, Freezes, Diff, Export .srproj, Re-baseline
app/R/ui/mod_project.R    + Import .srproj card; returns list(project, busy)
app/R/ui/settings_view.R  pure: the settings catalogue, each setting's effective value and source, validation
app/R/ui/mod_settings.R   the Settings table and editors
app/R/writer/export.R     + sr_export_verify(), sr_export_zip()
app/R/core/util.R         + sr_opt_source()
app/app.R                 Write / Archive / Settings tabs; app-wide busy; reactive badges
```

## 4. Core additions

### 4.1 `sr_export_verify(project, id)` (`writer/export.R`)

Verifies one export recorded in `state$exports`. Refused for an unknown id.

- **Files.** For each file in the `doc.exported` event, re-hash it on disk and compare with the event's SHA-256
  (the event is authoritative; `manifest.json` is checked too, and a disagreement between the two is a problem).
  Status per file: `ok`, `changed`, `missing`. A file on disk in the export folder that is in neither list is
  `extra`.
- **Numbers** (manuscript only). Rebuild the registry from `placeholders.json` with
  `sr_ph_registry(k = manifest$suppress_k)` (rows as in `test-writer-export.R`), then run
  `sr_ph_verify(reg, project)`.
- Returns `list(ok_files, files = data.frame(path, expected, got, status), numbers = <sr_ph_verify frame or NULL>,
  numbers_ok)`. `ok_files` is FALSE on any non-`ok` file. `numbers_ok` is FALSE when any number differs from the
  project now (for example after a re-run); that is not file corruption and is reported separately.

### 4.2 `sr_export_zip(project, id, dest)` (`writer/export.R`)

Writes a zip of the export folder to `dest` with the `zip` package (already a dependency). Entries are the
files listed in the event plus `manifest.json`, under a top folder named after the id. Refused for an unknown
id, for a SAP or feasibility export (those download as their single file), and when any listed file is missing.

### 4.3 `sr_opt_source(name, default)` (`core/util.R`)

Returns where `sr_opt(name, default)` takes its value from: `"option"`, `"env"` or `"default"`. Settings uses it
to show each setting's source.

## 5. The shared job layer (`ui/job.R`)

Extracted from `ui/analyses_job.R` with no change in behaviour:

- `ExtendedTask` + an ephemeral mirai worker (no `daemons()`); the worker sources the app.
- Job file `"<pid> <create_time>"` written atomically through a `.part` file; a cancel file.
- Cancel: suspend-first kill of the worker's whole process tree with a PID-reuse guard; survivors are resumed
  before an error is raised.
- **New: options snapshot.** Workers are fresh processes and do not inherit `options()`. The spawner passes the
  session's effective `sr.*` settings (section 8's catalogue) to the worker, which sets them before it opens the
  project, so an actor name or digits edited in Settings applies to work done in the worker. `jev_key` is never
  in the snapshot: no worker calls Jev.

One job runs at a time across the app: an analysis run, a data import, a `.srproj` import or an archive job. The
app-wide busy flag is the OR of all of them, and every panel refuses changes with the shared busy text, extended
with "an archive operation" and "a project import".

A job that writes to the project (`.srproj` export) releases the lock, keeps a read-only copy as `project()` while
it runs, and re-opens the project when it ends (the Analyses pattern). Verify with data opens the project
read-only in the worker and does not release the lock.

The synchronous test switch `sr.run_sync` also covers the archive and import jobs.

## 6. Write panel (`mod_write.R`)

Shown once a project is open. `mod_write_server(id, project, busy)`.

### 6.1 Options and gates

- Options: preset (from the writer's presets, each marked "not verified"), `suppress_k` (5 or more), Table 2
  on/off, alpha. They go through `sr_writer_opts()`; an invalid value shows the core message beside the field and
  disables Check, Preview and Export.
- Gates, read live from the project: project writable, SAP locked, every pre-specified analysis has an ok run,
  app not busy. Each failing gate shows its reason. When `sr_doc_model` refuses for a reason the gates did not
  predict, its message is shown verbatim.

### 6.2 Check, preview, checklist

- **Check manuscript** builds `sr_doc_model(project, opts)` and `sr_claim_guard(model)` and shows the findings
  table, block rows first (rule, block, analysis, detail). No file is written.
- The model is cached in the session keyed on (options, replayed event count). Preview and Checklist reuse it.
  Any project change or option change drops the cache.
- **Preview**: read-only HTML of the main and supplement blocks: headings, paragraphs with placeholders resolved
  through `sr_ph_resolve`, the main table, figure captions with thumbnails. Suppressed cells show as `<5`
  (the threshold in use), exactly as the docx renders them.
- **Checklist**: `sr_checklist_strobe(model, project)` as a table (item, section, label, status, detail). A
  CONSORT project shows the core's "complete by hand" note.

### 6.3 Export and history

- **Export** calls `sr_export(project, opts)` synchronously with a spinner. A refusal (read-only, model refusal,
  guard block) is shown verbatim and nothing is written.
- **History**: one row per entry of `state$exports`: kind, id, created, preset, file count.
  - Manuscript rows: **Download .zip** (`downloadHandler` calling `sr_export_zip` into Shiny's temp file),
    **Open folder** (opens the export folder in the OS file manager: `shell.exec` on Windows, `open` on macOS,
    `xdg-open` elsewhere; a local call, not a network call), **Verify**.
  - SAP and feasibility rows: a single-file `.docx` download.
  - A row whose folder or file is missing on disk shows "missing on disk" and disables its download and Open.
- **Verify** shows `sr_export_verify`: a red problem line for any `changed` / `missing` / `extra` file; a
  separate line "numbers differ from the project now" with the differing rows when `numbers_ok` is FALSE.

## 7. Archive panel (`mod_archive.R`) and import

Shown once a project is open. `mod_archive_server(id, project, busy)`; returns its own `running` reactive.

### 7.1 Freeze

- Gate checklist mirroring the refusals of `.sr_freeze_gate` (read-only, unlocked SAP, unhashed source, a
  pre-specified analysis without an ok run, a missing stored result, a project that does not verify).
- Label prefilled from `sr_freeze_next_label()`, editable, validated against `SR_FREEZE_LABEL_RE` and against
  labels already used.
- Freeze opens a dialog that only enables its button when the typed text is exactly `FREEZE`, then calls
  `sr_freeze(project, label)` synchronously. A refusal is shown verbatim.

### 7.2 Freezes and verify

- Table from `sr_freezes()`: label, created, events seq, libsnap hash (short).
- **Quick verify**: `sr_freeze_verify(project, label, data = FALSE)`, synchronous.
- **Verify with data**: `sr_freeze_verify(project, label, data = TRUE)` as a background job with Cancel.
- Result: ok or the problems list; `absent` data files are listed as information, not as problems.

### 7.3 Diff

Pick `from` (a freeze) and `to` (a freeze, or "project now"). Show the `sr_freeze_diff` sections (tables,
library, SAP, analyses) and, for "project now", `sr_freeze_results_diff(project, from)` (status, n values,
max abs diff, max rel diff). Numbers come from the core frames and are rounded for display only.

### 7.4 Export `.srproj`

- Inputs: freeze (latest by default), data auto / full / lean, destination path.
- Checked before the job starts: the path ends in `.srproj`, does not exist, and its parent folder exists. For
  auto the panel shows which mode it will pick and the total data size, from the same rule as the core
  (`sr.freeze_full_max_bytes`, nothing absent).
- When the actor would be the OS user name (`sr.user` unset), the core's warning is shown before the export
  starts, with a link to Settings.
- Runs as a background job with Cancel. On cancel or worker death: if the event count did not grow by an
  `srproj.exported`, remove `<path>`, `<path>.partial` and `out/.srproj-<label>.partial`; if it did, report
  "finished before it could be cancelled".

### 7.5 Re-baseline

Shows `sr_rebaseline_diff(project)`. A required reason text goes to `sr_rebaseline(project, reason)`. Disabled
when there is no freeze or the library already matches; a refusal is shown verbatim.

### 7.6 Import `.srproj` (Project tab)

- Inputs: archive path, target folder (must not exist, or be empty). No project needs to be open.
- Runs as a background job calling `sr_srproj_import(archive, dir, verify_data = TRUE)`.
- Success: the open project (if any) is closed, the imported project becomes `project()`, and the `$import`
  summary is shown (label, data mode, archive SHA, parts and sources missing, verify result).
- Cancel or failure: remove `<dir>`, `<dir>.partial` and `<dir>.partial.src`, but only those the job created
  (none of them existed when it started). The previously open project stays open.
- `mod_project_server(id, busy)` now returns `list(project, busy)`; `app.R` is updated.

## 8. Settings panel (`mod_settings.R`)

Always visible. `mod_settings_server(id, busy)`.

### 8.1 Catalogue

| Setting | Editor | Validation |
|---|---|---|
| `mode` | airgap / connected | `sr_mode()` |
| `profile` | auto / locked16 / laptop | `sr_profile()` |
| `user` (actor) | text | non-empty to set; empty = reset |
| `digits` | integer | integer, 0 or more |
| `router_mask` | on / off | logical |
| `jev_key` | masked input | non-empty |
| `jev_url`, `jev_model` | text | non-empty |
| `jev_timeout` | number | positive |
| `freeze_full_max_bytes`, `srproj_max_total` | number (bytes) | finite, 0 or more |
| `cran_snapshot` | date | a valid `YYYY-MM-DD` date; empty = reset |

Not shown: the test switches `run_sync`, `ingest_sync` and `jev_transport`.

### 8.2 Behaviour

- One table: setting, effective value, source (edited here / option / env / default / auto-detected for profile).
- An edit calls `options(sr.<name> = value)` after validation; **Reset** sets the option back to `NULL` so env or
  default applies. Nothing is written to disk; a restart starts from env and defaults.
- Settings are not project events: they do not change the project, and the audit log never records them.
- **Mode**: entering `connected` needs the typed word `CONNECT`. Back to `airgap` is one click and also clears the
  session's Jev key. Ask Jev keeps its own gates (key, per-project DPA ack, per-session consent).
- **Jev key**: held only as `options(sr.jev_key)`. Never rendered back, logged, put in events, audit, exports or
  the options snapshot, never written to a file. The table shows "set" or "not set".
- **Actor**: applies to events written from then on. When empty, the OS-user fallback warning shows.
- **Profile**: changing it changes the compute fingerprint (`sr_hw_fingerprint`), so later runs refit rather than
  reuse cached fits; the editor says so.
- **CRAN snapshot date**: it enters the library snapshot hash (`sr_libsnap`), so changing it makes later runs show
  "library changed" against the lock and can call for a re-baseline after a freeze; the editor says so.
- All settings are read-only while any job runs (workers already hold their snapshot).
- The top-bar badges (`profile . mode`, "no network") re-render reactively after an edit.

## 9. Failure handling summary

| Condition | Behaviour |
|---|---|
| Core refusal (export, freeze, re-baseline, srproj, import) | Shown verbatim; nothing written. |
| A job is running | Every other change control is disabled and refused with the shared busy text. |
| Cancel an `.srproj` export | Partial archive and staging folder removed; no event; or "finished before it could be cancelled". |
| Cancel or fail an import | Only folders the job created are removed; the previous project stays open. |
| Worker dies | Same cleanup as cancel; the project is re-opened from disk. |
| Export folder deleted by hand | History row shows "missing on disk"; Verify reports `missing` files. |
| Invalid option value | Core validation message beside the field; nothing set. |

## 10. Testing and Review Focus

- **Core**: `sr_export_verify` on a clean export (all ok), one changed byte (`changed`), a deleted file
  (`missing`), an added file (`extra`), a manifest that disagrees with the event, and a re-run that changes a
  number (`numbers_ok` FALSE, `ok_files` TRUE); `sr_export_zip` entries equal the event files plus the manifest,
  and its refusals; `sr_opt_source` for option, env and default.
- **Job layer**: the existing `test-analyses-job.R` passes unchanged; stale job file, dead PID, reused PID,
  cancel mid-job, a second job refused while one runs; the options snapshot reaches the worker and never holds
  `jev_key`.
- **Pure views**: gate rows and reasons; guard table ordering; preview blocks with `<5`; history rows with
  "missing on disk"; verify tables; freeze gate, diff and results-diff tables; settings sources.
- **Panels** (`testServer`, jobs synchronous through `sr.run_sync`): Write buttons disabled while gates fail;
  Check writes no files; Export adds `export-001` and a history row; the freeze dialog needs exactly `FREEZE`;
  bad and used labels refused; `.srproj` path checks; cancelled export leaves no file and no event; re-baseline
  needs a reason and is refused when the library matches; import cleanup removes only folders it created;
  Settings set / reset / validation, `CONNECT`, back to air-gap clears the key, settings locked during a job;
  during an archive job Data, Design, SAP and Analyses refuse with the shared text.
- **Key leak test**: a sentinel key set in Settings, then the Settings, Write, Archive and export flows; the
  rendered HTML, `events.jsonl`, the audit log and every file under the project and export folders are grepped
  for the sentinel, with no match allowed.
- **Real workers** (skipped on CRAN): one test each for verify with data, `.srproj` export and import through a
  real mirai worker, one of them cancelled.
- Existing lints (ASCII `.R`, no p-value branching, `sr_` / `.sr_` naming) cover the new files.

Review Focus (each pinned by a named test):

1. A cancelled or killed `.srproj` export leaves no partial file, no staging folder and no event.
2. A cancelled import never removes a folder that existed before it started.
3. The Jev key never appears in HTML, events, audit, exports or the worker snapshot.
4. Export Verify tells file corruption (`changed` / `missing` / `extra`) apart from numbers that moved after a
   re-run.
5. An actor name edited in Settings is the actor of events written by a background worker.

## 11. Acceptance

Run on the dev server (port 7790) with the synthetic sample only, with a screenshot of each step.

1. Open the sample project with its SAP locked and its analyses run.
2. Write: all gates green; Check shows the findings; the preview shows `<5` cells; Export; download the zip;
   Verify is ok; change one byte of an exported file and Verify turns red.
3. Archive: freeze `submission-v1` through the `FREEZE` dialog; quick verify; start Verify with data, cancel it
   once, then run it to completion; diff against "project now"; export a `.srproj` with data on auto.
4. Project tab: import that `.srproj` into a new folder on another path; it opens and freeze verify gives the
   identical hash (P1 MVP exit item 7).
5. Settings: change digits and see the preview change; set a key and see it never shown; enter connected mode
   with `CONNECT`; go back to air-gap in one click; restart the app and every setting is back to its default.

Done means: the full suite is green, the walk passes, `docs/writer.md` and `docs/archive.md` gain their panel
sections, a new `docs/settings.md` describes the Settings panel, `memory.md` is updated, and the final
whole-branch review says Yes.
