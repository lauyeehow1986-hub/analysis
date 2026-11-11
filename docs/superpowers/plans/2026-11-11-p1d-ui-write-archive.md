# P1d UI sub-project 3 (Write, Archive and Settings panels) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Put the finished writer and archive cores behind three panels. Write builds, checks, previews, exports and verifies a manuscript. Archive freezes, verifies, diffs, exports a `.srproj` and re-baselines. The Project tab imports a `.srproj`. Settings edits every user-facing option for this session only. With these, P1 MVP exit item 7 can be walked in the app.

**Architecture:**
- **Core (small).** `sr_export_verify` and `sr_export_zip` in `writer/export.R`; `sr_opt_source` in `core/util.R`.
- **Shared job layer** (`ui/job.R`). The generic parts of `ui/analyses_job.R` move here as `.sr_job_*`: spawn, job file, kill, read, result message. It also gains an options snapshot. The `.sr_an_*` names stay as thin wrappers, so Analyses behaves as before.
- **Pure view files** (`write_view.R`, `archive_view.R`, `settings_view.R`). Functions from a project, model or result to the data frames and tags each card shows.
- **Modules.** `mod_write.R`, `mod_archive.R`, `mod_import.R` (Project tab) and `mod_settings.R`.
- **App wiring.** `app.R` holds one busy hub: the OR of the data import, analysis run, archive job and project import. Every panel refuses changes with one shared busy text.

**Tech Stack:**
- R 4.5.2, shiny 1.14.0 (`ExtendedTask`, `downloadHandler`), bslib, DT.
- mirai 2.5.3 (no daemons; each `mirai()` is its own process), ps 1.9.1.
- zip (already a dependency), jsonlite (`base64_enc` for preview thumbnails).
- testthat 3 (`shiny::testServer`), withr.

**Spec:** [docs/superpowers/specs/2026-11-11-p1d-ui-write-archive-settings-design.md](../specs/2026-11-11-p1d-ui-write-archive-settings-design.md). It is the authority. Read it before any task. Section numbers below (§) refer to it.

## Deviations from the spec (rulings made while planning)

- **D1. Write panel prefix `.sr_wp_*`, not `.sr_wr_*`.**
  - Why: the writer core already uses `.sr_wr_*` (`.sr_wr_label`, `.sr_wr_template`, `.sr_wr_safe`, `.sr_wr_para` and others).
  - Cost if wrong: renaming one file's helpers.
- **D2. Import is its own module, `mod_import_ui(id)` / `mod_import_server(id, project, busy)` in `ui/mod_import.R`, shown on the Project tab under the project cards.**
  - `mod_project_server(id, busy = function() FALSE)` keeps returning the project reactiveVal, because about 12 tests in `test-mod-project.R` read `session$returned()` as the project.
  - It gains only the `busy` argument: Open, New and Close are refused while any job runs.
  - Spec §7.6 said `mod_project_server` returns `list(project, busy)`.
- **D3. Settings returns `list(version)`.** `version` is an integer reactive that goes up by one after every successful set or reset. Write (cache key, §6.2) and the navbar badges (§8.2) read it, so a digits edit reaches the preview. The spec does not say how the preview learns of an edit.
- **D4. The options snapshot (§5) is also passed by the analysis spawner (`.sr_an_spawn`) and the data import spawner (`.sr_data_spawn`).** Both workers write events, and Review Focus 5 needs the actor to apply in every worker.

## Global Constraints

- **Run commands from the worktree root** `<repo root>/.worktrees/p1d-ui-write-archive`.
  - Rscript: `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"`.
  - Never run long multi-line code through `Rscript -e`; write a script file under the scratchpad instead.
- **Run a single test file** with:
  `Rscript -e "source('tests/testthat/helper-load.R'); testthat::test_file('tests/testthat/<file>')"`.
  The full suite (`testthat::test_dir('tests/testthat')`) is slow: run it in the background.
- **Naming.** Exported core functions `sr_`, internal `.sr_`, snake_case.
  - Panel helpers: `.sr_wp_*` (Write, D1), `.sr_ar_*` (Archive), `.sr_im_*` (Import), `.sr_set_*` (Settings), `.sr_job_*` (job layer). Shared UI helpers stay `.sr_ui_*`.
  - Module pairs: `mod_<name>_ui(id)` / `mod_<name>_server(...)`.
- **ASCII only** in every new or changed `.R` file. Build a non-ASCII glyph from code points (`intToUtf8(0xB7)`).
- **Models never compute or write numbers.** Panels format numbers taken from core frames only, using `signif(x, 4)` for display. The preview renders the model's own templates through the docx renderer's piece builder (`.sr_dx_pieces`) and never builds prose of its own.
- **Event-sourced.** Cards re-derive from `project()`. The only session state is:
  - a live job handle;
  - the Write model cache (§6.2);
  - the Settings panel's own `options()`.
- **Gating.**
  - Every change control is rendered disabled when its refusal function returns a reason, and its handler refuses with the same reason. Refusal functions are `.sr_ui_refusal` or a panel's gate function.
  - Core refusals are shown verbatim (`.sr_ui_try` / `.sr_ui_notify`).
  - Cards holding user input re-render on `.sr_ui_flag` flags, not on bare `project()`.
- **Shared busy text** (Task 3): `.sr_ui_busy_text` = `"wait for the running import, analysis, archive operation or project import to finish"`.
- **No network calls.**
  - Options are read through `sr_opt(name, default)` only.
  - "Open folder" uses `.sr_ui_open_folder` (local `shell.exec` / `browseURL` on a folder).
  - The Jev key is never rendered, logged, written to a file or put in the snapshot.
- **No personal details in any committed file** (usernames, emails, hostnames, `C:\Users\<name>` paths). Before every commit, `git diff --cached | grep -inE "<your username>|users\\\\|@gmail"` must print nothing. Put the real username in the pattern locally; never commit it.
- **Commits** end with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`. Never push.

## Review Focus

The spec pins five (§10). Each has a named test in the task that owns the code:

1. **A cancelled or killed `.srproj` export leaves no partial file, no staging folder and no event.**
   - Task 6: `export cleanup: removes the archive, its .partial and the staging folder when no srproj.exported was written`.
   - Task 7: `a cancelled .srproj export leaves no file and no event`.
   - Task 10: real worker, `real worker: a killed .srproj export leaves nothing behind`.
2. **A cancelled import never removes a folder that existed before it started.**
   - Task 8: `import cleanup: a target folder that existed empty before the job is kept`.
   - Task 8: `import cleanup: a pre-existing <dir>.partial is never removed`.
3. **The Jev key never appears in HTML, events, audit, exports or the worker snapshot.**
   - Task 2: `opts snapshot never holds jev_key`.
   - Task 10: `key leak: the sentinel key is in no rendered HTML and no file under the project`.
4. **Export Verify tells file corruption apart from numbers that moved after a re-run.**
   - Task 1: `sr_export_verify: a re-run that moves a number is numbers_ok FALSE with ok_files TRUE`.
5. **An actor name edited in Settings is the actor of events written by a background worker.**
   - Task 10: `real worker: the actor set in Settings is the actor of the srproj.exported event`.

Also likely to bite, and pinned:
- A Write model cache that survives a project change (Task 5, `the model cache drops after a project change`).
- An export folder deleted by hand (Task 4, `history: a deleted export folder is missing on disk`).
- Mode `CONNECT` typed in lower case (Task 9, `connected needs exactly CONNECT`).
- Leaving connected mode keeps the key (Task 9, `back to airgap clears the key`).

---

### Task 1: Core: `sr_export_verify`, `sr_export_zip`, `sr_opt_source`

**Files:**
- Modify: `app/R/writer/export.R` (add both functions after `sr_exports`; extend the header comment)
- Modify: `app/R/core/util.R` (add `sr_opt_source` after `sr_opt`)
- Test: `tests/testthat/test-writer-export.R` (append), `tests/testthat/test-util.R` (append)

**Interfaces:**
- Produces:
  - **`sr_opt_source(name, default = NULL)`** -> `"option"` when `getOption(paste0("sr.", name))` is not NULL; else `"env"` when `Sys.getenv(toupper(paste0("SR_", name)))` is non-empty; else `"default"`. It mirrors `sr_opt`'s precedence (`core/util.R:5`).
  - **`sr_export_verify(project, id)`** -> `list(ok_files, files, numbers, numbers_ok)`. Refused for an unknown id with `stop("unknown export '<id>'")`.
    - `files` is `data.frame(path, expected, got, status)`: `path` relative to the project folder; `expected` the event SHA (NA for `extra`); `got` the SHA on disk (NA when missing).
    - `status` is `ok`, `changed`, `missing`, `extra` or `manifest`. `manifest` means `manifest.json` lists the file with a SHA other than the event's, or omits it.
    - `ok_files` is TRUE iff every row is `ok`.
    - `numbers`: for a manuscript, the `sr_ph_verify(reg, project)` frame, with `reg` rebuilt from `placeholders.json` with `sr_ph_registry(k = manifest$suppress_k)` and rows as in `test-writer-export.R:159-183` (move that row-building into an internal `.sr_ex_registry(dir, k)` and use it in the test too). NULL for `sap` / `feasibility`.
    - `numbers_ok`: TRUE when `numbers` is NULL or no row differs; FALSE otherwise.
  - **`sr_export_zip(project, id, dest)`** -> invisible `dest`. It writes with `zip::zip(dest, files, root = <out/exports>, mode = "mirror")`. Entries are `<id>/<file>` for every file of the event plus `<id>/manifest.json`.
    - Unknown id: `stop("unknown export '<id>'")`.
    - Non-manuscript: `stop("export '<id>' is a <kind> export; download its single file")`.
    - A missing listed file: `stop("export '<id>' is missing file(s): <paths>")`.
- Consumes: `state$exports` (`doc.exported` reducer), `.sr_ex_sha(path)`, `.sr_ex_files(dir)`.

- [ ] **Step 1: Write the failing tests.** Append to `test-writer-export.R`, each on `p <- .fx_writer_project(); p <- sr_export(p)` (id `export-001`):
  - `sr_export_verify: a clean export is all ok`. Expect `ok_files` TRUE, `numbers_ok` TRUE, and every `files$status == "ok"`.
  - `sr_export_verify: one changed byte is changed`. Flip the last byte of the `.docx`. Expect that row `changed` and `ok_files` FALSE.
  - `sr_export_verify: a deleted file is missing and an added file is extra`. Two files edited; two rows.
  - `sr_export_verify: a manifest that disagrees with the event is a manifest problem`. Rewrite one SHA inside `manifest.json`. The status of that path is `manifest`.
  - `sr_export_verify: a re-run that moves a number is numbers_ok FALSE with ok_files TRUE`:
    - change the frame value the project's `a_lr` reads;
    - re-run with `sr_run(p, "a_lr")` on a fixture built through `.fx_writer_build(env, ...)` (re-run needs a fresh fit);
    - expect `ok_files` TRUE, `numbers_ok` FALSE, and at least one differing row in `numbers`.

    If changing the data is not possible after the lock, instead edit the stored `fits/<key>.rds` value the registry reads and assert the same.
  - `sr_export_verify: unknown id is refused`. `expect_error(sr_export_verify(p, "export-999"), "unknown export 'export-999'")`.
  - `sr_export_zip: entries are the event files plus the manifest under the id folder`. Use `zip::zip_list(dest)$filename`, sorted, equal to `sort(c(paste0("export-001/", basename(<event paths>)), "export-001/manifest.json"))`. Use `setdiff` and ignore directory entries ending in `/`.
  - `sr_export_zip: refuses a sap export and a missing file`. Make the SAP export with `p <- sr_sap_docx(p)` (`writer/sapdoc.R:227`; its id is `SAP_v1`). Then delete one listed file of `export-001`.

  Append to `test-util.R`:
  - `sr_opt_source: option, env and default`. Use `withr::local_options(sr.zz_t = 1)` then `withr::local_envvar(SR_ZZ_T = "x")`, checking the order option > env > default.

- [ ] **Step 2: Run both files.** Expected: FAIL, `could not find function "sr_export_verify"`.
- [ ] **Step 3: Implement** the three functions to the Interfaces above. In verify, the `extra` scan lists `.sr_ex_files(<export folder>)` minus the event files minus `manifest.json` and `placeholders.json` (those two are not in `files`; check what `.sr_ex_files` and the event record hold and treat every file the exporter writes itself as expected).
- [ ] **Step 4: Run both files.** Expected: PASS.
- [ ] **Step 5: Commit.** `feat(writer): sr_export_verify, sr_export_zip and sr_opt_source`

---

### Task 2: The shared job layer `ui/job.R` with the options snapshot

**Files:**
- Create: `app/R/ui/job.R`
- Modify: `app/R/ui/analyses_job.R` (generic code moves out; wrappers stay)
- Modify: `app/R/ui/mod_data.R:201-224` (`.sr_data_spawn` gains `opts`)
- Test: `tests/testthat/test-job.R` (new). `tests/testthat/test-analyses-job.R` must pass **unchanged**.

**Interfaces:**
- Produces (moved verbatim unless noted):
  - **`.sr_job_path_ok(path)`** and **`.sr_job_put(text, path)`**: today's `.sr_an_path_ok` / `.sr_an_put`.
  - **`.sr_job_record(job_file)`**: writes `"<Sys.getpid()> <ps create time>"` through `<job_file>.part` and a rename. It is a no-op for NULL. It is the job-file part of `.sr_an_job`, which now calls it.
  - **`.sr_job_read(job_file)`** -> `list(pid, time)` or NULL.
  - **`.sr_job_kill(job_file, wait = 10)`**: today's `.sr_an_kill` body (suspend tree, kill children, kill worker, wait, resume survivors and stop).
  - **`.sr_job_res_message(res)`**: today's `.sr_an_res_message`.
  - **`.sr_job_opts()`** -> named list of `sr.<name>` = `getOption("sr.<name>")` for every `name` in `.sr_set_names()` (Task 9) whose option is non-NULL. `sr.jev_key` is always excluded. Until Task 9 lands, define `.sr_set_names()` in `job.R` as a stub returning the §8.1 names. Task 9 moves it to `settings_view.R` and deletes the stub.
  - **`.sr_job_spawn(app_dir, fn, args, opts = .sr_job_opts())`** -> a mirai. The worker:
    1. runs `source(file.path(app_dir, "global.R"), local = FALSE, encoding = "UTF-8")`;
    2. then `if (length(opts)) options(opts)`;
    3. then `do.call(fn, args)`, where `fn` is a function name (string).

    Options are set after sourcing, so `global.R` cannot overwrite them.
  - **`.sr_an_worker(project_dir, id, job, progress_file, job_file, cancel_file)`**: the current worker body of `.sr_an_spawn`, now a named function in `analyses_job.R`.
  - **`.sr_an_spawn(...)`** keeps its signature and becomes `.sr_job_spawn(app_dir, ".sr_an_worker", list(...))`.
  - **`.sr_an_path_ok`, `.sr_an_put`, `.sr_an_job_read`, `.sr_an_kill`, `.sr_an_res_message`** stay as one-line wrappers calling the `.sr_job_*` function (`test-app-wiring.R` stubs `.sr_an_spawn` and `.sr_an_kill` by name).
  - **`.sr_data_spawn(..., opts = .sr_job_opts())`**: the worker sets `options(opts)` right after sourcing `global.R` (D4).

- [ ] **Step 1: Write the failing tests** in `test-job.R`:
  - `job: record writes pid and start time; read returns them`. `.sr_job_record(jf)`; `.sr_job_read(jf)$pid == Sys.getpid()`; no `<jf>.part` is left.
  - `job: read of a missing, empty or malformed file is NULL`.
  - `job: kill of a dead PID is a no-op`, and `job: a recorded PID whose start time differs is not killed`. Copy the setup of the matching `.sr_an_kill` tests in `test-analyses-job.R`, calling `.sr_job_kill`.
  - `opts snapshot holds set sr.* settings only`. `withr::local_options(sr.user = "actor-x", sr.digits = 3L, sr.run_sync = TRUE)`; the snapshot has `sr.user` and `sr.digits` and no `sr.run_sync`.
  - `opts snapshot never holds jev_key`. With `withr::local_options(sr.jev_key = "SENTINEL-KEY-123")`, `expect_false("sr.jev_key" %in% names(.sr_job_opts()))`, and `"SENTINEL-KEY-123"` is absent from `deparse(.sr_job_opts())`.
  - `real worker: the snapshot reaches the worker` (`skip_on_cran()`):
    - `m <- .sr_job_spawn(<app dir>, "sr_opt", list("user"), opts = list(sr.user = "actor-x"))`;
    - `mirai::call_mirai(m)$data == "actor-x"`;
    - with `sr.jev_key` set in the session, a second spawn of `sr_opt("jev_key", "")` returns `""`.

    Get the app dir the way `test-app-wiring.R` / `helper-load.R` locate it.
- [ ] **Step 2: Run `test-job.R`.** Expected: FAIL (functions not found).
- [ ] **Step 3: Implement** `job.R`, then reduce `analyses_job.R` to the wrappers, `.sr_an_job`, `.sr_an_worker` and `.sr_an_settle`, and update its header comment. Update `.sr_data_spawn`.
- [ ] **Step 4: Run** `test-job.R`, `test-analyses-job.R`, `test-mod-analyses.R`, `test-mod-data.R` and `test-app-wiring.R`. Expected: all PASS, with no edit to `test-analyses-job.R`.
- [ ] **Step 5: Commit.** `refactor(ui): shared job layer ui/job.R with an options snapshot`

---

### Task 3: App-wide busy hub and shared busy text

**Files:**
- Modify: `app/R/ui/util.R` (busy text constant, `.sr_ui_busy_any`, `.sr_ui_refusal` uses the constant)
- Modify: `app/R/ui/analyses_view.R:140` and `app/R/ui/mod_data.R:549` (use the constant)
- Modify: `app/R/ui/mod_project.R` (`busy` argument; Open / New / Close refused while busy)
- Modify: `app/app.R` (the busy hub replaces `run_busy` / `any_busy`)
- Test: `tests/testthat/test-ui-util.R` (append), `tests/testthat/test-mod-project.R` (append), `tests/testthat/test-analyses-view.R:94,99,313` and `tests/testthat/test-app-wiring.R:96-101` (expect the constant instead of the old literal)

**Interfaces:**
- Produces:
  - **`.sr_ui_busy_text`**: the character constant `"wait for the running import, analysis, archive operation or project import to finish"`.
  - **`.sr_ui_busy_any(flags, except = character())`** -> TRUE iff any element of the named logical list `flags`, other than those named in `except`, is TRUE.
  - **`mod_project_server(id, busy = function() FALSE)`**: same return value (D2). Open, New and Close call `.sr_ui_notify("<Action>: " .. .sr_ui_busy_text, "error")` and do nothing while `busy()`.
  - **In `app.R`:**
    - `flags <- shiny::reactiveValues(data = FALSE, analyses = FALSE, archive = FALSE, import = FALSE)`;
    - `busy_for <- function(except = character()) shiny::reactive(.sr_ui_busy_any(shiny::reactiveValuesToList(flags), except))`;
    - one observer per job module copies its running flag into `flags`.

    Wiring:

    | Module | Receives |
    |---|---|
    | Data | `busy_for("data")` (its own import is `d$busy`) |
    | Analyses | `busy_for("analyses")` |
    | Design, SAP, Project | `busy_for()` |

    Tasks 5, 7, 8 and 9 add Write, Archive, Import and Settings to the same hub.

- [ ] **Step 1: Write the failing tests.**
  - In `test-ui-util.R`, `busy_any: OR of the flags minus except`: `list(data = FALSE, archive = TRUE)` is TRUE; with `except = "archive"` it is FALSE; `list()` is FALSE.
  - In `test-mod-project.R`, `Open is refused while busy`: run `testServer` with `args = list(busy = function() TRUE)`, set the dir input, click Open; `session$returned()` stays NULL and the spy has `.sr_ui_busy_text`.
  - Edit the literal in the four places listed under Files to `.sr_ui_busy_text`.
- [ ] **Step 2: Run** `test-ui-util.R` and `test-mod-project.R`. Expected: FAIL.
- [ ] **Step 3: Implement.** Keep the existing `test-app-wiring.R` assertions about which panels refuse during an import and a run; they now match the constant.
- [ ] **Step 4: Run** `test-ui-util.R`, `test-mod-project.R`, `test-analyses-view.R`, `test-app-wiring.R`, `test-mod-data.R`, `test-mod-design.R` and `test-mod-sap.R`. Expected: PASS.
- [ ] **Step 5: Commit.** `feat(ui): app-wide busy hub and shared busy text`

---

### Task 4: Write panel pure views (`ui/write_view.R`)

**Files:**
- Create: `app/R/ui/write_view.R`
- Test: `tests/testthat/test-write-view.R`

**Interfaces:**
- Produces:
  - **`.sr_wp_presets()`** -> `data.frame(name, label)` in `.sr_journal_all()` order. `label` is `"<name> (not verified)"` when `verified` is FALSE, else `name`.
  - **`.sr_wp_opts(preset, suppress_k, table2_on, alpha)`** -> `list(opts, error)`. `opts` is `sr_writer_opts(preset, suppress_k, if (isTRUE(table2_on)) "label" else "suppress", alpha)`, or NULL. `error` is the core message, or NULL. It never throws.
  - **`.sr_wp_gates(p, busy)`** -> `data.frame(gate, ok, reason)` with four rows in this order:

    | gate | reason when failing |
    |---|---|
    | `"Project writable"` | `"the project is read-only (locked by another session)"` |
    | `"SAP locked"` | `"lock the SAP first"` |
    | `"Pre-specified analyses run"` | `"no ok run: <ids, comma-separated>"`, over the analyses of `.sr_sap_analyses(p)` whose `runs[[id]]$ok_key` is NA (the same predicate `.sr_freeze_gate` uses, `archive/freeze.R:108-115`, extended to every pre-specified label) |
    | `"App not busy"` | `.sr_ui_busy_text` |

    `reason` is `""` when `ok`. A NULL `p` gives one row `"Project open"` / `"open a project first"`.
  - **`.sr_wp_gate_reason(gates)`** -> the first failing reason, or NULL.
  - **`.sr_wp_guard_rows(findings)`** -> `findings` (the `sr_claim_guard` frame: block, analysis, rule, severity, detail), ordered `severity == "block"` first, then by block. The columns are `severity, rule, block, analysis, detail`.
  - **`.sr_wp_text(template, model)`** -> a single string: `paste(.sr_dx_pieces(template, model$reg, <cite>)$text, collapse = "")`, where `<cite>` is built the way the docx builder builds `ctx$cite` (read `writer/docx.R`; factor that construction into a small `.sr_dx_cite(model)` if it is inline). This is how suppressed counts read `<k`, exactly as in the docx.
  - **`.sr_wp_preview(model, fig_dir, preset)`** -> an `htmltools::tagList` of the `main` then `supplement` blocks:
    - `heading`: `h3` (level 1) / `h4` (other);
    - `para`: `p(.sr_wp_text(text))`;
    - `table`: a caption line plus an HTML `<table class="table table-sm">`, with every header and cell through `.sr_wp_text`, and footnotes as `small`;
    - `figure`: an `img` of `sr_wr_figure_render(b, fig_dir, preset)$path` as `data:image/png;base64,` via `jsonlite::base64_enc`, max width 320px, plus the caption;
    - `refs` and `checklist`: skipped (the checklist has its own card).

    A supplement heading `h3("Supplement")` separates the two parts.
  - **`.sr_wp_history(p)`** -> `data.frame(kind, id, created, preset, n_files, on_disk, status)`, one row per `state$exports` entry in event order:
    - `created` is the event `ts`, or the export's `created` if stored;
    - `on_disk` is TRUE iff every listed file exists;
    - `status` is `"ok"` or `"missing on disk"`.
  - **`.sr_wp_verify_lines(v)`** -> `list(problems, numbers)`.
    - `problems` is a character vector with one `"<status>: <path>"` per non-ok file row.
    - `numbers` is NULL when `v$numbers_ok`, else the differing rows of `v$numbers`, with numeric columns passed through `signif(x, 4)`.
  - **`.sr_wp_checklist(model)`** -> the `df` of the supplement block with `type == "checklist"`, with columns `item, section, label, status, detail` (rename to these if the core names differ). For a CONSORT project, the `checklist_note` paragraph text instead (`writer/model.R:281`), as `list(df = NULL, note = <text>)`. Otherwise `list(df, note = NULL)`.

- [ ] **Step 1: Write the failing tests** in `test-write-view.R` (fixtures: `.fx_writer_project()`, `.fx_ui_project()`):
  - `presets: every preset is marked not verified`.
  - `opts: a valid set returns opts; suppress_k 3 returns the core message and no opts`. `error` matches `"suppress_k"`.
  - `gates: an unlocked project fails SAP locked and names the reason`. Use `.fx_ui_project()`.
  - `gates: a locked and run project passes all four; busy fails only App not busy`.
  - `gates: a pre-specified analysis without an ok run is named`. Use `.fx_writer_build(env, unrun = "a_rmst")`; the reason contains `a_rmst`.
  - `guard rows: block findings come first`. Use a hand-made findings frame with warn then block rows.
  - `preview: a suppressed count renders as <5 and no {{ token remains`:
    - `m <- sr_doc_model(p)`;
    - `html <- as.character(.sr_wp_preview(m, withr::local_tempdir(), sr_journal_preset()))`;
    - `expect_false(grepl("{{", html, fixed = TRUE))`;
    - and, when `sr_ph_table(m$reg)` has a suppressed row, `expect_match(html, "&lt;5")`. If the fixture has no suppressed row, set `m$reg$rows$suppressed[1] <- TRUE` on a count row first and use its text.
  - `history: one row per export with ok status`. Use `p <- sr_export(p)`.
  - `history: a deleted export folder is missing on disk`. `unlink(<export dir>, recursive = TRUE)`; `status == "missing on disk"`.
  - `verify lines: problems per bad file; numbers only when they differ`. Use hand-made `v` lists.
  - `checklist: the STROBE frame has item, section, label, status, detail`.
- [ ] **Step 2: Run `test-write-view.R`.** Expected: FAIL.
- [ ] **Step 3: Implement** to the Interfaces.
- [ ] **Step 4: Run `test-write-view.R` and `test-writer-docx.R`** (the latter if `.sr_dx_cite` was factored out). Expected: PASS.
- [ ] **Step 5: Commit.** `feat(ui): Write panel views`

---

### Task 5: Write panel module (`ui/mod_write.R`) and its tab

**Files:**
- Create: `app/R/ui/mod_write.R`
- Modify: `app/app.R`:
  - remove `"Write"` from `.sr_panel_names`;
  - add `bslib::nav_panel("Write", mod_write_ui("write"), value = "Write")` after Analyses;
  - hide and show it with the other project tabs;
  - `mod_write_server("write", project, busy_for(), settings = function() 0L)` (Task 9 replaces the stub with `s$version`).
- Test: `tests/testthat/test-mod-write.R`

**Interfaces:**
- Consumes: Task 1 `sr_export_verify`, `sr_export_zip`; Task 3 hub; Task 4 views; `sr_doc_model`, `sr_claim_guard`, `sr_export`, `.sr_ui_open_folder`, `.sr_ui_try`, `.sr_ui_flag`.
- Produces: `mod_write_ui(id)`; `mod_write_server(id, project, busy, settings = function() 0L)` returns nothing used.
- **Inputs:** `preset` (select from `.sr_wp_presets()`), `suppress_k` (numeric, default `SR_SDC_K`), `table2` (checkbox, TRUE), `alpha` (numeric 0.05), and buttons `check`, `preview`, `export`.
- **Per-row history buttons:** `dl_<id>` (`downloadButton`), `open_<id>` and `verify_<id>` (actionButtons, ids with `-` replaced by `_`).
- **Outputs:** `opts_error`, `gates`, `guard`, `preview`, `checklist`, `history`, `verify`.
- **Cache.** One session environment holding `key` and `model`.
  - `key` is `list(opts, n_events = length of the replayed events, digits = sr_opt("digits", 2), settings())`.
  - `model()` builds through `.sr_ui_try("Check manuscript", function() sr_doc_model(p, opts))` on a key miss.
  - Check, Preview and Checklist reuse it.
  - Export always rebuilds inside `sr_export` and then drops the cache.
- **Buttons.** Check, Preview and Export are disabled when `.sr_wp_gate_reason(gates)` or `opts_error` is non-NULL, and refuse with that reason.
- **Downloads.**
  - Manuscript download: `downloadHandler(filename = "<id>.zip", content = function(f) sr_export_zip(p, id, f))`.
  - SAP and feasibility: `filename = basename(<file>)`, `content = file.copy`.
  - Rows whose `status` is `"missing on disk"` render download and Open disabled.

- [ ] **Step 1: Write the failing tests** in `test-mod-write.R`. Use `testServer`, the `.ui_spy_notify()` spy, and `withr::local_options(sr.run_sync = TRUE)`; `project` is `shiny::reactiveVal(.fx_writer_project())`.
  - `buttons are disabled with the gate reason on an unlocked project`. Use `.fx_ui_project()`. Clicking `check` notifies `"Check manuscript: lock the SAP first"`, and `output$guard` has no table.
  - `Check shows findings and writes no file`. Snapshot `list.files(p$paths$dir, recursive = TRUE)` before and after.
  - `an invalid suppress_k shows the core message and disables Check`.
  - `the model cache drops after a project change`. Click check; count calls by stubbing `sr_doc_model` with a counter that calls the real one through `.ma_stub`-style globalenv assignment. Then append a note event and click check again: two builds. Click check a third time with no change: still two.
  - `Export adds export-001 and a history row`.
  - `Export refused on a read-only project is shown verbatim and writes nothing`.
  - `the zip download holds the export files`. Use `output$dl_export_001` in `testServer`, which returns the downloaded file path.
  - `Verify on a changed file shows a red problem line`. `output$verify` contains `"changed:"`.
  - `Open folder calls the OS opener with the export folder`. Stub `.sr_ui_open_folder`.
- [ ] **Step 2: Run `test-mod-write.R`.** Expected: FAIL.
- [ ] **Step 3: Implement** the module (cards in order: Options and gates, Check, Preview, Checklist, Export and history) and the `app.R` tab.
- [ ] **Step 4: Run `test-mod-write.R`, `test-app-wiring.R` and `test-app-launch.R`.** Expected: PASS.
- [ ] **Step 5: Commit.** `feat(ui): Write panel`

---

### Task 6: Archive views and archive job functions (`ui/archive_view.R`, `ui/archive_job.R`)

**Files:**
- Create: `app/R/ui/archive_view.R`, `app/R/ui/archive_job.R`
- Test: `tests/testthat/test-archive-view.R`

**Interfaces:**
- **`archive_view.R` (pure):**
  - **`.sr_ar_freeze_gates(p)`** -> `data.frame(gate, ok, reason)` mirroring `.sr_freeze_gate` (`archive/freeze.R:86-130`), one row each:
    - project writable;
    - SAP locked;
    - sources hashed (`sr_tables_needing_hash`);
    - primary analyses run;
    - stored results present;
    - project verifies (`sr_project_verify`).

    Build each from the same calls the gate makes; never call `.sr_freeze_gate` itself (it stops at the first failure). Reasons copy the gate's message text.
  - **`.sr_ar_label_problem(p, label)`** -> NULL, or one of:
    - `"the label must match ^[a-z][a-z0-9-]{0,39}$"` (use `SR_FREEZE_LABEL_RE`);
    - `"freeze label '<label>' is already used"`.
  - **`.sr_ar_freezes(p)`** -> `sr_freezes(p)` reduced to `label, created, events_seq, libsnap` with `libsnap` cut to 12 characters. Read the real column names from `sr_freezes` and map them.
  - **`.sr_ar_verify_rows(v)`** -> `list(ok, problems, absent)`. `absent` lists the data files reported as absent; they are information, not problems (§7.2).
  - **`.sr_ar_diff_tables(d)`** -> a named list of data frames, one per non-empty section of `sr_freeze_diff`. **`.sr_ar_results_diff(r)`** -> `sr_freeze_results_diff` with numeric columns through `signif(x, 4)`.
  - **`.sr_ar_srproj_plan(p, label, data)`** -> `list(mode, bytes, absent)`, computed by the same rule as `srproj_export.R:73-80`. Factor that rule into an internal `.sr_srproj_mode(project, label, data)` in `srproj_export.R` and call it from both places.
  - **`.sr_ar_path_problem(path)`** -> NULL, or:
    - `"the path must end in .srproj"`;
    - `"<path> already exists"`;
    - `"the folder <parent> does not exist"`.
  - **`.sr_ar_actor_warning()`** -> NULL when `sr_opt_source("user") != "default"`, else the OS-user warning text `sr_srproj_export` gives (`srproj_export.R:63`). Factor the text into a constant `SR_SRPROJ_ACTOR_WARNING` there.
- **`archive_job.R` (worker functions and cleanup):**
  - **`.sr_ar_verify_worker(project_dir, label, job_file)`**:
    1. `.sr_job_record(job_file)`;
    2. `p <- sr_project_open(project_dir)` (read-only, since the session keeps the lock);
    3. `sr_freeze_verify(p, label, data = TRUE)`;
    4. `sr_project_close` if writable;
    5. returns the verify list.
  - **`.sr_ar_export_worker(project_dir, path, label, data, job_file)`**:
    - records the job and opens the project;
    - refuses with `list(ok = FALSE, error = "the project is locked by another session")` when it is read-only;
    - otherwise `sr_srproj_export(p, path, label, data)`, then close;
    - returns `list(ok = TRUE, path)` or `list(ok = FALSE, error = <message>)`.
  - **`.sr_ar_n_events(project_dir)`** -> the number of lines in `events.jsonl`. **`.sr_ar_last_type(project_dir)`** -> the `type` of the last event.
  - **`.sr_ar_export_cleanup(project_dir, path, label, n_before)`** -> `"finished"` when `.sr_ar_n_events(project_dir) > n_before` and the last type is `"srproj.exported"`. Otherwise it removes `path`, `paste0(path, ".partial")` and `file.path(project_dir, "out", paste0(".srproj-", label, ".partial"))` (recursive) and returns `"removed"`.

- [ ] **Step 1: Write the failing tests** in `test-archive-view.R`:
  - `freeze gates: an unlocked project fails SAP locked; a locked, run project passes all`. Use `.fx_ui_project()` / `.fx_writer_project()`.
  - `label problem: bad pattern and used label`. Freeze `v1` first with `sr_freeze(p, "v1")`.
  - `freezes table and verify rows`. Use `sr_freeze_verify(p, "v1", data = FALSE)`.
  - `diff tables: an unchanged project gives empty sections; results diff rounds only`.
  - `srproj plan matches what sr_srproj_export picks`. Export with `data = "auto"` and compare `payload$data_mode` with `.sr_ar_srproj_plan(...)$mode`.
  - `path problem: extension, existing and missing parent`.
  - `actor warning only when sr.user is unset`. Use `withr::local_options(sr.user = "analyst-1")` for NULL.
  - `export cleanup: removes the archive, its .partial and the staging folder when no srproj.exported was written`. Create the three paths by hand; `n_before` is the current count; `"removed"`; none exist.
  - `export cleanup: a finished export is kept and reported`. Run a real `sr_srproj_export` with `n_before` taken before it; `"finished"`; the archive exists.
  - `verify worker on a project the session holds runs read-only and returns the verify`. Call `.sr_ar_verify_worker` inline in-process while `p` is open.
- [ ] **Step 2: Run `test-archive-view.R`.** Expected: FAIL.
- [ ] **Step 3: Implement**, including the two small factorings in `srproj_export.R`.
- [ ] **Step 4: Run `test-archive-view.R` and `test-archive-srproj-export.R`.** Expected: PASS.
- [ ] **Step 5: Commit.** `feat(ui): Archive views and job functions`

---

### Task 7: Archive panel module (`ui/mod_archive.R`) and its tab

**Files:**
- Create: `app/R/ui/mod_archive.R`
- Modify: `app/app.R` (Archive tab after Write; removed from `.sr_panel_names`; hidden with no project; `ar <- mod_archive_server("archive", project, busy_for("archive"))`; observer `flags$archive <- isTRUE(ar$running())`)
- Test: `tests/testthat/test-mod-archive.R`

**Interfaces:**
- Consumes: Task 2 (`.sr_job_spawn`, `.sr_job_kill`, `.sr_job_res_message`), Task 6, `sr_freeze`, `sr_freeze_verify`, `sr_freeze_diff`, `sr_freeze_results_diff`, `sr_rebaseline_diff`, `sr_rebaseline`.
- Produces: `mod_archive_ui(id)`; `mod_archive_server(id, project, busy)` returns `list(running = <reactive logical>)`.
- **Freeze.**
  - Input `label` is prefilled from `sr_freeze_next_label(p)` and refilled after each freeze.
  - Button `freeze` opens a `modalDialog` with `textInput("confirm")` and `actionButton("do_freeze")`. That button is disabled unless `identical(input$confirm, "FREEZE")`, and its handler re-checks.
  - Then `sr_freeze(p, label)` runs through `.sr_ui_actor`.
- **Freezes card.** A table, a select `freeze_pick`, and buttons `quick_verify` (synchronous), `data_verify` (job) and `cancel`.
- **Diff card.** `diff_from` (freezes), `diff_to` (freezes plus `"project now"`), button `diff`.
- **Export card.**
  - Inputs: `ex_label` (default the latest freeze), `ex_data` (`auto` / `full` / `lean`), `ex_path` (text; strip quotes as `mod_project`'s `dir_in()` does).
  - Output `ex_plan`: `"auto will pick <mode> (<size>)"` plus the actor warning with a link to the Settings tab (`actionLink` that calls `bslib::nav_select("main_nav", "Settings", session = <root session>)`).
  - Buttons `export` and `ex_cancel`.
- **Re-baseline card.** Output of `sr_rebaseline_diff(p)`, `rb_reason` (text), button `rebaseline`. Disabled with reason `"freeze the project first"` when `sr_freezes(p)` is empty, and `"the library already matches the baseline"` when the diff is empty (read its emptiness from the core frame). An empty reason is refused with `"Re-baseline: give a reason"`.
- **Jobs.**
  - One job at a time, held in a session `env` with `kind` (`"verify"` or `"export"`), `job_file`, `n_before`, `path`, `label` and `task` (an `ExtendedTask` wrapping `.sr_job_spawn`).
  - With `sr.run_sync` TRUE the worker function is called inline instead.
  - Starting a job while `running()` or `busy()` is refused with `.sr_ui_busy_text`.
  - **Export job**, the Analyses lock pattern:
    1. `n_before <- .sr_ar_n_events(dir)`;
    2. `sr_duck_close(p)`; `sr_project_close(p)`;
    3. set `project(<p with readonly = TRUE>)`;
    4. spawn;
    5. on settle (ok, error or cancel): run `.sr_ar_export_cleanup` unless the result is ok, then `project(sr_project_open(dir))`.

    Messages: ok -> `"Exported <path>"`; `"finished"` after cancel -> `"finished before it could be cancelled"`; error -> the message verbatim.
  - **Cancel:** `.sr_job_kill(job_file)`, then the settle step above.

- [ ] **Step 1: Write the failing tests** in `test-mod-archive.R`. Use `testServer`, `sr.run_sync = TRUE`, and project `.fx_writer_project()`.
  - `the freeze dialog needs exactly FREEZE`. `"freeze"` and `"Freeze"` leave `sr_freezes` empty; `"FREEZE"` creates `freeze-001` (or whatever `sr_freeze_next_label` gives).
  - `a bad label and a used label are refused before the dialog`.
  - `quick verify shows ok`.
  - `diff against project now shows the results diff`.
  - `.srproj path checks refuse before any job starts`. Three bad paths; the event count is unchanged.
  - `a cancelled .srproj export leaves no file and no event`. Stub `.sr_job_spawn` with a never-resolving pending task (reuse `.ma_pending_spawner` from `test-mod-analyses.R`; move it into `helper-ui.R` if it is local to that file). Create `<path>.partial` and the staging folder by hand to stand in for a half-done worker. Click `ex_cancel` with `.sr_job_kill` stubbed. None of the three paths exist; the event count equals the count before; the project is writable again.
  - `the export succeeds and the project is writable again`. `state$srproj` (or the reducer's field) gains the entry.
  - `re-baseline needs a reason and is refused when the library matches`.
  - `during an archive job Data, Design, SAP and Analyses refuse with the shared text`. Put this in `test-app-wiring.R`: drive the archive module into a pending export, then click a change control in each of the other four panels; each spy message ends with `.sr_ui_busy_text`.
  - `a second job is refused while one runs`.
- [ ] **Step 2: Run `test-mod-archive.R`.** Expected: FAIL.
- [ ] **Step 3: Implement** the module and the `app.R` tab.
- [ ] **Step 4: Run `test-mod-archive.R`, `test-app-wiring.R` and `test-app-launch.R`.** Expected: PASS.
- [ ] **Step 5: Commit.** `feat(ui): Archive panel`

---

### Task 8: Import `.srproj` on the Project tab (`ui/mod_import.R`)

**Files:**
- Create: `app/R/ui/mod_import.R`
- Modify: `app/R/ui/archive_job.R` (import worker and cleanup)
- Modify: `app/app.R` (`mod_import_ui("import")` under `mod_project_ui("project")` on the Project tab; `im <- mod_import_server("import", project, busy_for("import"))`; `flags$import`)
- Test: `tests/testthat/test-mod-import.R`

**Interfaces:**
- Produces:
  - **`.sr_im_snapshot(dir)`** -> a named logical: `dir`, `partial`, `src`, each TRUE iff that path (`dir`, `<dir>.partial`, `<dir>.partial.src`) exists now. Trailing slashes are stripped as `sr_srproj_import` does.
  - **`.sr_im_cleanup(dir, before)`** -> invisible character vector of the paths removed. It removes (recursive) each of the three paths that exists now and was FALSE in `before`.
  - **`.sr_im_worker(archive, dir, job_file)`**:
    - `.sr_job_record(job_file)`;
    - `p <- sr_srproj_import(archive, dir, verify_data = TRUE)`;
    - keeps `imp <- p$import`, then `sr_duck_close(p)` and `sr_project_close(p)`;
    - returns `list(ok = TRUE, dir, import = imp)`, or `list(ok = FALSE, error)`.
  - **`.sr_im_summary(imp)`** -> `data.frame(field, value)` for label, data mode, archive SHA (first 16 characters), parts missing, sources missing and verify result. Read the field names from `sr_srproj_import`'s `$import` (`srproj_import.R:20`).
  - **`mod_import_ui(id)`**: a card "Import .srproj" with `archive`, `target`, and buttons `import` and `cancel`.
  - **`mod_import_server(id, project, busy)`** -> `list(running)`.
    - It checks the paths first. The archive must exist. The target must not exist or be an empty folder, and its parent must exist. Each refusal copies the core message text.
    - It takes `before <- .sr_im_snapshot(dir)` and spawns the worker.
    - On ok: close the open project (`sr_duck_close`, `sr_project_close`, if any), then `project(sr_project_open(dir))`, and show `.sr_im_summary`.
    - On error or cancel: `.sr_im_cleanup(dir, before)`; the previous `project()` is untouched.
    - The worker's own failure path already removes partials it made; cleanup is the backstop for a killed worker.

- [ ] **Step 1: Write the failing tests** in `test-mod-import.R`. Make an archive once per file from `.fx_writer_project()`: `sr_freeze(p, "v1")`, then `sr_srproj_export(p, <tmp>.srproj, "v1", "full")`.
  - `import cleanup: removes only the paths the job created`. Before is all FALSE; create all three; all removed.
  - `import cleanup: a target folder that existed empty before the job is kept`. `dir.create(dir)` before the snapshot; create the partials; the dir is still present after cleanup and the partials are gone.
  - `import cleanup: a pre-existing <dir>.partial is never removed`.
  - `import opens the project and shows the summary`. Use `sr.run_sync = TRUE`; `project()$paths$dir` equals the target (normalised); the summary has the label `v1`.
  - `a failed import keeps the previous project open`. Use a corrupt archive: copy and truncate. The previous project is identical and the error is shown verbatim.
  - `a cancelled import removes what it created and keeps the previous project`. Use the pending spawner; create `<dir>.partial` by hand; cancel with `.sr_job_kill` stubbed.
  - `import is refused while another job runs`. Use `busy = function() TRUE`.
- [ ] **Step 2: Run `test-mod-import.R`.** Expected: FAIL.
- [ ] **Step 3: Implement.**
- [ ] **Step 4: Run `test-mod-import.R`, `test-mod-project.R` and `test-app-wiring.R`.** Expected: PASS.
- [ ] **Step 5: Commit.** `feat(ui): import .srproj on the Project tab`

---

### Task 9: Settings panel (`ui/settings_view.R`, `ui/mod_settings.R`) and reactive badges

**Files:**
- Create: `app/R/ui/settings_view.R`, `app/R/ui/mod_settings.R`
- Modify: `app/R/ui/job.R` (delete the `.sr_set_names` stub)
- Modify: `app/app.R`:
  - Settings tab, always visible, removed from `.sr_panel_names`;
  - `s <- mod_settings_server("settings", busy_for())`;
  - Write gets `settings = s$version`;
  - badges become `shiny::uiOutput("badges")` rendered from `.sr_badges()` with a dependency on `s$version()`.
- Test: `tests/testthat/test-settings-view.R`, `tests/testthat/test-mod-settings.R`

**Interfaces:**
- **`settings_view.R` (pure):**
  - **`.sr_set_catalogue()`** -> `data.frame(name, label, editor, default)` in §8.1 order:

    | name | editor | default |
    |---|---|---|
    | `mode` | `"choice"` (`airgap`, `connected`) | `"airgap"` |
    | `profile` | `"choice"` (`auto`, `locked16`, `laptop`; `auto` = option NULL) | `""` |
    | `user` | `"text"` | `NA` (OS user) |
    | `digits` | `"integer"` | `2` |
    | `router_mask` | `"logical"` | `FALSE` |
    | `jev_key` | `"secret"` | `""` |
    | `jev_url` | `"text"` | `"https://api.typesafe.ai/v1/systemone"` |
    | `jev_model` | `"text"` | `"jev-latest"` |
    | `jev_timeout` | `"number"` | `20` |
    | `freeze_full_max_bytes` | `"number"` | `2147483648` |
    | `srproj_max_total` | `"number"` | `68719476736` |
    | `cran_snapshot` | `"date"` | `NA` |

    These defaults are the ones the core's own `sr_opt` calls use; a test checks them by grepping `app/R`.
  - **`.sr_set_names()`** -> `.sr_set_catalogue()$name`.
  - **`.sr_set_coerce(name, value)`** -> the typed value, or `stop(<message>)`. Messages:
    - user: `"the actor must not be empty (use Reset)"`;
    - digits: `"digits must be a whole number, 0 or more"`;
    - router_mask: `"router_mask must be on or off"`;
    - jev_key, jev_url, jev_model: `"<name> must not be empty"`;
    - jev_timeout: `"jev_timeout must be a positive number of seconds"`;
    - the two byte limits: `"<name> must be a finite number of bytes, 0 or more"`;
    - cran_snapshot: `"cran_snapshot must be a date YYYY-MM-DD"` (use `as.Date(x, "%Y-%m-%d")` and require `format(d) == x`).
  - **`.sr_set_apply(name, value)`**:
    - coerces;
    - sets `options(sr.<name> = v)`;
    - then for `mode` calls `sr_mode()` and for `profile` calls `sr_profile()`;
    - on any error restores the previous option and re-throws the core message.
  - **`.sr_set_reset(name)`** -> `options(sr.<name> = NULL)`. For `mode` it also clears `sr.jev_key`.
  - **`.sr_set_table(edited)`** -> `data.frame(setting, value, source)`. `edited` is the character vector of names set in this session.
    - `value` is `format(sr_opt(name, default))`, except jev_key, which is `"set"` / `"not set"` and never the value.
    - `source` is `"edited here"` when in `edited`, else `sr_opt_source(name)`. For `profile` with source `"default"` it is `"auto-detected"`, and `value` is `sr_profile()`.
  - **`.sr_set_note(name)`** -> the profile fingerprint note and the cran_snapshot libsnap note (§8.2 wording), else NULL.
- **`mod_settings_server(id, busy)`** -> `list(version)`.
  - Per row: an editor fitting `editor`, plus `set_<name>` and `reset_<name>` buttons.
  - The jev_key editor is a `passwordInput` that is cleared (`updateTextInput(value = "")`) right after a set. The key never goes into an output.
  - `mode = connected` opens a modal with `textInput("connect_word")`; the confirm button works only for `identical(x, "CONNECT")`. Choosing `airgap` applies at once and resets `jev_key`.
  - While `busy()`, every set or reset is refused with `"Settings: " .. .sr_ui_busy_text`, and the editors render disabled.
  - The `user` row shows `.sr_ar_actor_warning()` when it is non-NULL.

- [ ] **Step 1: Write the failing tests.** Every test wraps option changes in `withr::local_options` / `withr::defer(options(...))` so nothing leaks into later files.
  - In `test-settings-view.R`:
    - `catalogue defaults match the core's sr_opt calls`. For each name except `user`, `profile` and `cran_snapshot`, find `sr_opt("<name>", <x>)` in `app/R` with `grep` and compare the deparsed `<x>`.
    - `coerce: each validation message`.
    - `apply: an invalid mode restores the previous option`.
    - `table: sources for edited, option, env and default; profile auto-detected; key shown as set`.
    - `table never shows the key value`.
  - In `test-mod-settings.R` (`testServer`):
    - `set and reset digits`. The `version` goes up by one each time, and `getOption("sr.digits")` is NULL after reset.
    - `connected needs exactly CONNECT`. `"connect"` leaves `sr_mode()` as `"airgap"`.
    - `back to airgap clears the key`.
    - `settings are locked during a job`. Use `busy = function() TRUE`.
    - `the key editor is emptied after a set and no output holds it`. Collect every `output$...` value as character and check that none contains the sentinel.
  - In `test-app-wiring.R`: `badges re-render after a mode change`.
- [ ] **Step 2: Run both new files.** Expected: FAIL.
- [ ] **Step 3: Implement**, and delete the `job.R` stub. Run `test-job.R` again.
- [ ] **Step 4: Run** `test-settings-view.R`, `test-mod-settings.R`, `test-job.R`, `test-app-wiring.R`, `test-app-launch.R` and `test-mod-write.R`. Expected: PASS.
- [ ] **Step 5: Commit.** `feat(ui): Settings panel and reactive badges`

---

### Task 10: Key-leak test and real-worker tests

**Files:**
- Test: `tests/testthat/test-p1d-ui3-e2e.R` (new)

**Interfaces:**
- Consumes: Tasks 1-9 only. No production code is expected. A failure here is fixed in the task's own file and committed with this task.

- [ ] **Step 1: Write the tests:**
  - `key leak: the sentinel key is in no rendered HTML and no file under the project`.
    1. `testServer(server, ...)` on `app.R`'s server, as `test-app-wiring.R` does, with `sr.run_sync = TRUE`.
    2. Set `jev_key = "SENTINEL-KEY-7f3a"` through the Settings inputs.
    3. Open a `.fx_writer_project()` copy and click Write Check, Preview and Export.
    4. Freeze `v1` through the dialog, then export a `.srproj` to a temp path.
    5. Collect every output value rendered by then.
    6. Grep these for the sentinel:
       - the rendered outputs;
       - every file under the project folder (`list.files(recursive = TRUE, all.files = TRUE)`, read as raw and matched with `grepRaw`);
       - the `.srproj` file;
       - the export zip from the download handler.

       No match is allowed.
  - `real worker: verify with data completes through a mirai worker` (`skip_on_cran()`). `sr.run_sync = FALSE`; drive `data_verify`, then wait with the `.ma_wait` loop from `test-mod-analyses.R`. The result is ok.
  - `real worker: a killed .srproj export leaves nothing behind` (`skip_on_cran()`).
    - Start a real export of `.fx_writer_project()` with `data = "full"` and click `ex_cancel` as soon as the job file exists. Add no test-only delay option to production code.
    - The kill can land before or after the worker writes `srproj.exported`, so assert the invariant, not one branch. Either the archive exists, no `.partial` or staging folder is left, and exactly one `srproj.exported` event was added; or none of the three paths exist and the event count is unchanged.
  - `real worker: the actor set in Settings is the actor of the srproj.exported event` (`skip_on_cran()`). Set `user = "actor-rf5"` in the Settings module, run a real export, and read the last line of `events.jsonl`: its `actor` field (`core/events.R:95`) is `"actor-rf5"`.
  - `real worker: import through a mirai worker opens the project`.
- [ ] **Step 2: Run `test-p1d-ui3-e2e.R`.** Expected: PASS. Fix any failure in the owning task's file.
- [ ] **Step 3: Run the full suite in the background**, check the lints (`test-lint.R`: ASCII, no p-value branching, naming), and confirm 0 failures.
- [ ] **Step 4: Commit.** `test(ui): key leak and real-worker tests for Write, Archive, Import and Settings`

---

### Task 11: Docs and memory

**Files:**
- Modify: `docs/writer.md` (a "Write panel" section: cards, gates, cache rule, download and verify meanings)
- Modify: `docs/archive.md` (an "Archive panel" section and an "Import" section: the `FREEZE` dialog, the jobs and Cancel cleanup, auto data mode, the actor warning)
- Create: `docs/settings.md` (the catalogue with sources, session-only rule, `CONNECT`, the key rule, profile and snapshot notes)
- Modify: `memory.md` (sub-project 3 state, D1-D4, gotchas found during the work)

- [ ] **Step 1: Write the four documents.** Their claims must match the code. Every option name, button label and message is copied from the code.
- [ ] **Step 2: Grep the diff** for personal details (Global Constraints) and for non-ASCII in any `.R` file.
- [ ] **Step 3: Commit.** `docs: Write, Archive, Import and Settings panels`

---

### Task 12: Acceptance walk (§11)

**Files:** none committed except fixes. Screenshots go to the scratchpad.

- [ ] **Step 1: Start the dev server** on port 7790 through `.claude/launch.json` with the built-in browser (`preview_start`). Open the synthetic sample project, its SAP locked and its analyses run. Build it with `samples/make_samples.R` and the existing sample-project script used in the sub-project 2 walk (see `memory.md`).
- [ ] **Step 2: Walk §11 steps 1-5**, with a screenshot per step:
  - Write gates are green; Check; preview `<5`; Export; zip download; Verify ok; one byte changed and Verify turns red.
  - Freeze `submission-v1` through `FREEZE`; quick verify; Verify with data cancelled once, then completed; diff against project now; `.srproj` export on auto.
  - Import into a new folder on another path; freeze verify gives the identical hash.
  - Settings: digits changes the preview; the key is never shown; `CONNECT`; back to air-gap; after a restart everything is at defaults.
- [ ] **Step 3: Fix** anything the walk finds in its owning file, with a test. Re-run that test file and commit `fix(ui): <what>`.
- [ ] **Step 4: Record the walk** (steps passed, any fixes) in `memory.md` and commit `docs: acceptance walk for P1d UI sub-project 3`.
