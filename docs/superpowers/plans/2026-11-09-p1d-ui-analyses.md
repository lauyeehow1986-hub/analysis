# P1d UI sub-project 2 (Analyses panel: run and results) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the Analyses panel. With it, a user runs the analyses of a locked SAP in a background worker, can cancel a run, sees an interrupted run after a crash, applies diagnostics, and reads every result, comparison, multiplicity adjustment and deviation without leaving the app.

**Architecture:**
- **Core (small).** `sr_run` records `run.started` (with a job id) before the fit; `sr_run_cancel` records `run.cancelled`; the run reducer derives a status and an attempt history. `sr_diag_apply` is split into a pure planner (`sr_diag_apply_plan`) and an executor of exactly the planned steps.
- **Worker layer** (`ui/analyses_job.R`). One queue item = one mirai worker that opens the project, runs `sr_run` then `sr_diag_run`, and closes it. Cancel kills the worker's process tree with `ps`. A pure `settle` step turns a finished item into the next project, a notice and a stop flag.
- **View layer** (`ui/analyses_view.R`). Pure functions from a project to the data frames each card shows.
- **Module** (`ui/mod_analyses.R`). Five cards, a queue in the main session, gating, and a navbar badge. `app.R` combines the import and run busy flags and passes them to every panel.

**Tech Stack:** R 4.5.2, shiny 1.14.0 (`ExtendedTask`), bslib, DT, mirai 2.5.3 (no daemons are set: each `mirai()` is its own ephemeral process), ps 1.9.1, callr 3.7.6, testthat 3 (`shiny::testServer`), withr.

**Spec:** [docs/superpowers/specs/2026-11-09-p1d-ui-analyses-design.md](../specs/2026-11-09-p1d-ui-analyses-design.md). It is the authority. Read it before any task. Section numbers below (§) refer to it.

## Global Constraints

- **Run commands from the worktree root** `<repo root>/.worktrees/p1d-ui-analyses`.
  - Rscript: `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"`.
  - Never run long multi-line code through `Rscript -e`; write a script file under the scratchpad instead.
- **Run a single test file** with:
  `Rscript -e "source('tests/testthat/helper-load.R'); testthat::test_file('tests/testthat/<file>')"`.
  The full suite (`testthat::test_dir('tests/testthat')`) is slow: run it in the background.
- **Naming.** Exported core functions `sr_`, internal `.sr_`, snake_case. Shared UI helpers `.sr_ui_*`; Analyses panel helpers `.sr_an_*`. Module pair `mod_analyses_ui(id)` / `mod_analyses_server(id, project, busy)`.
- **ASCII only** in every new or changed `.R` file. Build a non-ASCII glyph from code points (`intToUtf8(0x00B7)`).
- **Models never compute or write numbers.** The UI only rounds numbers taken from the result contract and the event state: `est`, `lo`, `hi`, `mcse`, `value`, `p_adj` with `signif(x, 4)`; p-values with `format.pval(p, digits = 3, eps = 1e-4)`; elapsed as `"<m> m <s> s"`.
- **Panels never cache project state.** Every card re-derives from `project()`. The only session state about a run is the live job (§2).
- **Scale (§6).** Cards read only the event state and small `fits/*.rds` files. No card calls `sr_sap_check` or scans DuckDB.
- **Gating.** Every run or apply control is rendered disabled when `.sr_an_can_run(p, busy)` returns a reason, and its handler refuses with the same reason. Core refusals are shown verbatim.
- **No network calls.** Options through `sr_opt(name, default)` only (new option: `sr.run_sync`, default `FALSE`).
- **No personal details in any committed file** (usernames, emails, hostnames, `C:\Users\<name>` paths). Before every commit: `git diff --cached | grep -inE "<your username>|users\\\\|@gmail"` must print nothing (put the real username in the pattern locally; never commit it).
- **Commits** end with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`. Never push.

## Review Focus

The spec pins five failure modes (§7). Each has a named test in the task that owns the code:

1. **Cancel racing completion:** the completed result stands, no `run.cancelled` is written, the user sees "finished before it could be cancelled". Task 1 (`sr_run_cancel refuses a completed run with class sr_run_stale`) and Task 3 (`settle: cancel that lost the race keeps the result and says so`).
2. **A crashed run shows as interrupted on reopen, and Run again works.** Task 1 (`a dangling run.started replays as running; a new run closes it as interrupted`), Task 4 (`status: running with no live job is interrupted`) and Task 5 (`a project reopened after a crash shows interrupted and Run selected completes it`).
3. **Cancel leaves no orphan callr process.** Task 3 (`kill: the worker and its callr child are both gone`, real processes, `skip_on_cran()`).
4. **A refusal stops the queue and shows the core text verbatim.** Task 5 (`a refusal stops Run all and is shown verbatim`).
5. **A sensitivity analysis added by Apply appears in the Runs table and runs.** Task 6 (`Apply adds m1_ph, the Runs table lists it as not run, and Run all runs it`).

Also likely to bite, and pinned: a PID reused by an unrelated process after the worker exited (Task 3, `kill: a recorded PID whose start time differs is not killed`); Cancel pressed before the worker wrote its PID (Task 3, `job: a cancel file present before the fit skips the run`).

---

### Task 1: Core run events: `run.started`, `run.cancelled`, status and history

**Files:**
- Modify: `app/R/routes/run.R` (header comment lines 14-16, reducer lines 42-61, `sr_run` lines 172-245)
- Create: `tests/testthat/helper-run.R` (move `.run_frame`, `.run_roles`, `.run_drive`, `.run_route`, `.run_spec`, `.run_drafted`, `.run_locked`, `.run_reopen`, `n_events`, `types_of` out of `test-run.R`, unchanged)
- Modify: `tests/testthat/test-run.R` (delete the moved helpers; add tests)

**Interfaces:**
- Produces:
  - `sr_run(project, analysis_id, timeout = 3600, job = NULL)` -> project. `job`: NULL (a new id from `.sr_run_job_id()`) or one string matching `^[A-Za-z0-9_.-]{1,64}$` (else `stop("job must be one string of letters, digits, '.', '_' or '-' (at most 64)")`).
  - `.sr_run_job_id()` -> `paste0("j", format(Sys.time(), "%Y%m%d%H%M%S"), substring(basename(tempfile("")), 5L))`. It must not touch `.Random.seed`.
  - `sr_run_cancel(project, analysis_id, job, reason = "cancelled by the user")` -> project.
  - Event `run.started` payload `list(analysis, key, extract_sha, job)`; `run.completed` payload gains `job`; event `run.cancelled` payload `list(analysis, job, reason)`.
  - `state$runs[[id]]` keeps every existing field and gains: `status` (`"running"`, `"ok"`, `"failed"`, `"cancelled"`), `job` (latest attempt's job, NA for old projects), `history` (list of `list(job, started, ended, outcome, reason)`; `started` / `ended` are event `ts` strings or NA; `outcome` is `"running"`, `"ok"`, `"failed"`, `"cancelled"` or `"interrupted"`; `reason` NA except for cancelled).
  - A stale cancel raises a condition of class `c("sr_run_stale", "error", "condition")` with message `"analysis '<id>' has no running job '<job>' (it finished or was cancelled already)"`.

- [ ] **Step 1: Move the `.run_*` helpers into `helper-run.R`** and run `test-run.R`. Expected: PASS, same count as before.

- [ ] **Step 2: Write the failing tests** in `test-run.R`:

```r
test_that("sr_run writes run.started with the job just before run.completed, and only after every refusal", {
  p <- .run_locked()
  n0 <- n_events(p)
  expect_error(sr_run(p, "nope", job = "j1"), "unknown analysis")
  expect_error(sr_run(p, "a1", job = "bad job!"), "job must be one string")
  expect_identical(n_events(p), n0)
  p <- sr_run(p, "a1", job = "j1")
  ty <- types_of(p)
  expect_identical(tail(ty, 2L), c("run.started", "run.completed"))
  ev <- tail(sr_events_read(p$paths$events), 2L)
  expect_identical(ev[[1]]$payload$job, "j1")
  expect_identical(ev[[2]]$payload$job, "j1")
  expect_identical(ev[[1]]$payload$key, ev[[2]]$payload$key)
  r <- p$state$runs$a1
  expect_identical(r$status, "ok")
  expect_identical(r$job, "j1")
  expect_length(r$history, 1L)
  expect_identical(r$history[[1]]$outcome, "ok")
  expect_false(is.na(r$history[[1]]$started))
})

test_that("a NULL job gets a generated id; the generator leaves the RNG alone", {
  set.seed(1); before <- .Random.seed
  j <- .sr_run_job_id()
  expect_identical(.Random.seed, before)
  expect_match(j, "^j[0-9]{14}[0-9a-f]+$")
  p <- sr_run(.run_locked(), "a1")
  expect_match(p$state$runs$a1$job, "^j[0-9]{14}[0-9a-f]+$")
})

test_that("a dangling run.started replays as running; a new run closes it as interrupted", {
  p <- .run_locked()
  p <- sr_project_append(p, "run.started", list(analysis = "a1", key = "k0", extract_sha = "s0", job = "jdead"))
  q <- .run_reopen(p)
  expect_identical(q$state$runs$a1$status, "running")
  expect_identical(q$state$runs$a1$job, "jdead")
  q <- sr_run(q, "a1", job = "j2")
  h <- q$state$runs$a1$history
  expect_identical(vapply(h, `[[`, "", "outcome"), c("interrupted", "ok"))
  expect_identical(q$state$runs$a1$status, "ok")
})

test_that("sr_run_cancel records run.cancelled and keeps the earlier good result", {
  p <- sr_run(.run_locked(), "a1", job = "j1")
  good <- p$state$runs$a1$ok_key
  p <- sr_project_append(p, "run.started", list(analysis = "a1", key = "k2", extract_sha = "s2", job = "j2"))
  p <- sr_run_cancel(p, "a1", "j2")
  r <- p$state$runs$a1
  expect_identical(r$status, "cancelled")
  expect_identical(r$ok_key, good)
  expect_identical(tail(r$history, 1L)[[1]]$outcome, "cancelled")
  expect_identical(tail(r$history, 1L)[[1]]$reason, "cancelled by the user")
  expect_s3_class(sr_result(p, "a1")$effects, "data.frame")
  expect_identical(.run_reopen(p)$state$runs, p$state$runs)
})

test_that("sr_run_cancel refuses a read-only project, an unknown analysis and a stale job, writing nothing", {
  p <- sr_run(.run_locked(), "a1", job = "j1")
  n0 <- n_events(p)
  expect_error(sr_run_cancel(p, "nope", "j1"), "unknown analysis")
  ro <- p; ro$readonly <- TRUE
  expect_error(sr_run_cancel(ro, "a1", "j1"), "read-only")
  expect_identical(n_events(p), n0)
})

test_that("sr_run_cancel refuses a completed run with class sr_run_stale", {
  p <- sr_run(.run_locked(), "a1", job = "j1")
  e <- tryCatch(sr_run_cancel(p, "a1", "j1"), error = function(e) e)
  expect_s3_class(e, "sr_run_stale")
  expect_match(conditionMessage(e), "no running job 'j1'")
  p <- sr_project_append(p, "run.started", list(analysis = "a1", key = "k", extract_sha = "s", job = "j2"))
  expect_s3_class(tryCatch(sr_run_cancel(p, "a1", "j1"), error = function(e) e), "sr_run_stale")
})

test_that("a project with run.completed only (no job, no run.started) replays to ok / failed", {
  p <- .run_locked()
  p <- sr_project_append(p, "run.completed", list(analysis = "a1", key = "k", extract_sha = "s", ok = FALSE,
                                                  cached = FALSE, error = "boom", elapsed = 1))
  r <- .run_reopen(p)$state$runs$a1
  expect_identical(r$status, "failed")
  expect_true(is.na(r$job))
  expect_identical(r$history[[1]]$outcome, "failed")
  expect_true(is.na(r$history[[1]]$started))
})
```

- [ ] **Step 3: Run `test-run.R` to verify the new tests fail.** Expected: FAIL (`unused argument (job = ...)`, `could not find function "sr_run_cancel"`).

- [ ] **Step 4: Implement in `app/R/routes/run.R`.**
  - Reducers `run.started`, `run.completed` (rewritten) and `run.cancelled`. Each reads the previous `runs[[id]]` and carries forward every field it does not set, including `history`. `run.started`: status `"running"`, `job`; if the last history entry's outcome is `"running"`, set it to `"interrupted"`; append `list(job, started = event$ts, ended = NA, outcome = "running", reason = NA)`. `run.completed`: today's fields plus `status`; if the last history entry is `"running"` with the same job, close it (`ended`, `outcome`); otherwise append a closed entry with `started = NA`. `run.cancelled`: `status = "cancelled"`, close the matching running entry with `outcome = "cancelled"` and `reason`. `ok`, `key`, `error`, `elapsed` are not touched by `run.started` / `run.cancelled`.
  - `sr_run`: validate `job` first (with the other argument checks); append `run.started` after `key` is computed and immediately before `sr_exec_cached`; add `job` to the `run.completed` payload.
  - `sr_run_cancel`: refusals in this order: read-only, `.sr_sap_id`, unknown analysis, stale (`status` not `"running"` or `job` differs) via `stop(structure(class = c("sr_run_stale", "error", "condition"), list(message = ..., call = NULL)))`.
  - Update the header comment's `State:` lines to the new fields.

- [ ] **Step 5: Run `test-run.R`, then `test-deviations.R`, `test-freeze.R`, `test-archive-diff.R`, `test-writer-model.R`** (they read `state$runs`). Expected: all PASS. If a file name differs, `ls tests/testthat | grep -E "freeze|diff|writer-model"`.

- [ ] **Step 6: Commit** `feat(run): record run.started and run.cancelled; run status and attempt history`.

---

### Task 2: `sr_diag_apply_plan` and the family members helper

**Files:**
- Modify: `app/R/sap/deviations.R` (`sr_diag_apply` lines 309-379, `sr_family_adjust` lines 441+)
- Create: `tests/testthat/helper-deviations.R` (move `.dv_layers`, `.dv_mod`, `.dv_cox`, `.dv_lm`, `.dv_logit`, `.dv_routes`, `.dv_surv`, `.dv_project` out of `test-deviations.R`, unchanged)
- Modify: `tests/testthat/test-deviations.R`

**Interfaces:**
- Produces:
  - `.sr_diag_apply_steps(project, analysis_id)` -> list of steps, exactly today's `plan` list (`type` = `"rule"` with `check`, `rule`, `why`; `"limitation"` with `check`, `reason`; `"add"` with `check`, `id`, `route`, `spec`, `reason`), with every refusal `sr_diag_apply` raises today, including the taken-id refusal. New: a limitation whose `reason` is identical to the one already recorded for that check is not a step.
  - `sr_diag_apply_plan(project, analysis_id)` -> `data.frame(type, check, target, route, reason, stringsAsFactors = FALSE)`, one row per step: `target` = rule id / new analysis id / NA; `route` = the new analysis's route or NA; `reason` = `why` for a rule. Zero rows with those columns when nothing is to be done.
  - `sr_diag_apply(project, analysis_id)` executes `.sr_diag_apply_steps()` and nothing else (behaviour unchanged).
  - `.sr_family_used(project, family_id)` -> the member ids after `replace_primary` substitution (today's `use` vector); `sr_family_adjust` calls it.

- [ ] **Step 1: Move the `.dv_*` helpers into `helper-deviations.R`** and run `test-deviations.R`. Expected: PASS.

- [ ] **Step 2: Write the failing tests** in `test-deviations.R`:

```r
test_that("sr_diag_apply_plan previews exactly what sr_diag_apply then does, and is empty afterwards", {
  p <- .dv_project()
  p <- sr_sap_add(p, "m1", "cox", .dv_cox(terms = list(list(var = "z"))), "t", label = "primary")
  p <- sr_diag_run(sr_run(sr_sap_lock(.fx_feasible(p)), "m1"), "m1")
  plan <- sr_diag_apply_plan(p, "m1")
  expect_identical(names(plan), c("type", "check", "target", "route", "reason"))
  expect_true("m1_ph" %in% plan$target[plan$type == "add"])
  n0 <- length(sr_events_read(p$paths$events))
  p <- sr_diag_apply(p, "m1")
  new <- tail(sr_events_read(p$paths$events), -n0)
  expect_length(new, nrow(plan))
  expect_identical(vapply(new, `[[`, "", "type"),
                   unname(c(rule = "sap.rule_fired", add = "analysis.added",
                            limitation = "diag.limitation")[plan$type]))
  expect_identical(nrow(sr_diag_apply_plan(p, "m1")), 0L)
})

test_that("sr_diag_apply_plan has the refusals of sr_diag_apply and writes nothing", {
  p <- .dv_project()
  p <- sr_sap_add(p, "m1", "cox", .dv_cox(), "t", label = "primary")
  expect_error(sr_diag_apply_plan(p, "m1"), "not locked")
  p <- sr_sap_lock(.fx_feasible(p))
  expect_error(sr_diag_apply_plan(p, "nope"), "unknown analysis")
  expect_error(sr_diag_apply_plan(p, "m1"), "sr_diag_run")
})
```

  Also add a taken-id case to the existing taken-id test: `expect_error(sr_diag_apply_plan(p, "m1"), "is taken by another analysis")`.

- [ ] **Step 3: Run `test-deviations.R` to verify the new tests fail.** Expected: FAIL (`could not find function "sr_diag_apply_plan"`).

- [ ] **Step 4: Implement** the split and `.sr_family_used` in `deviations.R`. `sr_diag_apply` keeps the read-only refusal before planning (the plan itself does not refuse read-only, so the panel can preview on a read-only copy).

- [ ] **Step 5: Run `test-deviations.R` and `test-diagnostics.R`.** Expected: PASS.

- [ ] **Step 6: Commit** `feat(sap): sr_diag_apply_plan previews exactly the steps sr_diag_apply executes`.

---

### Task 3: The worker layer (`ui/analyses_job.R`)

**Files:**
- Create: `app/R/ui/analyses_job.R`
- Create: `tests/testthat/helper-analyses.R`
- Create: `tests/testthat/test-analyses-job.R`

**Interfaces:**
- Consumes: `sr_run(..., job)`, `sr_run_cancel`, class `sr_run_stale` (Task 1); `.sr_run_*` helpers (Task 1).
- Produces:
  - `.sr_an_job(p, id, job, progress_file = NULL, job_file = NULL, cancel_file = NULL)` -> `list(status, message, project)`. `status` is `"ok"` (fit ok and diagnostics run), `"failed"` (fit recorded with ok = FALSE; `message` = the run's `error`), `"refused"` (`sr_run` or `sr_diag_run` raised; `message` = its text, prefixed `"diagnostics: "` for the latter) or `"skipped"` (the cancel file existed before the fit). Steps: write `"<pid> <create_time>"` to `job_file` (pid = `Sys.getpid()`, create_time = `ps::ps_create_time(ps::ps_handle())` as numeric seconds); return skipped if `cancel_file` exists; write `"fitting"` to `progress_file`; `sr_run(p, id, job = job)`; on ok write `"diagnostics"` and `sr_diag_run(p, id)`.
  - `.sr_an_spawn(app_dir, project_dir, id, job, progress_file, job_file, cancel_file)` -> a mirai. Same shape as `.sr_data_spawn` (`app/R/ui/mod_data.R:198-223`): source `global.R`, `sr_project_open`, read-only gives `list(status = "refused", message = "the project is locked by another session")`, any other error is `"refused"` with its message, close the project, drop `$project`, unlink the three files.
  - `.sr_an_kill(job_file, wait = 10)` -> `TRUE` when it killed a process, `FALSE` when no live matching process was found. Waits up to `wait` seconds for `job_file`; refuses to kill when the PID's `ps_create_time` differs from the recorded one by more than 0.01 s; kills `ps::ps_children(h, recursive = TRUE)` then `h` (each `ps::ps_kill` in `tryCatch`, ignoring `no_such_process`); then waits up to `wait` seconds until none of them is running (`.sr_pid_running`, `app/R/core/project.R:116`).
  - `.sr_an_settle(p, id, job, res, cancelling)` -> `list(project, notice, type, stop)`. `notice` NULL or one string; `type` one of `"message"`, `"warning"`, `"error"`. Rules (in order):
    - `cancelling`: try `sr_run_cancel(p, id, job)` -> notice `"Cancelled <id>."`, message. Class `sr_run_stale` and `res$status == "skipped"` -> `"Cancelled before <id> started."`, message. Class `sr_run_stale` otherwise -> `"<id> finished before it could be cancelled; if its diagnostics were cut short, run it again to complete them."`, warning. Any other error -> `"Cancel <id>: <message>"`, error. `stop = TRUE` always.
    - `"ok"`: no notice, `stop = FALSE`. `"failed"`: `"<id> failed: <message>"`, warning, `stop = FALSE`. `"refused"`: `"Run stopped at <id>: <message>"`, error, `stop = TRUE`. `"skipped"` (not cancelling): no notice, `stop = TRUE`.
  - Fixtures in `helper-analyses.R`: `.fx_an_project(body_fn = NULL, env = parent.frame())` -> `.run_drafted("stub_run", body_fn, env)` plus `sr_sap_add(p, "a2", "stub_run", .run_spec(), "t", label = "secondary")` and `sr_sap_add(p, "s1", "stub_run", .run_spec(), "t", label = "sensitivity", parent = "a1")`, then `sr_sap_lock(.fx_feasible(p))`. These stub routes exist only in the test session, so every test that spawns a real worker uses the registered `lm` / `cox` routes on `.dv_project()` data instead.

- [ ] **Step 1: Write the failing tests** in `test-analyses-job.R`:

```r
test_that("job: a fast analysis runs, records the job and reports ok", {
  p <- .fx_an_project()
  pf <- withr::local_tempfile(); jf <- withr::local_tempfile()
  res <- .sr_an_job(p, "a1", "j1", progress_file = pf, job_file = jf)
  expect_identical(res$status, "ok")
  expect_identical(res$project$state$runs$a1$job, "j1")
  expect_identical(readLines(pf), "diagnostics")
  expect_match(readLines(jf), sprintf("^%d [0-9.]+$", Sys.getpid()))
})

test_that("job: a failed fit is 'failed' with the run's error; a refusal is 'refused' verbatim", {
  p <- .fx_an_project(body_fn = function(spec) "stop('boom')")
  res <- .sr_an_job(p, "a1", "j1")
  expect_identical(res$status, "failed")
  expect_match(res$message, "boom")
  res <- .sr_an_job(p, "nope", "j2")
  expect_identical(res$status, "refused")
  expect_identical(res$message, "unknown analysis 'nope'")
})

test_that("job: a cancel file present before the fit skips the run", {
  p <- .fx_an_project()
  cf <- withr::local_tempfile(); file.create(cf)
  res <- .sr_an_job(p, "a1", "j1", cancel_file = cf)
  expect_identical(res$status, "skipped")
  expect_null(p$state$runs$a1)
})

test_that("settle: ok continues; failed warns and continues; refused stops with the text verbatim", { ... })
test_that("settle: cancel of a running job records run.cancelled and stops", { ... })
test_that("settle: cancel that lost the race keeps the result and says so", {
  p <- sr_run(.fx_an_project(), "a1", job = "j1")
  n0 <- length(sr_events_read(p$paths$events))
  out <- .sr_an_settle(p, "a1", "j1", list(status = "ok"), cancelling = TRUE)
  expect_identical(length(sr_events_read(out$project$paths$events)), n0)
  expect_identical(out$project$state$runs$a1$status, "ok")
  expect_match(out$notice, "finished before it could be cancelled")
  expect_identical(out$type, "warning")
  expect_true(out$stop)
})

test_that("spawn: the worker opens the project itself, runs an lm analysis and releases the lock", {
  skip_on_cran(); skip_if_not_installed("mirai")
  # a locked project with a real lm analysis (.dv_project data, .dv_lm spec with outcome = "time")
  ...
  m <- .sr_an_spawn(sr_root, dir, "l1", "jw1", pf, jf, cf)
  res <- mirai::call_mirai(m)$data
  expect_false(mirai::is_error_value(res))
  expect_identical(res$status, "ok")
  q <- sr_project_open(dir); withr::defer(sr_project_close(q))
  expect_false(q$readonly)
  expect_identical(q$state$runs$l1$job, "jw1")
  expect_false(any(file.exists(c(pf, jf, cf))))
})

test_that("kill: the worker and its callr child are both gone", {
  skip_on_cran(); skip_if_not_installed("mirai")
  jf <- withr::local_tempfile()
  m <- mirai::mirai({
    h <- ps::ps_handle()
    writeLines(paste(Sys.getpid(), format(as.numeric(ps::ps_create_time(h)), digits = 15)), jf)
    callr::r(function() Sys.sleep(120))
  }, jf = jf)
  # wait (up to 60 s) for the job file and one child process of the worker
  ...
  pids <- c(worker, child)
  expect_true(.sr_an_kill(jf))
  expect_false(any(vapply(pids, .sr_pid_running, NA)))
  expect_true(mirai::is_error_value(mirai::call_mirai(m)$data))
})

test_that("kill: a recorded PID whose start time differs is not killed", {
  jf <- withr::local_tempfile()
  writeLines(paste(Sys.getpid(), "1"), jf)        # this very process, with a wrong start time
  expect_false(.sr_an_kill(jf, wait = 1))
  expect_true(.sr_pid_running(Sys.getpid()))
})
```

  Fill the `...` bodies with the assertions their names state; the refused-verbatim test asserts `out$notice == "Run stopped at a1: <exact message>"`.

- [ ] **Step 2: Run `test-analyses-job.R` to verify it fails.** Expected: FAIL (`could not find function ".sr_an_job"`).

- [ ] **Step 3: Implement `app/R/ui/analyses_job.R`** and `helper-analyses.R` per the Interfaces block. The file header states the worker protocol: the three files, the statuses, and that the session never writes to a project it released.

- [ ] **Step 4: Run `test-analyses-job.R`.** Expected: PASS (the two real-process tests take under a minute).

- [ ] **Step 5: Commit** `feat(ui): analyses worker, process-tree cancel and settle`.

---

### Task 4: The view layer (`ui/analyses_view.R`)

**Files:**
- Create: `app/R/ui/analyses_view.R`
- Create: `tests/testthat/test-analyses-view.R`

**Interfaces:**
- Consumes: Task 1 state fields, `sr_diag_apply_plan` and `.sr_family_used` (Task 2), `sr_result`, `sr_deviations`, `sr_family_adjust`, `sr_project_stopped`, `SR_PLOT_KINDS`.
- Produces (all pure; `live` is a named character vector `c(<analysis id> = <job>)` of the run in progress, `character()` when none):
  - `.sr_an_order(p)` -> `data.frame(id, depth)`: primary analyses then secondary, each by id (radix), each followed depth-first by its children (`parent == id`; pre-specified before unplanned, then by id); then every analysis not yet placed, by id, at depth 0.
  - `.sr_an_status(p, live = character())` -> named character, one per analysis: `"running"` when the id is in `names(live)`; else `"not run"` with no run state; else `r$status` (`"ok"` / `"failed"` when an old state has no status), with `"running"` turned into `"interrupted"`.
  - `.sr_an_runs_table(p, live = character())` -> `data.frame(id, label, route, status, last_run, elapsed, cached, diagnostics)`; `id` is indented by `strrep("  ", depth)` plus `"- "` when depth > 0; `last_run` = the latest history entry's `ended` (or `started`), `""` when none; `diagnostics` = `"<n> ok, <n> warn, <n> fail"` over `state$diag[[id]]` (zero counts omitted; `""` when none).
  - `.sr_an_queue(p, mode = c("selected", "all"), selected = NULL)` -> character ids: `"selected"` gives `selected` (must be a known analysis); `"all"` gives the `.sr_an_order()` ids whose `.sr_an_status()` is not `"ok"`.
  - `.sr_an_can_run(p, busy = FALSE)` -> NULL or the reason, checked in this order: `"open a project first"`, `"the project is read-only (locked by another session)"`, `"wait for the running import or analysis to finish"`, `"lock the SAP first"`, `"the project is stopped: <reason>"`.
  - `.sr_an_effects(presented)` -> the effects frame sorted primary, covariate, descriptive (stable), numbers rounded per the Global Constraints, plus a `flags` column (`paste(flag, collapse = "; ")` of `presented$flags` rows with the same term, `""` otherwise).
  - `.sr_an_figures(presented)` -> named list of `list(kind, data)`: `km` -> `list(kind = "km", data = list(km = figures$km, km_risk = figures$km_risk))` (km_risk omitted when absent); `km_risk` is not drawn alone; any other name in `SR_PLOT_KINDS` -> that kind; other names are dropped.
  - `.sr_an_diag_rows(p, id)` -> `data.frame(check, verdict, value, warn, fail, detail)` in `SR_DIAG_IDS` order.
  - `.sr_an_diag_plot(p, id, check)` -> `list(kind, data)` or NULL: ph -> `"schoenfeld"`, linearity -> `"linearity"`; data = `readRDS(file.path(p$paths$fits, paste0(key, ".rds")))$value$plot_data` (NULL on any read error or an empty frame).
  - `.sr_an_plan_words(p, plan)` -> character, one per plan row: rule `"fire rule <rule> (<action> -> <alternative>)"`, add `"add sensitivity <target> (<route>)"`, limitation `"record limitation: <reason>"`; ids in backticks.
  - `.sr_an_compare(p, live = character())` -> `data.frame(block, analysis, role, status, measure, term, est, lo, hi)`: one block per primary / secondary analysis (the `.sr_an_order` order): its primary-role rows of its first primary measure (`role = "main"`), then for each child its rows of that measure (`role = "sensitivity"`); an analysis without an ok result gives one row with its status and NA numbers; a fired `replace_primary` marks the alternative `"now primary"` and the replaced one `"replaced"`.
  - `.sr_an_families(p)` -> list per family: `list(id, method, table, waiting)`: `table` = `sr_family_adjust()` rounded, or NULL; `waiting` = the `.sr_family_used()` ids without an ok result.
  - `.sr_an_history(p, live = character())` -> `data.frame(analysis, job, started, ended, outcome, reason)` of every history entry whose outcome is `"cancelled"` or `"interrupted"`, plus an open `"running"` entry not in `live` shown as `"interrupted"`; `ended` = `"interrupted"` for those.

- [ ] **Step 1: Write the failing tests** in `test-analyses-view.R`, one per function above. Pin at least:

```r
test_that("status: running with no live job is interrupted; a live job is running", {
  p <- .fx_an_project()
  p <- sr_project_append(p, "run.started", list(analysis = "a1", key = "k", extract_sha = "s", job = "j9"))
  expect_identical(.sr_an_status(p)[["a1"]], "interrupted")
  expect_identical(.sr_an_status(p, c(a1 = "j9"))[["a1"]], "running")
  expect_identical(.sr_an_status(p)[["a2"]], "not run")
  expect_identical(.sr_an_history(p)$outcome, "interrupted")
})

test_that("order: primary, its sensitivity children indented, then secondary", {
  p <- .fx_an_project()
  o <- .sr_an_order(p)
  expect_identical(o$id, c("a1", "s1", "a2"))
  expect_identical(o$depth, c(0L, 1L, 0L))
  expect_identical(.sr_an_runs_table(p)$id[2], "  - s1")
})

test_that("queue: Run all skips ok analyses and keeps SAP order", {
  p <- sr_run(.fx_an_project(), "s1")
  expect_identical(.sr_an_queue(p, "all"), c("a1", "a2"))
  expect_identical(.sr_an_queue(p, "selected", "s1"), "s1")
})

test_that("can_run: each reason in order", {
  p <- .fx_an_project()
  expect_null(.sr_an_can_run(p))
  expect_identical(.sr_an_can_run(NULL), "open a project first")
  expect_identical(.sr_an_can_run(p, busy = TRUE), "wait for the running import or analysis to finish")
  ro <- p; ro$readonly <- TRUE
  expect_identical(.sr_an_can_run(ro), "the project is read-only (locked by another session)")
})

test_that("families: a family with an unrun member says what it waits for", { ... })
test_that("plan words: rule, add and limitation", { ... })     # build the plan data.frame by hand
test_that("effects: primary rows first, flags inline, numbers rounded", { ... })
test_that("figures: km carries km_risk; unknown names are dropped", { ... })
```

  Add the "lock the SAP first" and "stopped" reasons to the can_run test using `.run_drafted()` and a feasibility decision of stop (`sr_feasibility_decide`, see `test-feasibility.R` for the call).

- [ ] **Step 2: Run `test-analyses-view.R` to verify it fails.** Expected: FAIL.

- [ ] **Step 3: Implement `app/R/ui/analyses_view.R`.**

- [ ] **Step 4: Run `test-analyses-view.R`.** Expected: PASS.

- [ ] **Step 5: Commit** `feat(ui): pure view builders for the Analyses panel`.

---

### Task 5: `mod_analyses`: Runs card, queue, Cancel, gating

**Files:**
- Create: `app/R/ui/mod_analyses.R`
- Create: `tests/testthat/test-mod-analyses.R`

**Interfaces:**
- Consumes: Tasks 3 and 4; `.sr_ui_card`, `.sr_ui_dt`, `.sr_ui_notify`, `.sr_ui_try`, `.sr_ui_readonly_banner`, `.sr_data_same_dir` (existing).
- Produces:
  - `mod_analyses_ui(id)`: cards in this order: Runs, Result, Primary vs sensitivity, Multiplicity, Deviations and run history. Runs card: `DT` output `runs` (single row selection), buttons `run_sel`, `run_all`, `cancel` (rendered by `uiOutput("run_btns")`), `uiOutput("progress")`.
  - `mod_analyses_server(id, project, busy)` -> `list(running = shiny::reactive(...))`. `busy` is a reactive (the import). `running` is TRUE while a queue item runs or ids are queued.
  - `output$badge`: `shiny::span(class = "badge text-bg-warning", "running")` while running, else nothing (the navbar title of Task 8 shows it as `analyses-badge`).
  - Session state (a `shiny::reactiveValues` named `run`): `queue`, `total`, `index`, `id`, `job`, `t0`, `dir`, `name`, `progress`, `job_file`, `cancel_file`, `cancelling`. Selection: `sel`, a `reactiveVal` holding an analysis id (from `input$runs_rows_selected` mapped through the current order; kept across re-renders while the id exists).
- Behaviour:
  - `run_sel` / `run_all`: refuse with `.sr_an_can_run(p, busy() || running())` (notified `"Run: <reason>"`); `run_sel` with no selection: `"Run: select an analysis first"`. Otherwise set the queue from `.sr_an_queue()`; `"Run all: every analysis has run successfully"` when it is empty; then `start_next()`.
  - `start_next()`: empty queue -> clear `run$id`. Else pop the head; re-check `.sr_an_can_run`; new `job <- .sr_run_job_id()`.
    - `sr_opt("run_sync", FALSE)` TRUE: `res <- .sr_an_job(p, id, job)`; `project(res$project %||% p)`; settle; continue.
    - Else: temp files `sr-an-progress-`, `sr-an-job-`, `sr-an-cancel-`; release the lock and set the read-only copy exactly as `mod_data.R:568-575`; `task$invoke(...)` with `.sr_an_spawn`; on an invoke error reopen and stop.
  - `task$status()` observer (success or error): `res` = `task$result()`, or `list(status = "refused", message = <condition message>)` on error, or on a mirai error value. Reopen the project as `mod_data.R:602-613` does (only the same folder, only the read-only copy). Then `.sr_an_settle()`, `project(out$project)`, notify, clear the queue when `out$stop`, `start_next()`.
  - `cancel`: `"Cancel: nothing is running"` when idle. Else `run$cancelling <- TRUE`, `run$queue <- character()`, `file.create(cancel_file)`, `.sr_an_kill(job_file)`. The status observer then settles with `cancelling = TRUE`.
  - `session$onSessionEnded`: nothing is cancelled (the worker finishes and records, §4.2); the queue is dropped with the session.
  - Progress (`renderUI`, `invalidateLater(1000)` while running): `"Running <index> of <total>: <id>, <step> (<elapsed>)"`; step from the progress file (`"fitting"` / `"diagnostics"`, `"starting"` before it exists).
  - Buttons rendered with `disabled` when `.sr_an_can_run()` gives a reason; Cancel enabled only while running.

- [ ] **Step 1: Write the failing tests** in `test-mod-analyses.R` (option `sr.run_sync = TRUE` unless stated). Use `.ui_spy_notify()` and `.fx_an_project()`:

```r
test_that("Run all runs every analysis not ok, in SAP order", { ... })          # runs a1, s1, a2; all ok
test_that("Run selected re-runs an ok analysis and records new events", { ... })
test_that("a failed fit is reported and the queue goes on", { ... })
  # .fx_an_project(body_fn = function(spec) "stop('boom')"): Run all runs a1, s1 (parent failed) and a2;
  # all three status "failed"; three warnings "<id> failed: ..."; running() FALSE at the end
test_that("a refusal stops Run all and is shown verbatim", {
  withr::local_options(sr.run_sync = TRUE)
  seen <- .ui_spy_notify()
  old <- .sr_an_job
  assign(".sr_an_job", function(p, id, job, ...) {
    if (id == "s1") return(list(status = "refused", message = "the SAP is not locked", project = p))
    old(p, id, job, ...)
  }, envir = globalenv())
  withr::defer(assign(".sr_an_job", old, envir = globalenv()))
  rv <- shiny::reactiveVal(.fx_an_project())
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(run_all = 1)
    expect_true(any(seen$msg == "Run stopped at s1: the SAP is not locked"))
    expect_identical(rv()$state$runs$a1$status, "ok")
    expect_null(rv()$state$runs$a2)
    expect_false(running())
  })
})
test_that("Run is refused while busy, read-only, unlocked or stopped, and the buttons are disabled", { ... })
test_that("a project reopened after a crash shows interrupted and Run selected completes it", { ... })
test_that("the badge shows while a queue runs and Cancel when idle says nothing is running", { ... })
test_that("Run all in the background reopens the project after each analysis", {
  skip_on_cran(); skip_if_not_installed("mirai"); skip_if_not_installed("later")
  withr::local_options(sr.run_sync = FALSE)
  # a locked lm project (as in test-analyses-job.R spawn test) with two analyses; loop later::run_now(0.2)
  # until !running() (90 s cap), then: both ok, rv() not read-only, the lock is this session's
  ...
})
```

  The crash test: append a dangling `run.started` for `a1`, re-open the folder (`.run_reopen`), check `output$runs` contains `interrupted`, select row 1, `run_sel`, check status `ok` and the history's first outcome `interrupted`.

- [ ] **Step 2: Run `test-mod-analyses.R` to verify it fails.** Expected: FAIL.

- [ ] **Step 3: Implement the module** (Runs card only; the other four cards are empty `.sr_ui_card` shells filled by Tasks 6-7).

- [ ] **Step 4: Run `test-mod-analyses.R`.** Expected: PASS.

- [ ] **Step 5: Commit** `feat(ui): Analyses panel runs card with a background queue and cancel`.

---

### Task 6: Result card with diagnostics and Apply

**Files:**
- Modify: `app/R/ui/mod_analyses.R`
- Modify: `tests/testthat/test-mod-analyses.R`

**Interfaces:**
- Consumes: `.sr_an_effects`, `.sr_an_figures`, `.sr_an_diag_rows`, `.sr_an_diag_plot`, `.sr_an_plan_words` (Task 4); `sr_result`, `sr_diag_apply_plan`, `sr_diag_apply`, `sr_plot`.
- Produces: outputs `result_head` (uiOutput), `effects` (DT), `tables` (uiOutput; one DT per `presented$tables` entry, titled by its name), `figs` (uiOutput of `plotOutput(ns("fig_<name>"))`, each filled by `renderPlot(sr_plot(data, kind))`), `fragments` (uiOutput; each fragment in a `<pre>`, escaped, placeholders left as written), `diag` (DT), `diag_figs` (as `figs`, ids `dfig_<check>`), `apply_preview` (uiOutput), button `apply`.
- Behaviour (§4.3):
  - No selection: `"Select an analysis in the Runs table."`.
  - Header: status badge, elapsed, `cached` / `fresh`; `lib_changed` -> `"The library snapshot of this run differs from the one recorded at the SAP lock."`; status failed -> the run's `error` verbatim in a `<pre>`. A not-ok analysis with an `ok_key` shows the good result under `"Showing the last good run (<ended time of the latest ok history entry>)."`. No `ok_key`: `"No successful run yet."` and nothing below.
  - `sr_result` errors (stored result missing) show their message verbatim instead of the result.
  - Apply preview: `tryCatch(sr_diag_apply_plan(p, id))`; an error is shown verbatim; zero rows -> `"Nothing to apply."`; else a `<ul>` of `.sr_an_plan_words()`. The `apply` button is disabled when `.sr_an_can_run()` gives a reason or the plan has no rows; its handler re-checks both, then `act("Apply diagnostics", function(p) sr_diag_apply(p, id))` via `.sr_ui_actor(project, busy = <busy or running>)`.

- [ ] **Step 1: Write the failing tests:**

```r
test_that("the result card shows effects, tables, figures, fragments and diagnostics of a cox run", { ... })
  # .dv_project() + m1 cox as in test-deviations.R; run (sync); select m1; expect output$effects has HR,
  # output$figs has "fig_km", output$diag has "ph" and "fail", output$apply_preview mentions `m1_ph`
test_that("a failed run shows its error verbatim and the last good result labelled", { ... })
test_that("Apply adds m1_ph, the Runs table lists it as not run, and Run all runs it", {
  # after run + Apply: rv()$state$analyses$m1_ph exists; output$runs contains "m1_ph" and "not run";
  # run_all; rv()$state$runs$m1_ph$status == "ok"; output$apply_preview says "Nothing to apply."
})
test_that("Apply is refused on a read-only project and writes nothing", { ... })
```

- [ ] **Step 2: Run to verify the new tests fail.** Expected: FAIL.

- [ ] **Step 3: Implement the card.**

- [ ] **Step 4: Run `test-mod-analyses.R`.** Expected: PASS.

- [ ] **Step 5: Commit** `feat(ui): Analyses result card with diagnostics and apply preview`.

---

### Task 7: Comparison, multiplicity, deviations and history cards

**Files:**
- Modify: `app/R/ui/mod_analyses.R`
- Modify: `tests/testthat/test-mod-analyses.R`

**Interfaces:**
- Consumes: `.sr_an_compare`, `.sr_an_families`, `.sr_an_history` (Task 4), `sr_deviations`.
- Produces: outputs `compare` (DT; `"No primary or secondary analysis."` when empty), `families` (uiOutput: per family a title `"<id> (<method>)"` and a DT, or `"Waiting for: a, b"`; `"No multiplicity family in the SAP."` when none), `deviations` (DT; `"No deviation from the locked SAP."` when empty), `history` (DT; `"No cancelled or interrupted run."` when empty).

- [ ] **Step 1: Write the failing tests:**

```r
test_that("compare shows each sensitivity analysis under its parent, never blank", { ... })   # s1 not run -> "not run"
test_that("a family waits for its unrun members, then matches sr_family_adjust", { ... })     # sr_sap_family a1, a2 holm
test_that("deviations match sr_deviations and history lists a cancelled attempt", { ... })
```

- [ ] **Step 2: Run to verify they fail.** Expected: FAIL.

- [ ] **Step 3: Implement the three cards.** The Multiplicity table's numbers must equal `sr_family_adjust()` after the rounding of the Global Constraints (test with `expect_equal(..., signif(fa$p_adj, 4))`).

- [ ] **Step 4: Run `test-mod-analyses.R`.** Expected: PASS.

- [ ] **Step 5: Commit** `feat(ui): primary vs sensitivity, multiplicity, deviations and run history cards`.

---

### Task 8: App wiring and the app-wide busy flag

**Files:**
- Modify: `app/app.R` (lines 1-56)
- Modify: `app/R/ui/mod_data.R` (`mod_data_server` signature line 421, `act` line 440, import refusal line 547)
- Modify: `app/R/ui/util.R:54`
- Modify: `tests/testthat/test-ui-util.R:11`, `tests/testthat/test-mod-design.R:111`, `tests/testthat/test-mod-sap.R:187` (new busy text)
- Modify: `tests/testthat/test-app-wiring.R`, `tests/testthat/test-mod-data.R`

**Interfaces:**
- Consumes: `mod_analyses_ui`, `mod_analyses_server` (Task 5).
- Produces:
  - `mod_data_server(id, project, busy = shiny::reactive(FALSE))`: `busy` is the run in progress. Data's `act` and Import refuse when `job$busy || busy()`; it still returns `list(busy = <the import only>)`.
  - `.sr_ui_refusal` busy text: `"wait for the running import or analysis to finish"`.
  - `app.R`: `.sr_panel_names` loses `"Analyses"`. The Analyses tab is `bslib::nav_panel(shiny::span("Analyses ", shiny::uiOutput("analyses-badge", inline = TRUE)), mod_analyses_ui("analyses"), value = "Analyses")` after SAP, and is not in `fillable`. Server:

```r
run_busy <- shiny::reactiveVal(FALSE)
d <- mod_data_server("data", project, run_busy)
a <- mod_analyses_server("analyses", project, d$busy)
shiny::observe(run_busy(isTRUE(a$running())))
any_busy <- shiny::reactive(isTRUE(d$busy()) || isTRUE(a$running()))
mod_design_server("design", project, any_busy)
mod_sap_server("sap", project, any_busy)
```

  and the show/hide observer covers `c("Data", "Design", "SAP", "Analyses")`. Update the header comment of `app.R`.

- [ ] **Step 1: Write the failing tests:**

```r
# test-app-wiring.R
test_that("the Analyses tab is wired and a run blocks Data, Design and SAP edits", {
  # build a locked project through the app (as the existing wiring test does), then set
  # `analyses-run_all` with sr.run_sync = FALSE replaced by a spawner that never resolves:
  # assign .sr_an_spawn in globalenv to function(...) mirai::mirai(Sys.sleep(30)); then
  # session$setInputs(`data-import` = 1) and a Design save: both notify the busy text;
  # finally `analyses-cancel` = 1 and wait for running() to clear.
})
# test-mod-data.R
test_that("Import is refused while an analysis runs", { ... })   # busy = shiny::reactive(TRUE)
```

- [ ] **Step 2: Run `test-app-wiring.R`, `test-mod-data.R`, `test-ui-util.R`, `test-mod-design.R`, `test-mod-sap.R` to verify the new tests fail.** Expected: FAIL.

- [ ] **Step 3: Implement** the wiring, the `mod_data_server` parameter and the message change (update the three existing assertions to the new text).

- [ ] **Step 4: Run the five files again.** Expected: PASS.

- [ ] **Step 5: Commit** `feat(app): wire the Analyses panel; an analysis run makes the app busy`.

---

### Task 9: Docs, memory, full suite and acceptance in the app

**Files:**
- Modify: `docs/analysis.md` (new section "The Analyses panel": run, cancel, interrupted, apply, the five cards, with the failure table of §5)
- Modify: `memory.md` (Status: P1d UI sub-project 2 done; a "P1d UI (Analyses) gotchas and rulings" subsection: no mirai daemons, so a killed worker is never reused; the PID plus start-time guard; cancel during diagnostics leaves the fit standing; `run.completed` now carries `job`)

- [ ] **Step 1: Write the docs and memory changes.**

- [ ] **Step 2: Run the full suite in the background.** Expected: 0 failed. Record the pass / skip counts in the commit message.

- [ ] **Step 3: Acceptance in the app** (§8). With a scratch script (scratchpad, not committed) build a locked synthetic project with a cox primary on the `.dv_surv` data (ph fails), an lm secondary in one holm family with it, and an lm analysis with the bootstrap layer at B = 2000. Start the app on port 7790 and, in the browser pane:
  1. Run all: every analysis and its diagnostics run; Card 2 shows effects, tables, figures, fragments and verdicts.
  2. Run the bootstrap analysis and Cancel it: it stops within seconds, the history shows it cancelled, `ps` shows no extra R process, Run selected then succeeds.
  3. Start a run, kill the R process (`Stop-Process`), restart the app, reopen: the analysis shows interrupted; Run selected completes it.
  4. Apply on the cox analysis adds `m1_ph`; running it fills Card 3.
  5. Card 4 and Card 5 match `sr_family_adjust` and `sr_deviations` (printed from an R script on the same project).
  6. During a run, the Data import, a Design save and an SAP edit are refused with the busy text.
  Stop the app afterwards.

- [ ] **Step 4: Commit** `docs: Analyses panel guide and memory`.
