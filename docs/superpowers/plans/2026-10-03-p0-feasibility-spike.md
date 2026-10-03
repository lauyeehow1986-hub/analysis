# P0 Feasibility Spike Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Prove or disprove, on the dev laptop *and* on the real locked-down 16 GB PC, that every
runtime component StatRouter depends on works there. The result is a go/no-go per component.

**Architecture:** This is a throwaway `spike/` folder of independent probe scripts. Each probe runs in
its own fresh R process under a timeout and writes one JSON result: PASS, FAIL or SKIP plus metrics.
A report script turns the results into a Markdown table. A staging script builds a portable zip
(relocatable R + only the spike's packages) for the locked PC. Nothing in `spike/` is reused by the
app. The deliverable is the *results*.

**Tech Stack:** R 4.5.2. Packages: callr, processx, ps, jsonlite, shiny, httpuv, curl, officer,
flextable, ragg, mirai, duckdb, DBI, data.table, rstanarm. Python 3 (system on the laptop, optional
embeddable on the locked PC). PowerShell 5.1 for staging.

**Spec:** `docs/specs/2026-10-03-statrouter-design.md` (Phases → P0; Verification → large data and
air-gap).

## Global Constraints
- Dev R: `C:\Program Files\R\R-4.5.2\bin\Rscript.exe`.
- Never pass long multi-line code to `Rscript -e`; it segfaults on this machine. Every probe is a
  script file.
- Generated `.ps1`, `.bat` and `.R` files are **ASCII only**. Never use `Set-Content -Encoding UTF8`,
  because the BOM breaks Rscript.
- PowerShell 5.1: variable names are case-insensitive, so don't name a loop variable like a param.
  Use .NET `System.IO.Compression.ZipFile` for zips, not tar.
- Build the staged bundle in a **short path**: `C:\sr_spike`, because of MAX_PATH.
- Synthetic data only. The spike must never read or produce real patient data.
- No probe may touch the network.
  - The Python runner installs a socket guard.
  - Never call `reticulate::py_available(initialize=TRUE)`; reticulate isn't used at all.
- Profile memory budgets from the spec, as DuckDB `memory_limit`: `locked16` = 6 GB, `laptop` = 14 GB.
- On `locked16` the spike runs a **20 GB proxy** file. The full 100 GB run happens in P1 if disk
  allows.

## Review Focus
1. **AppLocker/WDAC blocks a DLL** (duckdb, rstanarm, ragg) loaded from a user-writable folder. The
   probe must record FAIL with the loader error text, and must not hang or kill the runner.
2. **Not enough free disk** for the generated CSV + Parquet + spill. The generator must refuse up
   front, reporting bytes needed vs free, and must not fill the disk.
3. **No Python / no GPU / no `nvidia-smi`** on the locked PC. The affected probes report SKIP with a
   reason, not FAIL.
4. **A probe hangs** (e.g. the Shiny port never opens, or a Stan sampler deadlocks). The runner's
   per-probe timeout records `FAIL (timeout)` and continues with the next probe.
5. **The staged bundle exceeds MAX_PATH** when unzipped into a deep folder (e.g. Downloads). The path
   audit fails the build before zipping.

Each item above is pinned by a test in the task that owns that code (Tasks 1, 5, 3, 1, 7).

## File Structure
```
spike/
  lib.R              harness: result writer, timing, RSS monitor, safe wrapper
  rss_monitor.R      child process: samples a PID's RSS, writes the peak
  run_all.R          runs every probes/p*.R in a fresh Rscript with a timeout
  selftest.R         harness self-test (pass / error / hang probes)
  probes/p00_env.R  p01_shiny.R  p02_docx.R  p03_workers.R
         p04_py_stdio.R  p05_von.R  p06_duckdb.R  p07_rstanarm.R
  apps/hello/app.R   minimal Shiny app for p01
  py/echo_runner.py  stdio JSON-lines runner with a socket guard
  py/von_probe.py    optional ONNX forward-pass timer
  gen_big_csv.R      synthetic large CSV + sidecar truth JSON
  report.R           results → docs/spike/p0-results-<host>.md
  spike_deps.R       package closure (Depends+Imports only)
  stage_spike.ps1    portable bundle → C:\sr_spike.zip
  run_spike.bat      entry point on the target PC
docs/spike/p0-results-<host>.md     committed reports
```
Add to `.gitignore`: `spike/results/`, `spike/tmp/`.

---

### Task 1: Spike harness + runner

**Files:**
- Create: `spike/lib.R`, `spike/rss_monitor.R`, `spike/run_all.R`, `spike/selftest.R`, `spike/probes/p00_env.R`
- Modify: `.gitignore`

**Interfaces:**
- Produces:
  - `sp_result(probe, status, metrics = list(), notes = character()) -> invisible(path)`
    - `status` ∈ `"PASS","FAIL","SKIP"`.
    - Writes `spike/results/<host>/<probe>.json` with fields `probe, status, metrics, notes, host,
      profile, r_version, ts`.
    - `profile = Sys.getenv("SR_PROFILE", "laptop")`.
  - `sp_try(probe, expr) -> invisible(NULL)`: evaluates `expr`, which must return
    `list(status, metrics, notes)`. Any error becomes `sp_result(probe, "FAIL", notes = conditionMessage(e))`. Never throws.
  - `sp_rss_start(pid = Sys.getpid()) -> handle`: starts `rss_monitor.R` as a processx child,
    sampling every 0.25 s.
  - `sp_rss_stop(handle) -> peak_gb (numeric)`.
  - `sp_profile_caps() -> list(memory_gb, threads)`: `locked16` → `list(6, 4)`; `laptop` →
    `list(14, 12)`.
  - `run_all.R [--timeout 1800] [--only p06]`: runs each `spike/probes/p*.R` via
    `processx::run(Rscript, file, timeout = ...)`. On timeout it writes
    `sp_result(<probe>, "FAIL", notes = "timeout after N s")`.

- [ ] **Step 1: Write `spike/selftest.R`.** It creates three temporary probe files in `spike/tmp/selftest/`:
  - `p_ok.R`: returns PASS.
  - `p_err.R`: `stop("boom")` inside `sp_try`.
  - `p_hang.R`: `Sys.sleep(60)`.

  It runs `run_all.R` against that directory with `--timeout 5` and asserts with `stopifnot`:
  - the result statuses are exactly `PASS`, `FAIL`, `FAIL`;
  - the `p_err` notes contain `"boom"`;
  - the `p_hang` notes contain `"timeout"`;
  - the whole run takes under 20 s.

  It prints `SELFTEST PASS`.
- [ ] **Step 2: Run it and confirm it fails.** Run: `& $RS spike/selftest.R` → error, because `lib.R` and `run_all.R` don't exist yet.
- [ ] **Step 3: Implement `lib.R`, `rss_monitor.R` and `run_all.R`.**
  - Add a `--dir` argument to `run_all.R` (default `spike/probes`) so the selftest can point it elsewhere.
  - `rss_monitor.R <pid> <outfile>` loops on `ps::ps_memory_info(ps::ps_handle(pid))[["rss"]]` and
    rewrites the peak to `outfile` until the PID exits or `outfile.stop` appears.
- [ ] **Step 4: Run the selftest and confirm it passes.** Run: `& $RS spike/selftest.R` → `SELFTEST PASS`.
- [ ] **Step 5: Implement `p00_env.R` (always PASS).** Metrics:
  - `R.version.string`, `La_library()`, `extSoftVersion()["BLAS"]`, `parallel::detectCores()`;
  - RAM GB from `ps::ps_system_memory()`;
  - GPU name and VRAM from `nvidia-smi --query-gpu=name,memory.total --format=csv,noheader,nounits`
    via `processx::run(timeout = 5)`, or NA if `Sys.which` is empty;
  - OS build;
  - `LongPathsEnabled`, read with `utils::readRegistry("SYSTEM\\CurrentControlSet\\Control\\FileSystem", "HLM")`, NA on error;
  - free disk GB on the spike drive, from `fsutil volume diskfree` output or NA;
  - whether `tempdir()` and `spike/tmp` are writable;
  - `Sys.which("pandoc")`.
- [ ] **Step 6: Run it.** Run: `& $RS spike/run_all.R --only p00` → `spike/results/<host>/p00_env.json` exists with status PASS.
- [ ] **Step 7: Commit.** Add `spike/results/` and `spike/tmp/` to `.gitignore`, then:
  `git add spike .gitignore` and `git commit -m "spike: probe harness, runner and env probe"`

### Task 2: Shiny launch + docx bookmark-refresh probes

**Files:**
- Create: `spike/apps/hello/app.R`, `spike/probes/p01_shiny.R`, `spike/probes/p02_docx.R`

**Interfaces:**
- Consumes: `sp_try`, `sp_result` (Task 1).

- [ ] **Step 1: Implement `apps/hello/app.R`.** A `fluidPage` containing the text `sr-spike-ok`.
- [ ] **Step 2: Implement `p01_shiny.R`.**
  - Start Rscript on a script file that calls `shiny::runApp("spike/apps/hello", port = 7791, launch.browser = FALSE)`, via `processx::process$new`.
  - Poll `curl::curl_fetch_memory("http://127.0.0.1:7791")` every 0.5 s for up to 30 s.
  - **PASS** iff the status is 200 and the body contains `sr-spike-ok`.
  - Metric: `startup_s`.
  - Always kill the child in `on.exit`.
- [ ] **Step 3: Implement `p02_docx.R`.** Use officer only (no pandoc).
  1. Write `spike/tmp/p02.docx` containing:
     - the paragraph "The odds ratio was " followed by bookmark `E1` wrapping the text `0.42`, then
       " (95% CI ", bookmark `E2` = `0.30–0.58`, ").";
     - a flextable built from `head(mtcars)`;
     - a ragg PNG plot (`ggplot2` or base).
  2. Re-open the file with `read_docx`.
  3. Call `body_replace_text_at_bkm("E1", "0.43")` and save to `p02b.docx`.
  4. Read back with `docx_summary`.
  5. **PASS** iff all of these hold:
     - the text contains "The odds ratio was 0.43 (95% CI 0.30–0.58).";
     - it no longer contains `0.42`;
     - one table and one image are present;
     - the `–` (U+2013) character round-trips.
  6. Metrics: `bytes`, and `pandoc_present`.
- [ ] **Step 4: Run both.** Run: `& $RS spike/run_all.R --only p01` and the same with `--only p02` → both PASS on the laptop.
- [ ] **Step 5: Commit.** `git commit -am "spike: shiny launch and docx bookmark-refresh probes"`. Add the new files first.

### Task 3: Worker and Python stdio probes

**Files:**
- Create: `spike/probes/p03_workers.R`, `spike/probes/p04_py_stdio.R`, `spike/probes/p05_von.R`, `spike/py/echo_runner.py`, `spike/py/von_probe.py`

**Interfaces:**
- Consumes: Task 1 harness.
- Produces:
  - Python interpreter lookup order (spike only): `Sys.getenv("SR_PYTHON")` → `bin/python/python.exe` next to the bundle root → `Sys.which("python")`. If none is found, the probe reports **SKIP** `"no python"`.
  - `echo_runner.py` protocol: one JSON object per stdin line, one JSON reply per stdout line.
    - `{"id":n,"op":"echo","x":...}` → `{"id":n,"echo":...,"pid":...}`
    - `{"id":n,"op":"net"}` → attempts `socket.create_connection(("example.com",80),1)` and must
      return `{"id":n,"error":"network forbidden"}`.
    - Before reading stdin, the guard replaces `socket.socket.connect` and `socket.create_connection`
      with functions that raise.

- [ ] **Step 1: Implement `p03_workers.R`.** Checks:
  - `mirai::daemons(2)`; two `mirai(Sys.getpid())` calls return 2 distinct PIDs ≠ the parent's PID
    within 10 s;
  - `callr::r(function() quit(status = 3))` raises an error that is caught, and the parent continues;
  - `callr::r(function() Sys.sleep(10), timeout = 2)` raises a timeout error in under 5 s;
  - `mirai::daemons(0)` in `on.exit`.

  PASS iff all hold. Metric: `daemon_start_s`.
- [ ] **Step 2: Implement `echo_runner.py` and `p04_py_stdio.R`.**
  - `p04_py_stdio.R` uses `processx::process$new(py, "spike/py/echo_runner.py", stdin = "|", stdout = "|")`.
  - It sends 200 echo requests one at a time, timing each round trip, then one `net` request.
  - **PASS** iff all 200 ids round-trip with equal payloads (including the non-ASCII string `"µmol/L"`), the p95 latency is under 50 ms, and the `net` request returns `network forbidden`.
  - Metrics: `median_ms`, `p95_ms`.
  - **SKIP** if no Python is found.
- [ ] **Step 3: Implement `von_probe.py` and `p05_von.R` (optional).**
  - **SKIP** unless `SR_VON_DIR` is set and `import onnxruntime` succeeds inside the subprocess.
  - Read the model card in `SR_VON_DIR` for the expected inputs. The probe only needs to time
    tokenizer + one `InferenceSession.run`.
  - Measurements: load time, then 20 forward passes on a ~2k-token synthetic state text. The
    subprocess reports its own peak RSS (`psutil` if present, else `resource`/NA).
  - **PASS** iff p95 is under 1.0 s and RSS is under 1.5 GB.
- [ ] **Step 4: Run all three on the laptop.** Run each with `& $RS spike/run_all.R --only <probe>` → p03 PASS, p04 PASS (system Python 3.14), p05 SKIP or PASS.
- [ ] **Step 5: Commit.** `git add spike && git commit -m "spike: worker isolation and python stdio probes"`

### Task 4: rstanarm probe

**Files:**
- Create: `spike/probes/p07_rstanarm.R`

- [ ] **Step 1: Implement `p07_rstanarm.R`.**
  1. Time `library(rstanarm)` (this also proves the DLLs load).
  2. Simulate `n = 500` with seed 1: `x1, x2 ~ N(0,1)`, `y ~ Bernoulli(plogis(-0.5 + 0.8 x1 - 0.4 x2))`.
  3. Fit `stan_glm(y ~ x1 + x2, family = binomial(), data, chains = 4, iter = 1000, seed = 1, refresh = 0)` twice: with `cores = 1` and with `cores = 4`.
  4. **PASS** iff all hold:
     - no error;
     - max R-hat < 1.01;
     - min bulk ESS > 400 (from `summary(fit)`);
     - posterior medians are within 0.15 of the `glm` coefficients.
  5. Metrics: `load_s`, `fit_s_1core`, `fit_s_4core`, `max_rhat`.
- [ ] **Step 2: Run it.** Run: `& $RS spike/run_all.R --only p07` → PASS on the laptop.
- [ ] **Step 3: Commit.** `git add spike && git commit -m "spike: rstanarm load and sampling probe"`

### Task 5: Large-file DuckDB probe

**Files:**
- Create: `spike/gen_big_csv.R`, `spike/probes/p06_duckdb.R`

**Interfaces:**
- Produces: `gen_big_csv.R --gb <size> --out spike/tmp/big.csv`. It writes `big.csv` plus `big.truth.json` = `{rows, sum_event, n_na_creat, sites:{site: n}}`.

- [ ] **Step 1: Implement `gen_big_csv.R`.**
  - **Columns:**
    - `id` (int), `visit_date` (`"%d/%m/%Y"` strings);
    - `age` (18–95), `sex` (`F`/`M`), `sbp` (N(130,18)), `creat_umol` (lognormal, 1% NA);
    - `site` (20 levels `S01..S20`), `event` (Bernoulli 0.08);
    - `note` (short random ASCII text with commas inside quotes).
  - **Writing:** chunks of 1e6 rows with `data.table::fwrite(append = TRUE)`. Seed per chunk = chunk
    index. Truth totals accumulate as it goes.
  - **Free-space guard:**
    - Estimate the bytes needed: `gb` × 2.5 (CSV + Parquet + spill).
    - Read free space with `fsutil volume diskfree <drive>`, falling back to `ps::ps_disk_usage()`
      if it exists in this ps version.
    - If free space is lower than needed, `stop()` with both numbers, before writing anything.
- [ ] **Step 2: Test the free-space guard.** Run: `& $RS spike/gen_big_csv.R --gb 100000 --out spike/tmp/x.csv` → error mentioning "need" and "free". No file is created.
- [ ] **Step 3: Implement `p06_duckdb.R`.**
  1. If `big.csv` is missing, generate it at `SR_SPIKE_GB` (default 5 on laptop, 20 on locked16).
  2. Start the RSS monitor.
  3. Open `duckdb(dbdir = "spike/tmp/spike.duckdb")` and run:
     `SET memory_limit='<caps$memory_gb>GB'; SET threads=<caps$threads>; SET temp_directory='spike/tmp/spill'; SET preserve_insertion_order=false;`
  4. Convert: `COPY (SELECT * FROM read_csv('spike/tmp/big.csv', dateformat='%d/%m/%Y', header=true)) TO 'spike/tmp/big.parquet' (FORMAT parquet, ROW_GROUP_SIZE 1000000);`
  5. Run against the Parquet file: `SELECT count(*), sum(event), count(*) FILTER (WHERE creat_umol IS NULL)` and `SELECT site, count(*) GROUP BY site`.
  6. **PASS** iff all hold:
     - every aggregate equals `big.truth.json`;
     - `visit_date` was parsed as DATE;
     - peak RSS ≤ `memory_gb + 2`.
  7. Metrics: `csv_gb`, `ingest_s`, `mb_per_s`, `peak_rss_gb`, `parquet_ratio`, `spill_gb_max`.
- [ ] **Step 4: Run it.** Run: `$env:SR_SPIKE_GB=5; & $RS spike/run_all.R --only p06 --timeout 7200` → PASS on the laptop.
- [ ] **Step 5: Commit.** `git add spike && git commit -m "spike: DuckDB large-CSV streaming probe"`

### Task 6: Report generator

**Files:**
- Create: `spike/report.R`, `spike/tmp/fixture_results/` (test fixture generated by the test step), `docs/spike/p0-results-<host>.md` (output)

**Interfaces:**
- Produces: `report.R [--results spike/results/<host>] [--out docs/spike/p0-results-<host>.md]`.
  - It writes one table row per probe: `| probe | status | key metrics | decision |`.
  - Decision rules: PASS → `go`; SKIP → `untested: <notes>`; FAIL → `no-go: <notes>`, followed by
    the fallback in the table below.

| Probe | Fallback if no-go (copied into the report) |
|---|---|
| p01_shiny | Stop: the app cannot run. Escalate to IT for an AppLocker exception |
| p02_docx | Use content controls instead of bookmarks for refresh; if officer can't write at all, stop |
| p03_workers | Run long jobs inline with no cancel. ExtendedTask is degraded |
| p04_py_stdio | Run the router with rules only on this machine; Python learners are laptop-only |
| p05_von | Router uses rules + Jev only (connected) on this machine |
| p06_duckdb | Try arrow `open_dataset` streaming; else limit to in-memory data ≤ RAM budget |
| p07_rstanarm | Bayesian layer H is laptop-only |

- [ ] **Step 1: Write the fixture test.** In `spike/selftest.R`, add a step that writes three fake
  results (PASS/SKIP/FAIL for p01/p05/p07) to `spike/tmp/fixture_results/` and runs `report.R` on
  them. Assert that the Markdown contains `| p01_shiny | PASS |`, `untested:` and
  `no-go:`, and that the p07 row contains "Bayesian layer H is laptop-only".
- [ ] **Step 2: Run it and confirm it fails.** Run: `& $RS spike/selftest.R` → FAIL (no `report.R`).
- [ ] **Step 3: Implement `report.R`.** Write the header with host, profile, date and R version, then the table.
- [ ] **Step 4: Run the selftest.** Run: `& $RS spike/selftest.R` → `SELFTEST PASS`.
- [ ] **Step 5: Run the full laptop suite.** Run: `$env:SR_PROFILE="laptop"; & $RS spike/run_all.R --timeout 7200; & $RS spike/report.R` → `docs/spike/p0-results-<laptop-host>.md`, with p00–p04, p06 and p07 = PASS.
- [ ] **Step 6: Commit.** `git add spike docs/spike && git commit -m "spike: report generator and laptop results"`

### Task 7: Portable spike bundle for the locked PC

**Files:**
- Create: `spike/spike_deps.R`, `spike/stage_spike.ps1`, `spike/run_spike.bat`
- Reuse logic from: `…\structured_deidentification\.claude\worktrees\shiny-deidentification-app-6a7932\tools\stage_r.ps1`, `stage_r_deps.R`, `audit_pathlen.ps1`

**Interfaces:**
- Produces: `C:\sr_spike.zip`. Unzipped, it contains `bin\R\`, `spike\` and `run_spike.bat`.

- [ ] **Step 1: Implement `spike_deps.R`.**
  - It's a copy of `stage_r_deps.R` with
    `want <- c("shiny","httpuv","curl","officer","flextable","ragg","ggplot2","mirai","callr","processx","ps","duckdb","DBI","data.table","jsonlite","rstanarm")`.
  - Use `which = c("Depends","Imports")`, with **no `LinkingTo`**. LinkingTo would pull in header-only
    BH, and BH breaks MAX_PATH.
- [ ] **Step 2: Implement `stage_spike.ps1`.**
  - It follows the `stage_r.ps1` pattern, with the destination set to `C:\sr_spike\bin\R`.
  - After copying it deletes every `library\*\include` directory, which removes the StanHeaders and
    RcppEigen headers.
  - It copies `spike\` (without `results` and `tmp`) and `run_spike.bat`.
  - Optional `-PythonDir`: copies an embeddable Python into `C:\sr_spike\bin\python`.
  - It runs the path audit, assuming the root `C:\Users\<20 chars>\Downloads\sr_spike\`. It
    **throws** if any path is over 250 characters, before zipping.
  - It zips with `[System.IO.Compression.ZipFile]::CreateFromDirectory`.
- [ ] **Step 3: Implement `run_spike.bat`.**
  - It sets `R_LIBS=`, `R_LIBS_USER=`, `R_LIBS_SITE=` (empty) and `SR_PROFILE=locked16` (overridable
    with `%1`), and sets `SR_SPIKE_GB=20`.
  - It runs `bin\R\bin\Rscript.exe --vanilla spike\run_all.R --timeout 10800`, then `spike\report.R`.
  - All output is teed to `spike\results\run.log`.
  - It prints where the report is.
- [ ] **Step 4: Stage and verify on the laptop.**
  1. Run: `powershell -ExecutionPolicy Bypass -File spike\stage_spike.ps1` → `C:\sr_spike.zip`, path audit PASS.
  2. Unzip to `%TEMP%\sr_spike_check`.
  3. Run `run_spike.bat laptop` there with the user library hidden.
  4. Expected: the same PASS set as Task 6, which proves the bundle is self-contained.
- [ ] **Step 5: Commit.** `git add spike && git commit -m "spike: portable bundle for the locked-down PC"`

### Task 8: Locked-PC run and go/no-go record (user-executed)

**Files:**
- Create: `docs/spike/p0-results-<locked-host>.md` (copied back by the user)
- Modify: `memory.md` (Status + Decisions), `docs/specs/2026-10-03-statrouter-design.md` ("Open items" — mark the spike items resolved)

- [ ] **Step 1: User runs the spike on the locked PC.** Copy `sr_spike.zip` to the locked PC, unzip under `Downloads`, double-click `run_spike.bat`, then copy `spike\results\` and the generated report back.
- [ ] **Step 2: Commit the report.** Expected: a row for every probe, and a decision for every non-PASS row.
- [ ] **Step 3: Record decisions.**
  - In `memory.md`, add one line per component: `go`, or `no-go → fallback`.
  - For every no-go, add a matching note to the spec's affected phase. For example, p07 FAIL means
    "P5 Bayesian is laptop-only".
- [ ] **Step 4: Commit.** `git commit -am "docs: P0 go/no-go decisions from locked-PC spike"`
