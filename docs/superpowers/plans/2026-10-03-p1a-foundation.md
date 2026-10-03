# P1a Foundation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build StatRouter's headless core. The pieces are:
- an event-sourced, hash-chained, portable project;
- hardware profiles;
- reproducibility hashing;
- the route contract, registry and composition rules;
- isolated, seeded code-first execution with caching;
- MCSE-aware number formatting;
- the CI lints that enforce the non-negotiable rules;
- a minimal Shiny shell that opens and creates projects.

P1b (data), P1c (analysis routes) and P1d (router, writer, freeze, UI) all build on this.

**Architecture:** The core is pure R in `app/R/core/` and `app/R/design/`, sourced in a fixed order by
`app/global.R`.
- **Projects** are folders. `events.jsonl` is authoritative, and state is rebuilt by replaying
  registered reducers. Every event is also written to a hash-chained `audit.log`.
- **Routes** register a contract. `compile(spec)` returns an R code string, which `sr_exec()` runs in
  a callr child under a pinned RNG.
- **Results** are cached by a content hash of (data hash, spec, seed, library snapshot, hardware
  fingerprint).

**Tech Stack:** R 4.5.2, jsonlite, openssl (SHA-256), callr, processx, ps, shiny, bslib, testthat 3e.

**Spec:** `docs/specs/2026-10-03-statrouter-design.md`. Relevant sections: Architecture, Core mechanics,
M1 Global validity rules, Phases → P1, Verification.

## Global Constraints
- Exported functions use the prefix `sr_`, internal ones `.sr_`; names are `snake_case`. Write one
  concern per file.
- Read options as `getOption("sr.x", Sys.getenv("SR_X", default))`, through `sr_opt("x", default)`.
- Every `.R` file is **ASCII only**.
  - Tests build non-ASCII strings from code points: `intToUtf8(c(0xB5, 0x6D, 0x6F, 0x6C))`.
  - Never type `\u` escapes, because the tooling normalises them.
- Never run multi-line code through `Rscript -e`. Tests run through the script
  `tools/run_tests.R` (`testthat::test_dir("tests/testthat", stop_on_failure = TRUE)`).
- **No network calls anywhere in `app/R/`**; Task 7's lint enforces this.
  - Never call `reticulate::py_available(initialize=TRUE)`, `install.packages`, `download.file`,
    `url()`, `curl::`, `httr*`, `socketConnection`.
  - The single allowlisted file is `app/R/router/backends/jev.R` (it doesn't exist yet).
- Mode: `sr_mode()` ∈ `airgap|connected`, default **airgap**.
- Hardware profiles: `locked16 | laptop`. They are auto-detected, and overridden by `sr.profile` /
  `SR_PROFILE`.
- JSON: use `jsonlite` with `auto_unbox = TRUE, digits = NA, null = "null", na = "null"`. Files are
  UTF-8 with `\n` line endings.
- The time format is ISO-8601 UTC with milliseconds: `format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")`.
- Synthetic data only. Tests write to `withr::local_tempdir()` (or `tempfile()`), never to the repo.
- Work on branch `p1a-foundation` in a worktree of `%USERPROFILE%\Downloads\analysis`. Before any
  commit, check that `git rev-parse --show-toplevel` is that repo.

## Review Focus
1. **The session crashes mid-append, leaving a torn last line in `events.jsonl`.**
   - Expected: replay ignores the torn tail with a warning, and state equals the state before the
     torn event.
   - The next append first truncates the torn tail, with an audit entry, then writes cleanly.
   - Pinned in Task 3.
2. **A project folder is copied to another drive letter or path, e.g. `D:\proj` → `E:\x\proj`.**
   - Expected: open succeeds, all paths resolve under the new root, and the replayed state and audit
     tip are identical.
   - Pinned in Task 4.
3. **A stale lock remains from a crashed session on the same host.**
   - Expected: the lock is detected as stale because its PID is dead, and it is taken over.
   - A live lock from another host opens the project read-only, and appends then refuse.
   - Pinned in Task 4.
4. **One byte in a middle line of `audit.log` is edited by hand.**
   - Expected: verify returns `ok = FALSE` with `broken_at` pointing at that line.
   - Pinned in Task 2.
5. **Non-ASCII text in payloads (µmol/L, CJK names) and differing locales.**
   - Expected: canonical JSON and content hashes are byte-identical across a `Sys.setlocale("LC_COLLATE", "C")` session and the default session.
   - Pinned in Task 6.

## File Structure
```
app/global.R                 source order: core/util, audit, events, project, hardware, libsnap, hash,
                             exec, design/spec, design/compose, writer/numfmt
app/app.R                    bslib shell (Task 12)
app/R/core/util.R            sr_opt, sr_mode, .sr_now, .sr_json, .sr_write_lines_append, .sr_sha256
app/R/core/audit.R           hash-chained audit log
app/R/core/events.R          event log + reducer registry + replay
app/R/core/project.R         project folder, project.json, schema migration, lock
app/R/core/hardware.R        detection, profile choice, caps
app/R/core/libsnap.R         library snapshot
app/R/core/hash.R            canonical JSON + content hash
app/R/core/exec.R            callr execution, RNG preamble, parallel streams, result cache
app/R/core/lint.R            p-gate + air-gap static lints
app/R/design/spec.R          design spec validator + route contract + registry
app/R/design/compose.R       composition matrix
app/R/writer/numfmt.R        MCSE-limited and p-value formatting
app/R/ui/mod_project.R       Shiny project module
tools/run_tests.R  tools/lint.R
tests/testthat/helper-load.R  test-*.R  fixtures/lint/*.R
```

---

### Task 1: Loader, utilities and test runner

**Files:**
- Create: `app/global.R`, `app/R/core/util.R`, `tools/run_tests.R`, `tests/testthat/helper-load.R`, `tests/testthat/test-util.R`

**Interfaces:**
- Produces:
  - `sr_opt(name, default)`: returns `getOption(paste0("sr.", name), Sys.getenv(toupper(paste0("SR_", name)), default))`. An empty env string counts as unset.
  - `sr_mode()`: returns `"airgap"` or `"connected"` from `sr_opt("mode", "airgap")`, and errors on any other value.
  - `.sr_now()`: ISO UTC timestamp string.
  - `.sr_json(x)`: one-line JSON string using the Global Constraints settings.
  - `.sr_sha256(x)`:
    - for a character x, lowercase hex of `openssl::sha256(charToRaw(enc2utf8(x)))`;
    - for a raw x, its hash.
  - `.sr_write_lines_append(path, line)`: opens with `file(path, "ab")`, writes `charToRaw(enc2utf8(line))` + `0x0A`, flushes, closes.
  - `app/global.R`: `sr_root <- normalizePath(dirname(sys.frame(1)$ofile %||% "app"))`, then sources the files listed in File Structure, **skipping any that don't exist yet**. Later tasks only add files.
  - `helper-load.R`: sources `app/global.R` from the repo root (found via `testthat::test_path("../..")`).

- [ ] **Step 1: Write `test-util.R`.**
  - `sr_opt("zz", "d")` returns `"d"`.
  - With `withr::local_envvar(SR_ZZ = "e")` it returns `"e"`.
  - With `withr::local_options(sr.zz = "o")` it returns `"o"`.
  - `sr_mode()` returns `"airgap"` by default; with `SR_MODE = "bogus"` it errors.
  - `.sr_sha256("abc")` returns `"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"`.
  - `.sr_now()` matches `^\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d\.\d{3}Z$`.
  - Two `.sr_write_lines_append` calls produce a file whose raw bytes end in `0x0A` and that
    `readLines` gives 2 lines.
- [ ] **Step 2: Run it.** Run: `& $RS tools/run_tests.R` → FAIL (`sr_opt` not found). Write `run_tests.R` first if it's missing.
- [ ] **Step 3: Implement** `util.R`, `global.R`, `helper-load.R` and `run_tests.R`.
- [ ] **Step 4: Run it.** Run: `& $RS tools/run_tests.R` → all PASS.
- [ ] **Step 5: Commit.** `git add app tools tests && git commit -m "core: loader, options, json and hashing utilities"`

### Task 2: Hash-chained audit log

**Files:**
- Create: `app/R/core/audit.R`, `tests/testthat/test-audit.R`
- Port from: `…\structured_deidentification\.claude\worktrees\shiny-deidentification-app-6a7932\app\R\hashchain.R`. Rename `se_` → `sr_`, keep the genesis string `"GENESIS"`, and use the Task 1 helpers.

**Interfaces:**
- Produces:
  - `sr_audit_append(log_path, action, actor, detail = list()) -> chr hash`.
    - The line is `.sr_json(list(ts, action, actor, detail, prev_hash, hash))`.
    - `hash = .sr_sha256(paste0(prev_hash, "|", .sr_json(list(ts, action, actor, detail))))`.
  - `sr_audit_tip(log_path) -> chr`: the last line's hash, or `"GENESIS"` if the file is missing or empty.
  - `sr_audit_verify(log_path) -> list(ok = lgl, broken_at = int|NA, n = int)`.
  - `sr_audit_read(log_path) -> data.frame(ts, action, actor, prev_hash, hash)` plus a list-column `detail`.

- [ ] **Step 1: Write `test-audit.R`.**
  - Append 3 entries, then check:
    - `sr_audit_verify()` returns `ok = TRUE, n = 3`;
    - the second entry's `prev_hash` equals the first entry's `hash`;
    - `sr_audit_tip()` equals the third entry's hash.
  - **(Review Focus 4)** Read the lines, change one character in line 2's `actor`, and write them
    back. Verify then returns `ok = FALSE, broken_at = 2`.
  - Delete line 2. Verify returns `ok = FALSE, broken_at = 2`, because line 3's `prev_hash` no longer
    matches.
  - An empty or missing file gives `ok = TRUE, n = 0` and tip `"GENESIS"`.
- [ ] **Step 2: Run it.** Run: `& $RS tools/run_tests.R` → FAIL.
- [ ] **Step 3: Implement `audit.R`.** Verify walks the lines in order. It recomputes each hash and
  checks each `prev_hash` against the previous line, and reports the first line that fails.
- [ ] **Step 4: Run it.** → PASS.
- [ ] **Step 5: Commit.** `git commit -m "core: hash-chained audit log"` (after `git add`).

### Task 3: Event log, reducers and replay

**Files:**
- Create: `app/R/core/events.R`, `tests/testthat/test-events.R`

**Interfaces:**
- Consumes: `sr_audit_append` (Task 2), `.sr_write_lines_append`, `.sr_json`, `.sr_now` (Task 1).
- Produces:
  - `sr_reducer_register(type, fn)`.
    - `fn(state, event) -> state`; `type` is a string.
    - Registering a type twice replaces the reducer.
    - The registry lives in the environment `.sr_reducers`.
  - `sr_event_append(events_path, audit_path, type, payload = list(), actor = sr_opt("user", Sys.info()[["user"]])) -> list event`.
    - `event = list(seq, ts, type, actor, payload)`, where `seq` is the last seq + 1, starting at 1.
    - It writes `events.jsonl` and then calls `sr_audit_append(audit_path, paste0("event:", type), actor, list(seq = seq, payload_sha = .sr_sha256(.sr_json(payload))))`.
    - It errors if `type` has no reducer.
    - **Torn-tail repair:** if the file doesn't end in `0x0A`, it first truncates the file back to
      its last `0x0A`, by rewriting the bytes up to and including it with `writeBin`. The removed
      tail was never committed. It then audits
      `"events.repair"` with `list(removed_bytes, removed_sha)`.
  - `sr_events_read(events_path) -> list of events`.
    - Lines that fail `jsonlite::validate` are dropped with `warning("torn event line <n> ignored")`.
    - Only the **last** line may be torn. A malformed line anywhere else is an error
      (`"corrupt events.jsonl at line <n>"`).
  - `sr_state_replay(events, init = list()) -> state`: folds the events through their reducers in
    `seq` order.
  - Built-in reducers registered here:
    - `"project.created"`: sets `state$project <- payload`;
    - `"note"`: appends `payload$text` to `state$notes`.

- [ ] **Step 1: Write `test-events.R`.**
  - Register a test reducer `"counter.add"`, where `state$n <- (state$n %||% 0) + payload$k`.
  - Append k = 1, 2, 3. Replay gives `n == 6`, seqs `1:3`, and the audit log has 3 lines that verify
    OK.
  - An unknown type errors with `"no reducer"`.
  - **(Review Focus 1)** Append half of a JSON line to `events.jsonl` as raw bytes, without a newline:
    - replay gives a warning matching `"torn"`, and `n == 6`;
    - a following `sr_event_append(k = 4)` gives seq 4;
    - replay then gives `n == 10` with **no** warning;
    - the audit log contains `"events.repair"` and still verifies OK.
  - A malformed line in the middle errors with `"corrupt events.jsonl at line 2"`.
  - A payload containing `intToUtf8(c(0xB5, 0x6D, 0x6F, 0x6C))` round-trips identically.
- [ ] **Step 2: Run it.** → FAIL.
- [ ] **Step 3: Implement `events.R`.** The next seq comes from the last valid line, after any
  torn-tail repair.
- [ ] **Step 4: Run it.** → PASS.
- [ ] **Step 5: Commit.** `git commit -m "core: event log, reducer registry and replay"`

### Task 4: Project folder, schema migration and lock

**Files:**
- Create: `app/R/core/project.R`, `tests/testthat/test-project.R`
- Port from: the structured_deid `app/R/project.R` (`se_project_paths` / `create` / `open`, `se_sha256_file`)

**Interfaces:**
- Consumes: Tasks 1–3.
- Produces:
  - `sr_project_paths(dir) -> list(dir, json, events, audit, data, fits, out, lock)`.
    - Subfolders: `data/`, `fits/`, `out/`. Files: `project.json`, `events.jsonl`, `audit.log`,
      `.lock`.
    - All paths are relative to `dir` and normalised with `winslash = "/"`.
  - `sr_project_create(dir, name, profile = sr_profile(), mode = sr_mode()) -> project`.
    - `project` is a list with `$paths`, `$meta` and `$state`, plus `$readonly = FALSE`.
    - `project.json` holds `{schema_version: 1, id: <uuid4 from openssl::rand_bytes>, name, created, profile, mode}`.
    - It appends a `project.created` event and takes the lock.
    - It errors if `dir` exists and isn't empty.
  - `sr_project_open(dir, force = FALSE) -> project`.
    - It rebuilds `$paths` from `dir`; stored paths are never trusted.
    - It migrates `project.json` and replays the events into `$state`.
    - It takes the lock, or sets `$readonly = TRUE`.
  - `sr_project_close(project)`: releases the lock if this process holds it.
  - `sr_schema_migrations`: a list keyed by from-version. Each entry is a `function(meta) meta`.
    - `.sr_migrate(meta)` applies migrations up to `SR_SCHEMA_VERSION = 1L`.
    - A newer `schema_version` errors with `"project created by a newer StatRouter"`.
  - **Lock file:** `{host, pid, user, ts}`.
    - **Free:** there is no lock file, or it belongs to the same host and `!ps::ps_is_running(ps::ps_handle(pid))` (a stale lock is taken over with an audit entry `"lock.takeover"`), or `force = TRUE`.
    - **Live from another host**, or a running PID on this host: `$readonly = TRUE`.
  - `sr_project_append(project, type, payload)`: errors `"project is read-only"` when it's read-only.
    Otherwise it appends the event and updates `$state` incrementally.

- [ ] **Step 1: Write `test-project.R`.**
  - **Create and reopen:**
    - create, then `sr_project_append(p, "note", list(text = "a"))`, then close;
    - reopen: `state$notes == "a"`, `state$project$name` matches, and audit verify is OK.
  - **(Review Focus 2) Move the project.**
    - Copy the folder with `file.copy(recursive = TRUE)` to a different temp root and open it there.
    - Expected: `$paths$dir` is the new root, the state is identical, and `sr_audit_tip()` is equal.
  - **(Review Focus 3) Stale lock.** Write a lock with this host and PID `999999` (not running).
    Open gives `readonly == FALSE`, and the audit contains `"lock.takeover"`.
  - **(Review Focus 3) Live lock from another host.** Write a lock with `host = "OTHERHOST"`. Open
    gives `readonly == TRUE`, and `sr_project_append` errors with `"read-only"`.
  - **Migrations.**
    - A newer schema (`schema_version = 99`) errors with "newer StatRouter".
    - A fake migration from 0 → 1 runs for a `schema_version = 0` file. Register it inside the test
      with `withr::defer` cleanup.
  - Creating into a non-empty directory errors.
- [ ] **Step 2: Run it.** → FAIL.
- [ ] **Step 3: Implement `project.R`.**
- [ ] **Step 4: Run it.** → PASS.
- [ ] **Step 5: Commit.** `git commit -m "core: portable project folder, schema migration and lock"`

### Task 5: Hardware detection and profile caps

**Files:**
- Create: `app/R/core/hardware.R`, `tests/testthat/test-hardware.R`

**Interfaces:**
- Produces:
  - `sr_hw_detect() -> list(ram_gb, cores, gpu_name, gpu_vram_mb, blas, os)`.
    - Sources: `ps::ps_system_memory()$total`; `parallel::detectCores()`; `nvidia-smi` via
      `processx::run(timeout = 5)` or NA when it's missing or fails; `extSoftVersion()[["BLAS"]]`;
      `utils::osVersion`.
    - The result is cached in `.sr_hw_cache` for the session.
  - `sr_profile(hw = sr_hw_detect()) -> "locked16" | "laptop"`.
    - An override from `sr_opt("profile", "")` wins; an invalid override errors.
    - Otherwise it returns `"laptop"` iff `!is.na(gpu_vram_mb) && gpu_vram_mb >= 6000 && ram_gb >= 20`.
  - `sr_profile_caps(profile)`: pinned values below; an unknown profile errors.
  - `sr_hw_fingerprint(profile = sr_profile(), hw = sr_hw_detect()) -> chr`.
    - It's `.sr_sha256(.sr_json(list(profile, blas = basename(hw$blas), gpu = !is.na(hw$gpu_name), r = R.version.string)))`.
    - It enters content hashes (M1.6).

| cap | locked16 | laptop |
|---|---|---|
| `workers` | 3 | 8 |
| `ram_budget_gb` | 10 | 18 |
| `duckdb_memory_gb` | 6 | 14 |
| `duckdb_threads` | 4 | 12 |
| `model_tier` | `"von"` | `"clm"` |
| `gpu` | FALSE | TRUE |

- [ ] **Step 1: Write `test-hardware.R`.**
  - `sr_profile(list(ram_gb = 15.7, gpu_vram_mb = NA))` returns `"locked16"`.
  - `sr_profile(list(ram_gb = 23.7, gpu_vram_mb = 8188))` returns `"laptop"`.
  - With `SR_PROFILE = "locked16"` and laptop hardware, it returns `"locked16"`.
  - `SR_PROFILE = "x"` errors.
  - The caps table values hold exactly.
  - `sr_hw_detect()` returns all 6 names. Run it for real; `gpu_*` may be NA.
  - The fingerprint differs between `"locked16"` and `"laptop"` for the same hardware.
- [ ] **Step 2: Run it.** → FAIL.
- [ ] **Step 3: Implement `hardware.R`.**
- [ ] **Step 4: Run it.** → PASS.
- [ ] **Step 5: Commit.** `git commit -m "core: hardware detection and profile caps"`

### Task 6: Library snapshot, canonical JSON and content hash

**Files:**
- Create: `app/R/core/libsnap.R`, `app/R/core/hash.R`, `tests/testthat/test-hash.R`

**Interfaces:**
- Consumes: `sr_hw_fingerprint` (Task 5), `.sr_sha256` (Task 1).
- Produces:
  - `sr_canonical_json(x) -> chr`.
    - Named lists are sorted recursively by name, using `sort(method = "radix")` so the order is
      locale-independent.
    - Unnamed lists keep their order.
    - Numbers are written with `digits = NA`; strings are `enc2utf8`; there's no whitespace.
  - `sr_libsnap(pkgs = .sr_core_pkgs()) -> list(r, snapshot_date, pkgs = named chr versions)`.
    - `snapshot_date = sr_opt("cran_snapshot", NA)`.
    - `.sr_core_pkgs()` returns `c("jsonlite","openssl","callr","processx","ps","shiny","bslib")`.
      Later phases extend it.
  - `sr_libsnap_hash(snap = sr_libsnap()) -> chr`.
  - `sr_content_hash(data_hash, spec, seed, libsnap_hash = sr_libsnap_hash(), hw_fp = sr_hw_fingerprint()) -> chr`.
    - It's `.sr_sha256(sr_canonical_json(list(data = data_hash, spec = spec, seed = seed, lib = libsnap_hash, hw = hw_fp)))`.

- [ ] **Step 1: Write `test-hash.R`.**
  - `sr_canonical_json(list(b = 1, a = list(d = 2, c = "x")))` equals `{"a":{"c":"x","d":2},"b":1}`.
  - Reordering the input names doesn't change the output.
  - `0.1 + 0.2` serialises as `0.30000000000000004` (full precision).
  - **(Review Focus 5) Non-ASCII.**
    - Take a spec containing `intToUtf8(c(0x4E2D, 0x6587))` and `intToUtf8(c(0xB5, 0x6D, 0x6F, 0x6C))`
      as both names and values.
    - Hash it normally, then under `withr::local_collate("C")`, and then in a `callr::r()` child that
      sources `app/global.R` with `Sys.setlocale("LC_COLLATE", "C")`.
    - Expected: all three hashes are identical.
  - Changing any one of data / spec / seed / lib / hw changes `sr_content_hash`.
- [ ] **Step 2: Run it.** → FAIL.
- [ ] **Step 3: Implement `libsnap.R` and `hash.R`.**
- [ ] **Step 4: Run it.** → PASS.
- [ ] **Step 5: Commit.** `git commit -m "core: library snapshot, canonical JSON and content hash"`

### Task 7: Static lints (p-gate and air-gap)

**Files:**
- Create: `app/R/core/lint.R`, `tools/lint.R`, `tests/testthat/test-lint.R`, `tests/testthat/fixtures/lint/{bad_pgate.R, bad_pgate2.R, ok_route.R, bad_net.R}`

**Interfaces:**
- Produces:
  - `sr_lint_pgate(file) -> data.frame(file, line, msg)`.
    - It parses the file with `getParseData`. It finds every function assigned to the name `compile`,
      or to a list element `compile =`.
    - Inside those bodies it flags any `if` / `switch` / `ifelse` / `dplyr::if_else` condition that
      contains any of these:
      - the symbols or strings `p.value`, `p_value`, `pvalue`;
      - a `$p` access;
      - calls to `shapiro.test`, `ks.test`, `bartlett.test`, `leveneTest`, `fligner.test`,
        `mauchly.test`, `cox.zph`, `check_normality`, `check_heteroscedasticity`.
    - Message: `"compile() branches on a diagnostic test (M1.1)"`.
  - `sr_lint_airgap(files, allow = "app/R/router/backends/jev.R") -> data.frame(file, line, msg)`.
    - It flags calls to `download.file`, `url`, `socketConnection`, `install.packages`,
      `py_install`, `py_available` with `initialize = TRUE`, any `curl::` / `httr::` / `httr2::`
      namespace, and `options(repos=`.
  - `tools/lint.R`:
    - runs both lints over `app/R/**/*.R` (p-gate only on `app/R/routes/`; air-gap on everything);
    - prints the findings;
    - calls `quit(status = 1)` if there are any;
    - prints `LINT OK` otherwise.

- [ ] **Step 1: Write the fixtures and `test-lint.R`.**
  - **`bad_pgate.R`:** `route <- list(compile = function(spec) { if (shapiro.test(x)$p.value < .05) "wilcox" else "t" })`. It gives 1 finding.
  - **`bad_pgate2.R`:** `compile <- function(spec) { pv <- 1; switch(if (d$p_value > .05) "a" else "b", a = 1) }`. It gives ≥1 finding.
  - **`ok_route.R`:** a compile that branches on `spec$sap$rule == "x"` and on `spec$design$clustered`.
    It gives 0 findings. The file also *calls* `shapiro.test` outside `compile`, which is allowed
    because `present` may report diagnostics.
  - **`bad_net.R`:** contains `download.file(...)`, `curl::curl_fetch_memory(...)` and
    `reticulate::py_available(initialize = TRUE)`. It gives 3 findings with the correct line numbers.
  - The allowlisted path gives 0 findings for the same content.
  - `tools/lint.R` on the real `app/R` prints `LINT OK` with status 0.
- [ ] **Step 2: Run it.** → FAIL.
- [ ] **Step 3: Implement `lint.R` and `tools/lint.R`.**
- [ ] **Step 4: Run it.** Run: `& $RS tools/run_tests.R; & $RS tools/lint.R` → PASS, `LINT OK`.
- [ ] **Step 5: Commit.** `git commit -m "core: p-gate and air-gap static lints"`

### Task 8: Design spec validator, route contract and registry

**Files:**
- Create: `app/R/design/spec.R`, `tests/testthat/test-spec.R`

**Interfaces:**
- Consumes: `sr_canonical_json` (Task 6).
- Produces:
  - `SR_PURPOSES <- c("describe","predict","explain-causal")`.
  - `sr_estimand(population, variables, intercurrent, summary, level = c("marginal","conditional","not-applicable"))`: all fields are required and non-empty; returns a classed list `sr_estimand`.
  - `sr_route(id, version, family, purpose, estimand, spec_schema, compile, present, composes_with = character(), applicable = function(spec, ctx) list(ok = TRUE, reasons = character())) -> sr_route`.
    - `estimand` is either an `sr_estimand` or `function(spec) -> sr_estimand`.
    - `spec_schema` is a named list `field -> list(type = "chr|num|lgl|list", required = lgl, values = optional chr)`.
  - `sr_route_register(route)`. It errors:
    - when the purpose isn't in `SR_PURPOSES` (`"purpose"`);
    - when there's no estimand (`"estimand"`);
    - when `compile` or `present` isn't a function;
    - when the `id@version` is a duplicate.
  - `sr_route_get(id, version = NULL)`: the latest version when `version` is NULL. `sr_routes() -> data.frame(id, version, family, purpose)`.
  - `sr_spec_validate(spec, route) -> list(ok, errors = chr)`.
    - It checks required fields, types and allowed values.
    - It also requires `spec$purpose == route$purpose`. This is M1.3 cross-talk: a `predict` spec
      can't run on an `explain-causal` route.
  - `sr_compile(route, spec) -> chr`.
    - It validates first and stops with the joined errors.
    - It calls `route$compile(spec)` and asserts a length-1 character result.

- [ ] **Step 1: Write `test-spec.R`.**
  - **Registering:**
    - Register a toy route `toy.mean@1`. Its compile is `function(spec) sprintf("mean(data[[%s]])", deparse(spec$var))`.
    - `sr_routes()` lists it.
    - Registering it without `purpose` errors with `"purpose"`; with `purpose = "causal"` it also errors.
    - Without an estimand it errors with `"estimand"`.
    - A duplicate `id@version` errors; version 2 registers, and `sr_route_get("toy.mean")$version == 2`.
  - **Validating:**
    - A spec missing a required `var` gives `ok = FALSE` with "var".
    - A spec whose `purpose` is `"predict"` on a `describe` route gives an error containing `"purpose"`.
  - `sr_compile` returns a string that `eval(parse(text = ))` runs on `data.frame(x = 1:3)` → 2.
  - Clear the registry between tests with `.sr_routes_reset()`.
- [ ] **Step 2: Run it.** → FAIL.
- [ ] **Step 3: Implement `spec.R`.**
- [ ] **Step 4: Run it.** → PASS.
- [ ] **Step 5: Commit.** `git commit -m "design: route contract, registry and spec validation"`

### Task 9: Composition matrix

**Files:**
- Create: `app/R/design/compose.R`, `tests/testthat/test-compose.R`

**Interfaces:**
- Consumes: the spec layer fields. The design spec carries
  `spec$layers = list(missing = "cc|mi", weights = "none|ipw|overlap|survey", matching = lgl, resampling = "none|bootstrap", framework = "frequentist|bayesian|both", clustered = lgl)`
  plus optional `spec$choices` (a named chr).
- Produces:
  - `sr_compose_rules()`: a list of rules, each `list(id, when = function(L) lgl, outcome = "refuse"|"choice"|"require", options = chr, require = function(spec) lgl, msg)`.
  - `sr_compose_check(spec) -> list(ok = lgl, refused = chr ids, choices = list(id -> options), unmet = chr ids, notes = chr)`.
    - `ok` is TRUE only when there are no refusals, every `choice` rule has a value in
      `spec$choices[[id]]` from its options, and every `require` holds.

The pinned rules (spec M1.5 and catalogue H):

| id | when | outcome |
|---|---|---|
| `mi_x_ps` | missing = mi & weights ∈ {ipw, overlap} | choice: `MIte`, `MIps` |
| `mi_x_boot` | missing = mi & resampling = bootstrap | choice: `boot_then_impute`, `impute_then_boot` |
| `bayes_x_boot` | framework ∈ {bayesian, both} & resampling = bootstrap | refuse: "posterior already carries uncertainty" |
| `bayes_x_mi` | framework = bayesian & missing = mi | require `spec$pool == "draws"` ("pool draws, not Rubin") |
| `match_x_boot` | matching & resampling = bootstrap | require `spec$boot_type ∈ {pair, cluster}` (Abadie–Imbens) |
| `ipw_x_boot` | weights ∈ {ipw, overlap} & resampling = bootstrap | require `isTRUE(spec$refit_ps_each_rep)` |
| `survey_x_mi` | weights = survey & missing = mi | require `isTRUE(spec$design_vars_in_imputation)` |
| `survey_x_boot` | weights = survey & resampling = bootstrap | require `spec$boot_type == "replicate_weights"` |
| `cluster_x_boot` | clustered & resampling = bootstrap | require `spec$boot_type ∈ {cluster, pair, replicate_weights}` |
| `overlap_target` | weights = overlap | always ok; adds note "target population is ATO" |

- [ ] **Step 1: Write `test-compose.R`.** One `test_that` per rule, covering:
  - the triggering layers without the choice/requirement → `ok = FALSE`, with the id in the right slot;
  - with it satisfied → `ok = TRUE`;
  - a plain spec (cc, none, no matching, none, frequentist, not clustered) → `ok = TRUE` with no notes;
  - `overlap_target` → `ok = TRUE`, and the notes contain "ATO";
  - a choice value not in the options → `ok = FALSE`.
- [ ] **Step 2: Run it.** → FAIL.
- [ ] **Step 3: Implement `compose.R`.** Store the rules as data, as in the table above.
- [ ] **Step 4: Run it.** → PASS.
- [ ] **Step 5: Commit.** `git commit -m "design: composition matrix with refusals, choices and requirements"`

### Task 10: Seeded isolated execution and result cache

**Files:**
- Create: `app/R/core/exec.R`, `tests/testthat/test-exec.R`

**Interfaces:**
- Consumes: `sr_content_hash` (Task 6), `sr_profile_caps` (Task 5), project `$paths$fits` (Task 4).
- Produces:
  - `sr_rng_preamble(seed) -> chr`. It is exactly:
    ```
    RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
    set.seed(<seed>L)
    ```
  - `sr_streams(seed, n) -> list of n integer seeds`, from successive `parallel::nextRNGStream`
    calls starting at the preamble state.
    - Stream *i* depends only on `(seed, i)`, never on the number of workers.
  - `sr_exec(code, inputs = list(), seed, timeout = 3600) -> list(ok, value, error, elapsed, code_run)`.
    - It runs `paste(sr_rng_preamble(seed), code, sep = "\n")` in `callr::r(function(code, inputs) { list2env(inputs, environment()); eval(parse(text = code)) }, ...)`.
    - Errors, crashes (non-zero exit) and timeouts give `ok = FALSE` with a message. It never throws.
    - `code_run` is the exact string evaluated, and becomes the exported `analysis.R` text.
  - `sr_exec_cached(project, key_hash, code, inputs, seed, ...)`.
    - It loads `fits/<key_hash>.rds` if present, returning `cached = TRUE`.
    - Otherwise it runs `sr_exec`, saves the result only when `ok`, and returns `cached = FALSE`.
    - The `.rds` is a cache only and is never authoritative.

- [ ] **Step 1: Write `test-exec.R`.**
  - `sr_exec("runif(3)", seed = 42)` called twice → identical values. Seed 43 gives different values.
  - `sr_exec("stop('x')", seed = 1)` gives `ok = FALSE`, and the error contains "x".
  - `sr_exec("quit(status = 3)", seed = 1)` gives `ok = FALSE`, and the parent is alive.
  - `sr_exec("Sys.sleep(10)", seed = 1, timeout = 2)` gives `ok = FALSE` with "timeout" in under 6 s.
  - **Worker-count independence:**
    - Compute 8 bootstrap means of `inputs$x` using `sr_streams(7, 8)`: one stream per replicate,
      setting `.Random.seed` from the stream.
    - Run them sequentially, and with `parallel::parLapply` on 2 and on 4 PSOCK workers.
    - Expected: all three vectors are identical.
  - **Caching:**
    - `sr_exec_cached` twice with the same key → the second call has `cached = TRUE` and an identical
      value.
    - A different key → `cached = FALSE`.
    - A failed run writes no `.rds`.
  - `code_run` starts with `RNGkind("L'Ecuyer-CMRG"`.
- [ ] **Step 2: Run it.** → FAIL.
- [ ] **Step 3: Implement `exec.R`.**
- [ ] **Step 4: Run it.** → PASS.
- [ ] **Step 5: Commit.** `git commit -m "core: seeded isolated execution, RNG streams and result cache"`

### Task 11: MCSE-limited and p-value formatting

**Files:**
- Create: `app/R/writer/numfmt.R`, `tests/testthat/test-numfmt.R`

**Interfaces:**
- Produces:
  - `sr_fmt_num(x, mcse = NA, digits = as.integer(sr_opt("digits", 2)))`.
    - With `mcse` NA or 0, it uses `digits` decimals.
    - Otherwise it uses `d = max(0, floor(-log10(2 * mcse)))`, then `min(d, digits + 2)`.
    - It rounds with `formatC(round(x, d), format = "f", digits = d)`; `-0.00` prints as `0.00`.
  - `sr_fmt_ci(est, lo, hi, mcse = NA, ...)`: gives `"0.42 (0.30 to 0.58)"` with a shared `d`. The
    separator is `" to "`, which avoids the minus/dash ambiguity.
  - `sr_fmt_p(p)`:
    - NA → `"NA"`;
    - p < 0.001 → `"<0.001"`;
    - p > 0.99 → `">0.99"`;
    - p < 0.01 → 3 dp;
    - otherwise 2 dp, but if p < 0.05 and the rounded value is ≥ 0.05, raise to 3 and then 4 dp
      until the rounded value is < 0.05. A significant p never prints as "0.05".
  - `sr_mcse_digits(mcse)`: the `d` used above, exported for the writer.

- [ ] **Step 1: Write `test-numfmt.R`.**
  - `sr_fmt_num(0.4237, mcse = 0.004)` returns `"0.42"`.
  - `sr_fmt_num(0.4237, mcse = 0.0004)` returns `"0.424"`.
  - `sr_fmt_num(0.4237, mcse = 0.04)` returns `"0.4"`.
  - `sr_fmt_num(12.3456)` returns `"12.35"`.
  - `sr_fmt_num(-0.001)` returns `"0.00"`.
  - `sr_fmt_ci(0.4237, 0.301, 0.5799)` returns `"0.42 (0.30 to 0.58)"`.
  - `sr_fmt_p(0.0004)` returns `"<0.001"`.
  - `sr_fmt_p(0.0042)` returns `"0.004"`.
  - `sr_fmt_p(0.0496)` returns `"0.0496"`.
  - `sr_fmt_p(0.0451)` returns `"0.045"`.
  - `sr_fmt_p(0.0449)` returns `"0.04"`.
  - `sr_fmt_p(0.0501)` returns `"0.05"`.
  - `sr_fmt_p(0.32)` returns `"0.32"`.
  - `sr_fmt_p(0.995)` returns `">0.99"`.
  - `sr_fmt_p(NA)` returns `"NA"`.
- [ ] **Step 2: Run it.** Run: `& $RS tools/run_tests.R` → FAIL.
- [ ] **Step 3: Implement `numfmt.R`.**
- [ ] **Step 4: Run it.** → PASS.
- [ ] **Step 5: Commit.** `git commit -m "writer: MCSE-limited number and p-value formatting"`

### Task 12: Shiny shell with project module

**Files:**
- Create: `app/app.R`, `app/R/ui/mod_project.R`, `tests/testthat/test-mod-project.R`, `tests/testthat/test-app-launch.R`
- Modify: `app/global.R` (source `R/ui/*.R`)

**Interfaces:**
- Consumes: `sr_project_create/open/close`, `sr_profile`, `sr_profile_caps`, `sr_mode` (Tasks 1, 4, 5).
- Produces:
  - **`app.R`**: `bslib::page_navbar(title = "StatRouter", ...)` with these panels, in this order:
    `Project`, `Data`, `Profile`, `Design`, `SAP`, `Analyses`, `Write`, `Archive`, `Reviewer`,
    `Settings`.
    - Only `Project` is functional in P1a; the others show `"Coming in P1b–P1d"`.
    - The navbar shows a badge `"<profile> · <mode>"`. In airgap mode it adds a "no network" badge.
  - **`mod_project_ui(id)` / `mod_project_server(id) -> reactive(project or NULL)`**:
    - inputs `dir` (text path), `name`, buttons `create` and `open`;
    - outputs `status` (text) and `events` (a table of seq / ts / type / actor);
    - opening shows `"read-only (locked by <host>)"` when it's read-only;
    - the session's `onSessionEnded` calls `sr_project_close`.

- [ ] **Step 1: Write `test-mod-project.R` using `shiny::testServer(mod_project_server, ...)`.**
  - **Create:** set `dir` to a temp path and `name = "demo"`, then click `create`.
    - The returned reactive has `$meta$name == "demo"`;
    - `output$status` contains `"demo"`;
    - the `project.json` file exists.
  - **Open:** write a foreign-host lock, then open → `output$status` contains "read-only".
- [ ] **Step 2: Write `test-app-launch.R`.**
  - Start `processx` Rscript on `tools/run_app.R` (port 7790), polling up to 30 s.
  - `curl::curl_fetch_memory("http://127.0.0.1:7790")` returns 200, and the body contains
    `StatRouter` and `Reviewer`.
  - Kill the child in `withr::defer`.
  - `skip_on_cran()`. The localhost-only curl is a test-only exception to the air-gap lint; tests
    aren't linted.
- [ ] **Step 3: Run them.** → FAIL.
- [ ] **Step 4: Implement** `app.R`, `mod_project.R` and `tools/run_app.R` (`shiny::runApp("app", port = as.integer(sr_opt("port", 7790)), launch.browser = FALSE)`).
- [ ] **Step 5: Run everything.** Run: `& $RS tools/run_tests.R; & $RS tools/lint.R` → all PASS, `LINT OK`.
- [ ] **Step 6: Check the UI by hand.** Run `tools/run_app.R` and open http://127.0.0.1:7790 in the
  built-in browser. Create a project, reopen it, and see its events listed.
- [ ] **Step 7: Commit and record.**
  1. `git commit -m "ui: bslib shell with project create/open module"`.
  2. Update `memory.md`: Status gets "P1a foundation done", and add any gotchas found.
  3. Commit that too.
