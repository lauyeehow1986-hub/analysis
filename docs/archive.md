# StatRouter archive guide (P1d.3)

This guide covers the freeze: the immutable snapshot of a finished analysis, the library gate and
re-baseline that follow it, the `.srproj` archive that moves a frozen project to another machine, and the
freeze diffs. The writer that makes the export files is described in [writer.md](writer.md).

StatRouter is a research and governance tool, not for clinical or diagnostic use. A freeze records what was
run; it does not certify it. Archives are **not signed** (signing is out of scope): the hashes detect
accidental damage and edits that leave the index alone, not a forger who rewrites both.

## 1. What a freeze holds

`sr_freeze(project, label)` writes `out/freezes/<label>/` (`manifest.json`, `analysis.R`, `renv.lock`) and
appends one `freeze.created` event. The folder appears whole or not at all, and the event is appended last.

`manifest.json` holds: the format and version, the label, the creation time, the project id and schema
version, the `events.jsonl` prefix (line count and SHA-256 of the lines before the freeze event), the audit
tip, the SAP hash, the library snapshot (`libsnap`, `libsnap_hash`, `lock_libsnap_hash`), a `tables` block
(store hash, rows, base table, version, derived and source flags), a `runs` block (ok key, extract SHA and
library hash of each analysis with an ok run) and `files`. Each file has a path, SHA-256, byte count and
kind:

| kind | what |
|---|---|
| `store_manifest`, `part` | the store manifest of each table and its Parquet parts |
| `source` | an in-project source file (external sources are never listed) |
| `extract`, `fit` | the hashed extract and stored result of each ok run |
| `export` | every finished `out/exports/export-<n>/` file |
| `code` | the freeze's own `analysis.R` and `renv.lock` |

Events appended after the freeze do not change its verification.

## 2. Labels

The first freeze is `submission-v1`; later ones are `revision-r1`, `revision-r2`, and so on
(`sr_freeze_next_label()`, the default). A label matches `^[a-z][a-z0-9-]{0,39}$` and is never reused.

## 3. The freeze gate

`sr_freeze` refuses, writing nothing, in this order:

1. the project is read-only;
2. the label is invalid or already used;
3. `events.jsonl` ends in an unterminated line (a torn write; the next recorded event repairs it);
4. the SAP is not locked;
5. a table's full source SHA-256 is missing (run `sr_source_hash()`);
6. a primary analysis has no successful run;
7. a stored result or its extract is missing for an ok run;
8. the project does not verify (events/audit mismatch, or an event without an audit entry).

## 4. The library gate and re-baseline

A freeze makes the library snapshot (R version, package versions, `sr.cran_snapshot` date) the baseline.
From then on `sr_run` refuses to run when the current library hash differs from the baseline. The flow:

1. `sr_run()` stops and names `sr_rebaseline()`;
2. `sr_rebaseline_diff(project)` lists what changed (`item`, `from`, `to`, `change`);
3. `sr_rebaseline(project, reason)` accepts the current library and logs a `freeze.rebaselined` event with
   the diff and the reason (refused: an empty reason, no freeze yet, a read-only project, or a library that
   already matches the baseline);
4. re-run the analysis;
5. `sr_freeze_results_diff(project, label)` compares the frozen and the new result.

`sr_lib_baseline(project)` returns the current baseline (hash, snapshot, `from` = freeze or rebaseline,
label).

## 5. The `.srproj` archive

`sr_srproj_export(project, path, label = NULL, data = c("auto", "full", "lean"))` writes a gzip tar. `path`
must end in `.srproj`, must not exist (it is never overwritten) and its folder must exist; `label = NULL`
is the latest freeze. The archive is the project as it stood at the freeze: `project.json`, `events.jsonl`
and `audit.log` cut after the `freeze.created` entry (later runs and exports are not in it), the folders of
this and earlier freezes, and the files the manifest lists. `srproj.json` is the index (SHA-256 and size of
every member, plus what was left out). A `srproj.exported` event is appended last; it records the file
name, not the folder. Owner and group names are blanked in the tar headers. The history itself is not
scrubbed: every `events.jsonl` line and `audit.log` entry has an `actor` field, which is the OS user name
unless the `sr.user` option (env `SR_USER`) is set to a role or pseudonym before the project is recorded.
The export warns when the exported history holds the OS user name. Before staging, every member the freeze
manifest lists must still hash as the manifest says (a file changed during the export is refused).

**Data modes.**

- `full`: keeps store parts and in-project sources.
- `lean`: leaves them out. A result can be read after import (`sr_result`); re-running needs the parts
  (P2 replays from the extract).
- `auto`: full when the data files total at most `sr.freeze_full_max_bytes` (env
  `SR_FREEZE_FULL_MAX_BYTES`, default 2 GiB) and none is missing from the project; otherwise lean.

An explicit `full` refuses when a data file is missing from the project, and any file of 8 GiB or more is
refused.

**External sources** are never archived. After an import the source needs `sr_source_relocate()` and
`sr_source_hash()` before a re-run. The archive's `events.jsonl` keeps the external source path, the one
absolute path the events hold, so check it before sharing an archive.

## 6. Import and what it verifies

`sr_srproj_import(archive, dir, verify_data = TRUE)` imports at any path, all or nothing: on any failure the
partial folders, and the project folder once renamed, are removed. `dir` must be new or empty, and neither
`<dir>.partial` nor `<dir>.partial.src` may exist. Steps:

0. the archive is hashed and copied into a private folder `<dir>.partial.src` (beside the target, never
   inside the imported project, removed on success and on every failure); the copy must hash the same, and
   from here on only the copy is read, so changing the original file during the import has no effect. This
   needs free disk space equal to the archive's size, for the length of the import. An archive file larger
   than `sr.srproj_max_total` plus 64 MiB (for tar headers) is refused before it is copied. A writable
   target parent folder is outside the threat model: whoever can write there can change the copy;
1. the tar headers are scanned before anything is extracted, reading each name exactly as R's internal
   extractor will (a GNU long name, the name and the prefix field); any way they could disagree is refused;
2. the copy is hashed again after the scan and after extraction, and every hash must match;
3. only the regular files the scan approved are extracted, to `<dir>.partial`, and their sizes checked
   against the headers;
4. the index is read (format known, version not newer than this build) and every member is checked against
   it: SHA-256, none extra, none missing;
5. the folder is renamed into place and opened: it must open writable, its history must verify and end
   exactly at the freeze event;
6. `sr_freeze_verify` re-hashes every file the manifest lists;
7. a `srproj.imported` event is appended and `$import` is attached to the project (label, data mode,
   archive SHA-256, parts and sources missing, the verify result).

A lean import reports the missing data under `verify$absent`; those are not problems.

**Limits and refusals.** Members are at most 8 GiB, names at most 255 bytes, the archive at most 64 GiB in
total (`sr.srproj_max_total`) and 1e6 members. Refused: any member that is not a regular file or folder
(symlinks, hard links, devices, pax `x` and `g` headers), a non-file member that declares data, an absolute, `~` or `..` path, a backslash, colon,
control character, Windows device name or trailing dot or space, case-insensitive duplicates, members under
a `.lock` folder or ending in `.partial` or `.tmp` (files and folders), a member already at
`out/freezes/<label>/srproj.json`, a GNU long name with bytes past its declared size, an embedded NUL, or
followed by anything but a regular file, bytes after the terminating NUL of a name or prefix field, a size
field with a non-octal character (also mode and mtime), a bad or base-256 header checksum and a truncated tar. If the `.partial` folder
cannot be removed after a failure, a warning names it.

## 7. Data versioning and diffs

A corrected source re-imported with `sr_reimport()` becomes the table `<t>_r<k>`.
`sr_freeze_diff(project, from, to = NULL)` returns `list(tables, library, sap, analyses)` between two
freezes, or between a freeze and the project now (`to = NULL`). `tables` lists each table with a change
(`added`, `removed`, `same`, `changed`), hashes and row counts; `analyses` lists `same`, `rerun`, `added`
or `removed`. A manifest whose snapshot lost its package names (a freeze written before the names fix) is
compared on R and the snapshot date only, with one `packages` row when the hashes differ. Analysing a new
data version needs a SAP amendment (P2).

`sr_freeze_results_diff(project, label)` compares the frozen and the current stored result of each
analysis. It reads stored fits only; nothing is recomputed. Status: `same_run`, `identical`, `changed`,
`structure`, `not_run`, `missing`.

## 8. Function list

- `sr_freeze(project, label = sr_freeze_next_label(project))` returns the project.
- `sr_freeze_next_label(project)`.
- `sr_freezes(project)` returns data.frame(label, created, manifest_sha, libsnap_hash, events_seq).
- `sr_freeze_manifest(project, label)` returns the parsed manifest.
- `sr_freeze_verify(project, label, data = TRUE)` returns list(ok, problems, checked, absent).
- `sr_lib_baseline(project)`.
- `sr_rebaseline_diff(project, snap = sr_libsnap())`.
- `sr_rebaseline(project, reason, snap = sr_libsnap())`.
- `sr_srproj_export(project, path, label = NULL, data = c("auto", "full", "lean"))` returns the project.
- `sr_srproj_import(archive, dir, verify_data = TRUE)` returns the open project.
- `sr_freeze_diff(project, from, to = NULL)`.
- `sr_freeze_results_diff(project, label)`.

## 10. The Archive panel (P1d UI)

The Archive tab is hidden until a project is open. It has five cards. Refusals show the core's message
(prefixed with the action, for example "Freeze: ..."). While any job runs, or on a read-only project, the
changing controls are refused with the shared text "wait for the running import, analysis, archive operation
or project import to finish". One job runs at a time across the app.

**Freeze.** A checklist shows every refusal of the freeze gate at once, with its reason when failing: Project
writable, SAP locked, Sources hashed, Primary analyses run, Stored results present, Project verifies. The
"Freeze label" field is prefilled from `sr_freeze_next_label()` and is checked against the label pattern
(`^[a-z][a-z0-9-]{0,39}$`) and the labels already used. "Freeze..." opens the dialog "Freeze as '<label>'":
"A freeze records the SAP, the stored results and the library, and cannot be undone. Type FREEZE to confirm."
Its Freeze button stays disabled until the box holds exactly `FREEZE`; then `sr_freeze()` runs synchronously
and the panel reports "Frozen as '<label>'".

**Freezes and verify.** The table lists label, created, events_seq and the first 12 characters of the library
hash. Pick a freeze, then:

- **Quick verify**: `sr_freeze_verify(data = FALSE)`, synchronous.
- **Verify with data**: `sr_freeze_verify(data = TRUE)` in a background worker. The worker opens the project
  read-only, so the session keeps the lock. **Cancel** kills the worker and its process tree; the notice is
  "Verify cancelled" and nothing is written.

The result is "Every file matches the freeze." or the problem list. Data files that are not in the project
folder are listed as information ("Data files not in the project folder (information, not a problem)"), not as
problems.

**Diff.** "From" is a freeze; "To" is another freeze or "project now". "Show diff" shows the non-empty
sections of `sr_freeze_diff` (Tables, Library, SAP, Analyses) with unchanged table and analysis rows left out,
or "No differences." Against "project now" it adds "Stored results against the project now" from
`sr_freeze_results_diff` (numbers to 4 significant digits). Diff reads only; nothing is recomputed.

**Export .srproj.** Choose the freeze (the latest by default), "Data files" auto / full / lean and a
destination path. The path is typed (quotes added by a Windows copy are stripped), must end in `.srproj`, must
not exist (nor `<path>.partial`) and its folder must exist; otherwise the refusal names the problem. Under the
path the panel shows the plan: for auto, "auto will pick <full or lean> (<size>)" from the same rule as the core
(`sr.freeze_full_max_bytes`, and nothing missing from the folder), with "; some data files are missing from
the project folder" when that applies; for full or lean, "<mode> data files".

- **Auto data mode** is therefore decided by the total size of the data files against
  `sr.freeze_full_max_bytes` (editable in Settings) and by whether any file is missing.
- **Actor warning.** When `sr.user` / `SR_USER` is not set, the core's warning is shown beside the plan (the
  exported history holds the OS user name as the actor), with an "Open Settings" link. Set the actor before
  recording a project that will be shared (see [settings.md](settings.md)).
- **Job and Cancel.** "Export" releases the project lock to the worker; the panel keeps a read-only copy as the
  open project while the job runs and re-opens the project when it ends. "Cancel" (on the export card) kills
  the worker. Cleanup: if an `srproj.exported` event was written, the archive is kept and the notice is "Export:
  finished before it could be cancelled (<path>)"; otherwise the archive, `<path>.partial` and the staging
  folder `out/.srproj-<label>.partial` are removed and the notice is "Export cancelled: nothing was written".
  Cleanup uses a keep list of the paths that existed before the job started, so a pre-existing path (including
  an older `.partial`) is never deleted. A worker that dies, or closing the browser session mid-job, gets the
  same cleanup. A cancel whose kill fails keeps the job and says so; it can be cancelled again.

**Re-baseline.** Shows `sr_rebaseline_diff`. It is disabled, with the reason, when there is no freeze
("freeze the project first") or the library already matches ("the library already matches the baseline"). A
non-empty "Reason" is required (`sr_rebaseline(project, reason)`); a refusal is shown verbatim.

## 11. Import in the app (Project tab)

The "Import .srproj" card is on the Project tab and needs no open project. Give the archive path ("Archive (a
.srproj file)") and the "Target folder (new, or an empty folder)". Before starting, the panel refuses: an archive
that does not exist ("archive not found: ..."), a non-empty target ("the target folder is not empty: ..."), a
missing parent folder, and an existing `<dir>.partial` or `<dir>.partial.src` ("... already exists; remove it
first").

"Import" runs `sr_srproj_import(archive, dir, verify_data = TRUE)` in a background worker with the
options snapshot in [settings.md](settings.md); "Cancel" kills it.

- **Success.** The project that was open is closed, the imported project becomes the open project, the notice
  is "Imported <dir>" and the card shows the `$import` summary (label, data mode, archive sha256 to 16
  characters, parts missing, sources missing, verify). The Write and Archive tabs appear.
- **Failure or Cancel.** Only what the job created is removed: the target folder, `<dir>.partial` and
  `<dir>.partial.src`, each only when it did not exist when the job started. A target folder that existed empty
  is kept (and put back empty). The previously open project stays open. The notice is "Import cancelled:
  nothing was imported" or "Import: <reason>".
- **Cancel after the worker finished.** If the worker had already written its `srproj.imported` event, the
  import is kept and opened, and the notice is "Import already finished; kept".
