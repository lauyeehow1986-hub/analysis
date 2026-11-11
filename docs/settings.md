# StatRouter settings guide (P1d UI)

The Settings tab is always visible. It edits the user-facing settings as **process-wide R options**
(`options(sr.<name>)`) of the app's R process. They are not per browser tab: every browser tab or Shiny
session served by that one R process shares them, and the last Set or Reset from any tab wins (another tab
sees the change the next time it re-reads a setting). Nothing is written to disk and nothing is a project
event: the audit log never records a settings change, and when the R process exits the edits are lost; a
restart starts again from the environment variables and the defaults.

StatRouter is a research and governance tool, not for clinical or diagnostic use.

## 1. How a setting is read

Each setting is the R option `sr.<name>`. `sr_opt()` reads the option, then the environment variable
`SR_<NAME>`, then the default. The panel's first card, "Effective settings", shows for each setting its value
and its source:

| Source | Meaning |
|---|---|
| edited here | set through the panel in this R process (from any browser tab it serves) |
| option | an `sr.<name>` option set some other way |
| env | the `SR_<NAME>` environment variable |
| default | the built-in default |
| auto-detected | the hardware profile when nothing sets it |

Set applies the option after validation. Reset sets the option back to `NULL`, so the environment variable or
the default applies again. While any job runs, every Set and Reset is refused with "Settings: wait for the
running import, analysis, archive operation or project import to finish" and the editors are disabled: a
worker already holds its snapshot of the options.

## 2. The catalogue

| Setting | Editor | Default | Rule |
|---|---|---|---|
| `mode` | airgap / connected | `airgap` | "mode must be airgap or connected"; then `sr_mode()` |
| `profile` | auto / the profiles in `SR_PROFILES` | auto-detected | then `sr_profile()`; `auto` resets the option |
| `user` (actor) | text | the OS user name | "the actor must not be empty (use Reset)" |
| `digits` | whole number | 2 | "digits must be a whole number, 0 or more" |
| `router_mask` | on / off | off | "router_mask must be on or off" |
| `jev_key` | masked input | not set | "jev_key must not be empty" |
| `jev_url` | text | `https://api.typesafe.ai/v1/systemone` | must not be empty; "jev_url must be an http:// or https:// URL with a host name, ..." (a DNS name, IPv4 or `[IPv6]` host, optional port 1-65535, no spaces, no `user:password@`; checked by pattern only, never resolved or contacted) |
| `jev_model` | text | `jev-latest` | must not be empty |
| `jev_timeout` | number (s) | 20 | "jev_timeout must be a positive number of seconds" |
| `freeze_full_max_bytes` | number (bytes) | 2147483648 (2 GiB) | "... must be a finite number of bytes, 0 or more" |
| `srproj_max_total` | number (bytes) | 68719476736 (64 GiB) | same rule |
| `cran_snapshot` | text, YYYY-MM-DD | none | "cran_snapshot must be a date YYYY-MM-DD"; an empty Set resets it |

The test switches `run_sync`, `ingest_sync` and `jev_transport` are not settings and are not shown. A failed
Set leaves the previous value in place and shows the message; the message never contains the typed value.

Note: because `jev_url` is checked by pattern only, a host name with an underscore or a non-ASCII (IDN) name is
rejected; use the ASCII (punycode) form of the name or an IP address.

## 3. Mode and CONNECT

Switching `mode` to `connected` opens the dialog "Switch to connected mode": "Connected mode allows network
calls (Ask Jev, behind its own gates). Type CONNECT to confirm." Anything but the exact word `CONNECT` gives
"Settings: type CONNECT exactly (capital letters) to switch to connected mode". Going back to `airgap` is one
click and also clears the session's Jev key. Ask Jev keeps its own gates (key, per-project DPA
acknowledgement, per-session consent); connected mode alone sends nothing.

## 4. The Jev key rule

The key is held only as `options(sr.jev_key)`. It is a password input, cleared right after a Set, and never
rendered back: the table says "set" or "not set". It is never logged, never put in events, the audit log or an
export, never written to a file and never part of the options snapshot a worker receives (no worker calls
Jev). Reset on the key, or Reset or Set of `mode` to `airgap`, removes it. Reset removes the option only: an
`SR_JEV_KEY` environment variable still applies afterwards, and worker processes inherit it through their
environment (not through the options snapshot).

## 5. Actor

The actor is the `actor` field of every event and audit entry written from then on, including those written
by a background worker (section 7). When `sr.user` / `SR_USER` is not set, the actor is the OS user name and
the editor shows the core's warning that the exported history would record it; the Archive panel shows the
same warning next to an export. Set a role or a pseudonym before recording a project that will be shared.

## 6. Profile and CRAN snapshot notes

- **Profile.** Changing it changes the compute fingerprint (`sr_hw_fingerprint`), so later runs refit rather
  than reuse cached fits.
- **CRAN snapshot date.** It enters the library snapshot hash (`sr_libsnap`). Changing it makes later runs show
  "library changed" against the lock, and after a freeze it can call for a re-baseline (see
  [archive.md](archive.md) section 4).

After any successful Set or Reset the top-bar badges (profile and mode, "no network") re-render. The Write panel
drops its cached model (its cache key holds the Settings version) and the Archive panel re-renders its export
plan and re-baseline diff.

## 7. The options snapshot for workers

A background worker is a fresh R process and does not inherit `options()`. Every job spawner passes the
session's effective settings: `.sr_job_opts()` returns the `sr.<name>` options of this catalogue that are set,
always dropping `sr.jev_key` and the test-only options. The worker sets them after sourcing the app, before it
opens the project, so an actor or digits edited here applies to work done in the worker. The snapshot goes to
every worker: the analyses worker, the data import (`.sr_data_spawn`), the Archive verify and export jobs and
the `.srproj` import.
