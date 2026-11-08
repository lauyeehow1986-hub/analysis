# StatRouter router guide (P1d.2)

This guide covers the System One router built in P1d.2: the research question (PICO), the decision
nodes (study design, purpose, roles, analysis choices), the rules and shadow-mode model predictions, the
Jev cloud backend and its gates, the decision table, the shadow report and export, and what the router adds
to the SAP lock and to the manuscript. The analysis layer the router feeds is in
[analysis.md](analysis.md); the manuscript side is in [writer.md](writer.md).

StatRouter is a research and governance tool, not for clinical or diagnostic use. Every output needs
review by a qualified statistician.

All calls in this guide are headless. The router panel, the payload preview dialog and the consent dialog
belong to the P1d UI plan; the calls they will use (`sr_router_table`, `sr_router_preview`,
`sr_router_consent`, `sr_router_jev_ack`) are described here.

## 1. Principles

- **Models route; they never decide.** A model answers one closed-choice question about one decision. The
  answer is logged as a `router.predicted` event and, in P1, is never applied. Only a rule or the user
  makes a `router.decided` event. The writer reads decided choices only, never a probability.
- **Rules first.** A node's rule is code. Where it decides, no model is asked.
- **Air-gap by default.** `sr_mode()` is `"airgap"` unless set otherwise, and there the router makes no
  network call of any kind. Only `app/R/router/backends/jev.R` references `curl`, and only behind the four
  gates and the payload approval of section 6.
- **Metadata only.** A question sees the research question, the table name and row count, a few facts and
  bucketed column metadata. It never sees a cell value (section 5).
- **Event-sourced.** Every prediction, decision, veto, consent and acknowledgement is an event in
  `events.jsonl` (section 4). The decision table is a view of the replayed state.
- **A decision goes stale.** Each decision stores the input hash it was made under. When the facts, the
  options or an upstream choice change, the hash changes and the decision is shown as stale (section 7).

## 2. The research question (PICO)

`sr_pico_set(project, ...)` records the whole question as one `pico.set` event. Setting it again replaces
it. The fields are `population`, `exposure`, `comparator`, `outcome`, `timeframe`, `setting`, `period`
and `aim`.

- `population` and `outcome` are required. The rest may be absent (NA).
- Each field is one string, trimmed, at most 2000 characters. An empty string is NA.
- `aim` must be one of `describe`, `predict`, `explain-causal`. It is an input to the purpose node, not the
  decision.
- The call is refused once the SAP is locked, and on a read-only project.
- `sr_pico(project)` returns the question (the eight fields and `at`), or NULL before any `pico.set`.

PICO text is typed by a person and is sent as written in every payload. Do not type patient data into it.

## 3. The routing engine

### 3.1 Nodes

A node is one routing decision with a closed set of options. Nodes are registered in dependency order
(`sr_nodes()`), so the registry order is a topological order. A node has a scope:

| Scope | Instances |
|---|---|
| `project` | One. |
| `table` | One per table that has confirmed roles. |
| `column` | One per column the node names, for each such table. |

An instance has a key: `node`, `node@table` or `node@table:column`.

The P1 nodes:

| Node | Scope | Depends on | Options | Rule | Model question |
|---|---|---|---|---|---|
| `design` | project | none | `cohort_prospective`, `cohort_retrospective`, `case_control`, `cross_sectional`, `rct`, `other` | none | Yes (6 criteria). Facts: the role counts of each table. |
| `purpose` | project | none | `describe`, `predict`, `explain-causal` | The PICO `aim`, when it is one of the three. | Yes (3 criteria). |
| `guideline` | project | `design` | `strobe`, `consort`, `none` | `rct` gives `consort`, `other` gives `none`, any other design gives `strobe`. | None. |
| `outcome_col` | table | `purpose` | the table's column names, then `none` | The one column with the confirmed outcome role; else the one column whose normalised name or label equals the normalised PICO outcome. | Yes: one criterion per column, plus `none`. |
| `exposure_col` | table | `purpose` | as `outcome_col` | As `outcome_col`, with the exposure role and the PICO exposure. Applies unless the purpose is `describe`. | Yes. |
| `outcome_type` | table | `outcome_col` | `binary`, `continuous`, `time_to_event`, `count`, `other` | Time and event roles (or an event-date role) confirmed gives `time_to_event`; the outcome has two distinct values gives `binary`; a DOUBLE, FLOAT, REAL, DECIMAL or NUMERIC outcome gives `continuous`. | None. |
| `primary_route` | table | `purpose` | the registered route ids, then `none` | `describe` gives `table1`; `explain-causal` with outcome type `continuous`, `binary` or `time_to_event` gives `lm`, `logistic` or `cox`. Nothing for `predict`. | None. |
| `covariate_timing` | column | `purpose`, `outcome_col` | `baseline`, `mediator`, `post_outcome`, `unclear` | A DQ finding with check `leakage` or `post_outcome_date` on the column gives `post_outcome`. | Yes (4 criteria). Applies when the purpose is undecided or `explain-causal`; the instances are the columns with the covariate role. |

Notes that follow from the code:

- `primary_route` depends only on the purpose. The outcome type is read into its facts, so a change of
  outcome type makes the route decision stale, but the route is not blocked while the type is undecided.
- A node waits (status `waiting`, section 7) while any dependency is undecided or stale.
- The input hashes of `design` and `purpose` also include the PICO text (the node flag `pico = TRUE`), so
  any PICO edit, the population alone included, makes both decisions stale. The other nodes do not hash
  the PICO text.
- `design`, `purpose` and `covariate_timing` carry cost-matrix danger cells
  (section 3.3). `design` and `guideline` cite the STROBE reference (`vonelm2007`).
- A node with no question (`guideline`, `outcome_type`, `primary_route`) is decided by its rule or by the
  user only.

### 3.2 The run loop

`sr_router_run(project, backends = character(), approve = character())` walks every instance in registry
order.

1. A project whose SAP is locked is returned unchanged: a router run after the lock is a no-op.
2. An instance is skipped when the node does not apply, a dependency is undecided or stale, or the instance
   is already decided and current.
3. The rule runs. A rule that names a choice outside the node's options is an error that names the node. A
   rule's choice that the node vetoes (section 3.4) is not applied: the instance stays pending and no event
   is written. Otherwise the choice is logged as `router.decided` with `resolver = "rules"`.
4. When no rule decides and the node has a question, the backends named in `backends` are asked in
   escalation order, `SR_ROUTER_ESCALATION <- c("local", "clm", "jev")`, until one would act. A backend
   that is not registered or not available is skipped without an event. Each answer is logged as
   `router.predicted`. A successful prediction made under the current input hash is not asked again; a
   failed one is asked again on the next run. `approve` is the list of payload SHA-256 values the user has
   approved (section 6).
5. If the request cannot be built (no question, more than `SR_SYSONE_MAX_CRITERIA <- 255L` criteria, or the
   state is over the token budget, section 5) the instance stays for the user.

Only `jev` is registered in P1d. The `local` (Von) and `clm` ids are reserved in the escalation order;
nothing runs under them yet. `rules` is not a backend in this chain; rules always run first.

### 3.3 Abstain rule

A prediction records `would_act`: whether the model would be allowed to act on its own if predictions were
applied. The rule is expected cost.

- The cost matrix has rows for the true option and columns for the chosen option. The diagonal is 0 and
  every other cell is 1, except each danger cell `c(true, chosen)` of the node, which is
  `SR_ROUTER_DANGER_COST <- 10`.
- The option with the least expected cost is the abstain rule's choice.
- `would_act` is TRUE only when that least expected cost is below `SR_ROUTER_ASK_COST <- 0.1` and the
  option is the model's own choice.
- `confidence` is the TypeSafe confidence, `(max(p) - 1/n) / (1 - 1/n)` over the n options (1 when n is 1).

The two thresholds are placeholders until the P3 calibration.

### 3.4 Decisions, vetoes and overrides

`sr_router_decide(project, node, choice, reason, table, column, override = FALSE)` records the user's
decision as `router.decided` with `resolver = "user"`. It is refused after the SAP lock.

- The instance must exist, apply, and have all its dependencies decided. The choice must be one of its
  options.
- A node may veto a choice (for example `time_to_event` with no time and event roles, `binary` for an outcome
  without two distinct values, a route that serves another purpose, or a column that is already the other
  role's column). A vetoed attempt appends `router.vetoed` and signals an error of class
  `sr_router_vetoed`. The condition's `$project` is the project with the veto counted, and `$reason` is the
  veto reason.
- `override = TRUE` with a reason lets a vetoed choice through. An override needs a reason.
- The suggestion is the rule's choice or, without a rule, the latest successful prediction under the current
  input hash. A choice that differs from the suggestion needs a reason and is recorded with
  `override = TRUE`. The prediction never applies by itself; it only decides whether a reason is required.
- `sr_router_choice(project, node, table, column)` returns the current choice, or NA when the instance is
  undecided or stale.

`SR_ROUTER_MAX_VETOES <- 3L`: from the third veto an instance with no decision shows `override_required`.

## 4. Events

Every event below is refused while the project is read-only. In each, `table` and `column` are NA when out
of scope.

| Event | Payload | Effect on state | After the lock |
|---|---|---|---|
| `pico.set` | population, exposure, comparator, outcome, timeframe, setting, period, aim | Sets `state$pico`. | Refused. |
| `router.predicted` | key, node, table, column, backend, model, input_hash, payload_sha, payload, raw, choice, probabilities, confidence, would_act, latency_ms, error | Sets the prediction of the key for that backend (the latest replaces the earlier). | No run is possible (the run is a no-op). |
| `router.decided` | key, node, table, column, choice, resolver (`rules` or `user`), input_hash, reason, override, suggested | Sets the decision of the key. | Refused. |
| `router.vetoed` | key, node, table, column, choice, reason | Counts one veto for the key. | Decide is refused first. |
| `router.consent` | session, granted | Sets consent for that session id. | Not restricted by the lock. |
| `router.jev_ack` | reason | Sets the project's DPA / IT acknowledgement. | Not restricted by the lock. |

`payload` is the exact request body that was sent and `raw` is the response text. An `error` is set when the
backend failed or its answer did not validate; then `choice` is NA and the prediction is never used.

## 5. What a question sees

`sr_router_state(ctx)` builds the text of a question. It is plain text, one item per line:

1. `node: <id>`;
2. the non-empty PICO fields as `field: value`;
3. for a table or column node, `table: <name>` and `rows: <n>`;
4. one `fact.<name>: <value>` line per fact of the node;
5. when the node needs them (`facts_columns`), one `column.<key>: ...` line per column, in schema order.

A column line holds `name`, `label`, `type`, `role` and `distinct`. There is no cell value of any column.

- A person count of 1 to 4 prints as `<5`; the row count follows the same rule.
- A distinct count is bucketed as `0`, `1`, `2`, `3-9` or `10+`.
- Newlines in any text become spaces, and `;` and `=` in names and labels are escaped.
- The budget is `SR_ROUTER_MAX_TOKENS <- 8000L`, estimated as `ceiling(nchar(text) / 4)`. Column lines are
  dropped from the end to fit. If any column line is omitted, or the text is still over the budget, no request
  is built and the instance is left to the user.
- **Masking.** With `sr.router_mask` (env `SR_ROUTER_MASK`) TRUE, every real column name in the payload is
  replaced by its key (`c001`, `c002`, ...) and labels are blanked, in the PICO text, the facts, the column
  lines and the question. Masking matches whole tokens, case-sensitively: a name is masked when it is not
  preceded by a letter, digit, `_` or `.`, and not followed by a letter, digit, `_` or a `.` that continues
  the token. So the name in "outcome is death." or "adults with sepsis. Then" is masked, while `death.rate`
  stays one token (a different name). The `role=` value of a column line is the fixed role vocabulary and is
  not masked. The input hash still uses the real values.
- Free text typed by the user (PICO fields, column labels) is sent as written unless masked. Masking
  replaces column names found in it; it does not remove other words.

The request is `sr_sysone_request(state, list(choice = question))`: the model name (`sr.jev_model`), the
state text and one question named `choice` with instructions and criteria (key to description). The body is
canonical JSON with sorted keys, and the payload SHA-256 is the hash of that body.

An example, from a synthetic test table of 300 rows (three covariates, an outcome, an id and a follow-up
column), with the PICO population "adults", exposure "smoker", outcome "Death", aim "explain-causal".
`sr_router_preview(project, "outcome_col", "t")` returns `key`, `sha`, `body` and `text`; the `text` is:

```
node: outcome_col
population: adults
exposure: smoker
outcome: Death
aim: explain-causal
table: t
rows: 300
fact.outcome_role: death
fact.pico_outcome: Death
column.c001: name=id; label=; type=BIGINT; role=id; distinct=10+
column.c002: name=age; label=; type=DOUBLE; role=covariate; distinct=10+
column.c003: name=sex; label=; type=VARCHAR; role=covariate; distinct=2
column.c004: name=smoker; label=; type=BIGINT; role=covariate; distinct=2
column.c005: name=bmi_6m; label=; type=BIGINT; role=covariate; distinct=2
column.c006: name=death; label=; type=BIGINT; role=outcome; distinct=2
column.c007: name=fu_days; label=; type=BIGINT; role=covariate; distinct=10+
```

and the question in the body (criteria keyed `c001` to `c007`, plus `none`) is:

```
instructions: Choose the column of the table that holds the outcome named in the PICO lines.
criteria: c001 "the column id", c002 "the column age", ..., c007 "the column fu_days", none "the option none"
```

The model's answer names a criteria key; the router maps `c006` back to the column `death` before it is
stored.

## 6. The Jev backend and its gates

Jev is the TypeSafe `POST /v1/systemone` cloud model. It is for connected mode and de-identified data
only. The gates are checked in this order, and the first one that fails stops the request:

| Order | Gate | What opens it | Message when closed |
|---|---|---|---|
| 1 | Mode | `sr_mode()` is `"connected"` (option `sr.mode` or env `SR_MODE`). | `air-gap mode` |
| 2 | Key | `sr.jev_key` (env `SR_JEV_KEY`) is not empty. | `no Jev key (set SR_JEV_KEY)` |
| 3 | DPA / IT acknowledgement | `sr_router_jev_ack(project, reason)` has been recorded for the project. The reason is required and must cite the DPA / PDPA and the institutional IT sign-off. | `no DPA / IT acknowledgement for this project` |
| 4 | Session consent | `sr_router_consent(project, TRUE)` was recorded in this R session. | `no consent in this session` |
| 5 | Payload approval | The SHA-256 of the exact request is in `approve`, as shown by `sr_router_preview`. | `payload not approved` |

Gates 1 to 4 are `available(project)`: when one fails, `sr_router_run` skips the backend without an event.
Gate 5 is checked when the request is made, so a request that was not approved is logged as a failed
`router.predicted` (an `error` and no choice) and is asked again after approval.

- The router session id is 16 hex characters made once per R process from `openssl::rand_bytes`. It never
  uses the R random-number generator, so `.Random.seed` is untouched. Consent lasts for the session id, so a
  new R process needs a new consent. Consent can be withdrawn with `granted = FALSE`.
- The acknowledgement is per project and is recorded once.
- Options (each is `sr.<name>`, then env `SR_<NAME>`, then the default):

  | Option | Default | Notes |
  |---|---|---|
  | `jev_url` | `https://api.typesafe.ai/v1/systemone` | |
  | `jev_model` | `jev-latest` | |
  | `jev_timeout` | `20` | Seconds. |
  | `jev_key` | empty | Never logged. |
  | `jev_transport` | none | Test hook: `function(url, body, key, timeout)` returning `list(status, body)`. Tests never reach the network. |
  | `router_mask` | `FALSE` | Column-name masking in every payload. |

- The key is sent only in the Authorization header. It is never in an event, the audit log, a condition
  message or the shadow export: text that may be stored (an error, a response) has the key replaced by
  `<redacted>`.
- A failure is logged, not raised. The error text is one line: `HTTP 400: <detail>`, `HTTP 422: <messages>`,
  `HTTP <status>`, `invalid JSON`, `invalid HTTP status`, or the transport error with the key removed.
- A response is accepted only when its answer has type `choice`, names one of the criteria keys, gives
  numeric probabilities in [0, 1] over known keys that sum to 1 within 0.001. Anything else is a failed
  prediction.

## 7. The decision table

`sr_router_table(project)` has one row per applicable instance. The columns are `key`, `node`, `label`,
`table`, `column`, `status`, `choice`, `resolver`, `override`, `reason`, `suggested`, `backend`,
`model_choice`, `confidence`, `would_act` and `vetoes`. The model columns come from the latest successful
prediction under the current input hash. A decision whose node is no longer registered, or whose instance
has gone (a column that left the covariate role, a table without roles), has no row: it is not current,
so a reader such as the gate or the SAP hash treats it as undecided, and never errors on it.

Once the SAP is locked, the table shows the lock's snapshot (section 9): every snapshot key is `decided`
with its stored choice (a snapshot key whose instance has gone since the lock keeps a `decided` row), and
any other decision is `stale`.

| Status | Meaning |
|---|---|
| `decided` | A decision exists and its input hash is current. |
| `stale` | A decision exists but its input hash has changed. The stored choice is still shown. Decide it again. |
| `pending` | No decision, every dependency is decided, and fewer than `SR_ROUTER_MAX_VETOES` vetoes. |
| `waiting` | No decision and a dependency is undecided or stale. |
| `override_required` | No decision and `SR_ROUTER_MAX_VETOES` or more vetoes. |

## 8. The shadow report and export

In P1 the model is observed, not used. `sr_router_shadow(project)` joins the latest prediction of each
`(key, backend)` with the current decision:

- columns `key`, `node`, `table`, `column`, `backend`, `model`, `model_choice`, `confidence`, `would_act`,
  `error`, `final_choice`, `same_input`, `resolver`, `override`, `agree` and `latency_ms`;
- `final_choice` is NA when the instance is undecided or stale;
- `same_input` is TRUE when the prediction was made under the input hash of the current decision, FALSE
  when the decision was made under other inputs, and NA when there is no current decision;
- `agree` is scored only when `same_input` is TRUE: it is NA when either choice is NA or the model saw
  other inputs than the decision was made under.

`sr_router_shadow_summary(project)` gives, per node and backend: `n` predictions, `decided` (with a final
choice), `agree_rate` (over the rows with a scored `agree`, without an error), `would_act_rate` (over
n), `act_accuracy` (agreement among the scored rows that would act, NA when none, the seed of a risk and
coverage curve) and `error_rate` (over n).

`sr_router_shadow_export(project, path)` writes the golden-set feed for P3 as JSON lines, one per
`(key, backend)`:

- it keeps the last `router.predicted` event of each pair, from the event log;
- each line holds `key`, `node`, `table`, `column`, `backend`, `model`, `input_hash`, `payload_sha`,
  `payload`, `raw`, `choice`, `probabilities`, `confidence`, `would_act`, `error`, `final_choice`,
  `decision_input_hash`, `same_input`, `resolver`, `override`, `agree` and `latency_ms`. `input_hash` is
  the prediction's; `decision_input_hash` is the one the current decision was made under (NA without a
  current decision);
- it refuses to overwrite an existing file, and writes no key;
- the payload holds metadata, including column names unless masking was on. Treat the file like the
  project's metadata.

## 9. What the router adds to the SAP lock

`sr_sap_check()` carries the router's rows just before the feasibility rows, in this order:

| Item | Status and condition |
|---|---|
| `study design` | `block` while the design is undecided or stale; otherwise `ok`, with the choice. |
| `study purpose` | `block` while the purpose is undecided or stale; otherwise `ok`, with the choice. |
| `purpose fits the analyses` | `warn` for each primary analysis whose route serves another purpose than the decided one; otherwise `ok`. Checked once the purpose is decided (while it is not, the row is `ok` and the `study purpose` row blocks). |
| `primary route` | Only when a primary route is decided. `warn` for each decided route of a table that no primary analysis on that table uses; otherwise `ok`. |
| `covariate timing` | `block` for a causal (explain-causal) primary analysis that adjusts for a column decided `mediator` or `post_outcome`. `warn` when adjusted covariates of a causal primary have no current timing decision (each table and column counted once, however many primaries adjust for it). Otherwise `ok`. |

The router also adds a part to `sr_sap_hash()`: the eight PICO fields and every current decision, ordered by
key, with its choice, resolver, override, reason and input hash. The part is absent when there is no
`pico.set` and no current decision, so a SAP that never used the router keeps its hash. After the lock
the question and the decisions cannot change: `pico.set` and `router.decided` are refused, and a router run
is a no-op.

**The lock freezes the router's view.** `sr_sap_lock()` writes a snapshot of every decision current at the
lock into the `sap.locked` payload (`router$decisions`: key, choice, resolver, override, reason, input
hash). The snapshot is not an input to the hash. Once the SAP is locked, every reader of "current" uses the
snapshot instead of recomputing input hashes: `sr_router_choice()`, `sr_router_table()`, the router part
of `sr_sap_hash()`, the gate rows, the writer and the shadow report. A snapshot key is decided with its
stored choice, and any other key is not current. So edits still allowed after the lock (roles re-confirmed,
a DQ re-run) do not make a locked decision stale and leave `sr_sap_hash()` equal to the locked hash. A lock
made before P1d has no snapshot and keeps the live view.

## 10. In the manuscript

When the design is decided, the writer adds a "Study design" paragraph to the Methods (design, objective,
setting, the guideline and a pointer to the decision log) and a supplementary table "Analysis decisions",
which holds each decided instance, its choice, who decided it, any override and the reason. No probability
appears in either. STROBE items 3 (objective), 4 (design) and 5 (setting and dates) are scored from the PICO
and the decisions. A CONSORT (`rct`) project gets a CONSORT citation and a note that the checklist is not
produced; `sr_export` then writes no `checklist_strobe.csv`. The software sentence names the reporting
guideline only when it is decided `strobe` or `consort`; for `none` or an undecided guideline it names none.
See [writer.md](writer.md).

## 11. What P3 adds

P3 (local System One) registers the `local` (Von) and `clm` backends under the ids already in the escalation
order, builds the golden set from the shadow exports, runs the bake-off (accuracy, cost-weighted error,
calibration, abstain rate, latency and memory per hardware profile), calibrates the cost-matrix thresholds,
and then enables a model per node. Until then no model output is applied.

## 12. Known limitations

- **No UI yet.** The router panel, the payload preview dialog and the consent dialog are in the P1d UI plan.
- **Only Jev is registered.** The escalation order lists `local` and `clm`, but they are skipped until P3.
- **The abstain thresholds are placeholders** (`SR_ROUTER_ASK_COST`, `SR_ROUTER_DANGER_COST`).
- **Free text leaves the machine as typed.** PICO fields and column labels are sent as written; masking
  covers column names only. A Jev request is for de-identified data and approved payloads only.
- **A payload that is too large is not sent.** A question whose state is over the token budget, or that has
  more than 255 criteria (a table with more than 254 columns for a column choice), is left to the user.
