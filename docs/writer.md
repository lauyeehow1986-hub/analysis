# StatRouter manuscript writer guide (P1d.1)

This guide covers the writer built in P1d.1: the document model, the placeholder registry, the claim
guard, small-cell suppression, the Word files, the export folder, the journal presets and the STROBE
checklist. The analysis layer it reports on is described in [analysis.md](analysis.md).

StatRouter is a research and governance tool, not for clinical or diagnostic use. Every manuscript needs
review by a qualified statistician before submission.

## 1. What the writer does

The writer turns a locked and run project into a manuscript. It never computes a number: every number in
the output is a placeholder whose text is evaluated R output, and the prose is a template over those
placeholders.

- `sr_doc_model(project, opts)` builds the model: the main text, the supplement, the tables, the figures,
  the reference list and the STROBE checklist, plus the placeholder registry.
- `sr_claim_guard(model)` checks every template, caption and footnote against the claim rules (section 4).
- `sr_export(project, opts)` runs the guard, then writes the export folder (section 2). A guard block
  refuses the export.
- `sr_writer_opts(preset, suppress_k, table2, alpha)` validates the options of a build.

The SAP and the feasibility report are Word files of their own, written by `sr_sap_docx()` and
`sr_feasibility_docx()`. Both are plain text with no placeholders, and neither is run through the guard.

## 2. The export files

`sr_export()` writes `out/exports/export-NNN/` in the project folder. The folder is written as
`.export-NNN.partial` and renamed once complete, so an export is whole or absent. An export folder is never
overwritten. Each file is recorded in a `doc.exported` event with its SHA-256.

| File | What it is |
|---|---|
| `manuscript.docx` | The main text: Methods, Results, the main-results table, the main figures and the reference list. |
| `supplement.docx` | The supplementary tables and figures, the sensitivity analyses (when there are any) and the STROBE checklist. |
| `figures/*.png` | One PNG per figure block, named by the block id, at the preset's width, height and resolution. Flow diagrams go through `sr_flow_plot()`. |
| `analysis.R` | The code that ran, for each reported analysis. It re-runs the analyses (section 7). |
| `renv.lock` | The library snapshot as JSON: the R version, the packages and their versions. |
| `references.json` | The cited references in CSL-JSON, in order of first use. |
| `references.ris` | The same references as RIS. |
| `placeholders.json` | The registry rows: id, kind, text, analysis, source, suppressed, and a value only when the row is not suppressed. |
| `checklist_strobe.csv` | The 34 STROBE items: item, section, label, status (green, amber or red) and detail. |
| `manifest.json` | The export id, the creation time, the audit tip, the SAP hash, the preset, `suppress_k`, and the relative path and SHA-256 of every other file. |

Outside the export folder: `out/sap/SAP_v1.docx` (locked; `SAP_draft.docx` before the lock, with a DRAFT
footer) and `out/feasibility/feasibility-NNN.docx`. Each has its own `doc.exported` event.

## 3. Placeholders

The only token forms are:

- `{{E<n>}}`: a number or numeric text. Bookmarked in the Word file (section 8).
- `{{L<n>}}`: a data-derived label or a fixed constant that holds a digit ("95%", "80%", "SHA-256",
  "Table 1"). Not bookmarked.
- `{{C:<key>}}`: a citation, rendered as `[k]` in order of first use.

No template holds a digit outside a token. A number typed into prose is a G1 block.

A token's source is `<prefix>:<rest>`:

| Source | Meaning |
|---|---|
| `effect:<analysis>:<row>:<field>` | An effect row. Fields: `est`, `lo_hi`, `p`, `n`, `events`. |
| `family:<fid>:<row>:<field>` | A multiplicity family row. Fields: `p`, `p_adj`. |
| `fragment:<analysis>:<name>:<k>` | The k-th number of a route's text fragment, lifted by `sr_ph_lift()`. |
| `deviation:<row>:<k>` | The k-th number of a deviation reason. |
| `feas:<analysis>:<field>` | A feasibility figure: `n`, `events`, `h`, `mde`, `delta`. |
| `sap:locked_at`, `sap:sap_hash` | The day of the lock, and the first 16 hex characters of the SAP hash. |
| `libsnap:r` | The R version that wrote the manuscript. |
| `label:<text>` | The text itself. |

Route tables are the exception: their cells hold the route's own evaluated text (rulings R6 and R10). The
guard does not scan them. `sr_ph_verify(reg, project)` re-derives every row from its source and compares
it with the registered text.

## 4. Claim guard

`sr_claim_guard(model)` returns one row per finding: block, analysis, rule, severity and detail. Zero rows
means clean. Sentences split at ". " only. The guard blocks an export on any "block" row; a "warn" row is
reported and does not block.

| Rule | Blocks (severity) | Example |
|---|---|---|
| G1 `digits` | A number typed into the text, the main-table header or cells (block) | "There were 12 deaths." |
| G2 `causal_language` | Causal wording ("reduced", "leads to", "effect of", ...) on a route not declared explain-causal (block) | "Treatment reduced mortality ({{E1}})." on a describe route |
| G3 `risk_factor` | "risk factor" or "determinant" on a predict route (block) | "Age is a risk factor for death." on a predict route |
| G4 `significance` | "significant" with no p value in the sentence, with a p value on the wrong side of alpha, or in a describe analysis (block) | "The difference was significant ({{E7}})." with p = 0.20 |
| G5 `absence_of_evidence` | "no difference", "did not differ", "no association", ... (block) | "There was no difference between the arms." |
| G6 `direction` | "higher" (or "lower") against an estimate below (or above) its null (block). Two estimates in one sentence: "direction not checked" (warn) | "Mortality was higher with the drug ({{E3}})." with an odds ratio of 0.70 |
| G7 `hypothesis_generating` | In a Results sentence of an exploratory analysis: no "exploratory" or "hypothesis-generating", or confirmatory wording such as "confirmed" (block) | "This confirms the effect ({{E5}})." |
| G8 `table2` | A covariate coefficient quoted in the prose of an explain-causal route (block; the Table 2 fallacy) | "The coefficient for age was {{E4}}." |
| G9 `non_collapsible` | A conditional OR or HR not called "conditional" or "adjusted" (warn) | "The odds ratio was {{E2}}." from a conditional logistic model |

G4 reads only statistical negations: "not (statistically) significant" is a negation, "not clinically
significant" is not. The STROBE checklist is not guarded (ruling R27).

## 5. Suppression

- `suppress_k` (default 5, the floor `SR_SDC_K`; `sr_writer_opts()` refuses a lower value) is one threshold
  for the whole document, so prose, tables and figures agree (ruling R15).
- A count of 1 to `suppress_k` - 1 prints as `<5` (the registry text). Zero is shown.
- A lifted count in prose is masked when it is put in the registry (ruling R13): its `value` is not
  stored, and `placeholders.json` marks it suppressed.
- In tables, `sr_sdc_table()` hides the count cells of a row and of a categorical block. A row or block
  that holds exactly one hidden cell also hides its smallest other non-zero cell. The two rules repeat
  until nothing changes (ruling R8). Only count cells ("3" or "12 (4.5%)") are masked; "m (sd)" and
  "n [lo to hi]" are not.
- In figures, the Kaplan-Meier number-at-risk strip and the flow diagram print `<5` for a count of 1 to 4.
- In prose, a whole number is a count when the word after it is rows, events, cases, participants,
  patients, deaths, subjects, observations or excluded. A knot count or a bootstrap B is never masked.

## 6. Journal presets

Presets are in `inst/templates/journals.json`. `sr_journal_preset(name)` returns one. All four are
`verified = FALSE`: check each against the journal's author guide before submission.

| Preset | Font | Size (pt) | Figure width (mm) | Height (mm) | DPI |
|---|---|---|---|---|---|
| `default` | Calibri | 11 | 170 | 120 | 300 |
| `bmj` | Arial | 11 | 171 | 120 | 300 |
| `lancet` | Times New Roman | 12 | 170 | 120 | 300 |
| `jama` | Arial | 12 | 178 | 120 | 600 |

The text width is the figure width (at least 6.3 in), and the side margins give way to it on the A4 page.
With `jama`'s 178 mm figures the side margins are about 0.63 in, against about 0.79 in for `default`. The
preset sets the font, the text size and the figure sizes only. It does not check the journal's rules on
structure, word counts or references.

## 7. Re-running analysis.R

`analysis.R` holds, for each reported analysis, the RNG setup, the extract check and the compiled route code
(the code that ran). It needs only R, the packages of `renv.lock`, `openssl`, and the extract:

1. Make sure the packages are installed at the versions in `renv.lock`.
2. Set `SR_PROJECT_DIR` to the project folder, which must hold `data/extracts/<sha>.rds`.
3. Run it in a fresh R process and inspect the results, for example:
   `Rscript -e 'e <- new.env(); sys.source("analysis.R", envir = e); str(e$sr_results)'`

The script stops with an error if the extract's SHA-256 has changed. The export itself refuses to write
`analysis.R` when the code no longer matches the latest ok run ("run it again"); that check uses the library
hash and hardware fingerprint recorded with the run, so an export on another machine still passes when the
code is unchanged.

**Lockfile caveat.** `renv.lock` records the R version of the exporting machine, not the one you re-run on.
Its `Repositories` list is empty and every package is marked CRAN, so it is a version record, not a full
renv lockfile.

## 8. What P2 builds on

- **Bookmarks.** The first use of each `{{E<n>}}` token in a Word part is bookmarked as `sr_E<n>`, with the
  registry text as its text. `sr_docx_bookmarks(path)` reads them.
- **`placeholders.json`.** Each row gives the token id, kind, text, analysis and source, so P2 can
  re-derive a number from its source (`sr_ph_verify()` does this on the project).
- **`manifest.json`.** It carries `suppress_k`, the preset, the SAP hash and the audit tip, which a
  refresh needs to rebuild the same registry.

"Refresh numbers" in an edited Word file is P2. The writer stores the bookmarks and `placeholders.json`
only.

## 9. Known limitations

- **Differencing (R12).** Totals shown next to masked counts can reveal them. The flow box (`n_before` minus
  `n_after`) and an Overall column minus a visible group count can recover a 1 to 4 count. The same holds
  for a header n (for example "B (n=<5)" recovered from Overall minus A). Masking totals would gut the flow
  diagram and Table 1, so this is accepted residual risk. Complementary suppression of totals is later work.
- **A percent next to a masked count stays visible.** A text such as "3 events (1.5%)" keeps the percentage,
  which reveals the count with a known denominator.
- **Only first-use bookmarks refresh (R30).** Later uses of the same token are plain runs and are not
  bookmarked. A P2 refresh updates only the bookmarked uses.
- **Open one output in real Word before the freeze.** The section properties (sectPr child order) have not
  been checked in Word.
- **A crash can leave an orphan export.** If the process dies between the folder rename and the
  `doc.exported` append, `out/exports/export-NNN/` is left without an event. The next export then gets the
  same id and is refused. Check that no event lists the folder, then delete it by hand.
- **The same class applies to the SAP and feasibility files.** Each is written before its event is appended.
  A failed append leaves a file that blocks a retry (`SAP_v1.docx` is never overwritten), so remove it by
  hand after checking the event log.
- **Presets are unverified** (section 6).
- **STROBE items 1 to 5 stay amber** until the router records the PICO and the study design. They are for
  the authors to write.
- **Method references need re-checking.** The method references were copied from the plan and must be
  checked against the publisher's records before release.
