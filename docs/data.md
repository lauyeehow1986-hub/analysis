# StatRouter data guide

This guide explains, in plain language, how StatRouter takes your data in, what it checks, and what
you are asked to decide. It is written for the person running the analysis, not for a developer.

Everything on this page runs on your own computer. StatRouter never sends data anywhere and makes
no network calls. All numbers below are the values coded into the application; if a threshold is
changed in the code, this page must change with it.

## 1. What you can import

StatRouter turns every source into a column-store (Parquet) copy inside the project folder. The
original file is never modified.

| Kind | Formats |
|---|---|
| Delimited text | `.csv`, `.tsv`, `.txt` (also inside `.gz`, `.bz2`, `.xz` or `.zip`) |
| Spreadsheets | `.xlsx`, `.xlsm`, `.xls` |
| Statistical packages | SPSS `.sav` / `.por`, Stata `.dta`, SAS `.sas7bdat` / `.xpt` |
| R | `.rds`, `.RData` |
| Columnar and database | Parquet, Feather, `.fst` (needs the optional fst package), SQLite, DuckDB |
| JSON | NDJSON (one record per line) and JSON documents |
| REDCap | ODM XML export (one row per subject, event and repeat instance; read in one piece, so not resumable) |

For formats with a recognisable signature (Parquet, SQLite, DuckDB, Feather, SPSS, Stata, SAS, R and so on), the first bytes of the file win over its extension; plain text and a few other formats are told apart by the extension alone.

### Limits

| Limit | Value | What happens above it |
|---|---|---|
| Excel file size | 200 MB on disk | Refused before anything is written |
| Excel rows | 1,048,576 (the Excel limit) | Refused before anything is written |
| JSON document (not NDJSON) | 512 MB uncompressed | Refused; use NDJSON instead |
| File size for a copy | above 2 GB on disk | External mode is suggested (section 2) |
| Free disk space | see section 3 | Refused when the space needed is not available |

Text files are read in 8 MiB blocks, so the size of the file does not decide the memory used.

### How text columns get their type

Delimited text is read as text first. The type of each column (whole number, decimal, date,
timestamp, time, true/false, or text) is decided once from a sample of the first 1,000,000 rows.
Two rules protect you from silent damage:

- A whole number with a leading zero (a postcode, a patient number such as `007`) stays text.
- If a later value does not fit the decided type, the column is widened (whole number to decimal to
  text) and the import restarts automatically. The widening is written to the checkpoint so it is
  traceable.

### Resuming an interrupted import

A large import is written in parts. After each part is safely on disk the project records it in a
checkpoint. If the program is stopped (power cut, drive removed, window closed), importing the same
file again resumes from the last committed part. A resume is only allowed when the source file is
unchanged, the parse settings are unchanged, and every committed part still matches its recorded
SHA-256. Otherwise the import starts again from the beginning and says why.

Two cases cannot resume and are read in a single pass:

- a `.gz` file (a gzip stream cannot be entered in the middle);
- a text file that has line breaks inside quoted fields (the position of a record boundary cannot
  be known without reading from the start).

Sources that need staging (`.bz2`, `.xz`, `.zip`, and text that is not UTF-8) are first unpacked or
re-encoded to a staged `source.csv` in the project, then read in the normal way.

## 2. External mode (very large files)

By default the source file is copied into the project (`data/source/`) with its SHA-256 recorded.
For a file larger than 2 GB on disk, StatRouter suggests external mode instead: the file stays where
it is, and the project records its path and a fingerprint (size, modification time and a SHA-256 of
three 1 MiB blocks taken from the start, middle and end).

- Use it for sources up to about 100 GB that live on a data drive.
- The project folder then needs space only for the Parquet store, not for a second copy.
- Before every later use the fingerprint is compared with the one recorded at import. If the file
  has changed or has moved, StatRouter says so rather than silently using different data.
- A full SHA-256 of an external source is computed later (the readiness panel shows it as pending)
  and is required before the analysis plan is frozen.

You can choose external mode yourself for a smaller file, or decline it for a larger one.

## 3. What the pre-flight checks

Before anything is written, StatRouter checks that the import is going to fit and warns about
risky locations. Nothing is created in the project if the pre-flight refuses.

| Check | Rule |
|---|---|
| Space for the Parquet store | 0.30 x the uncompressed size of the data |
| Space for DuckDB spill files | 1 x the DuckDB memory cap of the hardware profile |
| Headroom | the larger of 2 GB or 5% of the uncompressed size |
| Space for the source copy | the file's size on disk (not needed in external mode) |
| Compressed sources | for gz, bz2 and xz the uncompressed size is estimated from a 1 MB sample and is never assumed to be less than 3 x the file; for a zip the exact length of the member is used |
| Excel and JSON limits | a file over 200 MB (Excel) or a JSON document over 512 MB uncompressed is refused here; the 1,048,576-row Excel cap is enforced later by the reader, before any part is written |
| Free space unknown | refused (it fails closed) |
| Network share | a UNC path or a remote drive is flagged, never probed |
| Time | estimated from the size at 4 MB/s (locked16) or 9 MB/s (laptop) of uncompressed text |

The hardware profile decides the memory budget:

| Profile | DuckDB memory cap | DuckDB threads |
|---|---|---|
| locked16 (16 GB locked-down PC) | 6 GB | 4 |
| laptop | 14 GB | 12 |

Work that does not fit in the memory cap spills to temporary files under `data/tmp` inside the project.

A data dictionary (variable, label, type, units, min, max, levels, missing codes) can be attached
to a table from a generic CSV, a REDCap data dictionary CSV, or REDCap ODM XML metadata, or it is
taken from the labels a statistical package file carries. The dictionary is optional but valuable:
its levels, bounds and missing codes feed the checks in section 4, and a dictionary bound replaces
the built-in plausible range for that column.

## 4. The data-quality (DQ) checks

After import you profile the table, then run the DQ checks. The profile and every check read the
whole table with SQL inside DuckDB; only the answers come into R. A few checks (the robust range
used by the outlier check, the missingness patterns, the standardised differences, collinearity and
leakage) use a fixed, reproducible random sample of at most 100,000 rows. The same sample (same
seed) is used every time, so a re-run on unchanged data gives the same findings.

No check uses a p-value. Every threshold is a share of rows, a count, or an effect size, so a very
large table does not make a check fire more easily.

Each finding has a severity:

| Severity | Meaning |
|---|---|
| info | Worth knowing; you do not have to act |
| warn | Probably a data problem; you are asked to decide an action (section 6) |
| block | Must be decided before the table is ready for analysis |

### 4.1 Conformance: does the data look like what it is declared to be?

| Check | Fires when | Severity |
|---|---|---|
| coercion | a text column is at least 95% numeric (or date) text, but some values do not parse | warn |
| dict_type | the dictionary type disagrees with the stored type | warn |
| dict_levels | values lie outside the dictionary levels | warn |
| dict_range | values lie outside the dictionary min / max | warn |

The three dictionary checks do nothing when the table has no dictionary.

### 4.2 Completeness: what is missing, and why?

| Check | Fires when | Severity |
|---|---|---|
| missing_col | a column has at least 5% missing | info; warn at 30% or more; block at 30% or more if the column has a protected role (see section 5) |
| missing_row | a row has more than half of its fields missing | info below 1% of rows, otherwise warn |
| missing_codes | a sentinel value stands in for "missing". Numeric column: the code (-99, -999, -9999, 999, 9999, -1, -9, plus any missing codes declared in the dictionary) is at least 0.5% of the non-missing values AND lies more than 10 x MAD from the column's median (any value other than the median when MAD = 0), so -1 on a -1/0/1 scale is not flagged. Text column: one of "NA", "N/A", ".", "-", "?", "unknown", "missing" or empty text is at least 0.1% of the values | warn |
| missing_pattern | the ten most common joint missingness patterns are listed | info |
| missing_dependence | for a column with at least 5% missing: the missingness depends on other columns, i.e. the largest standardised mean difference of the other columns between its missing and observed rows is at least 0.2 (info) or 0.5 (warn) | info or warn |

For missing_dependence a group needs at least 30 rows, and a categorical column is considered only
up to 20 levels.

### 4.3 Plausibility: are the values and dates believable?

| Check | Fires when | Severity |
|---|---|---|
| range | a known analyte (age, weight, heart rate and so on) lies outside its hard range, or outside its soft range | block (hard); info (soft) |
| unit_mix | values fit the alternative unit of an analyte (for example pounds for kilograms, mg/dL for umol/L): at least 0.155% of the column's non-missing values and at least 3 values | warn |
| heaping | every non-missing value of the column is a whole number and 40% or more of them end in 0 or 5 (20% is expected); needs 200 values and 20 distinct values | warn |
| outlier | a value is more than 6 robust SDs from the median and outside the plausible range | info |
| future_date | a date is after the run date plus one day | block |
| old_date | a date is before 1900-01-01 | warn |
| date_order | a later date comes before an earlier one (admission, surgery, discharge; birth before every date; time zero before event or censor) | warn |
| immortal_time | exposure dates fall after time zero, so the exposed rows were immortal until exposed: strictly more than 1% of the exposed rows that have both dates | warn |

The analytes, their name patterns, units and ranges come from a template shipped with the
application (`inst/templates/plausibility.json`). If the dictionary gives a min or max for a column,
that value replaces the built-in hard and soft limit on that side.

### 4.4 Uniqueness and structure

| Check | Fires when | Severity |
|---|---|---|
| dup_rows | rows are exact duplicates (the extra copies are counted) | warn |
| dup_id | a value of the id column repeats on rows that are not exact duplicates | block |
| nzv | near-zero variance: the most common value is more than 19 times as common as the second (95/5) and fewer than 10% of values are distinct; needs 20 rows | info for a protected role, otherwise warn |
| high_card | a text column with more than 50 distinct values and distinct values / rows above 0.5; id and ignore roles are skipped, and so is text that parses as numbers or dates | info |
| collinear | two numeric columns have an absolute Pearson r of 0.98 or more, on the sample, with at least 30 complete pairs (at most 200 columns examined) | warn |
| cluster_detect | an id column where at least 1% of the values are repeats, or a column named like site / hospital / centre / surgeon / cluster has 2 to 200 groups of at least 5 rows | info |
| offset_detect | a positive follow-up / person-time column is found | info |

### 4.5 Leakage (outcome-blind)

| Check | Fires when | Severity |
|---|---|---|
| leakage | one column alone predicts the binary outcome with an AUC of 0.99 or more (either direction) | block |
| separation | a level of a categorical column (at least 10 rows, a column with 2 to 50 levels) never varies in the outcome | warn |
| post_outcome_date | a date column is mostly after the event date | warn |

Columns that are deterministic for the outcome by construction (id, cluster, time zero, event and
censor dates, time, event) are not examined, and while the plan is not locked the exposure
columns are skipped too (see section 5).

## 5. The outcome-blind rule

Looking at how the outcome relates to the other columns before the analysis plan is fixed is one of
the easiest ways to bend an analysis towards a wanted answer. So until the statistical analysis
plan (SAP) is locked, StatRouter is outcome-blind:

- The rule covers the whole **outcome family**: the columns with the role outcome, event,
  event_date, censor_date, time or offset. A table made from another one (a cleaned table, a join,
  a cohort) inherits the family of its sources, by column name, so a role set on the source later
  still reaches it.
- The outcome column gets only completeness and plausibility marginals: its type, number and share
  missing, minimum, maximum, distinct count and most frequent values. Its mean, standard deviation
  and quantiles are not even computed. Every family column keeps its own missingness and
  plausibility checks.
- No output may cross a family column with another column. The data viewer and the sample the
  checks use leave the family columns out; a missingness pattern or a missingness-by-covariate
  check never involves one. Cross-tabulations, odds ratios, risk ratios, hazard ratios, AUCs and
  by-outcome differences are refused by a structural check before anything is shown.
- Leakage and separation findings, while blind, name the column and nothing else. The AUC, the
  level, the direction and the counts appear only after the SAP is locked. A date that is mostly
  after the event date, and a pair of dates out of order where one is a family date, are flags of
  the same kind: the later column is named, with no count.
- A recipe step that touches a family column (its column, or any column of its filter) shows no
  counts in the recipe or the flow, and neither does any step after it. A cleaned table made by
  such a recipe, a cohort with such a step, and anything derived from either is
  **outcome-dependent**: its row count, its column counts, the counts of its DQ findings and the
  counts of a diff against it are hidden until the SAP is locked. This stays so when the branch
  that made it is switched or undone. Run the DQ checks again after the lock to see the counts.
- A check that fails while blind records only its name, not its error message.
- The text of every suggested reason is built from the check, the column and the counts only, never
  from an example value or an outcome statistic.

Some roles are protected because deleting or silently changing them would change the question
being asked. The protected roles are: outcome, exposure, id, cluster, time_zero, time and event.
For a protected column the suggestor never proposes dropping the column; it proposes
keep_sensitivity (or recheck for leakage) instead.

## 6. The Data Action Suggestor: the six actions

For every warn or block finding StatRouter suggests one of six actions. You can accept it or
override it. An override needs a typed reason, and every decision is recorded in the project's
event log and audit trail.

| Action | What it means | Changes the data? |
|---|---|---|
| recheck | Look at the source: the value may be a typing, export or coding problem. Nothing is changed | No |
| fix | Correct the values (convert a unit, cast a type) through a recipe step | Yes, in the cleaned copy |
| drop_cases | Remove the offending rows (for example exact duplicates) | Yes, in the cleaned copy |
| drop_var | Remove the column | Yes, in the cleaned copy |
| set_missing_impute | Turn sentinel values into real missing values, to be handled by the analysis's missing-data method | Yes, in the cleaned copy |
| keep_sensitivity | Keep the data as it is and plan a sensitivity analysis | No |

The default suggestion for each check:

| Check | Suggested action |
|---|---|
| unit_mix | fix |
| missing_codes, missing_col | set_missing_impute |
| dup_rows | drop_cases |
| nzv, collinear, high_card | drop_var (keep_sensitivity if the column has a protected role) |
| leakage | drop_var (recheck if the column has a protected role) |
| separation, outlier, immortal_time, heaping, missing_pattern, missing_dependence | keep_sensitivity |
| coercion, range, future_date, old_date, date_order, dup_id, missing_row, dict_type, dict_levels, dict_range, post_outcome_date, cluster_detect, offset_detect | recheck |

The suggestor proposes the recipe step itself only when it follows from the finding alone: a unit
conversion for unit_mix, and set-to-missing for missing_codes. dup_rows and missing_col have no
recipe step, so your decision is recorded and the step is marked pending.

Nothing is changed in the original table. Fixes become steps of a recipe that is applied to produce
a separate cleaned table; the recipe can be reviewed, undone step by step and re-applied.

The readiness panel lists what still blocks analysis:

| Item | Status when not satisfied |
|---|---|
| Ingest complete | block |
| Dictionary attached | warn |
| Roles confirmed, outcome role set | block |
| DQ run matches the current data | block |
| Every block finding has a decision | block |
| Every warn finding has a decision | warn |
| Recipe applied (newer steps than the cleaned table) | warn |
| Full SHA-256 of the source known | warn until the plan is frozen, then required |

## 7. Joining tables and building a cohort

A join reports its key quality first (unmatched keys on each side, duplicate keys, whether the join
would multiply rows). A type mismatch between the keys (a number against text) is refused rather
than silently cast. The cohort builder starts from the distinct ids of a base table, picks an index
date per id (first or last), applies include and exclude criteria with optional windows around the
index date, aggregates features (first, last, count, min, max, mean, any) inside windows, and can
derive an outcome with an event time and a censoring time. Both produce a new table whose defining
SQL is recorded in the event log, so the project can reproduce it.

## 8. Scale test (for large data)

`tools/scale_test.R` checks that a big file can be ingested, killed and resumed, profiled and
DQ-checked inside the memory cap of the chosen hardware profile.

    Rscript tools/scale_test.R [rows=20000000] [cols=60] [dir] [--clean] [--profile=locked16|laptop]

It prints wall times, the peak memory use, the size of the spill folder and of the Parquet store,
and ends with PASS or FAIL. The pass rule is: peak resident memory below the DuckDB cap plus 2 GB.
The full 20,000,000 x 60 run is meant to be run on the locked-down 16 GB PC and needs the free
space that the pre-flight reports.
