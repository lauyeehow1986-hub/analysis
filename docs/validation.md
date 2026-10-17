# StatRouter validation record

This page records two kinds of evidence that the analysis engine gives correct numbers:

1. A **confidence-interval coverage simulation**. Each cell simulates data with a known true effect,
   runs the route's own compiled analysis code on it, and counts how often the 95% interval contains
   the truth.
2. **Textbook reproductions**. Published values are reproduced through the public path
   (SAP -> lock -> run -> result) on public data.

These are research-software checks, not a clinical validation. Results still need review by a
qualified statistician.

## 1. CI-coverage simulation

### Run details

| Item | Value |
|---|---|
| Harness commit | `bcaf5e4` (`app/R/resample/coverage.R`, `tools/coverage_sim.R`) |
| Date | 2026-10-17 |
| Sample size per replication | n = 200 |
| Seed | 1 |
| R | R version 4.5.2 (2025-10-31 ucrt) |
| Packages | stats 4.5.2, parallel 4.5.2, mice 3.19.0, sandwich 3.1.1 (survival 3.8.3, testthat 3.3.2 and withr 3.0.2 for the tests below) |
| Parallel workers | Workers do not change the results. Replication r always uses the r-th L'Ecuyer stream of the seed. |

The bootstrap intervals are computed by StatRouter itself (`app/R/resample/boot.R`). The boot
package (1.3.32) is used only as the reference in `tests/testthat/test-boot.R`.

### Pass rule

```
pass = |coverage - 0.95| < 3 * mcse,   mcse = sqrt(coverage * (1 - coverage) / (reps - failed))
```

- A coverage of exactly 1.0 has an MCSE of 0, so it does **not** pass. Over-coverage is a failure
  too.
- A failed replication is one whose fit errors or returns a non-finite primary row. Failed
  replications are counted, named, and left out of the denominator.
- **A failing cell is a finding about the method or the code. The tolerance is never changed to make
  a cell pass.**

### Cells

Every cell scores the route's primary effect row as reported.

| Cell | Data generator | True value | Analysis |
|---|---|---|---|
| cc | z1, z2 ~ N(0,1); x ~ Bern(plogis(0.5 z1)); y = 1 + 0.5x + 0.8z1 + 0.4z2 + N(0,1); z1 missing completely at random (20%) | MD 0.5 | linear model y ~ x + z1 + z2 on complete cases |
| mi_mar | Same model. z1 is missing at random, with logit P(missing) = a + std(y) + z2 and a calibrated to a mean of exactly 0.30 | MD 0.5 | linear model with multiple imputation (mice, m = 20), pooled by Rubin's rules |
| boot | z ~ N(0,1); x ~ Bern(plogis(0.5z)); y ~ Bern(plogis(-0.5 + 0.8x + 0.6z)) | marginal RD 0.18231. Exact numerical integration (`stats::integrate`, rel.tol 1e-10), checked against a 1e6-draw Monte Carlo | logistic model, marginal risk difference by g-computation, percentile bootstrap B = 500 (g-computation re-run in every replicate) |
| boot_mi | As mi_mar | MD 0.5 | linear model, bootstrap then impute (B = 200, m = 10), percentile interval |
| cluster_robust | 50 clusters. Exposure x assigned at cluster level (half the clusters); z ~ N(0,1); cluster effect u ~ N(0, 0.2); e ~ N(0, 0.8); y = 1 + 0.5x + 0.5z + u + e | MD 0.5 | linear model y ~ x + z, cluster-robust SE (sandwich::vcovCL, HC1) |
| cluster_boot | As cluster_robust | MD 0.5 | linear model y ~ x + z, cluster bootstrap B = 500, percentile interval |

### Results (seed 1, n 200, harness bcaf5e4, 2026-10-17)

| Cell | Analysis | Reps | Failed | Coverage | MCSE | Pass | Bias | Mean width | Truth |
|---|---|---|---|---|---|---|---|---|---|
| cc | lm, complete cases | 1000 | 0 | 0.9480 | 0.0070 | TRUE | -0.0059 | 0.6492 | 0.5000 |
| mi_mar | lm + MI (m 20) | 500 | 0 | 0.9380 | 0.0108 | TRUE | 0.0080 | 0.6327 | 0.5000 |
| boot | logistic RD, bootstrap (B 500) | 500 | 0 | 0.9460 | 0.0101 | TRUE | -0.0029 | 0.2747 | 0.1823 |
| boot_mi | lm, bootstrap then MI (B 200, m 10) | 500 | 0 | 0.9400 | 0.0106 | TRUE | 0.0091 | 0.6502 | 0.5000 |
| cluster_robust | lm, cluster-robust SE | 1000 | 0 | 0.9500 | 0.0069 | TRUE | -0.0006 | 0.7061 | 0.5000 |
| cluster_boot | lm, cluster bootstrap (B 500) | 500 | 0 | 0.9560 | 0.0092 | TRUE | -0.0108 | 0.6953 | 0.5000 |

**ALL CELLS PASS. There were 0 failed replications.**

The exact tool output (table and CSV block):

```
StatRouter CI-coverage simulation
pass = |coverage - 0.95| < 3 * mcse, mcse = sqrt(coverage * (1 - coverage) / (reps - failed))

           cell reps failed coverage   mcse pass    bias mean_width  truth   B  m
             cc 1000      0   0.9480 0.0070 TRUE -0.0059     0.6492 0.5000  NA NA
         mi_mar  500      0   0.9380 0.0108 TRUE  0.0080     0.6327 0.5000  NA 20
           boot  500      0   0.9460 0.0101 TRUE -0.0029     0.2747 0.1823 500 NA
        boot_mi  500      0   0.9400 0.0106 TRUE  0.0091     0.6502 0.5000 200 10
 cluster_robust 1000      0   0.9500 0.0069 TRUE -0.0006     0.7061 0.5000  NA NA
   cluster_boot  500      0   0.9560 0.0092 TRUE -0.0108     0.6953 0.5000 500 NA

ALL CELLS PASS

--- CSV ---
"cell","reps","failed","coverage","mcse","pass","bias","mean_width","truth","B","m"
"cc",1000,0,0.948,0.00702111102319284,TRUE,-0.00592402357559058,0.649186487889228,0.5,NA,NA
"mi_mar",500,0,0.938,0.0107848041243223,TRUE,0.00799078867090952,0.632733239691098,0.5,NA,20
"boot",500,0,0.946,0.0101078187557949,TRUE,-0.00285437111601478,0.274707544461359,0.182313431929984,500,NA
"boot_mi",500,0,0.94,0.0106207344378814,TRUE,0.00913376812842992,0.650185650147836,0.5,200,10
"cluster_robust",1000,0,0.95,0.00689202437604511,TRUE,-0.00056558075324048,0.706073451046424,0.5,NA,NA
"cluster_boot",500,0,0.956,0.00917213170424412,TRUE,-0.0107613497369977,0.695277798125682,0.5,500,NA
--- END CSV ---
```

The output above leaves out two things from the original run: the per-cell `seconds` column and the
header line, because they record wall-clock time and the worker count of the machine. They do not
affect the results.

### How to re-run

```
Rscript tools/coverage_sim.R [cells] [reps_scale]
```

- `cells` is `all` (the default) or a comma-separated list, for example `cc,boot`.
- `reps_scale` multiplies each cell's replications. For example, 0.1 gives a quick check, and the
  default is 1.
- Set the number of workers with the environment variable `SR_COVERAGE_WORKERS` (or the R option
  `sr.coverage_workers`). The default is the number of cores minus 2. The worker count does not
  change any result.
- The tool prints the table and a CSV block to the console and writes nothing into the repository.
- The full run takes about 4 hours serially. Almost all of that is the boot_mi cell. On many workers
  it takes well under an hour.
- To record a new run, paste its table here with the new commit, date and versions. If a cell fails,
  record it as a finding and investigate. **Never widen the pass rule.**

The smoke test `tests/testthat/test-coverage-smoke.R` runs every cell with 20 replications. It checks
the harness: the returned fields, exact repeats, and identical results with one or two workers. It
does not check coverage.

## 2. Textbook reproductions

These checks are in `tests/testthat/test-textbook.R`. Every check goes through the public path in a
temporary project.

| Data (source) | Analysis | Reproduced value | Check |
|---|---|---|---|
| `survival::lung`: NCCTG advanced lung cancer (Loprinzi et al., 1994; survival 3.8.3) | Cox model, time to death on sex | HR 0.588 (0.424, 0.816); unrounded 0.588003 (0.423718, 0.815985) | Rounded to 3 dp. Estimate equal to a direct `coxph` to 1e-8. Printed as "0.59 (0.42 to 0.82)" |
| `survival::lung` | RMST by sex up to 365 days | difference 55.97 days (55.970324) | Within 1e-4 of 55.9703. Equal to `survfit` restricted means to 1e-8 |
| `survival::gbsg`: German Breast Cancer Study Group, 686 patients (Schumacher et al., 1994) | Multivariable Cox model: hormon + age + meno + size + grade + nodes + pgr + er | 8 HRs with Wald 95% CIs, n = 686 | HR, lower and upper limits equal a direct `coxph` to 1e-8 |
| `mice::nhanes` (van Buuren, *Flexible Imputation of Missing Data*; mice 3.19.0) | lm(chl ~ bmi + age) under MI, m = 5, maxit 10, seed 1 | Estimates, SEs and df pooled by Rubin's rules | Equal to `mice::pool` to 1e-10 (df to 1e-8). CI uses t with the Barnard-Rubin df |
| NHEFS (Hernan & Robins, *Causal Inference: What If*, Program 15.1) | Standardisation (g-computation) of weight change 1971-82 on quitting smoking, with the book's outcome model. Fitted on the 1566 rows with an outcome, standardised over all 1629 | est 3.51857, 95% CI (2.64414, 4.45720); percentile bootstrap B = 2000, seed 1 | Published value 3.52 (2.6, 4.5). The test requires the estimate within 0.01 and each limit within 0.15 |

### Notes on NHEFS

- **Outcome model.** The book's model is wt82_71 ~ qsmk + sex + race + age + age^2 + education
  (factor) + smokeintensity + smokeintensity^2 + smokeyrs + smokeyrs^2 + exercise (factor) + active
  (factor) + wt71 + wt71^2 + qsmk x smokeintensity.
- **The squared term.** P1c interactions accept only linear or factor terms, so smokeintensity^2
  enters as a data column (`smokeintensity_sq`). This is the same model: a hand-written `lm` with
  `I(smokeintensity^2)` gives the same estimate.
- **Where the dataset comes from.** The NHEFS dataset is never committed. The value above was
  obtained once from a locally installed copy of the dataset (the causaldata package).
- **How the test runs.** The test reads the CSV named in the environment variable `SR_NHEFS_CSV` and
  skips when that variable is unset. A synthetic stand-in with the same column names always runs. It
  checks that the spec compiles and that the route's g-computation equals the hand computation to
  1e-8.

### Re-running the textbook checks

```
Rscript tools/run_tests.R textbook
```

To include NHEFS, set `SR_NHEFS_CSV` to a local copy of the book's CSV first.
