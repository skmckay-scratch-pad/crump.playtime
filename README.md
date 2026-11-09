# crump.playtime

A sensitivity and uncertainty analysis of **annual failure probabilities for
infrastructure assets**, and of how they depend on condition grades. It covers
31 asset groups × condition grades A to F-, plus a worst-case conditional
failure probability (CF). The analysis is done in **R** and follows the global
sensitivity workflow of Cushway et al. (2026). The results can be explored in
several web-app frameworks so they can be compared:

| App | Framework | Server needed? | Where |
|---|---|---|---|
| **Main app** (8 tabs, live recalculation, system builder) | Shiny + bslib + plotly | Yes (R) | `apps/shiny/`, run locally |
| Shinylive app | Shiny compiled to WebAssembly (webR) | No | GitHub Pages `/shinylive/` |
| Quarto dashboard | Quarto + plotly + Observable JS | No | GitHub Pages `/quarto-dashboard/` |
| flexdashboard | R Markdown + plotly + crosstalk | No | GitHub Pages `/flexdashboard/` |
| Analysis report | Quarto HTML | No | GitHub Pages `/report.html` |

See [`plan.md`](plan.md) for the approach and the decisions behind it.

## Quick start

```bash
# 1. Install R packages (R >= 4.3). DESCRIPTION lists everything needed.
Rscript -e 'install.packages("pak"); pak::local_install_deps(dependencies = TRUE)'

# 2. (Optional) Rerun the analysis. Takes about 1 minute; writes results/*.rds
Rscript analysis/run_analysis.R

# 3. Launch the main Shiny app
Rscript -e 'shiny::runApp("apps/shiny")'

# 4. Run the tests
Rscript -e 'testthat::test_dir("tests/testthat")'

# 5. Build the static site (needs the Quarto CLI)
Rscript scripts/build_site.R
```

Pushing to `main` or to the development branch runs `.github/workflows/pages.yml`,
which runs the tests, builds `site/` and deploys it to GitHub Pages.

## What the analysis does

1. **Data checks and exploration.** Every curve rises steadily from A to CF. On
   the logit scale each curve has three segments (A–C, C-–D-, F+–F-), with a
   **large jump at D- → F+**.
2. **Curve shape.** A descriptive fit, `logit(p) = a_s + b_s·log(g + c)`, is
   within 2% of the table for every group.
3. **Notch sensitivity.** How much P(fail) changes if a grade is off by ±1 or ±2
   notches.
4. **Misclassification.** Expected P(fail) when inspectors can mis-grade. The
   curves are convex, so the expected value is usually higher than the table
   value.
5. **Multi-year risk.** Cumulative failure probability over 1 to 50 years, with
   three deterioration options: none, a fixed schedule (one notch every *k*
   years), or Markov (one notch with probability *q* each year).
6. **Global Sobol' sensitivity** with `sensobol`: Saltelli and Jansen
   estimators, bootstrap CIs, and dummy-parameter screening.
   - **L2, single asset:** for every group × grade, the inputs are the grade
     (±1 notch), curve level, slope and F-jump (±20%), and the deterioration
     rate for 10-year runs.
   - **L3, facility:** an example facility (invented for illustration) plus 100
     random facilities, comparing five aggregation structures: series,
     parallel, limiting factor, mean, and expected failures. Friedman and
     Wilcoxon tests compare the structures.
7. **Cross-group patterns.** Spearman correlations between curve features and
   sensitivity indices.

> **Iteration cap.** At the project's request, no iteration count exceeds
> **250**: the Sobol' base sample (n = 250), the bootstrap replicates
> (R = 250), and the random facilities (100). The paper grew n until every CI
> was narrower than 0.05, which took n ≥ 60,000. At n = 250 none of the runs
> meet that target, so many inputs cannot be told apart from the dummy. Read
> "non-influential" as "not shown to be influential at this sample size". To
> raise the cap, change `ITER_CAP` in `R/sobol_analysis.R`.

## Repository layout

```
R/                     analysis functions (sourced via R/_load_all.R)
analysis/              run_analysis.R (pipeline) and report.qmd
data/raw/              FailureProbabilityDemo.xlsx (input)
data/processed/        tidy long-format CSV
results/               precomputed results read by every app
apps/shiny/            main Shiny app (+ methods.md)
apps/shinylive/app/    browser-only Shiny app
apps/quarto-dashboard/ Quarto dashboard
apps/flexdashboard/    flexdashboard
scripts/build_site.R   builds site/ for GitHub Pages
tests/testthat/        unit tests
```

## Reference

Cushway, K., Shaw, C., McKay, S.K., Swannack, T. (2026). Sensitivity and
uncertainty in habitat suitability modeling: guidance and pitfalls for
ecological applications. *Environmental Modelling & Software* 203, 106984.
<https://doi.org/10.1016/j.envsoft.2026.106984>
