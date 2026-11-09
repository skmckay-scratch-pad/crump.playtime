# Plan: Sensitivity Analysis of Asset Failure-Probability Ratings

**Status:** Draft v2 for review. Updated with answers to the open questions. No analysis has been run yet.

**Decision context:** the analyses support decisions about **how likely infrastructure is to fail**. The main output throughout is therefore annual probability of failure, P(fail).

## 1. Goal

Find out how sensitive annual failure-probability estimates for infrastructure
assets are to their **condition ratings** and to the **rating-to-probability
curves**. Then build web apps for exploring the results. Everything is done in
**R**. The main app uses **Shiny**, and a few alternative frameworks are built
alongside it so they can be compared.

The method follows Cushway et al. (2026), *Sensitivity and uncertainty in
habitat suitability modeling* (Environmental Modelling & Software 203,
106984). It uses global variance-based (Sobol') sensitivity analysis plus
uncertainty analysis, built with the `sensobol` package. That method is
adapted here to this data, which has a different structure.

## 2. The data

`FailureProbabilityDemo.xlsx` has one sheet: **31 asset groups × 15 condition
ratings**.

- Ratings, best to worst: `A, A-, B+, B, B-, C+, C, C-, D+, D, D-, F+, F, F-, CF`.
  - `A` to `F-` are **condition grades**, like school grades: A is good and F is bad.
  - `CF` is the **conditional failure probability**, the worst case. It is the
    top end of each curve, not a grade. Analyses report it but treat it
    separately from grades: rating perturbations move between the 14 grades, and
    "grade → CF" appears as a worst-case comparison.
- Each value is an annual probability of failure.
- Every row increases steadily from A to CF.
- Ranges differ widely between groups. At A they run from about 0.03% (Concrete A,
  Cranes and Hoists B) to about 1.2% (Security A). At CF they run from about 3%
  (Earthen A) to about 79% (Electrical A).
- **The curves look piecewise.** The ratio between neighbouring ratings shrinks
  smoothly from A to C. It drops slightly at C → C-, and **jumps sharply at
  D- → F+** (usually about 1.1× before it and about 1.6–2.7× after). The table
  looks like it was generated from a model with about three segments
  (A–C, C-–D-, F+–F-).
- **Where the curves came from doesn't matter** (confirmed). The table is
  treated as given. Ratings near the D-/F+ boundary are therefore likely to
  be the most sensitive.

## 3. How this data differs from the paper

In the paper, each model takes several inputs (SIVs) and **combines them into
one score with an equation**. Here, each asset group is a **lookup table**:
rating in, probability out. A sensitivity analysis needs an output that
depends on uncertain inputs, so this plan defines that output in three layers.
Each layer answers a different question.

**Priority:** L1 and L2 are the main analyses because they answer the
failure-likelihood question for individual assets. L3 is an extension and
needs a facility to analyse (see the open questions).

| Layer | Question | Uncertain inputs | Output |
|---|---|---|---|
| **L1: Single asset** | If an asset's rating is off by one or two notches, how much does its failure probability change? | Rating (ordinal) | P(fail) of one asset |
| **L2: Curve uncertainty** | How much of a group's P(fail) uncertainty comes from the rating, and how much from the curve itself? | Rating plus curve parameters (level, slope, the size of the F-jump) | P(fail) of one asset |
| **L3: System / portfolio** | For a facility made of several asset groups, which components and inputs drive the facility's risk? | Ratings and curve parameters for every component | P(facility fails) or expected number of failures |

L3 is the closest match to the paper: many inputs, one composite output, Sobol'
indices. It also allows the paper's **structure comparison**, swapping the
aggregation equation the way the paper swaps limiting-factor, multiplicative
and mean forms:

| Structure | Equation | Plain meaning |
|---|---|---|
| Series (any failure) | 1 − ∏(1 − pᵢ) | The facility fails if any component fails |
| Parallel (all fail) | ∏ pᵢ | Fully redundant components |
| Limiting factor | max(pᵢ) | The weakest link sets the risk |
| Mean | Σ pᵢ / n | Average component risk |
| Expected failures | Σ pᵢ | Portfolio budgeting view |

## 4. Analysis steps (all in R)

### Step 0: Setup
- `renv` locks package versions so the analysis is reproducible.
- A `targets` pipeline means only steps whose inputs changed are rerun. It is
  optional, and worth learning.
- The raw Excel file is copied to `data/raw/`. A tidy long table
  (`asset_group, rating, rating_index 1–15, p_fail`) is written to
  `data/processed/`.

### Step 1: Explore and check the data
- Check the curves rise steadily and values stay between 0 and 1. Look for
  duplicate rows.
- Plot all 31 curves on log and logit scales, with a heatmap of groups ×
  ratings.
- Compute ratios and log-odds steps between neighbouring ratings, which shows
  the segment breaks.
- Cluster the groups by curve shape, to see which asset types behave alike.

### Step 2: Fit a curve to each group (used as a tool, not a claim about where the data came from)
- Fit a **piecewise-linear model on the logit scale** to each group, with
  breakpoints near C/C- and D-/F+, across the 14 grades. CF is kept as a separate
  worst-case anchor. The breakpoints can be fixed, or estimated
  with `segmented`.
- This turns each 15-number row into a few parameters with a meaning: a level,
  a slope for each segment, and the size of the jump at F+. Those parameters
  can then be varied in L2 and L3.
- Report fit quality. If fits are poor, interpolate the table directly instead
  (see the open questions).

### Step 3: Local, one-at-a-time sensitivity (L1)
- For each group and rating, find the change in P(fail) when the rating moves
  ±1 or ±2 notches, both in absolute terms and as a ratio.
- Compute elasticity, the percentage change in P(fail) per notch.
- Outputs: tornado charts and a "risky rating boundaries" heatmap.
- This is simple and easy to read. Like the paper, it is treated as a baseline,
  because one-at-a-time methods miss interactions.

### Step 4: Rating misclassification (L1, probabilistic)
- Model inspector error with a misclassification matrix, for example a 60%
  chance the observed rating is correct and 20% for each neighbouring notch.
  The percentages can be set by the user.
- Output: expected P(fail) and its spread, given an observed rating.
- Shows where the published value is *biased*. Because the curves are convex,
  expected P is higher than P at the observed rating.

### Step 5: Global Sobol' sensitivity (L2 and L3) with `sensobol`

This mirrors the paper's Fig. 4 workflow:

1. **Inputs and distributions.**
   - Ratings are categorical, sampled as discrete uniform over the 14 grades
     or over a narrower plausible window around an observed rating (an open
     question), using `extraDistr`.
   - Curve parameters vary ±X% around their fitted values, with uniform
     distributions as in the paper. X is an open question; ±20% is the
     starting suggestion.
2. **Sample matrix.** The `A, B, A_B` design with quasi-random sampling.
   N = n × (M + 2).
3. **Estimators.** Saltelli for first-order (Sᵢ) and Jansen for total-order
   (Tᵢ).
4. **Convergence.** Start at n = 2¹² and increase until every 95% CI is
   narrower than 0.05, as in the paper. This model is cheap to run, so a
   larger n is fine.
5. **Bootstrap.** 1,000 replicates for the confidence intervals.
6. **Dummy parameter.** `sobol_dummy()` estimates numerical noise. Any input
   whose CI overlaps the dummy's is marked **non-influential**.
7. **Uncertainty analysis.** The same model runs give the output distribution:
   quantiles, median, and how much of the 0–1 range is covered.
8. **Structure comparison (L3).** Repeat steps 1–7 for each aggregation
   structure in §3. Compare the share of non-influential inputs and the median
   output across structures with Friedman and pairwise Wilcoxon tests, adjusted
   with Benjamini–Hochberg.

### Step 6: Cross-group patterns
These mirror the paper's §3.2–3.5:
- Use Spearman correlations to relate curve features (steepness, F-jump size,
  CF ceiling) to sensitivity indices.
- Find which asset groups' risk is driven mostly by rating and which by curve
  uncertainty.

## 5. Web apps

All the apps read the same precomputed results in `results/*.rds` or
`*.parquet`. That way the comparison is between frameworks, not between
analyses. Heavy Sobol' runs are precomputed. Light calculations, such as the
one-at-a-time and misclassification steps, run live where the framework
allows it.

| # | Option | What it is | Why it's worth learning |
|---|---|---|---|
| 1 | **Shiny + `bslib`** (main app) | Full reactive app with a server | The standard. Fully interactive and runs live calculations. |
| 2 | **Quarto dashboard** | `.qmd` rendered to a dashboard page, optionally with Shiny | New-style publishing. Mixes prose and code, and can be a static page. |
| 3 | **Shinylive** | Shiny compiled to WebAssembly, running in the browser | No server needed and can be hosted on GitHub Pages. Has limits on package size and speed. |
| 4 | **flexdashboard** (optional) | Older R Markdown dashboards | Still common in existing projects. Useful as a comparison with Quarto. |

Planned tabs for the main Shiny app:
1. **Data:** curves and heatmap. Pick groups and switch between log and linear
   scales.
2. **Curve fits:** fitted vs. observed values, with parameter tables.
3. **Rating sensitivity (L1):** tornado charts, notch-change heatmap, and
   misclassification sliders that recalculate live.
4. **Global sensitivity (L2/L3):** Sᵢ/Tᵢ bars with CIs and the dummy line, plus
   a convergence plot.
5. **Uncertainty:** output distributions as raincloud plots and heatmaps, in
   the style of the paper's Figs. 6 and 8.
6. **System builder (L3):** choose component groups and ratings, then compare
   aggregation structures side by side.
7. **Methods:** a plain-language explanation of each method, for learning.

Main R packages: `readxl`, `dplyr`, `tidyr`, `ggplot2`, `segmented`,
`sensobol`, `extraDistr`, `data.table`, `shiny`, `bslib`, `plotly`, `DT`,
`ggdist` (raincloud plots), `quarto`, `shinylive`, `testthat`, `renv`,
`targets`.

## 6. Proposed repository layout

```
crump.playtime/
├── README.md
├── plan.md
├── renv.lock
├── _targets.R               # pipeline (optional)
├── data/
│   ├── raw/FailureProbabilityDemo.xlsx
│   └── processed/
├── R/                       # analysis functions
│   ├── load_data.R
│   ├── curve_fit.R
│   ├── local_sensitivity.R
│   ├── misclassification.R
│   ├── sobol_analysis.R
│   └── system_models.R
├── analysis/                # Quarto reports that document each step
├── results/                 # precomputed outputs read by the apps
├── apps/
│   ├── shiny/               # main app (app.R + modules)
│   ├── quarto-dashboard/
│   ├── shinylive/
│   └── flexdashboard/
└── tests/testthat/
```

## 7. Build order (each stage is reviewable)

1. Setup, data import and exploration report (Steps 0–1). **Checkpoint.**
2. Curve fitting (Step 2). **Checkpoint:** is the piecewise-logit model acceptable?
3. One-at-a-time and misclassification analysis (Steps 3–4).
4. Sobol' analysis, uncertainty and structure comparison (Steps 5–6). **Checkpoint.**
5. Main Shiny app.
6. Alternative apps: Quarto dashboard, then Shinylive, then flexdashboard.
7. A README with instructions for running everything.

## 8. Open questions

### Answered
- **CF:** the conditional failure probability, the worst case. Handled as the
  upper anchor of each curve, separate from the A to F- grades.
- **Ratings:** A to F- are condition grades, like school grades (A good,
  F bad).
- **Where the curves came from:** doesn't matter. The table is treated as given.
- **Decision context:** infrastructure failure likelihood. The main output is
  P(fail), and L1/L2 come first.

### Still open (the default in **bold** is used if there is no answer)
1. **Letter suffixes in group names** ("Electrical A–E", "Metals A–E"): are
   they subtypes? Default: **treat each as an independent asset group.**
2. **Facility inventory for L3:** default: **an invented example facility
   with one asset from each of 6 to 8 groups, clearly labelled as made up.**
   The app's system builder lets users make their own.
3. **How far can a grade realistically be off?** Default: **±1 notch, with a
   60/20/20 misclassification matrix.** Users can change this in the app.
4. **Curve-parameter variation for L2/L3:** default **±20%, uniform.**
5. **Time horizon:** default **annual, with an app option for cumulative
   probability over T years, 1 − (1 − p)ᵀ.**
6. **Hosting:** default **run locally. Shinylive and Quarto are also set up so
   they can be published to GitHub Pages.**
