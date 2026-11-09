## Methods in brief

**Data.** 31 asset groups × 15 ratings of annual failure probability. **A to F-** are
condition grades, like school grades (A good, F bad). **CF** is the *conditional
failure probability*, the worst case. It is shown as a reference point and is not
treated as a grade an asset can be rated or deteriorate into.

**Curve shape.** On the logit scale every group's curve has three segments: A to C,
C- to D-, and F+ to F-. There is a large jump from D- to F+. A descriptive fit,
`logit(p) = a_s + b_s·log(g + c)`, matches the table to within 2% for every group.

**Curve perturbation.** The sensitivity analyses change a group's curve with three
multipliers, applied to the table itself:

- *level* multiplies the odds of failure at every grade;
- *slope* scales the steps between neighbouring grades, except the D- to F+ jump;
- *F-jump* scales the D- to F+ step.

All three equal to 1 gives back the table exactly. Because *slope* scales the
total distance from grade A, its effect grows at worse grades.

**Local (one-at-a-time) sensitivity.** The change in P(fail) when the grade is off
by ±1 or ±2 notches. Simple and easy to read, but it misses interactions.

**Misclassification.** A matrix gives P(true grade | observed grade). The expected
P(fail) is the weighted average of the table over the possible true grades.
Because the curves are convex, the expected value is usually *higher* than the
table value at the observed grade.

**Multi-year risk.** Cumulative risk is `1 − Π(1 − pₜ)` over the years, where pₜ is
the annual probability at the grade held in year t. There are three
deterioration options:

- *none*: the grade stays the same, giving `1 − (1 − p)ᵀ`;
- *schedule*: the grade drops one notch every *k* years;
- *Markov*: each year the grade drops one notch with probability *q*.

Grades stop at F-. A failed asset stays failed (it is not repaired). With the
grade-uncertainty option on, the starting grade is a distribution taken from the
misclassification matrix.

**Global sensitivity (Sobol').** Follows Cushway et al. (2026) using `sensobol`:

- quasi-random A, B and A_B matrices;
- Saltelli first-order (Sᵢ) and Jansen total-order (Tᵢ) estimators;
- bootstrap 95% percentile CIs;
- a dummy parameter to estimate the approximation error. An input whose Sᵢ *and*
  Tᵢ CIs both overlap the dummy's is **non-influential**.

Inputs are the grade (discrete uniform, ±1 notch from the observed grade) and the
curve multipliers (uniform, ±20%). Multi-year runs add q ~ U(0, 0.3).

- **L2 (single asset):** every group × observed grade, annual and 10-year.
- **L3 (facility):** each component's grade and level, plus shared slope, F-jump
  and (for 10-year runs) q. The output is facility risk under five structures:
  series, parallel, limiting factor, mean, and expected failures.
- **Structure comparison:** 100 random facilities. Friedman tests with the
  facility as block, then BH-adjusted pairwise Wilcoxon signed-rank tests.

**Iteration cap.** At the project's request, **no iteration count exceeds 250**:
the base sample size n = 250, the bootstrap replicates R = 250, and 100 random
facilities. The paper grows n until every CI is narrower than 0.05, which needed
n ≥ 60,000. At n = 250 the CIs are much wider (the widest is often above 0.5), so:

- none of the runs meets the convergence target;
- the dummy's CI is wide, so many inputs are labelled *non-influential*. Read
  this as **"not shown to be influential at this sample size"**;
- point estimates of Sᵢ and Tᵢ are still useful for ranking inputs.

**Reference.** Cushway, K., Shaw, C., McKay, S.K., Swannack, T. (2026). Sensitivity
and uncertainty in habitat suitability modeling: guidance and pitfalls for
ecological applications. *Environmental Modelling & Software* 203, 106984.
