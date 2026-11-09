# Descriptive curve fits ------------------------------------------------------
#
# Each group's 14 grade probabilities are summarised with a piecewise model on
# the logit scale:
#
#   logit(p_g) = a_s + b_s * log(g + c),   s = segment of grade g
#
# with three segments (A to C, C- to D-, F+ to F-) and an offset `c` shared by
# the segments and fitted per group. The fit describes the curve shape; it
# makes no claim about how the table was produced. The perturbation model used
# for sensitivity analysis (R/perturb.R) works on the table itself, so fit error
# does not carry into the sensitivity results.

fit_group_curve <- function(p14) {
  stopifnot(length(p14) == N_GRADES)
  y   <- qlogis(p14)
  g   <- seq_len(N_GRADES)
  seg <- grade_segment(g)

  rss <- function(c) sum(resid(lm(y ~ seg / log(g + c) - 1))^2)
  c_hat <- optimize(rss, interval = c(0, 20))$minimum
  fit   <- lm(y ~ seg / log(g + c_hat) - 1)
  co    <- coef(fit)

  fitted_p <- plogis(fitted(fit))
  list(
    params = c(offset = c_hat,
               a1 = co[[1]], a2 = co[[2]], a3 = co[[3]],
               b1 = co[[4]], b2 = co[[5]], b3 = co[[6]]),
    fitted = fitted_p,
    rmse_logit  = sqrt(mean(resid(fit)^2)),
    max_rel_err = max(abs(fitted_p / p14 - 1))
  )
}

#' Curve-shape features computed straight from the table (no fitting). These
#' feed the cross-group comparisons in Step 6.
curve_features <- function(p) {
  L <- qlogis(p[, GRADES, drop = FALSE])
  data.frame(
    asset_group   = rownames(p),
    p_A           = p[, "A"],
    p_Fminus      = p[, "F-"],
    p_CF          = p[, "CF"],
    range_logit   = L[, "F-"] - L[, "A"],
    slope_AC      = (L[, "C"]  - L[, "A"])  / 6,   # mean logit step per grade
    slope_CD      = (L[, "D-"] - L[, "C-"]) / 3,
    slope_F       = (L[, "F-"] - L[, "F+"]) / 2,
    jump_F        = L[, "F+"] - L[, "D-"],          # the D- to F+ step
    jump_share    = (L[, "F+"] - L[, "D-"]) / (L[, "F-"] - L[, "A"]),
    cf_over_Fminus = p[, "CF"] / p[, "F-"],
    row.names = NULL
  )
}

fit_all_curves <- function(p) {
  fits <- lapply(rownames(p), function(gname) fit_group_curve(p[gname, GRADES]))
  names(fits) <- rownames(p)
  params <- do.call(rbind, lapply(fits, `[[`, "params"))
  data.frame(
    asset_group = rownames(p),
    params,
    rmse_logit  = vapply(fits, `[[`, numeric(1), "rmse_logit"),
    max_rel_err = vapply(fits, `[[`, numeric(1), "max_rel_err"),
    row.names = NULL
  ) -> summary
  fitted <- do.call(rbind, lapply(names(fits), function(gname) data.frame(
    asset_group = gname, rating = factor(GRADES, levels = RATINGS),
    rating_index = seq_len(N_GRADES),
    observed = p[gname, GRADES], fitted = fits[[gname]]$fitted,
    row.names = NULL)))
  list(summary = summary, fitted = fitted)
}
