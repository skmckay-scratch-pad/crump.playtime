# Step 3: one-at-a-time (local) rating sensitivity ------------------------------

#' Change in P(fail) when the grade is off by `delta` notches.
#' Shifts that run past A or F- are left out (NA) rather than clipped.
notch_sensitivity <- function(p, deltas = c(-2, -1, 1, 2)) {
  out <- list()
  for (gname in rownames(p)) {
    v <- p[gname, GRADES]
    for (g in seq_len(N_GRADES)) for (d in deltas) {
      h <- g + d
      ok <- h >= 1 && h <= N_GRADES
      out[[length(out) + 1]] <- data.frame(
        asset_group = gname, grade = GRADES[g], grade_index = g, delta = d,
        p_base  = v[g],
        p_shift = if (ok) v[h] else NA_real_,
        abs_change = if (ok) v[h] - v[g] else NA_real_,
        ratio      = if (ok) v[h] / v[g] else NA_real_)
    }
  }
  res <- do.call(rbind, out)
  res$grade <- factor(res$grade, levels = GRADES)
  rownames(res) <- NULL
  res
}

#' Elasticity: % change in P(fail) per notch, using the central difference where
#' possible (one-sided at A and F-). Also the ratio of CF to each grade.
grade_elasticity <- function(p) {
  do.call(rbind, lapply(rownames(p), function(gname) {
    v <- p[gname, GRADES]; lv <- log(v)
    d <- numeric(N_GRADES)
    d[1] <- lv[2] - lv[1]
    d[N_GRADES] <- lv[N_GRADES] - lv[N_GRADES - 1]
    d[2:(N_GRADES - 1)] <- (lv[3:N_GRADES] - lv[1:(N_GRADES - 2)]) / 2
    data.frame(asset_group = gname,
               grade = factor(GRADES, levels = GRADES), grade_index = seq_len(N_GRADES),
               p_fail = v,
               pct_per_notch = 100 * (exp(d) - 1),
               worse_notch_ratio = c(v[-1] / v[-N_GRADES], NA),
               cf_ratio = p[gname, "CF"] / v,
               row.names = NULL)
  }))
}
