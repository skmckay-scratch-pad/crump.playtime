# Step 4: grade misclassification ---------------------------------------------
#
# A misclassification matrix M has rows for the observed grade and columns for
# the true grade. M[o, t] = P(true grade = t | observed grade = o). Each row puts
# `p_correct` on the observed grade and spreads the rest over the neighbours
# within `width` notches, either evenly or falling off with distance. At A and
# F- the missing neighbours' share is spread over the others in proportion
# (the row is renormalised).

misclass_matrix <- function(p_correct = 0.6, width = 1,
                            shape = c("uniform", "triangular"),
                            bias = 0) {
  shape <- match.arg(shape)
  n <- N_GRADES
  M <- matrix(0, n, n, dimnames = list(observed = GRADES, true = GRADES))
  offs <- setdiff(-width:width, 0)
  w <- if (shape == "uniform") rep(1, length(offs)) else (width + 1 - abs(offs))
  # bias > 0 shifts weight toward worse true grades (positive offsets)
  w <- w * exp(bias * sign(offs))
  w <- w / sum(w)
  for (o in seq_len(n)) {
    M[o, o] <- p_correct
    for (k in seq_along(offs)) {
      t <- o + offs[k]
      if (t >= 1 && t <= n) M[o, t] <- M[o, t] + (1 - p_correct) * w[k]
    }
    M[o, ] <- M[o, ] / sum(M[o, ])
  }
  M
}

#' Expected P(fail) and its spread for each observed grade, given M.
misclass_effect <- function(p, M) {
  do.call(rbind, lapply(rownames(p), function(gname) {
    v  <- p[gname, GRADES]
    ev <- as.vector(M %*% v)
    sd <- sqrt(pmax(as.vector(M %*% v^2) - ev^2, 0))
    data.frame(asset_group = gname, grade = factor(GRADES, levels = GRADES),
               grade_index = seq_len(N_GRADES),
               p_observed = v, p_expected = ev, p_sd = sd,
               bias_ratio = ev / v, row.names = NULL)
  }))
}
