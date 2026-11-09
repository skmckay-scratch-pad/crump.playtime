# Multi-year and cumulative failure risk ---------------------------------------
#
# Annual P(fail) depends on the grade. Over several years an asset may also
# deteriorate. Two deterioration models are available:
#
#   "none"     the grade stays the same; cumulative risk = 1 - (1 - p)^T
#   "schedule" the grade drops one notch every `k` years (fixed timing)
#   "markov"   each year the grade drops one notch with probability `q`
#              (random timing; the average rate matches "schedule" when q = 1/k)
#
# Grades stop at F-. CF stays a separate worst-case reference and is not a
# state an asset deteriorates into. Years are independent given the grade, and
# a failure in year t means the asset has failed by the horizon (it is not
# repaired).
#
# The start can be one grade or a distribution over grades, for example a row
# of a misclassification matrix, so grade uncertainty carries into the
# multi-year result.

#' One-year Markov transition matrix (grades only).
deterioration_matrix <- function(q) {
  n <- N_GRADES
  Tm <- diag(1 - q, n)
  for (g in seq_len(n - 1)) Tm[g, g + 1] <- q
  Tm[n, n] <- 1
  Tm
}

#' Year-by-year risk for one asset.
#' @param p14 annual P(fail) by grade (length 14)
#' @param start grade index, or a length-14 probability vector over grades
#' @param years horizon T
#' @param model "none", "schedule" or "markov"
#' @param k years per notch for "schedule"
#' @param q annual probability of dropping a notch for "markov"
#' @return data.frame with, for each year: expected grade index of survivors,
#'   annual hazard (P fail in year t | survived to t), unconditional P(fail in
#'   year t), cumulative P(failed by end of year t), survival.
multiyear_risk <- function(p14, start, years = 10,
                           model = c("none", "schedule", "markov"),
                           k = 5, q = 0.2) {
  model <- match.arg(model)
  n <- N_GRADES
  pi0 <- if (length(start) == 1) replace(numeric(n), start, 1) else start / sum(start)

  if (model == "schedule") {
    # Mixture over starting grades of a deterministic path
    surv_by_year <- matrix(0, years, n)    # survival contribution by start grade
    grade_by_year <- matrix(0, years, n)
    for (g0 in which(pi0 > 0)) {
      g_t <- pmin(g0 + floor((seq_len(years) - 1) / k), n)
      surv_by_year[, g0]  <- pi0[g0] * cumprod(1 - p14[g_t])
      grade_by_year[, g0] <- g_t
    }
    S  <- rowSums(surv_by_year)
    S_prev <- c(1, S[-years])
    # expected grade among survivors at the start of each year
    surv_start <- rbind(pi0, surv_by_year[-years, , drop = FALSE])
    eg <- rowSums(surv_start * grade_by_year) / pmax(rowSums(surv_start), 1e-300)
  } else {
    Tm <- if (model == "markov") deterioration_matrix(q) else diag(n)
    alive <- pi0                       # P(alive and in grade g) at start of year
    S <- S_prev <- eg <- numeric(years)
    for (t in seq_len(years)) {
      S_prev[t] <- sum(alive)
      eg[t] <- sum(alive * seq_len(n)) / max(S_prev[t], 1e-300)
      alive <- as.vector((alive * (1 - p14)) %*% Tm)
      S[t] <- sum(alive)
    }
  }
  data.frame(
    year = seq_len(years),
    expected_grade = eg,
    annual_hazard  = 1 - S / S_prev,
    p_fail_in_year = S_prev - S,
    cumulative     = 1 - S,
    survival       = S
  )
}

#' Fast vectorised cumulative risk at horizon T, for use inside Sobol' models.
#' Each row of `p_mat` (samples x 14) is one sample's grade curve, `g0` the
#' starting grade, and `q` the annual Markov deterioration probability (q = 0
#' means no deterioration).
cumulative_risk_vec <- function(p_mat, g0, years, q = 0) {
  n <- ncol(p_mat); ns <- nrow(p_mat)
  q <- rep_len(q, ns)
  alive <- matrix(0, ns, n)
  alive[cbind(seq_len(ns), g0)] <- 1
  for (t in seq_len(years)) {
    a <- alive * (1 - p_mat)
    moved <- a[, -n, drop = FALSE] * q
    a[, -n] <- a[, -n] - moved
    a[, -1] <- a[, -1] + moved
    alive <- a
  }
  1 - rowSums(alive)
}
