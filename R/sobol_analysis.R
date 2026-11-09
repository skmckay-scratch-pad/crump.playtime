# Step 5: global variance-based (Sobol') sensitivity with sensobol -------------
#
# Follows the workflow in Cushway et al. (2026, Fig. 4):
#   1. Define the inputs and their distributions (uniform; discrete uniform for
#      categorical grades, via extraDistr).
#   2. Build A, B and A_B sample matrices with quasi-random numbers (QRN).
#      Total model runs N_total = n * (M + 2).
#   3. Run the model on every row.
#   4. Estimate first-order (Saltelli) and total-order (Jansen) indices with
#      bootstrap replicates and 95% percentile CIs.
#   5. Check convergence: every CI should be narrower than 0.05.
#
#   ITERATION CAP (project requirement): no iteration count may exceed 250.
#   That covers the base sample size n, the bootstrap replicates R and the
#   number of random facilities. The paper grows n until the CIs are narrow
#   enough (it uses n = 60,000 or more). Here n stays fixed at 250, so the
#   convergence check is *reported* (converged / max CI width) rather than
#   forced. Wider CIs mean fewer inputs can be told apart from the dummy.
#   6. Estimate the approximation error with a dummy parameter (sobol_dummy).
#      An input whose Si and Ti CIs both overlap the dummy's is non-influential.
#   7. Uncertainty analysis: the outputs from the A matrix give an empirical
#      distribution of the output.

ITER_CAP <- 250L

#' Input specification helper.
#' @param name input name
#' @param type "continuous" (uniform on [lo, hi]) or "discrete" (integers lo..hi)
sobol_input <- function(name, type = c("continuous", "discrete"), lo, hi, label = name) {
  data.frame(name = name, type = match.arg(type), lo = lo, hi = hi, label = label,
             stringsAsFactors = FALSE)
}

transform_inputs <- function(U, inputs) {
  X <- as.data.frame(U)
  names(X) <- inputs$name
  for (i in seq_len(nrow(inputs))) {
    u <- U[, i]
    X[[i]] <- if (inputs$type[i] == "discrete") {
      if (inputs$lo[i] == inputs$hi[i]) rep(inputs$lo[i], length(u))
      else extraDistr::qdunif(pmin(pmax(u, 1e-12), 1 - 1e-12), inputs$lo[i], inputs$hi[i])
    } else {
      inputs$lo[i] + (inputs$hi[i] - inputs$lo[i]) * u
    }
  }
  X
}

next_n <- function(n, ratio) {
  if (ratio <= 1.5) n + 1000 else if (ratio <= 3) ceiling(n * 1.25) else ceiling(n * 1.5)
}

#' Run a converged Sobol' analysis.
#' @param model function(X data.frame) returning a numeric vector, or a matrix
#'   with one column per output (all outputs share the same sample).
#' @param inputs data.frame from sobol_input() (rbind several)
#' @return list(indices, dummy, influence, uncertainty, sample, n, N_total,
#'   converged, max_ci_width) with one entry per output in each table.
run_sobol <- function(model, inputs, n_start = ITER_CAP, n_max = ITER_CAP,
                      R = ITER_CAP, ci_tol = 0.05, seed = 1, keep_sample = ITER_CAP) {
  stopifnot(n_start <= ITER_CAP, n_max <= ITER_CAP, R <= ITER_CAP)
  params <- inputs$name
  M <- length(params)
  n <- n_start
  repeat {
    set.seed(seed)
    U <- sensobol::sobol_matrices(N = n, params = params, type = "QRN")
    X <- transform_inputs(U, inputs)
    Y <- model(X)
    if (is.null(dim(Y))) Y <- matrix(Y, ncol = 1, dimnames = list(NULL, "output"))
    outs <- colnames(Y)

    ind_list <- lapply(outs, function(o) {
      y <- Y[, o]
      if (var(y) <= 0) return(NULL)
      r <- sensobol::sobol_indices(Y = y, N = n, params = params,
                                   first = "saltelli", total = "jansen",
                                   boot = TRUE, R = R, type = "percent", conf = 0.95)
      res <- as.data.frame(r$results)
      res$output <- o
      res
    })
    names(ind_list) <- outs
    widths <- unlist(lapply(ind_list, function(r) if (is.null(r)) 0 else r$high.ci - r$low.ci))
    max_w <- max(widths)
    converged <- max_w < ci_tol
    if (converged || n >= n_max) break
    n <- min(next_n(n, max_w / ci_tol), n_max)
  }

  indices <- do.call(rbind, ind_list)
  dummy <- do.call(rbind, lapply(outs, function(o) {
    y <- Y[, o]
    if (var(y) <= 0) return(NULL)
    d <- as.data.frame(sensobol::sobol_dummy(Y = y, N = n, params = params,
                                             boot = TRUE, R = R, type = "percent"))
    d$output <- o
    d
  }))

  influence <- classify_influence(indices, dummy)

  yA <- Y[seq_len(n), , drop = FALSE]          # A matrix rows = plain MC sample
  uncertainty <- do.call(rbind, lapply(outs, function(o) {
    y <- yA[, o]
    q <- quantile(y, c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975))
    data.frame(output = o, mean = mean(y), sd = sd(y), min = min(y), max = max(y),
               q025 = q[1], q05 = q[2], q25 = q[3], median = q[4], q75 = q[5],
               q95 = q[6], q975 = q[7], row.names = NULL)
  }))
  keep <- seq_len(min(keep_sample, n))
  sample <- data.frame(X[keep, , drop = FALSE], yA[keep, , drop = FALSE], check.names = FALSE)

  rownames(indices) <- NULL
  list(indices = indices, dummy = dummy, influence = influence,
       uncertainty = uncertainty, sample = sample, inputs = inputs,
       n = n, N_total = n * (M + 2), converged = converged, max_ci_width = max_w)
}

#' Non-influential if both the Si and Ti CIs overlap the dummy's CIs.
classify_influence <- function(indices, dummy) {
  if (is.null(indices) || is.null(dummy)) return(NULL)
  out <- list()
  for (o in unique(indices$output)) {
    ii <- indices[indices$output == o, ]
    dd <- dummy[dummy$output == o, ]
    d_si <- dd$high.ci[dd$sensitivity == "Si"]
    d_ti <- dd$high.ci[dd$sensitivity == "Ti"]
    for (prm in unique(ii$parameters)) {
      si <- ii[ii$parameters == prm & ii$sensitivity == "Si", ]
      ti <- ii[ii$parameters == prm & ii$sensitivity == "Ti", ]
      out[[length(out) + 1]] <- data.frame(
        output = o, parameter = prm,
        Si = si$original, Si_low = si$low.ci, Si_high = si$high.ci,
        Ti = ti$original, Ti_low = ti$low.ci, Ti_high = ti$high.ci,
        dummy_Si_high = d_si, dummy_Ti_high = d_ti,
        influential = !(si$low.ci <= d_si && ti$low.ci <= d_ti))
    }
  }
  do.call(rbind, out)
}

# ---- L2: single asset with an uncertain grade and an uncertain curve ---------------

#' Inputs for L2. The grade is discrete uniform within +/- `window` notches of
#' the observed grade (clipped at A and F-). Curve multipliers are uniform on
#' [1 - spread, 1 + spread]. For multi-year runs, q (annual probability of
#' dropping a notch) is uniform on [0, q_max].
l2_inputs <- function(g_obs, window = 1, spread = 0.2, multiyear = FALSE, q_max = 0.3) {
  inp <- rbind(
    sobol_input("grade", "discrete", max(1, g_obs - window), min(N_GRADES, g_obs + window),
                "Grade (± notch)"),
    sobol_input("level", "continuous", 1 - spread, 1 + spread, "Curve level"),
    sobol_input("slope", "continuous", 1 - spread, 1 + spread, "Curve slope"),
    sobol_input("jump",  "continuous", 1 - spread, 1 + spread, "F-jump size"))
  if (multiyear) inp <- rbind(inp, sobol_input("q", "continuous", 0, q_max,
                                               "Deterioration rate"))
  inp
}

#' L2 model: annual P(fail) when years = 1 and there is no q; otherwise the
#' cumulative P(fail) over `years` with Markov deterioration at rate q.
make_l2_model <- function(p14, years = 1) {
  comp <- curve_components(p14)
  function(X) {
    if (years == 1 && is.null(X$q)) {
      return(perturbed_p(comp, X$grade, X$level, X$slope, X$jump))
    }
    g <- seq_len(N_GRADES)
    Lmat <- outer(comp$L1 + log(X$level), rep(1, N_GRADES)) +
      outer(X$slope, comp$S) + outer(X$jump * comp$J, as.numeric(g > JUMP_STEP))
    cumulative_risk_vec(plogis(Lmat), X$grade, years, if (is.null(X$q)) 0 else X$q)
  }
}

# ---- L3: facility with many components ---------------------------------------------

#' Inputs for L3: each component's grade (discrete, +/- window) and level
#' (odds multiplier). Curve slope and F-jump multipliers are shared by all
#' components, on the view that shape errors come from one common
#' curve-building method. Multi-year adds a shared deterioration rate q.
l3_inputs <- function(facility, window = 1, spread = 0.2, multiyear = FALSE, q_max = 0.3) {
  rows <- list()
  for (i in seq_len(nrow(facility))) {
    g <- match(facility$grade[i], GRADES)
    rows[[length(rows) + 1]] <- sobol_input(sprintf("grade_%d", i), "discrete",
      max(1, g - window), min(N_GRADES, g + window),
      sprintf("Grade: %s", facility$component[i]))
    rows[[length(rows) + 1]] <- sobol_input(sprintf("level_%d", i), "continuous",
      1 - spread, 1 + spread, sprintf("Level: %s", facility$component[i]))
  }
  rows[[length(rows) + 1]] <- sobol_input("slope", "continuous", 1 - spread, 1 + spread,
                                          "Curve slope (shared)")
  rows[[length(rows) + 1]] <- sobol_input("jump", "continuous", 1 - spread, 1 + spread,
                                          "F-jump size (shared)")
  if (multiyear) rows[[length(rows) + 1]] <- sobol_input("q", "continuous", 0, q_max,
                                                         "Deterioration rate (shared)")
  do.call(rbind, rows)
}

#' L3 model: returns a matrix with one column per aggregation structure.
make_l3_model <- function(p, facility, years = 1,
                          structures = names(SYSTEM_STRUCTURES)) {
  comps <- lapply(facility$asset_group, function(a) curve_components(p[a, GRADES]))
  function(X) {
    P <- sapply(seq_along(comps), function(i) {
      cp <- comps[[i]]
      gi <- X[[sprintf("grade_%d", i)]]; lv <- X[[sprintf("level_%d", i)]]
      if (years == 1 && is.null(X$q)) return(perturbed_p(cp, gi, lv, X$slope, X$jump))
      Lmat <- outer(cp$L1 + log(lv), rep(1, N_GRADES)) + outer(X$slope, cp$S) +
        outer(X$jump * cp$J, as.numeric(seq_len(N_GRADES) > JUMP_STEP))
      cumulative_risk_vec(plogis(Lmat), gi, years, if (is.null(X$q)) 0 else X$q)
    })
    P <- matrix(P, ncol = length(comps))
    out <- sapply(structures, function(s) aggregate_system(P, s))
    matrix(out, ncol = length(structures), dimnames = list(NULL, structures))
  }
}

#' A random facility for the structure-comparison ensemble.
random_facility <- function(groups, m) {
  data.frame(component = sprintf("C%d", seq_len(m)),
             asset_group = sample(groups, m),
             grade = GRADES[sample.int(N_GRADES, m, replace = TRUE)],
             stringsAsFactors = FALSE)
}
