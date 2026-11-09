# Run the full analysis and write results for the apps -------------------------
#
#   Rscript analysis/run_analysis.R
#
# Writes results/analysis_results.rds (everything) and results/app_data.rds (the
# smaller subset the web apps need). Every iteration count is capped at 250
# (ITER_CAP in R/sobol_analysis.R).

source("R/_load_all.R")
suppressPackageStartupMessages(library(parallel))

cores <- max(1L, min(4L, detectCores()))
set.seed(20261109)
t_start <- Sys.time()
msg <- function(...) message(format(Sys.time(), "%H:%M:%S "), sprintf(...))

# ---- Steps 0-1: data ------------------------------------------------------------
dat    <- read_failure_table()
p      <- dat$p
groups <- rownames(p)
checks <- check_failure_table(p)
write.csv(dat$long, "data/processed/failure_probability_long.csv", row.names = FALSE)
msg("Data: %d groups x %d ratings; checks passed: %s", nrow(p), ncol(p), all(checks$passed))

# ---- Step 2: curve fits -------------------------------------------------------------
fits     <- fit_all_curves(p)
features <- curve_features(p)
msg("Curve fits: median logit RMSE %.4f, worst relative error %.1f%%",
    median(fits$summary$rmse_logit), 100 * max(fits$summary$max_rel_err))

# ---- Steps 3-4: local sensitivity and misclassification -----------------------------
notch      <- notch_sensitivity(p)
elasticity <- grade_elasticity(p)
M_default  <- misclass_matrix(p_correct = 0.6, width = 1)
misclass   <- misclass_effect(p, M_default)

# ---- Multi-year reference table ----------------------------------------------------------
# Cumulative risk for every group x starting grade x scenario, years 1-30.
my_scenarios <- list(
  list(id = "none",       model = "none",   q = 0,   label = "No deterioration"),
  list(id = "markov_0.1", model = "markov", q = 0.1, label = "Drops a notch ~every 10 yrs (q = 0.1)"),
  list(id = "markov_0.2", model = "markov", q = 0.2, label = "Drops a notch ~every 5 yrs (q = 0.2)"))
multiyear <- do.call(rbind, lapply(groups, function(gname) {
  do.call(rbind, lapply(seq_len(N_GRADES), function(g) {
    do.call(rbind, lapply(my_scenarios, function(sc) {
      r <- multiyear_risk(p[gname, GRADES], g, 30, sc$model, q = sc$q)
      data.frame(asset_group = gname, start_grade = GRADES[g], start_index = g,
                 scenario = sc$id, r[, c("year", "annual_hazard", "cumulative")])
    }))
  }))
}))
msg("Multi-year table: %d rows", nrow(multiyear))

# ---- Step 5a: L2 Sobol' for every group x observed grade ----------------------------------
l2_grid <- expand.grid(asset_group = groups, grade_index = seq_len(N_GRADES),
                       horizon = c(1, 10), stringsAsFactors = FALSE)
msg("L2: %d Sobol' runs on %d cores", nrow(l2_grid), cores)
l2_runs <- mclapply(seq_len(nrow(l2_grid)), function(i) {
  row <- l2_grid[i, ]
  my  <- row$horizon > 1
  run_sobol(make_l2_model(p[row$asset_group, GRADES], years = row$horizon),
            l2_inputs(row$grade_index, multiyear = my), seed = i)
}, mc.cores = cores)

tag <- function(df, row) cbind(row[rep(1, nrow(df)), , drop = FALSE], df, row.names = NULL)
l2_influence <- do.call(rbind, lapply(seq_along(l2_runs), function(i)
  tag(l2_runs[[i]]$influence, l2_grid[i, ])))
l2_uncertainty <- do.call(rbind, lapply(seq_along(l2_runs), function(i)
  tag(cbind(l2_runs[[i]]$uncertainty, n = l2_runs[[i]]$n, N_total = l2_runs[[i]]$N_total,
            converged = l2_runs[[i]]$converged, max_ci_width = l2_runs[[i]]$max_ci_width),
      l2_grid[i, ])))
l2_influence$grade   <- factor(GRADES[l2_influence$grade_index], levels = GRADES)
l2_uncertainty$grade <- factor(GRADES[l2_uncertainty$grade_index], levels = GRADES)
l2_samples <- lapply(l2_runs, `[[`, "sample")
msg("L2 done: %.0f%% of runs converged (CI < 0.05); median max CI width %.2f",
    100 * mean(l2_uncertainty$converged), median(l2_uncertainty$max_ci_width))

# ---- Step 5b: L3 example facility ------------------------------------------------------------
facility <- example_facility()
l3_example <- lapply(c(annual = 1, ten_year = 10), function(h)
  run_sobol(make_l3_model(p, facility, years = h),
            l3_inputs(facility, multiyear = h > 1), seed = 7))
msg("L3 example facility done")

# ---- Step 5c: structure comparison over random facilities ----------------------------------
N_FACILITIES <- 100L                       # <= ITER_CAP
stopifnot(N_FACILITIES <= ITER_CAP)
fac_list <- lapply(seq_len(N_FACILITIES), function(i)
  random_facility(groups, sample(4:10, 1)))
msg("Structure ensemble: %d random facilities x 2 horizons", N_FACILITIES)
ens <- mclapply(seq_len(N_FACILITIES * 2), function(j) {
  i <- (j - 1) %/% 2 + 1; h <- c(1, 10)[(j - 1) %% 2 + 1]
  f <- fac_list[[i]]
  r <- run_sobol(make_l3_model(p, f, years = h), l3_inputs(f, multiyear = h > 1),
                 seed = 1000 + j)
  inf <- r$influence
  pct <- tapply(!inf$influential, inf$output, mean) * 100
  data.frame(facility = i, horizon = h, n_components = nrow(f),
             n_inputs = nrow(r$inputs), structure = names(pct),
             pct_noninfluential = as.vector(pct),
             median_output = r$uncertainty$median[match(names(pct), r$uncertainty$output)],
             range_output = (r$uncertainty$q975 - r$uncertainty$q025)[
               match(names(pct), r$uncertainty$output)],
             max_ci_width = r$max_ci_width, row.names = NULL)
}, mc.cores = cores)
ensemble <- do.call(rbind, ens)

structure_tests <- do.call(rbind, lapply(c(1, 10), function(h) {
  e <- ensemble[ensemble$horizon == h, ]
  do.call(rbind, lapply(c("pct_noninfluential", "median_output"), function(v) {
    w  <- reshape(e[, c("facility", "structure", v)], idvar = "facility",
                  timevar = "structure", direction = "wide")
    mat <- as.matrix(w[, -1]); colnames(mat) <- sub(paste0(v, "."), "", colnames(mat))
    fr <- friedman.test(mat)
    data.frame(horizon = h, variable = v, chi_sq = unname(fr$statistic),
               df = unname(fr$parameter), p_value = fr$p.value)
  }))
}))
structure_pairwise <- do.call(rbind, lapply(c(1, 10), function(h) {
  e <- ensemble[ensemble$horizon == h, ]
  do.call(rbind, lapply(c("pct_noninfluential", "median_output"), function(v) {
    e <- e[order(e$facility), ]
    pw <- suppressWarnings(pairwise.wilcox.test(e[[v]], e$structure, paired = TRUE,
                                                p.adjust.method = "BH"))
    m <- pw$p.value
    idx <- which(!is.na(m), arr.ind = TRUE)
    data.frame(horizon = h, variable = v, structure_1 = rownames(m)[idx[, 1]],
               structure_2 = colnames(m)[idx[, 2]], p_adj_BH = m[idx])
  }))
}))
msg("Structure comparison done")

# ---- Step 6: cross-group patterns -------------------------------------------------------
l2_ann <- l2_influence[l2_influence$horizon == 1, ]
group_ti <- aggregate(Ti ~ asset_group + parameter, data = l2_ann, FUN = mean)
group_ti <- reshape(group_ti, idvar = "asset_group", timevar = "parameter", direction = "wide")
names(group_ti) <- sub("^Ti\\.", "Ti_", names(group_ti))
patterns <- merge(features, group_ti, by = "asset_group")
feat_cols <- c("p_A", "range_logit", "slope_AC", "slope_CD", "slope_F", "jump_F", "jump_share")
ti_cols <- grep("^Ti_", names(patterns), value = TRUE)
spearman <- do.call(rbind, lapply(feat_cols, function(fc) do.call(rbind, lapply(ti_cols, function(tc) {
  ct <- suppressWarnings(cor.test(patterns[[fc]], patterns[[tc]], method = "spearman"))
  data.frame(feature = fc, index = tc, rho = unname(ct$estimate), p_value = ct$p.value)
}))))
spearman$p_adj_BH <- p.adjust(spearman$p_value, "BH")

l2_ann$segment <- grade_segment(l2_ann$grade_index)
segment_tests <- do.call(rbind, lapply(unique(l2_ann$parameter), function(prm) {
  d <- l2_ann[l2_ann$parameter == prm, ]
  kw <- kruskal.test(Ti ~ segment, data = d)
  data.frame(parameter = prm, chi_sq = unname(kw$statistic), p_value = kw$p.value,
             t(tapply(d$Ti, d$segment, median)), check.names = FALSE)
}))
segment_tests$p_adj_BH <- p.adjust(segment_tests$p_value, "BH")

# ---- Save ------------------------------------------------------------------------------
settings <- list(iter_cap = ITER_CAP, n = ITER_CAP, R = ITER_CAP, ci_tol = 0.05,
                 grade_window = 1, curve_spread = 0.2, q_max = 0.3,
                 n_facilities = N_FACILITIES, misclass = list(p_correct = 0.6, width = 1),
                 run_time_min = as.numeric(difftime(Sys.time(), t_start, units = "mins")),
                 created = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
                 r_version = R.version.string,
                 sensobol_version = as.character(packageVersion("sensobol")))

app_data <- list(
  settings = settings, grades = GRADES, ratings = RATINGS,
  p = p, long = dat$long, checks = checks,
  fits = fits, features = features,
  notch = notch, elasticity = elasticity,
  multiyear = multiyear, my_scenarios = do.call(rbind, lapply(my_scenarios, as.data.frame)),
  l2_influence = l2_influence, l2_uncertainty = l2_uncertainty,
  l3_example = lapply(l3_example, function(r) r[c("influence", "uncertainty", "sample",
                                                   "inputs", "n", "N_total", "converged",
                                                   "max_ci_width")]),
  facility = facility,
  ensemble = ensemble, structure_tests = structure_tests,
  structure_pairwise = structure_pairwise,
  spearman = spearman, segment_tests = segment_tests
)
dir.create("results", showWarnings = FALSE)
saveRDS(app_data, "results/app_data.rds", compress = "xz")
saveRDS(c(app_data, list(l2_grid = l2_grid, l2_samples = l2_samples, misclass = misclass)),
        "results/analysis_results.rds", compress = "xz")
msg("Saved results/ (%.1f min total)", settings$run_time_min)
