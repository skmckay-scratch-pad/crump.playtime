test_that("misclassification rows sum to 1 and identity gives no change", {
  M <- misclass_matrix(0.6, 2, "triangular", bias = 0.5)
  expect_equal(unname(rowSums(M)), rep(1, 14))
  e <- misclass_effect(DAT$p, misclass_matrix(1, 1))
  expect_equal(e$p_expected, e$p_observed)
})

test_that("notch sensitivity leaves out shifts past A and F-", {
  n <- notch_sensitivity(DAT$p["Building A", , drop = FALSE])
  expect_true(is.na(n$ratio[n$grade == "A" & n$delta == -1]))
  expect_false(is.na(n$ratio[n$grade == "B" & n$delta == -2]))
})

test_that("system structures behave as expected", {
  P <- matrix(c(0.1, 0.2, 0.3), 1)
  expect_equal(aggregate_system(P, "series"), 1 - 0.9 * 0.8 * 0.7)
  expect_equal(aggregate_system(P, "parallel"), 0.006)
  expect_equal(aggregate_system(P, "limiting"), 0.3)
  expect_equal(aggregate_system(P, "expected"), 0.6)
})

test_that("Sobol' run respects the 250 cap and flags the F-jump as irrelevant at grade A", {
  expect_error(run_sobol(identity, l2_inputs(1), n_start = 500))
  r <- run_sobol(make_l2_model(unname(DAT$p["Building A", GRADES])), l2_inputs(1), seed = 1)
  expect_equal(r$n, 250)
  expect_equal(r$N_total, 250 * (4 + 2))
  jump <- r$influence[r$influence$parameter == "jump", ]
  expect_equal(jump$Ti, 0)
  expect_false(jump$influential)
})
