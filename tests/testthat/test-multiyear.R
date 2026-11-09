v <- unname(DAT$p["Building A", GRADES])

test_that("no deterioration matches 1 - (1 - p)^T", {
  r <- multiyear_risk(v, 5, 10, "none")
  expect_equal(r$cumulative, 1 - (1 - v[5])^(1:10))
  expect_equal(r$annual_hazard, rep(v[5], 10))
})

test_that("Markov risk matches the vectorised version and exceeds no-deterioration", {
  r <- multiyear_risk(v, 5, 15, "markov", q = 0.2)
  expect_equal(tail(r$cumulative, 1), cumulative_risk_vec(matrix(v, 1), 5, 15, 0.2))
  expect_gt(tail(r$cumulative, 1), 1 - (1 - v[5])^15)
  expect_equal(r$cumulative, cumsum(r$p_fail_in_year))
})

test_that("q = 0 Markov equals no deterioration; schedule follows k", {
  expect_equal(multiyear_risk(v, 3, 8, "markov", q = 0)$cumulative,
               multiyear_risk(v, 3, 8, "none")$cumulative)
  s <- multiyear_risk(v, 1, 6, "schedule", k = 2)
  expect_equal(s$expected_grade, c(1, 1, 2, 2, 3, 3))
})

test_that("grade stops at F- and a start distribution is a mixture", {
  r <- multiyear_risk(v, 14, 5, "markov", q = 0.5)
  expect_equal(r$annual_hazard, rep(v[14], 5))
  M <- misclass_matrix(0.6, 1)
  mix <- multiyear_risk(v, M[7, ], 10, "none")$cumulative
  manual <- sum(M[7, ] * (1 - (1 - v)^10))
  expect_equal(tail(mix, 1), manual)
})
