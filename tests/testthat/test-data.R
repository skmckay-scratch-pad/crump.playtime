test_that("table loads with 31 groups and 15 ratings and passes checks", {
  expect_equal(dim(DAT$p), c(31, 15))
  expect_identical(colnames(DAT$p), RATINGS)
  expect_true(all(check_failure_table(DAT$p)$passed))
  expect_equal(nrow(DAT$long), 31 * 15)
})

test_that("curve fit is close to the table", {
  f <- fit_group_curve(DAT$p["Building A", GRADES])
  expect_lt(f$max_rel_err, 0.05)
})
