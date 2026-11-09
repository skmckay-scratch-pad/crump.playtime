test_that("unit multipliers reproduce the table exactly", {
  for (g in rownames(DAT$p)) {
    v <- unname(DAT$p[g, GRADES])
    expect_equal(perturbed_curve(v), v, tolerance = 1e-12)
  }
})

test_that("jump multiplier only affects F grades", {
  v <- unname(DAT$p["Electrical A", GRADES])
  pc <- perturbed_curve(v, jump = 1.2)
  expect_equal(pc[1:11], v[1:11])
  expect_true(all(pc[12:14] > v[12:14]))
})

test_that("level multiplies the odds", {
  v <- unname(DAT$p["Concrete A", GRADES])
  pc <- perturbed_curve(v, level = 1.1)
  expect_equal((pc / (1 - pc)) / (v / (1 - v)), rep(1.1, 14), tolerance = 1e-10)
})
