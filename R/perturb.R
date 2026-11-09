# Curve perturbation model -----------------------------------------------------
#
# A group's curve is the 14 logit-scale grade values L_1..L_14 from the table.
# They are rebuilt as a starting level plus 13 steps between neighbouring
# grades. One of those steps is the D- to F+ "F-jump". Three multipliers change
# the curve:
#
#   level  multiplies the odds of failure at every grade (shifts the curve)
#   slope  scales every step except the F-jump (steeper or flatter decline)
#   jump   scales the D- to F+ step only
#
#   L'_g = L_1 + log(level) + slope * S_g + jump * J * 1[g >= 12]
#
# Here S_g is the cumulative sum of the non-jump steps up to grade g, and J is
# the F-jump. With level = slope = jump = 1, the curve is exactly the table.

JUMP_STEP <- which(GRADES == "F+") - 1L   # step 11: D- -> F+

curve_components <- function(p14) {
  L     <- qlogis(p14)
  steps <- diff(L)
  nonjump <- steps
  nonjump[JUMP_STEP] <- 0
  list(L1 = L[1], S = c(0, cumsum(nonjump)), J = steps[JUMP_STEP])
}

#' Perturbed failure probability for one group, vectorised over samples.
#' @param comp output of curve_components()
#' @param grade integer grade index (1..14), vector
#' @param level,slope,jump multipliers, vectors recycled to length(grade)
perturbed_p <- function(comp, grade, level = 1, slope = 1, jump = 1) {
  grade <- pmin(pmax(as.integer(grade), 1L), N_GRADES)
  plogis(comp$L1 + log(level) + slope * comp$S[grade] +
           jump * comp$J * (grade > JUMP_STEP))
}

#' The full perturbed 14-grade curve (one sample) for plotting.
perturbed_curve <- function(p14, level = 1, slope = 1, jump = 1) {
  perturbed_p(curve_components(p14), seq_len(N_GRADES), level, slope, jump)
}
