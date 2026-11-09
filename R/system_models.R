# System (facility) aggregation structures (L3) --------------------------------
#
# A facility is made of components. Each one is an asset group with an observed
# grade. Component failure probabilities (annual, or cumulative over T years)
# are combined into one facility-level output. Each structure is a different
# assumption about how component failures add up to facility failure, like the
# structure comparison in Cushway et al. (2026). Components are assumed
# independent.

SYSTEM_STRUCTURES <- list(
  series   = list(label = "Series (any fails)",
                  fun = function(P) 1 - apply(1 - P, 1, prod)),
  parallel = list(label = "Parallel (all fail)",
                  fun = function(P) apply(P, 1, prod)),
  limiting = list(label = "Limiting factor (max)",
                  fun = function(P) apply(P, 1, max)),
  mean     = list(label = "Mean",
                  fun = function(P) rowMeans(P)),
  expected = list(label = "Expected failures (sum)",
                  fun = function(P) rowSums(P))
)

#' Apply a structure to a samples x components matrix.
aggregate_system <- function(P, structure) {
  P <- as.matrix(P)
  SYSTEM_STRUCTURES[[structure]]$fun(P)
}

#' An invented example facility used to demonstrate L3. Asset groups and grades
#' are made up for illustration and do not describe a real facility.
example_facility <- function() {
  data.frame(
    component   = c("Dam concrete", "Embankment", "Main electrical",
                    "Gates (metals)", "Gate machinery", "Control system",
                    "Bulkhead crane", "Guide walls"),
    asset_group = c("Concrete A", "Earthen A", "Electrical A", "Metals A",
                    "Operating Equipment A", "Controls A",
                    "Cranes and Hoists B", "In-River Structures A"),
    grade       = c("C+", "B", "C-", "C", "D+", "B-", "C", "B+"),
    stringsAsFactors = FALSE
  )
}

#' Deterministic facility risk at the observed grades (no uncertainty), for
#' the app's system builder. Returns component and system values.
facility_point_risk <- function(p, facility, years = 1,
                                model = "none", k = 5, q = 0.2) {
  comp <- vapply(seq_len(nrow(facility)), function(i) {
    v <- p[facility$asset_group[i], GRADES]
    g <- match(facility$grade[i], GRADES)
    if (years == 1 && model == "none") return(v[g])
    tail(multiyear_risk(v, g, years, model, k, q)$cumulative, 1)
  }, numeric(1))
  P <- matrix(comp, nrow = 1)
  list(component = data.frame(facility, p = comp),
       system = vapply(names(SYSTEM_STRUCTURES), function(s) aggregate_system(P, s),
                       numeric(1)))
}
