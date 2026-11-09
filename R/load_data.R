# Load and tidy the failure-probability table ---------------------------------

#' Condition grades, best to worst. CF (conditional failure probability) is the
#' worst-case anchor and is kept separate from the 14 grades.
GRADES  <- c("A", "A-", "B+", "B", "B-", "C+", "C", "C-",
             "D+", "D", "D-", "F+", "F", "F-")
RATINGS <- c(GRADES, "CF")
N_GRADES <- length(GRADES)

#' Grade segments suggested by the shape of the curves (see analysis/01).
grade_segment <- function(idx) {
  cut(idx, breaks = c(0, 7, 11, 14), labels = c("A to C", "C- to D-", "F+ to F-"))
}

#' Read the raw Excel table.
#' @return list with `wide` (data.frame, one row per asset group, one column per
#'   rating), `p` (numeric matrix groups x 15 ratings) and `long` (tidy table).
read_failure_table <- function(path = "data/raw/FailureProbabilityDemo.xlsx") {
  raw <- readxl::read_excel(path)
  names(raw)[1] <- "asset_group"
  stopifnot(identical(names(raw)[-1], RATINGS))

  p <- as.matrix(raw[, RATINGS])
  rownames(p) <- raw$asset_group

  long <- data.frame(
    asset_group  = rep(raw$asset_group, times = length(RATINGS)),
    rating       = rep(RATINGS, each = nrow(raw)),
    rating_index = rep(seq_along(RATINGS), each = nrow(raw)),
    p_fail       = as.vector(p),
    stringsAsFactors = FALSE
  )
  long$rating <- factor(long$rating, levels = RATINGS)
  long$is_cf  <- long$rating == "CF"
  long <- long[order(long$asset_group, long$rating_index), ]
  rownames(long) <- NULL

  list(wide = as.data.frame(raw), p = p, long = long)
}

#' Data-quality checks. Returns a data.frame of check results (one row per check).
check_failure_table <- function(p) {
  inc <- apply(p, 1, function(v) all(diff(v) > 0))
  data.frame(
    check  = c("All values in (0, 1)",
               "No missing values",
               "No duplicate asset groups",
               "Strictly increasing from A to CF in every group"),
    passed = c(all(p > 0 & p < 1),
               !anyNA(p),
               !anyDuplicated(rownames(p)),
               all(inc)),
    detail = c(sprintf("range %.5f to %.5f", min(p), max(p)),
               sprintf("%d missing", sum(is.na(p))),
               sprintf("%d groups", nrow(p)),
               if (all(inc)) "all groups" else paste(rownames(p)[!inc], collapse = ", "))
  )
}
