# Source every analysis function file in R/ (except this one).
for (f in setdiff(list.files("R", pattern = "\\.R$", full.names = TRUE), "R/_load_all.R")) source(f)
