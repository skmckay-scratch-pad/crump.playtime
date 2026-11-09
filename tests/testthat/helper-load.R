root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in setdiff(list.files(file.path(root, "R"), "\\.R$", full.names = TRUE),
                  file.path(root, "R", "_load_all.R"))) source(f)
DAT <- read_failure_table(file.path(root, "data", "raw", "FailureProbabilityDemo.xlsx"))
