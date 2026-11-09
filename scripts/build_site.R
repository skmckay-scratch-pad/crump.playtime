# Build the static GitHub Pages site into site/ ---------------------------------
#
#   Rscript scripts/build_site.R
#
# Uses the committed results/app_data.rds; it does not rerun the analysis
# (run analysis/run_analysis.R first if the data or methods change).
# Needs the Quarto CLI plus the R packages rmarkdown, flexdashboard, plotly, DT,
# crosstalk and shinylive.

Sys.setlocale("LC_ALL", "C.UTF-8")
stopifnot(file.exists("results/app_data.rds"))
site <- "site"
unlink(site, recursive = TRUE)
dir.create(site)
step <- function(...) message("==> ", sprintf(...))

quarto_render <- function(input) {
  status <- system2("quarto", c("render", shQuote(input)))
  if (status != 0) stop("quarto render failed: ", input)
}

# 1. Analysis report (self-contained HTML)
step("Rendering analysis report")
quarto_render("analysis/report.qmd")
file.copy("analysis/report.html", file.path(site, "report.html"), overwrite = TRUE)

# 2. Quarto dashboard
step("Rendering Quarto dashboard")
quarto_render("apps/quarto-dashboard/dashboard.qmd")
dir.create(file.path(site, "quarto-dashboard"))
file.copy("apps/quarto-dashboard/dashboard.html", file.path(site, "quarto-dashboard", "index.html"))
file.copy("apps/quarto-dashboard/dashboard_files", file.path(site, "quarto-dashboard"), recursive = TRUE)

# 3. flexdashboard
step("Rendering flexdashboard")
rmarkdown::render("apps/flexdashboard/flexdashboard.Rmd", quiet = TRUE)
dir.create(file.path(site, "flexdashboard"))
file.copy("apps/flexdashboard/flexdashboard.html", file.path(site, "flexdashboard", "index.html"))

# 4. Shinylive: stage functions and a trimmed results file next to app.R
step("Exporting Shinylive app")
app <- "apps/shinylive/app"
dir.create(file.path(app, "R"), showWarnings = FALSE)
dir.create(file.path(app, "data"), showWarnings = FALSE)
for (f in c("load_data.R", "perturb.R", "misclassification.R", "multiyear.R"))
  file.copy(file.path("R", f), file.path(app, "R", f), overwrite = TRUE)
A <- readRDS("results/app_data.rds")
saveRDS(A[c("settings", "grades", "ratings", "p", "l2_influence")],
        file.path(app, "data", "shinylive_data.rds"))
shinylive::export(app, file.path(site, "shinylive"))

# 5. Landing page and downloadable data
step("Writing landing page")
file.copy("scripts/site_index.html", file.path(site, "index.html"))
dir.create(file.path(site, "data"))
file.copy("data/processed/failure_probability_long.csv", file.path(site, "data"))
write.csv(A$l2_influence, file.path(site, "data", "sobol_l2_indices.csv"), row.names = FALSE)
writeLines("", file.path(site, ".nojekyll"))
step("Done: %s", normalizePath(site))
