# Failure-probability sensitivity explorer: main Shiny app ---------------------
#
# Run from the repository root:
#   shiny::runApp("apps/shiny")
#
# Reads precomputed results (results/app_data.rds, from analysis/run_analysis.R)
# and recomputes the light parts (misclassification, multi-year risk, curve
# perturbation, system builder) live with the functions in R/.

library(shiny)
library(bslib)
library(plotly)
library(DT)

# ---- Locate the repository root and load functions + results -----------------------
find_root <- function() {
  for (d in c(".", "..", "../..")) if (file.exists(file.path(d, "R", "_load_all.R"))) return(normalizePath(d))
  stop("Run the app from the repository root: shiny::runApp('apps/shiny')")
}
ROOT <- find_root()
for (f in setdiff(list.files(file.path(ROOT, "R"), "\\.R$", full.names = TRUE),
                  file.path(ROOT, "R", "_load_all.R"))) source(f)
A <- readRDS(file.path(ROOT, "results", "app_data.rds"))
P      <- A$p
GROUPS <- rownames(P)
PAL    <- c("#2a6f97", "#e07a5f", "#3d405b", "#81b29a", "#f2cc8f",
            "#9c6644", "#6d597a", "#e63946", "#457b9d", "#588157")
PARAM_LABEL <- c(grade = "Grade (±1 notch)", level = "Curve level",
                 slope = "Curve slope", jump = "F-jump size", q = "Deterioration rate")
`%||%` <- function(a, b) if (is.null(a)) b else a
fmt_p <- function(x) ifelse(x < 0.001, formatC(x, format = "e", digits = 2),
                            sprintf("%.2f%%", 100 * x))

theme <- bs_theme(version = 5, bootswatch = "flatly", primary = "#2a6f97",
                  base_font = font_collection("system-ui", "sans-serif"))

# Two side-by-side cards on wide screens, stacked below the "xl" breakpoint
# (1200px; the sidebar takes ~300px), so charts and tables never get squeezed.
two_col <- function(left = 6) breakpoints(sm = c(12, 12), xl = c(left, 12 - left))

# Layout guards: value-box text scales with the viewport and never breaks
# mid-word; tables scroll inside their card instead of spilling out of it.
app_css <- tags$style(HTML("
  .bslib-value-box .value-box-title { font-size: 0.95rem; overflow-wrap: normal; word-break: normal; }
  .bslib-value-box .value-box-value { font-size: clamp(1.3rem, 0.9rem + 1.2vw, 1.9rem);
                                      line-height: 1.15; overflow-wrap: normal; word-break: normal; }
  .bslib-value-box p { font-size: 0.85rem; margin-bottom: 0; }
  .table-scroll { overflow-x: auto; max-width: 100%; }
  .table-scroll table { width: 100%; }
  .card, .card-body, .bslib-sidebar-layout > .main { min-width: 0; }
  .dataTables_wrapper { max-width: 100%; overflow-x: auto; }
  .dataTables_wrapper .pagination { flex-wrap: wrap; }
  .dataTables_filter label { display: flex; gap: .4rem; align-items: center; max-width: 100%; }
  .dataTables_filter input { min-width: 0; width: 100%; margin-left: 0 !important; }
  .selectize-input { overflow: hidden; }
  .selectize-control.single .selectize-input > .item { white-space: nowrap; overflow: hidden;
                                                       text-overflow: ellipsis; max-width: calc(100% - 1rem); }
"))
tbl <- function(id) div(class = "table-scroll", tableOutput(id))

cap_note <- div(class = "alert alert-warning py-2 small mb-2",
  icon("triangle-exclamation"),
  sprintf(" Every iteration count is capped at %d (base sample, bootstrap replicates, facilities).
           The confidence intervals are therefore wide (none reach the 0.05 target), so many
           inputs cannot be told apart from the dummy. Read 'non-influential' as 'not shown to
           be influential at this sample size'.", A$settings$iter_cap))

# ---- UI -------------------------------------------------------------------------
ui <- page_navbar(
  title = "Failure Sensitivity",
  window_title = "Failure-Probability Sensitivity",
  theme = theme,
  fillable = FALSE,
  header = app_css,

  # 1. Data -------------------------------------------------------------------------
  nav_panel("Data", icon = icon("table"),
    layout_sidebar(
      sidebar = sidebar(
        selectizeInput("data_groups", "Asset groups", GROUPS, multiple = TRUE,
                       selected = c("Building A", "Electrical A", "Earthen A", "Controls B")),
        radioButtons("data_scale", "Y-axis", c("Log" = "log", "Linear" = "linear", "Logit" = "logit"),
                     inline = TRUE),
        checkboxInput("data_cf", "Show CF (worst case)", TRUE),
        p(class = "small text-muted",
          "Grades A to F- are condition grades. CF is the conditional failure probability,",
          "the worst case, shown as a separate point.")
      ),
      card(card_header("Annual failure probability by grade"), plotlyOutput("data_curves", height = 420)),
      layout_columns(col_widths = two_col(7),
        card(card_header("All groups (log₁₀ P(fail))"), plotlyOutput("data_heat", height = 620)),
        card(card_header("Data checks"), tbl("data_checks"),
             card_header("Table"), DTOutput("data_table")))
    )),

  # 2. Curve shape -------------------------------------------------------------------
  nav_panel("Curves", icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        selectInput("fit_group", "Asset group", GROUPS, "Building A"),
        h6("Perturb the curve"),
        sliderInput("pert_level", "Level (odds multiplier)", 0.5, 1.5, 1, 0.05),
        sliderInput("pert_slope", "Slope multiplier", 0.5, 1.5, 1, 0.05),
        sliderInput("pert_jump", "F-jump multiplier", 0.5, 1.5, 1, 0.05),
        actionButton("pert_reset", "Reset", class = "btn-sm"),
        p(class = "small text-muted", "The Sobol' analysis varies each multiplier within ±20%.")
      ),
      layout_columns(col_widths = two_col(7),
        card(card_header("Table vs. fitted vs. perturbed (logit scale)"),
             plotlyOutput("fit_plot", height = 420)),
        card(card_header("Fit: logit(p) = a_s + b_s·log(g + c)"), tbl("fit_params"),
             p(class = "small text-muted",
               "Three segments: A to C, C- to D-, F+ to F-. The fit describes the curve shape only;",
               "the sensitivity analysis perturbs the table itself."))),
      card(card_header("Curve features across groups"), DTOutput("fit_features"))
    )),

  # 3. Rating sensitivity -------------------------------------------------------------
  nav_panel("Ratings", icon = icon("arrows-up-down"),
    navset_card_underline(
      nav_panel("Notch changes",
        layout_sidebar(
          sidebar = sidebar(
            selectInput("ns_group", "Asset group", GROUPS, "Electrical A"),
            selectInput("ns_grade", "Observed grade", A$grades, "D-")),
          layout_columns(col_widths = two_col(5),
            card(card_header("If the grade is off by ±1 or ±2 notches"), plotlyOutput("ns_tornado", height = 330),
                 uiOutput("ns_text")),
            card(card_header("% change in P(fail) per notch (all groups)"),
                 plotlyOutput("ns_heat", height = 620))))),
      nav_panel("Misclassification",
        layout_sidebar(
          sidebar = sidebar(
            sliderInput("mc_correct", "P(grade is correct)", 0.2, 1, 0.6, 0.05),
            sliderInput("mc_width", "Max error (notches)", 1, 3, 1, 1),
            radioButtons("mc_shape", "Spread", c("Even" = "uniform", "Falls off" = "triangular")),
            sliderInput("mc_bias", "Bias (+ = true grade tends to be worse)", -1.5, 1.5, 0, 0.25),
            selectInput("mc_group", "Asset group (chart)", GROUPS, "Electrical A")),
          layout_columns(col_widths = two_col(6),
            card(card_header("Expected P(fail) given the observed grade"), plotlyOutput("mc_curve", height = 380)),
            card(card_header("Expected ÷ table value (all groups)"), plotlyOutput("mc_heat", height = 620))),
          card(card_header("Misclassification matrix"), plotlyOutput("mc_matrix", height = 380))))
    )),

  # 4. Multi-year ----------------------------------------------------------------------
  nav_panel("Multi-year", icon = icon("calendar-days"),
    layout_sidebar(
      sidebar = sidebar(width = 300,
        selectizeInput("my_groups", "Asset groups (compare up to 6)", GROUPS, multiple = TRUE,
                       selected = c("Electrical A", "Concrete A"), options = list(maxItems = 6)),
        selectInput("my_grade", "Starting grade", A$grades, "C"),
        sliderInput("my_years", "Horizon (years)", 1, 50, 20, 1),
        radioButtons("my_model", "Deterioration",
                     c("None (grade constant)" = "none",
                       "Fixed schedule: 1 notch every k years" = "schedule",
                       "Random (Markov): drop a notch w.p. q each year" = "markov")),
        conditionalPanel("input.my_model == 'schedule'",
          sliderInput("my_k", "k (years per notch)", 1, 20, 5, 1)),
        conditionalPanel("input.my_model == 'markov'",
          sliderInput("my_q", "q (annual prob. of dropping a notch)", 0, 0.5, 0.2, 0.01)),
        checkboxInput("my_mc", "Include grade uncertainty (Misclassification tab settings)", FALSE),
        sliderInput("my_threshold", "Risk threshold for 'years until…'", 0.01, 0.9, 0.1, 0.01),
        checkboxInput("my_compare", "Also show 'no deterioration' for comparison", TRUE)
      ),
      uiOutput("my_boxes"),
      layout_columns(col_widths = two_col(6),
        card(card_header("Cumulative P(failed by end of year)"), plotlyOutput("my_cum", height = 380)),
        card(card_header("Annual P(fail in year | survived so far)"), plotlyOutput("my_haz", height = 380))),
      layout_columns(col_widths = two_col(6),
        card(card_header("Expected grade of survivors"), plotlyOutput("my_grade_plot", height = 300)),
        card(card_header("Year-by-year table"), DTOutput("my_table"))),
      card(card_header("Cumulative risk at the horizon, every group × starting grade"),
           plotlyOutput("my_heat", height = 620))
    )),

  # 5. Global sensitivity ---------------------------------------------------------------
  nav_panel("Sobol'", icon = icon("chart-bar"),
    cap_note,
    navset_card_underline(
      nav_panel("Single asset (L2)",
        layout_sidebar(
          sidebar = sidebar(
            selectInput("gs_group", "Asset group", GROUPS, "Building A"),
            selectInput("gs_grade", "Observed grade", A$grades, "D-"),
            radioButtons("gs_horizon", "Output", c("Annual P(fail)" = 1, "10-year cumulative" = 10)),
            p(class = "small text-muted",
              "Inputs: grade (±1 notch, discrete uniform), curve level, slope, F-jump (each ±20%,",
              "uniform), and for 10-year runs the deterioration rate q ~ U(0, 0.3).")),
          layout_columns(col_widths = two_col(6),
            card(card_header("Sobol' indices with 95% CIs (dashed = dummy)"), plotlyOutput("gs_bars", height = 380),
                 uiOutput("gs_meta")),
            card(card_header("Total-order index Tᵢ across observed grades"), plotlyOutput("gs_grade_line", height = 380))),
          card(card_header("Mean Tᵢ by group (all observed grades)"), plotlyOutput("gs_heat", height = 620)))),
      nav_panel("Facility (L3)",
        layout_sidebar(
          sidebar = sidebar(
            radioButtons("l3_horizon", "Output", c("Annual" = "annual", "10-year cumulative" = "ten_year")),
            selectInput("l3_structure", "Aggregation structure",
                        setNames(names(SYSTEM_STRUCTURES), sapply(SYSTEM_STRUCTURES, `[[`, "label"))),
            p(class = "small text-muted", "Example facility (invented for illustration):"),
            tbl("l3_facility")),
          card(card_header("Sobol' indices for the example facility"), plotlyOutput("l3_bars", height = 520)))),
      nav_panel("Structure comparison",
        layout_sidebar(
          sidebar = sidebar(
            radioButtons("sc_horizon", "Output", c("Annual" = 1, "10-year cumulative" = 10)),
            p(class = "small text-muted", sprintf(
              "%d random facilities (4 to 10 components with random groups and grades), each analysed
               under every structure. This follows the paper's structure comparison.",
              A$settings$n_facilities))),
          layout_columns(col_widths = two_col(6),
            card(card_header("% of inputs non-influential"), plotlyOutput("sc_noninf", height = 380)),
            card(card_header("Median output"), plotlyOutput("sc_median", height = 380))),
          card(card_header("Friedman tests (facility = block) and BH-adjusted pairwise Wilcoxon"),
               layout_columns(col_widths = two_col(5), tbl("sc_friedman"), DTOutput("sc_pairwise")))))
    )),

  # 6. Uncertainty -------------------------------------------------------------------------
  nav_panel("Uncertainty", icon = icon("wave-square"),
    layout_sidebar(
      sidebar = sidebar(
        selectInput("ua_group", "Asset group", GROUPS, "Electrical A"),
        radioButtons("ua_horizon", "Output", c("Annual P(fail)" = 1, "10-year cumulative" = 10))),
      card(card_header("Output distribution under all input uncertainty: median and 95% interval vs. table value"),
           plotlyOutput("ua_band", height = 400)),
      layout_columns(col_widths = two_col(6),
        card(card_header("Width of 95% interval ÷ median (all groups)"), plotlyOutput("ua_heat", height = 620)),
        card(card_header("Example facility: output distributions by structure (log scale)"),
             radioButtons("ua_l3_h", NULL, c("Annual" = "annual", "10-year" = "ten_year"), inline = TRUE),
             plotlyOutput("ua_l3", height = 560)))
    )),

  # 7. System builder ----------------------------------------------------------------------
  nav_panel("System", icon = icon("diagram-project"),
    layout_sidebar(
      sidebar = sidebar(width = 330,
        numericInput("sb_n", "Number of components", 5, 1, 10),
        uiOutput("sb_rows"),
        sliderInput("sb_years", "Horizon (years)", 1, 50, 10),
        radioButtons("sb_model", "Deterioration", c("None" = "none", "Schedule (k)" = "schedule",
                                                     "Markov (q)" = "markov"), inline = TRUE),
        sliderInput("sb_k", "k (years per notch)", 1, 20, 5, 1),
        sliderInput("sb_q", "q (annual prob. of dropping a notch)", 0, 0.5, 0.2, 0.01)),
      uiOutput("sb_boxes"),
      layout_columns(col_widths = two_col(6),
        card(card_header("Component cumulative risk at the horizon"), plotlyOutput("sb_comp", height = 380)),
        card(card_header("Facility cumulative risk by year, per structure"), plotlyOutput("sb_years_plot", height = 380))),
      p(class = "small text-muted",
        "Components are treated as independent. 'Expected failures' is the expected number of",
        "components failed by the horizon, not a probability.")
    )),

  # 8. Methods ----------------------------------------------------------------------------
  nav_panel("Methods", icon = icon("book"),
    card(card_body(includeMarkdown(file.path(ROOT, "apps", "shiny", "methods.md")),
                   p(class = "small text-muted", sprintf("Results computed %s · n = R = %d.",
                                                         A$settings$created, A$settings$iter_cap)))))

)

# ---- Server -----------------------------------------------------------------------
server <- function(input, output, session) {

  plotly_clean <- function(p) {
    p |>
      layout(legend = list(orientation = "h", x = 0, y = 1.02, xanchor = "left", yanchor = "bottom"),
             margin = list(t = 40)) |>
      config(displaylogo = FALSE)
  }

  # 1. Data ----------------------------------------------------------------------
  output$data_curves <- renderPlotly({
    req(input$data_groups)
    d <- A$long[A$long$asset_group %in% input$data_groups, ]
    if (!input$data_cf) d <- d[!d$is_cf, ]
    tr <- switch(input$data_scale, logit = qlogis, identity)
    d$y <- tr(d$p_fail)
    g <- d[!d$is_cf, ]; c <- d[d$is_cf, ]
    p <- plot_ly()
    for (i in seq_along(input$data_groups)) {
      gi <- g[g$asset_group == input$data_groups[i], ]
      p <- add_trace(p, data = gi, x = ~rating, y = ~y, type = "scatter", mode = "lines+markers",
                     name = input$data_groups[i], legendgroup = input$data_groups[i],
                     line = list(color = PAL[(i - 1) %% 10 + 1]),
                     marker = list(color = PAL[(i - 1) %% 10 + 1]),
                     text = ~paste0(asset_group, " ", rating, ": ", fmt_p(p_fail)), hoverinfo = "text")
      if (nrow(c)) {
        ci <- c[c$asset_group == input$data_groups[i], ]
        p <- add_trace(p, data = ci, x = ~rating, y = ~y, type = "scatter", mode = "markers",
                       legendgroup = input$data_groups[i], showlegend = FALSE,
                       marker = list(symbol = "diamond-open", size = 11, color = PAL[(i - 1) %% 10 + 1]),
                       text = ~paste0(asset_group, " CF (worst case): ", fmt_p(p_fail)), hoverinfo = "text")
      }
    }
    layout(p, xaxis = list(title = "Rating", categoryorder = "array", categoryarray = A$ratings),
           yaxis = list(title = switch(input$data_scale, log = "P(fail) (log)", logit = "logit P(fail)",
                                       "P(fail)"),
                        type = if (input$data_scale == "log") "log" else "linear")) |> plotly_clean()
  })
  output$data_heat <- renderPlotly({
    z <- log10(P)
    plot_ly(x = A$ratings, y = GROUPS, z = z, type = "heatmap", colorscale = "Viridis",
            text = matrix(fmt_p(P), nrow(P)), hovertemplate = "%{y} %{x}: %{text}<extra></extra>",
            colorbar = list(title = "log₁₀ p")) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "", autorange = "reversed")) |> plotly_clean()
  })
  output$data_checks <- renderTable(A$checks)
  output$data_table <- renderDT({
    w <- data.frame(asset_group = GROUPS, round(P, 5), check.names = FALSE)
    datatable(w, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 8, dom = "ftp", pagingType = "simple"))
  })

  # 2. Curve shape ---------------------------------------------------------------------
  observeEvent(input$pert_reset, {
    for (s in c("pert_level", "pert_slope", "pert_jump")) updateSliderInput(session, s, value = 1)
  })
  output$fit_plot <- renderPlotly({
    v <- P[input$fit_group, A$grades]
    f <- A$fits$fitted[A$fits$fitted$asset_group == input$fit_group, ]
    pc <- perturbed_curve(v, input$pert_level, input$pert_slope, input$pert_jump)
    plot_ly(x = factor(A$grades, A$grades)) |>
      add_trace(y = qlogis(v), type = "scatter", mode = "markers", name = "Table",
                marker = list(size = 9, color = PAL[1]),
                text = fmt_p(v), hovertemplate = "%{x}: %{text}<extra>Table</extra>") |>
      add_trace(y = qlogis(f$fitted), type = "scatter", mode = "lines", name = "Fitted",
                line = list(color = PAL[3], dash = "dot")) |>
      add_trace(y = qlogis(pc), type = "scatter", mode = "lines+markers", name = "Perturbed",
                line = list(color = PAL[2]), marker = list(color = PAL[2]),
                text = fmt_p(pc), hovertemplate = "%{x}: %{text}<extra>Perturbed</extra>") |>
      layout(yaxis = list(title = "logit P(fail)"), xaxis = list(title = "Grade"),
             shapes = list(list(type = "line", x0 = 10.5, x1 = 10.5, y0 = 0, y1 = 1, yref = "paper",
                                line = list(dash = "dash", color = "grey")),
                           list(type = "line", x0 = 6.5, x1 = 6.5, y0 = 0, y1 = 1, yref = "paper",
                                line = list(dash = "dot", color = "lightgrey")))) |> plotly_clean()
  })
  output$fit_params <- renderTable({
    s <- A$fits$summary[A$fits$summary$asset_group == input$fit_group, ]
    data.frame(Term = c("offset c", "a (A to C)", "a (C- to D-)", "a (F+ to F-)", "b (A to C)", "b (C- to D-)",
                        "b (F+ to F-)", "RMSE (logit)", "Max relative error"),
               Value = c(sprintf("%.3f", unlist(s[2:9])), sprintf("%.2f%%", 100 * s$max_rel_err)))
  })
  output$fit_features <- renderDT({
    f <- A$features
    num <- vapply(f, is.numeric, TRUE)
    f[num] <- lapply(f[num], signif, 3)
    datatable(f, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 8, dom = "ftp", pagingType = "simple"))
  })

  # 3a. Notch sensitivity ------------------------------------------------------------------
  output$ns_tornado <- renderPlotly({
    d <- A$notch[A$notch$asset_group == input$ns_group & A$notch$grade == input$ns_grade, ]
    d <- d[!is.na(d$ratio), ]
    d$lab <- sprintf("%+d notch", d$delta)
    d$pct <- 100 * (d$ratio - 1)
    plot_ly(d, y = ~factor(lab, rev(lab)), x = ~pct, type = "bar", orientation = "h",
            marker = list(color = ifelse(d$pct > 0, PAL[2], PAL[1])),
            text = ~sprintf("%s → %s", fmt_p(p_base), fmt_p(p_shift)),
            hovertemplate = "%{y}: %{x:.0f}%<br>%{text}<extra></extra>") |>
      layout(xaxis = list(title = "% change in P(fail)"), yaxis = list(title = "")) |> plotly_clean()
  })
  output$ns_text <- renderUI({
    e <- A$elasticity[A$elasticity$asset_group == input$ns_group & A$elasticity$grade == input$ns_grade, ]
    p(class = "small", sprintf("At %s, P(fail) = %s. One notch is worth about %.0f%%. The CF worst case is %.1f× this value.",
                               input$ns_grade, fmt_p(e$p_fail), e$pct_per_notch, e$cf_ratio))
  })
  output$ns_heat <- renderPlotly({
    e <- A$elasticity
    z <- tapply(e$pct_per_notch, list(e$asset_group, e$grade), identity)
    plot_ly(x = A$grades, y = rownames(z), z = z, type = "heatmap", colorscale = "YlOrRd",
            reversescale = TRUE,
            hovertemplate = "%{y} %{x}: %{z:.0f}% per notch<extra></extra>",
            colorbar = list(title = "% / notch")) |>
      layout(yaxis = list(autorange = "reversed", title = ""), xaxis = list(title = "")) |> plotly_clean()
  })

  # 3b. Misclassification ----------------------------------------------------------------
  M <- reactive(misclass_matrix(input$mc_correct, input$mc_width, input$mc_shape, input$mc_bias))
  mc <- reactive(misclass_effect(P, M()))
  output$mc_curve <- renderPlotly({
    d <- mc()[mc()$asset_group == input$mc_group, ]
    plot_ly(d, x = ~grade) |>
      add_ribbons(ymin = ~pmax(p_expected - p_sd, 1e-6), ymax = ~p_expected + p_sd, name = "±1 SD",
                  fillcolor = "rgba(224,122,95,0.2)", line = list(width = 0)) |>
      add_trace(y = ~p_observed, type = "scatter", mode = "lines+markers", name = "Table value",
                line = list(color = PAL[1])) |>
      add_trace(y = ~p_expected, type = "scatter", mode = "lines+markers", name = "Expected",
                line = list(color = PAL[2])) |>
      layout(yaxis = list(type = "log", title = "P(fail)"), xaxis = list(title = "Observed grade")) |> plotly_clean()
  })
  output$mc_heat <- renderPlotly({
    e <- mc()
    z <- tapply(e$bias_ratio, list(e$asset_group, e$grade), identity)
    plot_ly(x = A$grades, y = rownames(z), z = z, type = "heatmap", colorscale = "RdBu", reversescale = TRUE,
            zmid = 1, hovertemplate = "%{y} %{x}: ×%{z:.3f}<extra></extra>",
            colorbar = list(title = "ratio")) |>
      layout(yaxis = list(autorange = "reversed", title = ""), xaxis = list(title = "")) |> plotly_clean()
  })
  output$mc_matrix <- renderPlotly({
    plot_ly(x = A$grades, y = A$grades, z = M(), type = "heatmap", colorscale = "Blues",
            hovertemplate = "observed %{y}, true %{x}: %{z:.2f}<extra></extra>") |>
      layout(xaxis = list(title = "True grade"), yaxis = list(title = "Observed grade", autorange = "reversed")) |>
      plotly_clean()
  })

  # 4. Multi-year ------------------------------------------------------------------------
  my_data <- reactive({
    req(input$my_groups)
    g <- match(input$my_grade, A$grades)
    start <- if (input$my_mc) M()[g, ] else g
    runs <- list()
    for (gr in input$my_groups) {
      v <- P[gr, A$grades]
      r <- multiyear_risk(v, start, input$my_years, input$my_model, input$my_k, input$my_q)
      runs[[length(runs) + 1]] <- data.frame(asset_group = gr, scenario = "Selected", r)
      if (input$my_compare && input$my_model != "none") {
        r0 <- multiyear_risk(v, start, input$my_years, "none")
        runs[[length(runs) + 1]] <- data.frame(asset_group = gr, scenario = "No deterioration", r0)
      }
    }
    do.call(rbind, runs)
  })
  my_line_plot <- function(var, ytitle, logy = FALSE) {
    d <- my_data()
    p <- plot_ly()
    for (i in seq_along(input$my_groups)) for (sc in unique(d$scenario)) {
      di <- d[d$asset_group == input$my_groups[i] & d$scenario == sc, ]
      p <- add_trace(p, x = di$year, y = di[[var]], type = "scatter", mode = "lines",
                     name = if (sc == "Selected") input$my_groups[i] else paste(input$my_groups[i], "(no det.)"),
                     line = list(color = PAL[(i - 1) %% 10 + 1], dash = if (sc == "Selected") "solid" else "dot"),
                     hovertemplate = paste0("Year %{x}: %{y:.4f}<extra>", input$my_groups[i], "</extra>"))
    }
    layout(p, xaxis = list(title = "Year"), yaxis = list(title = ytitle, type = if (logy) "log" else "linear")) |> plotly_clean()
  }
  output$my_cum <- renderPlotly({
    my_line_plot("cumulative", "P(failed by year t)") |>
      layout(shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper",
                                y0 = input$my_threshold, y1 = input$my_threshold,
                                line = list(dash = "dash", color = "grey"))))
  })
  output$my_haz <- renderPlotly(my_line_plot("annual_hazard", "Annual hazard", logy = TRUE))
  output$my_grade_plot <- renderPlotly({
    my_line_plot("expected_grade", "Expected grade") |>
      layout(yaxis = list(tickvals = 1:14, ticktext = A$grades, range = c(14.5, 0.5)))
  })
  output$my_boxes <- renderUI({
    d <- my_data(); d <- d[d$scenario == "Selected", ]
    boxes <- lapply(seq_along(input$my_groups), function(i) {
      di <- d[d$asset_group == input$my_groups[i], ]
      cross <- di$year[di$cumulative >= input$my_threshold][1]
      value_box(title = input$my_groups[i],
                value = fmt_p(tail(di$cumulative, 1)),
                p(sprintf("chance of failing within %d years", input$my_years)),
                p(sprintf("Year 1: %s · reaches %.0f%% %s", fmt_p(di$annual_hazard[1]),
                          100 * input$my_threshold,
                          if (is.na(cross)) "after the horizon" else sprintf("in year %d", cross))),
                theme = value_box_theme(bg = PAL[(i - 1) %% 10 + 1], fg = "white"))
    })
    do.call(layout_column_wrap, c(list(width = "220px", fill = FALSE), boxes))
  })
  output$my_table <- renderDT({
    d <- my_data()
    d[, c("expected_grade", "annual_hazard", "p_fail_in_year", "cumulative", "survival")] <-
      lapply(d[, c("expected_grade", "annual_hazard", "p_fail_in_year", "cumulative", "survival")], signif, 4)
    datatable(d, rownames = FALSE, options = list(pageLength = 6, scrollX = TRUE, dom = "tp", pagingType = "simple"))
  })
  output$my_heat <- renderPlotly({
    z <- sapply(seq_along(A$grades), function(g) sapply(GROUPS, function(gr)
      tail(multiyear_risk(P[gr, A$grades], g, input$my_years, input$my_model, input$my_k, input$my_q)$cumulative, 1)))
    plot_ly(x = A$grades, y = GROUPS, z = z, type = "heatmap", colorscale = "YlOrRd", reversescale = TRUE,
            zmin = 0, zmax = 1, hovertemplate = "%{y}, start %{x}: %{z:.3f}<extra></extra>",
            colorbar = list(title = "P(fail)")) |>
      layout(yaxis = list(autorange = "reversed", title = ""), xaxis = list(title = "Starting grade")) |>
      plotly_clean()
  })

  # 5a. L2 global sensitivity ---------------------------------------------------------------
  gs_sel <- reactive({
    i <- A$l2_influence
    i[i$asset_group == input$gs_group & i$grade == input$gs_grade & i$horizon == as.numeric(input$gs_horizon), ]
  })
  output$gs_bars <- renderPlotly({
    d <- gs_sel()
    lab <- PARAM_LABEL[d$parameter]
    plot_ly() |>
      add_trace(x = lab, y = d$Si, type = "bar", name = "First-order Sᵢ", marker = list(color = PAL[1]),
                error_y = list(type = "data", symmetric = FALSE, array = d$Si_high - d$Si,
                               arrayminus = d$Si - d$Si_low)) |>
      add_trace(x = lab, y = d$Ti, type = "bar", name = "Total-order Tᵢ", marker = list(color = PAL[2]),
                error_y = list(type = "data", symmetric = FALSE, array = d$Ti_high - d$Ti,
                               arrayminus = d$Ti - d$Ti_low)) |>
      layout(barmode = "group", yaxis = list(title = "Index"),
             shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = d$dummy_Ti_high[1],
                                y1 = d$dummy_Ti_high[1], line = list(dash = "dash", color = "grey")))) |> plotly_clean()
  })
  output$gs_meta <- renderUI({
    d <- gs_sel()
    u <- A$l2_uncertainty
    u <- u[u$asset_group == input$gs_group & u$grade == input$gs_grade & u$horizon == as.numeric(input$gs_horizon), ]
    tagList(
      p(class = "small", tags$b("Influential: "),
        paste(PARAM_LABEL[d$parameter[d$influential]], collapse = ", "),
        if (!any(d$influential)) "none could be told apart from the dummy"),
      p(class = "small text-muted", sprintf("n = %d, model runs = %d, widest CI = %.2f (target < 0.05), converged: %s",
                                            u$n, u$N_total, u$max_ci_width, ifelse(u$converged, "yes", "no"))))
  })
  output$gs_grade_line <- renderPlotly({
    i <- A$l2_influence
    i <- i[i$asset_group == input$gs_group & i$horizon == as.numeric(input$gs_horizon), ]
    p <- plot_ly()
    for (k in seq_along(unique(i$parameter))) {
      prm <- unique(i$parameter)[k]; d <- i[i$parameter == prm, ]
      d <- d[order(d$grade_index), ]
      p <- add_trace(p, x = d$grade, y = d$Ti, type = "scatter", mode = "lines+markers",
                     name = PARAM_LABEL[prm], line = list(color = PAL[k]),
                     marker = list(color = PAL[k], symbol = ifelse(d$influential, "circle", "circle-open"), size = 8))
    }
    layout(p, yaxis = list(title = "Tᵢ"), xaxis = list(title = "Observed grade")) |> plotly_clean()
  })
  output$gs_heat <- renderPlotly({
    i <- A$l2_influence[A$l2_influence$horizon == as.numeric(input$gs_horizon), ]
    z <- tapply(i$Ti, list(i$asset_group, i$parameter), mean)
    plot_ly(x = PARAM_LABEL[colnames(z)], y = rownames(z), z = z, type = "heatmap", colorscale = "Viridis",
            hovertemplate = "%{y}, %{x}: mean Tᵢ %{z:.2f}<extra></extra>") |>
      layout(yaxis = list(autorange = "reversed", title = ""), xaxis = list(title = "")) |> plotly_clean()
  })

  # 5b. L3 example facility --------------------------------------------------------------
  output$l3_facility <- renderTable(A$facility[, c("component", "asset_group", "grade")])
  output$l3_bars <- renderPlotly({
    r <- A$l3_example[[input$l3_horizon]]
    d <- r$influence[r$influence$output == input$l3_structure, ]
    lab <- r$inputs$label[match(d$parameter, r$inputs$name)]
    o <- order(d$Ti)
    plot_ly() |>
      add_trace(y = factor(lab[o], lab[o]), x = d$Ti[o], type = "bar", orientation = "h", name = "Tᵢ",
                marker = list(color = ifelse(d$influential[o], PAL[2], "lightgrey")),
                error_x = list(type = "data", symmetric = FALSE, array = (d$Ti_high - d$Ti)[o],
                               arrayminus = (d$Ti - d$Ti_low)[o])) |>
      layout(xaxis = list(title = "Total-order Tᵢ (grey = non-influential)"), yaxis = list(title = ""),
             shapes = list(list(type = "line", y0 = 0, y1 = 1, yref = "paper", x0 = d$dummy_Ti_high[1],
                                x1 = d$dummy_Ti_high[1], line = list(dash = "dash", color = "grey")))) |>
      plotly_clean()
  })

  # 5c. Structure comparison -----------------------------------------------------------------
  sc_box <- function(var, ytitle) {
    e <- A$ensemble[A$ensemble$horizon == as.numeric(input$sc_horizon), ]
    e$label <- sapply(SYSTEM_STRUCTURES, `[[`, "label")[e$structure]
    plot_ly(e, x = ~label, y = e[[var]], type = "box", boxpoints = "all", jitter = 0.4, pointpos = 0,
            marker = list(size = 4, opacity = 0.5), color = ~label, colors = PAL[1:5], showlegend = FALSE) |>
      layout(xaxis = list(title = ""), yaxis = list(title = ytitle)) |> plotly_clean()
  }
  output$sc_noninf <- renderPlotly(sc_box("pct_noninfluential", "% non-influential"))
  output$sc_median <- renderPlotly(sc_box("median_output", "Median output"))
  output$sc_friedman <- renderTable({
    t <- A$structure_tests[A$structure_tests$horizon == as.numeric(input$sc_horizon), ]
    t$p_value <- format.pval(t$p_value, digits = 2); t
  })
  output$sc_pairwise <- renderDT({
    t <- A$structure_pairwise[A$structure_pairwise$horizon == as.numeric(input$sc_horizon), ]
    t$p_adj_BH <- signif(t$p_adj_BH, 3)
    datatable(t, rownames = FALSE, options = list(pageLength = 5, scrollX = TRUE, dom = "tp", pagingType = "simple"))
  })

  # 6. Uncertainty -------------------------------------------------------------------
  output$ua_band <- renderPlotly({
    u <- A$l2_uncertainty
    u <- u[u$asset_group == input$ua_group & u$horizon == as.numeric(input$ua_horizon), ]
    u <- u[order(u$grade_index), ]
    base <- if (input$ua_horizon == "1") P[input$ua_group, A$grades] else
      1 - (1 - P[input$ua_group, A$grades])^10
    plot_ly(x = u$grade) |>
      add_ribbons(ymin = u$q025, ymax = u$q975, name = "95% interval",
                  fillcolor = "rgba(42,111,151,0.2)", line = list(width = 0)) |>
      add_trace(y = u$median, type = "scatter", mode = "lines+markers", name = "Median",
                line = list(color = PAL[1])) |>
      add_trace(y = base, type = "scatter", mode = "markers", name = "Table (no deterioration)",
                marker = list(color = PAL[2], symbol = "x", size = 9)) |>
      layout(yaxis = list(type = "log", title = "Output"), xaxis = list(title = "Observed grade")) |> plotly_clean()
  })
  output$ua_heat <- renderPlotly({
    u <- A$l2_uncertainty[A$l2_uncertainty$horizon == as.numeric(input$ua_horizon), ]
    u$rel <- (u$q975 - u$q025) / u$median
    z <- tapply(u$rel, list(u$asset_group, u$grade), identity)
    plot_ly(x = A$grades, y = rownames(z), z = z, type = "heatmap", colorscale = "Magma", reversescale = TRUE,
            hovertemplate = "%{y} %{x}: %{z:.2f}<extra></extra>") |>
      layout(yaxis = list(autorange = "reversed", title = ""), xaxis = list(title = "")) |> plotly_clean()
  })
  output$ua_l3 <- renderPlotly({
    s <- A$l3_example[[input$ua_l3_h]]$sample
    st <- names(SYSTEM_STRUCTURES)
    p <- plot_ly()
    for (k in seq_along(st)) p <- add_trace(p, y = s[[st[k]]], type = "box", name = SYSTEM_STRUCTURES[[st[k]]]$label,
                                            boxpoints = FALSE, line = list(color = PAL[k]),
                                            fillcolor = "rgba(0,0,0,0.04)")
    layout(p, yaxis = list(type = "log", title = "Facility output"), showlegend = FALSE) |> plotly_clean()
  })

  # 7. System builder ------------------------------------------------------------------
  output$sb_rows <- renderUI({
    n <- input$sb_n; req(n >= 1, n <= 10)
    ex <- A$facility
    lapply(seq_len(n), function(i) {
      layout_columns(col_widths = c(9, 3), gap = "0.3rem",
        selectInput(paste0("sb_g", i), NULL, GROUPS,
                    isolate(input[[paste0("sb_g", i)]]) %||% ex$asset_group[(i - 1) %% nrow(ex) + 1]),
        selectInput(paste0("sb_r", i), NULL, A$grades,
                    isolate(input[[paste0("sb_r", i)]]) %||% ex$grade[(i - 1) %% nrow(ex) + 1]))
    })
  })
  sb_fac <- reactive({
    n <- input$sb_n; req(n >= 1, n <= 10)
    gs <- sapply(seq_len(n), function(i) input[[paste0("sb_g", i)]] %||% NA)
    rs <- sapply(seq_len(n), function(i) input[[paste0("sb_r", i)]] %||% NA)
    req(!anyNA(gs), !anyNA(rs))
    data.frame(component = sprintf("%d. %s (%s)", seq_len(n), gs, rs), asset_group = gs, grade = rs)
  })
  sb_traj <- reactive({
    f <- sb_fac()
    cum <- sapply(seq_len(nrow(f)), function(i)
      multiyear_risk(P[f$asset_group[i], A$grades], match(f$grade[i], A$grades), input$sb_years,
                     input$sb_model, input$sb_k, input$sb_q)$cumulative)
    matrix(cum, nrow = input$sb_years)
  })
  output$sb_boxes <- renderUI({
    C <- sb_traj(); last <- C[nrow(C), , drop = FALSE]
    vals <- sapply(names(SYSTEM_STRUCTURES), function(s) aggregate_system(last, s))
    do.call(layout_column_wrap, c(list(width = "190px", fill = FALSE), lapply(seq_along(vals), function(k)
      value_box(title = SYSTEM_STRUCTURES[[k]]$label,
                value = if (names(vals)[k] == "expected") sprintf("%.2f", vals[k]) else fmt_p(vals[k]),
                p(if (names(vals)[k] == "expected") sprintf("components failed by year %d", input$sb_years)
                  else sprintf("by year %d", input$sb_years)),
                theme = value_box_theme(bg = PAL[k], fg = "white")))))
  })
  output$sb_comp <- renderPlotly({
    f <- sb_fac(); C <- sb_traj()
    plot_ly(y = factor(f$component, rev(f$component)), x = C[nrow(C), ], type = "bar", orientation = "h",
            marker = list(color = PAL[1]), hovertemplate = "%{y}: %{x:.4f}<extra></extra>") |>
      layout(xaxis = list(title = sprintf("P(failed by year %d)", input$sb_years)), yaxis = list(title = "")) |>
      plotly_clean()
  })
  output$sb_years_plot <- renderPlotly({
    C <- sb_traj()
    p <- plot_ly()
    for (k in seq_along(SYSTEM_STRUCTURES)) {
      s <- names(SYSTEM_STRUCTURES)[k]
      if (s == "expected") next
      p <- add_trace(p, x = seq_len(nrow(C)), y = aggregate_system(C, s), type = "scatter", mode = "lines",
                     name = SYSTEM_STRUCTURES[[k]]$label, line = list(color = PAL[k]))
    }
    layout(p, xaxis = list(title = "Year"), yaxis = list(title = "Facility P(failed by year t)")) |> plotly_clean()
  })
}

shinyApp(ui, server)
