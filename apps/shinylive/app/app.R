# Failure-probability explorer: Shinylive edition ------------------------------
#
# The same kind of Shiny app, compiled to run fully in the browser with webR
# (R in WebAssembly). No R server is needed, so it can be hosted on GitHub
# Pages. Trade-offs: a slower first load (it downloads R and its packages),
# and only packages built for webR can be used. That is why this edition uses
# ggplot2 rather than plotly/DT, and reads a trimmed results file.
#
# Build: Rscript scripts/build_site.R copies data/ and R/ into this folder and
# runs shinylive::export().

library(shiny)
library(bslib)
library(ggplot2)

for (f in list.files("R", "\\.R$", full.names = TRUE)) source(f)
A <- readRDS("data/shinylive_data.rds")
P <- A$p
GROUPS <- rownames(P)
PAL <- c("#2a6f97", "#e07a5f", "#3d405b", "#81b29a", "#f2cc8f", "#9c6644")
theme_set(theme_minimal(base_size = 14))
pct <- function(x) sprintf("%.2f%%", 100 * x)

ui <- page_navbar(
  title = "Failure Sensitivity · Shinylive",
  window_title = "Failure-Probability Sensitivity · Shinylive",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2a6f97"),
  header = tags$style(HTML("
    .bslib-value-box .value-box-title, .bslib-value-box .value-box-value { overflow-wrap: normal; word-break: normal; }
    .bslib-value-box .value-box-value { font-size: clamp(1.3rem, 0.9rem + 1.2vw, 1.9rem); line-height: 1.15; }
    .card, .card-body, .bslib-sidebar-layout > .main { min-width: 0; }")),
  nav_panel("Multi-year risk",
    layout_sidebar(
      sidebar = sidebar(
        selectizeInput("groups", "Asset groups", GROUPS, c("Electrical A", "Concrete A"),
                       multiple = TRUE, options = list(maxItems = 6)),
        selectInput("grade", "Starting grade", A$grades, "C"),
        sliderInput("years", "Horizon (years)", 1, 50, 20),
        radioButtons("model", "Deterioration", c("None" = "none", "Schedule (every k yrs)" = "schedule",
                                                 "Markov (prob. q per yr)" = "markov"), "markov"),
        sliderInput("k", "k", 1, 20, 5), sliderInput("q", "q", 0, 0.5, 0.2, 0.01),
        checkboxInput("mc", "Grade uncertainty (60/20/20)", FALSE)),
      uiOutput("boxes"),
      card(card_header("Cumulative P(failed by year t)"), plotOutput("cum", height = 360)),
      card(card_header("Annual hazard (log scale)"), plotOutput("haz", height = 300)))),
  nav_panel("Misclassification",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("mc_group", "Asset group", GROUPS, "Electrical A"),
        sliderInput("mc_p", "P(grade correct)", 0.2, 1, 0.6, 0.05),
        sliderInput("mc_w", "Max error (notches)", 1, 3, 1)),
      card(card_header("Expected vs. table P(fail) by observed grade"), plotOutput("mc_plot", height = 420)))),
  nav_panel("Sobol' (L2)",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("gs_group", "Asset group", GROUPS, "Building A"),
        selectInput("gs_grade", "Observed grade", A$grades, "D-"),
        radioButtons("gs_h", "Output", c("Annual" = 1, "10-year" = 10)),
        p(class = "small text-muted", sprintf("Precomputed with sensobol; n = R = %d (iteration cap).",
                                              A$settings$iter_cap))),
      card(card_header("Sobol' indices with 95% CIs (dashed = dummy Tᵢ upper bound)"),
           plotOutput("gs_plot", height = 380)),
      card(card_header("Tᵢ across observed grades"), plotOutput("gs_line", height = 320)))),
  nav_panel("About",
    card(card_body(
      h4("What is Shinylive?"),
      p("This page runs a Shiny app entirely in your browser using webR. It is the same R code style as the",
        "server-based app, but nothing is computed on a server. The multi-year and misclassification tabs",
        "are recomputed live in WebAssembly. The Sobol' results were computed ahead of time with sensobol,",
        "which is too heavy to run here."),
      p("The full-featured app (8 tabs, plotly charts, system builder) is the server-based Shiny app in",
        tags$code("apps/shiny"), ". Run it locally with ", tags$code("shiny::runApp('apps/shiny')"), ".")))))

server <- function(input, output, session) {
  traj <- reactive({
    req(input$groups)
    g <- match(input$grade, A$grades)
    start <- if (input$mc) misclass_matrix(0.6, 1)[g, ] else g
    do.call(rbind, lapply(input$groups, function(gr)
      data.frame(group = gr, multiyear_risk(P[gr, A$grades], start, input$years, input$model,
                                            input$k, input$q))))
  })
  output$boxes <- renderUI({
    d <- traj()
    do.call(layout_column_wrap, c(list(width = "220px", fill = FALSE),
      lapply(seq_along(input$groups), function(i) {
        di <- d[d$group == input$groups[i], ]
        value_box(input$groups[i], pct(tail(di$cumulative, 1)),
                  p(sprintf("chance of failing within %d years", input$years)),
                  theme = value_box_theme(bg = PAL[i], fg = "white"))
      })))
  })
  output$cum <- renderPlot(ggplot(traj(), aes(year, cumulative, colour = group)) + geom_line(linewidth = 1.2) +
    scale_colour_manual(values = PAL) + scale_y_continuous(labels = scales::percent) +
    labs(x = "Year", y = NULL, colour = NULL) + theme(legend.position = "bottom"))
  output$haz <- renderPlot(ggplot(traj(), aes(year, annual_hazard, colour = group)) + geom_line(linewidth = 1.2) +
    scale_colour_manual(values = PAL) + scale_y_log10(labels = scales::percent) +
    labs(x = "Year", y = NULL, colour = NULL) + theme(legend.position = "none"))

  output$mc_plot <- renderPlot({
    e <- misclass_effect(P[input$mc_group, , drop = FALSE], misclass_matrix(input$mc_p, input$mc_w))
    d <- rbind(data.frame(grade = e$grade, p = e$p_observed, what = "Table"),
               data.frame(grade = e$grade, p = e$p_expected, what = "Expected"))
    ggplot(d, aes(grade, p, colour = what, group = what)) + geom_line(linewidth = 1.1) + geom_point(size = 2.5) +
      scale_y_log10(labels = scales::percent) + scale_colour_manual(values = PAL[2:1]) +
      labs(x = "Observed grade", y = "P(fail)", colour = NULL) + theme(legend.position = "bottom")
  })

  gs <- reactive({
    i <- A$l2_influence
    i[i$asset_group == input$gs_group & i$horizon == as.numeric(input$gs_h), ]
  })
  output$gs_plot <- renderPlot({
    d <- gs()[gs()$grade == input$gs_grade, ]
    long <- rbind(data.frame(param = d$parameter, idx = "Sᵢ", v = d$Si, lo = d$Si_low, hi = d$Si_high),
                  data.frame(param = d$parameter, idx = "Tᵢ", v = d$Ti, lo = d$Ti_low, hi = d$Ti_high))
    ggplot(long, aes(param, v, fill = idx)) +
      geom_col(position = position_dodge(0.8), width = 0.75) +
      geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(0.8), width = 0.2) +
      geom_hline(yintercept = d$dummy_Ti_high[1], linetype = 2, colour = "grey40") +
      scale_fill_manual(values = PAL[1:2]) + labs(x = NULL, y = "Index", fill = NULL) +
      theme(legend.position = "bottom")
  })
  output$gs_line <- renderPlot({
    ggplot(gs(), aes(grade, Ti, colour = parameter, group = parameter)) + geom_line(linewidth = 1.1) +
      geom_point(aes(shape = influential), size = 3) + scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1)) +
      scale_colour_manual(values = PAL) + labs(x = "Observed grade", y = "Tᵢ", colour = NULL, shape = "Influential") +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui, server)
