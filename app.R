# app.R — The Couch Economy
# ─────────────────────────────────────────────────────────────────────────────
# Shiny app exploring the wealth-inactivity-NCD paradox across countries.
# Run with: shiny::runApp() from the project root.
# ─────────────────────────────────────────────────────────────────────────────

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(here)
library(glue)
library(shinycssloaders)

source(here("R", "generate_brief.R"))
# jsonlite (loaded in generate_brief.R) masks shiny::validate — restore it
validate <- shiny::validate

# ── Load data ─────────────────────────────────────────────────────────────────

data_path <- here("data", "processed", "couch_economy.rds")

if (!file.exists(data_path)) {
  stop(
    "Processed data not found. Run R/fetch_data.R then R/process_data.R first.",
    call. = FALSE
  )
}

df <- readRDS(data_path)

if (file.exists(here(".env"))) readRenviron(here(".env"))

# ── Lookup tables ─────────────────────────────────────────────────────────────

countries <- sort(unique(df$country_name))
years     <- sort(unique(df$year))

all_ncd_choices <- c(
  "Obesity prevalence (%)"             = "obesity_prevalence",
  "Diabetes prevalence (%)"            = "diabetes_prevalence",
  "Premature NCD mortality (%, 30-70)" = "ncd_mortality"
)
ncd_choices <- all_ncd_choices[all_ncd_choices %in% names(df)]

income_levels <- c(
  "All", "Low income", "Lower middle income",
  "Upper middle income", "High income"
)

income_colors <- c(
  "Low income"          = "#E63946",
  "Lower middle income" = "#F4A261",
  "Upper middle income" = "#2A9D8F",
  "High income"         = "#264653"
)

ncd_label_for <- function(col) names(ncd_choices)[ncd_choices == col]

# ── Theme ─────────────────────────────────────────────────────────────────────

app_theme <- bs_theme(
  version    = 5,
  bootswatch = "flatly",
  primary    = "#1A3C5E",
  secondary  = "#2E75B6",
  base_font  = font_google("Inter"),
  code_font  = font_google("Fira Code")
) |>
  bs_add_rules("
    /* ── Cards ── */
    .card {
      box-shadow: 0 2px 12px rgba(0,0,0,0.07);
      border: 1px solid rgba(0,0,0,0.07) !important;
      border-radius: 0.75rem !important;
    }
    .card-header {
      font-weight: 600;
      font-size: 0.82rem;
      letter-spacing: 0.05em;
      text-transform: uppercase;
      color: #6c757d;
      background-color: #f8f9fa !important;
      border-bottom: 1px solid rgba(0,0,0,0.07) !important;
      padding: 0.6rem 1rem;
    }
    .card-footer {
      background-color: transparent !important;
      border-top: 1px solid rgba(0,0,0,0.05) !important;
      padding: 0.45rem 1rem;
    }
    .card-body { padding: 0.75rem; }

    /* ── Value boxes ── */
    .bslib-value-box {
      border-radius: 0.75rem !important;
      box-shadow: 0 3px 14px rgba(0,0,0,0.13) !important;
    }
    .bslib-value-box .value-box-value { font-size: 1.6rem; font-weight: 700; }
    .bslib-value-box .value-box-title { font-size: 0.78rem; opacity: 0.85; letter-spacing: 0.03em; }

    /* ── Sidebar ── */
    .bslib-sidebar-layout > .sidebar {
      border-right: 1px solid rgba(0,0,0,0.08) !important;
    }
    .sidebar h6 {
      font-size: 0.7rem;
      letter-spacing: 0.08em;
      color: #adb5bd;
      margin-bottom: 0.4rem;
      margin-top: 0.25rem;
    }
    .sidebar .form-label { font-size: 0.88rem; font-weight: 500; color: #495057; margin-bottom: 0.2rem; }
    .sidebar hr { border-color: rgba(0,0,0,0.08); margin: 0.85rem 0; }

    /* ── Inputs ── */
    .selectize-input, .form-control {
      border-radius: 0.4rem !important;
      font-size: 0.88rem;
      border-color: #dee2e6 !important;
    }
    .selectize-input:focus-within { border-color: #2E75B6 !important; box-shadow: 0 0 0 0.15rem rgba(46,117,182,0.2) !important; }
    .irs--shiny .irs-bar, .irs--shiny .irs-bar--single { background: #1A3C5E; border-color: #1A3C5E; }
    .irs--shiny .irs-handle { background: #1A3C5E; border-color: #1A3C5E; }
    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: #1A3C5E; }

    /* ── Buttons ── */
    .btn-primary {
      border-radius: 0.45rem;
      font-weight: 500;
      letter-spacing: 0.02em;
      font-size: 0.9rem;
    }

    /* ── Navbar ── */
    .navbar-brand strong { letter-spacing: -0.3px; }

    /* ── Badge ── */
    .badge.bg-success { font-size: 0.68rem; font-weight: 500; }

    /* ── Footer ── */
    .app-footer {
      border-top: 1px solid #e9ecef;
      padding: 0.7rem 1rem;
      font-size: 0.78rem;
      color: #adb5bd;
      text-align: center;
      margin-top: 0.5rem;
    }
    .app-footer a { color: #6c757d; text-decoration: none; }
    .app-footer a:hover { color: #1A3C5E; text-decoration: underline; }

    /* ── Spinner ── */
    .shiny-spinner-output-container { min-height: 120px; }

    /* ── Mobile ── */
    @media (max-width: 767px) {
      .bslib-sidebar-layout > .main { padding: 0.6rem !important; }
      .layout-nm { gap: 0.5rem !important; }
    }
  ")

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  id    = "main_nav",
  title = tags$span(
    tags$strong("The Couch Economy"),
    tags$span(
      " — Wealth, Inactivity & NCD Burden",
      style = "font-size: 0.82em; color: rgba(255,255,255,0.6);"
    )
  ),
  theme = app_theme,
  navbar_options = navbar_options(bg = "#1A3C5E", theme = "dark"),
  fillable = FALSE,

  # ── Tab 1: Home ──────────────────────────────────────────────────────────────
  nav_panel(
    title = "Home",
    icon  = icon("house"),

    layout_columns(
      col_widths = breakpoints(sm = 12, md = 10, lg = 8),

      tagList(
        tags$div(
          class = "text-center py-4",
          tags$h1(
            class = "fw-bold mb-3",
            style = "color: #1A3C5E; font-size: 2.4rem;",
            "The Couch Economy"
          ),
          tags$p(
            class = "lead mb-3",
            style = "font-size: 1.15rem; color: #495057; max-width: 700px; margin: 0 auto;",
            "As countries grow wealthier, their citizens become more physically ",
            "inactive \u2014 and sicker from preventable diseases. ",
            "This dashboard makes that paradox visible, explorable, and actionable."
          ),
          tags$div(
            class = "d-flex justify-content-center gap-3 flex-wrap mb-2",
            tags$span(class = "badge rounded-pill bg-primary px-3 py-2",
                      style = "font-size: 0.85rem; font-weight: 500;",
                      icon("flag", class = "me-1"), "195 countries"),
            tags$span(class = "badge rounded-pill bg-primary px-3 py-2",
                      style = "font-size: 0.85rem; font-weight: 500;",
                      icon("calendar", class = "me-1"), "2000\u20132022"),
            tags$span(class = "badge rounded-pill bg-primary px-3 py-2",
                      style = "font-size: 0.85rem; font-weight: 500;",
                      icon("database", class = "me-1"), "5 data sources"),
            tags$span(class = "badge rounded-pill bg-danger px-3 py-2",
                      style = "font-size: 0.85rem; font-weight: 500;",
                      icon("robot", class = "me-1"), "AI-powered briefs")
          )
        ),

        card(
          card_header("The Question"),
          card_body(
            tags$p(
              style = "font-size: 1.05rem; line-height: 1.8;",
              tags$strong("Why do the world\u2019s richest countries have some of the highest ",
                          "rates of physical inactivity and chronic disease?"),
              " High-income nations invest heavily in healthcare, yet sedentary jobs, ",
              "car-dependent cities, and screen-filled leisure time drive up ",
              tags$strong("NCDs"), " (non-communicable diseases) \u2014 conditions like ",
              "obesity, diabetes, and cardiovascular disease that are largely preventable ",
              "through physical activity."
            ),
            tags$p(
              style = "font-size: 1.05rem; line-height: 1.8;",
              "This app lets you explore the relationship between a country\u2019s wealth ",
              "(GDP per capita) and its population\u2019s physical inactivity, then see how ",
              "that inactivity connects to real health outcomes across 195 countries ",
              "and over two decades of data."
            )
          )
        ),

        card(
          card_header("Who Is This For?"),
          card_body(
            layout_columns(
              col_widths = breakpoints(sm = c(12, 12, 12), md = c(4, 4, 4)),
              tags$div(
                class = "text-center p-2",
                icon("user-graduate", class = "fa-2x mb-2", style = "color: #2E75B6;"),
                tags$h6(class = "fw-bold", "Students & Researchers"),
                tags$p(class = "text-muted small",
                       "Explore global health data for coursework, theses, or research ",
                       "on the wealth\u2013inactivity\u2013disease nexus.")
              ),
              tags$div(
                class = "text-center p-2",
                icon("landmark", class = "fa-2x mb-2", style = "color: #2A9D8F;"),
                tags$h6(class = "fw-bold", "Policymakers"),
                tags$p(class = "text-muted small",
                       "Compare your country to peers, identify trends, and generate ",
                       "AI-written briefs tailored to a policy audience.")
              ),
              tags$div(
                class = "text-center p-2",
                icon("earth-americas", class = "fa-2x mb-2", style = "color: #E63946;"),
                tags$h6(class = "fw-bold", "Curious Citizens"),
                tags$p(class = "text-muted small",
                       "See where your country stands and learn why wealth alone ",
                       "doesn\u2019t protect against lifestyle diseases.")
              )
            )
          )
        ),

        card(
          card_header("Explore the Dashboard"),
          card_body(
            tags$p(
              style = "font-size: 0.95rem; color: #6c757d; margin-bottom: 1rem;",
              "Four tabs, one story. Start with the big picture, then drill down."
            ),
            layout_columns(
              col_widths = breakpoints(sm = c(12, 12), md = c(6, 6)),
              gap = "0.75rem",

              tags$div(
                class = "border rounded-3 p-3 h-100",
                style = "border-color: #dee2e6 !important;",
                tags$div(
                  class = "d-flex align-items-center mb-2",
                  icon("globe", class = "fa-lg me-2", style = "color: #2E75B6;"),
                  tags$h6(class = "fw-bold mb-0", "Explorer")
                ),
                tags$p(class = "small text-muted mb-1",
                       tags$strong("Overview"), " \u2014 interactive scatter plot (GDP vs. inactivity) ",
                       "with value boxes and income-group colour coding. Click any bubble to jump to country detail."),
                tags$p(class = "small text-muted mb-1",
                       tags$strong("Year-over-Year"), " \u2014 compare two time points with ",
                       "grouped bar charts. Filter by income group or see the top 10 biggest movers."),
                tags$p(class = "small text-muted mb-0",
                       tags$strong("Country Detail"), " \u2014 trend chart and AI-generated health brief ",
                       "(policy or general audience) powered by Gemini.")
              ),

              tags$div(
                class = "border rounded-3 p-3 h-100",
                style = "border-color: #dee2e6 !important;",
                tags$div(
                  class = "d-flex align-items-center mb-2",
                  icon("scale-balanced", class = "fa-lg me-2", style = "color: #2A9D8F;"),
                  tags$h6(class = "fw-bold mb-0", "Compare")
                ),
                tags$p(class = "small text-muted mb-0",
                       "Pick 2\u20133 countries for head-to-head trend charts, a data snapshot table, ",
                       "NCD comparison, and an AI-written comparative brief. ",
                       "CSV export included.")
              ),

              tags$div(
                class = "border rounded-3 p-3 h-100",
                style = "border-color: #dee2e6 !important;",
                tags$div(
                  class = "d-flex align-items-center mb-2",
                  icon("layer-group", class = "fa-lg me-2", style = "color: #E63946;"),
                  tags$h6(class = "fw-bold mb-0", "Regions")
                ),
                tags$p(class = "small text-muted mb-0",
                       "See how the seven World Bank regions stack up: scatter plot, ",
                       "side-by-side bar charts, dual-axis trend lines (inactivity + NCD), ",
                       "and a summary table. Filter which regions appear on the trend chart.")
              ),

              tags$div(
                class = "border rounded-3 p-3 h-100",
                style = "border-color: #dee2e6 !important;",
                tags$div(
                  class = "d-flex align-items-center mb-2",
                  icon("download", class = "fa-lg me-2", style = "color: #264653;"),
                  tags$h6(class = "fw-bold mb-0", "Data Export")
                ),
                tags$p(class = "small text-muted mb-0",
                       "Every tab offers CSV downloads \u2014 filtered country data, year-over-year ",
                       "comparisons, and regional summaries \u2014 ready for R, Excel, or Python.")
              )
            )
          )
        ),

        card(
          card_header("Key Terms"),
          card_body(
            tags$dl(
              style = "font-size: 0.95rem; line-height: 1.8;",
              tags$dt("NCD (Non-Communicable Disease)"),
              tags$dd(
                "Chronic conditions not passed from person to person \u2014 including ",
                "heart disease, diabetes, cancer, and chronic respiratory disease. ",
                "Together they account for 74% of all deaths worldwide (WHO)."
              ),
              tags$dt("Physical Inactivity Prevalence"),
              tags$dd(
                "The percentage of adults (18+) who do not meet the WHO\u2019s recommended ",
                "minimum of 150 minutes of moderate-intensity activity per week."
              ),
              tags$dt("Correlation (r)"),
              tags$dd(
                "A number between \u22121 and +1 measuring how strongly two variables ",
                "move together. A positive r (e.g. r = +0.45) means that as GDP rises, ",
                "inactivity tends to rise too. It does ", tags$em("not"), " prove that ",
                "one causes the other."
              ),
              tags$dt("Premature NCD Mortality"),
              tags$dd(
                "The probability (%) of dying between ages 30 and 70 from one of the ",
                "four major NCDs \u2014 the WHO\u2019s SDG 3.4 indicator."
              )
            )
          )
        ),

        tags$div(
          class = "text-center py-3",
          actionButton(
            "go_explorer", "Start Exploring",
            class = "btn-primary btn-lg px-4",
            icon  = icon("arrow-right")
          )
        )
      )
    )
  ),

  # ── Tab 2: Explorer ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Explorer",
    icon  = icon("globe"),

    layout_sidebar(
      sidebar = sidebar(
        width = 310,
        bg    = "#f8f9fa",
        open  = "desktop",

        tags$h6("FILTERS"),

        selectInput("income_filter", "Income group",
                    choices = income_levels, selected = "All"),

        selectInput("sex_filter", "Sex",
                    choices = c("Both", "Male", "Female"), selected = "Both"),

        selectInput("ncd_outcome",
                    tags$span("NCD outcome",
                              tags$small(class = "text-muted fw-normal",
                                         " (chronic disease measure)")),
                    choices = ncd_choices, selected = ncd_choices[1]),

        selectInput("year_filter", "Year",
                    choices = rev(years), selected = max(years)),

        hr(),

        tags$h6("DATA EXPORT"),

        downloadButton("dl_csv", "Download filtered data (CSV)",
                       class = "btn btn-outline-secondary btn-sm w-100")
      ),

      # ── Main content ──────────────────────────────────────────────────────────
      navset_pill(
        id = "explorer_sub",

        # ── Sub-tab: Overview ────────────────────────────────────────────────
        nav_panel(
          title = "Overview",
          icon  = icon("chart-line"),

          tags$div(class = "pt-3",

            layout_columns(
              col_widths = breakpoints(xs = c(12, 12, 12, 12),
                                       sm = c(6,  6,  6,  6),
                                       lg = c(3,  3,  3,  3)),
              class = "mb-3",

              value_box(
                title    = "Countries with data",
                value    = textOutput("n_countries", inline = TRUE),
                showcase = icon("flag"),
                theme    = value_box_theme(bg = "#1A3C5E", fg = "white")
              ),
              value_box(
                title    = "Avg physical inactivity",
                value    = textOutput("avg_inactivity", inline = TRUE),
                showcase = icon("couch"),
                theme    = value_box_theme(bg = "#2E75B6", fg = "white")
              ),
              value_box(
                title    = textOutput("ncd_box_title", inline = TRUE),
                value    = textOutput("avg_ncd", inline = TRUE),
                showcase = icon("heart-pulse"),
                theme    = value_box_theme(bg = "#2A9D8F", fg = "white")
              ),
              value_box(
                title    = tags$span(
                  "GDP-inactivity correlation",
                  tags$span(
                    class = "d-block fw-normal",
                    style = "font-size: 0.65rem; opacity: 0.8; margin-top: 2px;",
                    "r: \u22121 to +1 \u00b7 strength of linear association"
                  )
                ),
                value    = textOutput("gdp_cor", inline = TRUE),
                showcase = icon("arrow-trend-up"),
                theme    = value_box_theme(bg = "#E63946", fg = "white")
              )
            ),

            card(
              card_header("Physical Inactivity vs. GDP per Capita"),
              card_body(
                padding = "0.5rem",
                withSpinner(
                  plotlyOutput("scatter_plot", height = "480px"),
                  type = 6, color = "#2E75B6", size = 0.8
                )
              ),
              card_footer(
                class = "d-flex align-items-center justify-content-between",
                tags$small(
                  class = "text-muted",
                  "Bubble size = selected health outcome (larger = higher burden) \u00b7 ",
                  "Colour = World Bank income group \u00b7 ",
                  tags$strong("Click a point"), " to view country detail"
                ),
                downloadButton("dl_scatter", "",
                               class = "btn btn-sm btn-outline-secondary py-0 px-1",
                               icon = icon("download"))
              )
            ),


            tags$div(
              class = "app-footer",
              "Data: ",
              tags$a("WHO GHO", href = "https://www.who.int/data/gho", target = "_blank"),
              " \u00b7 ",
              tags$a("World Bank", href = "https://data.worldbank.org", target = "_blank"),
              " \u00b7 ",
              tags$a("Our World in Data", href = "https://ourworldindata.org", target = "_blank"),
              " \u00b7 GLHLTH 562 Final Project \u00b7 ",
              tags$a(icon("github"), " Source code",
                     href = "https://github.com/aayange/couch-economy", target = "_blank")
            )
          )
        ),


        # ── Sub-tab: Year-over-Year ─────────────────────────────────────────
        nav_panel(
          title = "Year-over-Year",
          icon  = icon("clock-rotate-left"),

          tags$div(class = "pt-3",

            card(
              card_body(
                class = "py-2",
                layout_columns(
                  col_widths = breakpoints(sm = c(12, 12, 12), md = c(4, 4, 4)),
                  selectInput("year_filter_2", "Compare year",
                              choices = rev(years), selected = min(years)),
                  radioButtons("hist_view", "Show countries",
                               choices = c("Top 10 by change" = "top10",
                                           "Low income" = "Low income",
                                           "Lower middle income" = "Lower middle income",
                                           "Upper middle income" = "Upper middle income",
                                           "High income" = "High income"),
                               selected = "top10"),
                  downloadButton("dl_hist_csv", "Download comparison (CSV)",
                                 class = "btn btn-outline-secondary btn-sm w-100 mt-md-4")
                )
              )
            ),

            card(
              card_header(textOutput("hist_title")),
              card_body(
                padding = "0.5rem",
                withSpinner(
                  plotlyOutput("hist_slope_plot", height = "auto"),
                  type = 6, color = "#2E75B6", size = 0.8
                )
              ),
              card_footer(
                tags$small(
                  class = "text-muted",
                  "Grouped bars show inactivity (%) for each year. ",
                  "Filter by income group to focus on specific country classes."
                )
              )
            ),

            tags$div(
              class = "app-footer",
              "Data: ",
              tags$a("WHO GHO", href = "https://www.who.int/data/gho", target = "_blank"),
              " \u00b7 ",
              tags$a("World Bank", href = "https://data.worldbank.org", target = "_blank"),
              " \u00b7 ",
              tags$a("Our World in Data", href = "https://ourworldindata.org", target = "_blank")
            )
          )
        ),

        # ── Sub-tab: Country Detail ──────────────────────────────────────────
        nav_panel(
          title = "Country Detail",
          icon  = icon("magnifying-glass-chart"),

          tags$div(class = "pt-3",

            card(
              card_body(
                class = "py-2",
                layout_columns(
                  col_widths = breakpoints(sm = c(12, 12, 12), md = c(5, 4, 3)),
                  selectInput("brief_country", "Country",
                              choices = countries, selected = "United States"),
                  radioButtons("brief_tone", "Audience",
                               choices  = c("Policy" = "policy", "General public" = "general"),
                               selected = "policy", inline = TRUE),
                  actionButton("generate_brief", "Generate Brief",
                               class = "btn-primary w-100 mt-md-4",
                               icon  = icon("wand-magic-sparkles"))
                )
              )
            ),

            card(
              card_header(
                class = "d-flex align-items-center justify-content-between",
                textOutput("trend_title"),
                downloadButton("dl_trend", "",
                               class = "btn btn-sm btn-outline-secondary py-0 px-1",
                               icon = icon("download"))
              ),
              card_body(
                padding = "0.5rem",
                withSpinner(
                  plotlyOutput("trend_plot", height = "420px"),
                  type = 6, color = "#E63946", size = 0.8
                )
              ),
              card_footer(
                tags$small(
                  class = "text-muted",
                  "Solid line = physical inactivity \u00b7 Dashed = selected NCD outcome"
                )
              )
            ),

            card(
              card_header(
                class = "d-flex align-items-center justify-content-between",
                tags$span(
                  "Country Brief",
                  tags$span(
                    class = "badge bg-success fw-normal ms-2",
                    icon("robot", class = "me-1"), "Gemini 2.5 Flash Lite"
                  )
                ),
                downloadButton("dl_brief", "",
                               class = "btn btn-sm btn-outline-secondary py-0 px-1",
                               icon = icon("download"))
              ),
              uiOutput("brief_output"),
              min_height = "110px"
            ),

            tags$div(
              class = "app-footer",
              "Data: ",
              tags$a("WHO GHO", href = "https://www.who.int/data/gho", target = "_blank"),
              " \u00b7 ",
              tags$a("World Bank", href = "https://data.worldbank.org", target = "_blank"),
              " \u00b7 ",
              tags$a("Our World in Data", href = "https://ourworldindata.org", target = "_blank")
            )
          )
        )
      )
    )
  ),


  # ── Tab 3: Compare ────────────────────────────────────────────────────────────
  nav_panel(
    title = "Compare",
    icon  = icon("scale-balanced"),

    layout_sidebar(
      sidebar = sidebar(
        width = 310,
        bg    = "#f8f9fa",
        open  = "desktop",

        tags$h6("SELECT COUNTRIES"),

        selectizeInput("compare_countries", "Countries (2\u20133)",
                       choices  = countries,
                       selected = c("United States", "Japan", "Nigeria"),
                       multiple = TRUE,
                       options  = list(maxItems = 3, plugins = list("remove_button"))),

        selectInput("compare_sex", "Sex",
                    choices = c("Both", "Male", "Female"), selected = "Both"),

        selectInput("compare_ncd", "NCD outcome",
                    choices = ncd_choices, selected = ncd_choices[1]),

        hr(),

        tags$h6("COMPARATIVE BRIEF"),

        radioButtons("compare_tone", "Audience",
                     choices  = c("Policy" = "policy", "General public" = "general"),
                     selected = "policy", inline = TRUE),

        actionButton("generate_compare_brief", "Generate Comparison",
                     class = "btn-primary w-100 mt-2",
                     icon  = icon("wand-magic-sparkles")),

        tags$small(
          class = "text-muted d-block mt-2",
          "AI-generated side-by-side analysis of the selected countries."
        )
      ),

      # ── Compare main content ──────────────────────────────────────────────────
      tagList(

        # Snapshot table
        card(
          card_header(
            class = "d-flex align-items-center justify-content-between",
            "Snapshot",
            downloadButton("dl_compare_csv", "",
                           class = "btn btn-sm btn-outline-secondary py-0 px-1",
                           icon = icon("download"))
          ),
          card_body(
            padding = "0.5rem",
            tableOutput("compare_table")
          ),
          card_footer(
            tags$small(class = "text-muted",
                       "Most recent year with data for each country \u00b7 Sex filter applied")
          )
        ),

        # Overlaid trend charts
        layout_columns(
          col_widths = breakpoints(sm = 12, lg = c(6, 6)),
          fill = FALSE,
          class = "mb-3",

          card(
            card_header("Physical Inactivity Over Time"),
            card_body(
              padding = "0.5rem",
              withSpinner(
                plotlyOutput("compare_inactivity_plot", height = "380px"),
                type = 6, color = "#2E75B6", size = 0.8
              )
            )
          ),

          card(
            card_header(textOutput("compare_ncd_title")),
            card_body(
              padding = "0.5rem",
              withSpinner(
                plotlyOutput("compare_ncd_plot", height = "380px"),
                type = 6, color = "#E63946", size = 0.8
              )
            )
          )
        ),

        # Comparison brief
        card(
          card_header(
            class = "d-flex align-items-center justify-content-between",
            tags$span(
              "Comparative Brief",
              tags$span(
                class = "badge bg-success fw-normal ms-2",
                icon("robot", class = "me-1"), "Gemini 2.5 Flash Lite"
              )
            ),
            downloadButton("dl_compare_brief", "",
                           class = "btn btn-sm btn-outline-secondary py-0 px-1",
                           icon = icon("download"))
          ),
          uiOutput("compare_brief_output"),
          min_height = "110px"
        ),

        # Footer
        tags$div(
          class = "app-footer",
          "Data: ",
          tags$a("WHO GHO", href = "https://www.who.int/data/gho", target = "_blank"),
          " \u00b7 ",
          tags$a("World Bank", href = "https://data.worldbank.org", target = "_blank"),
          " \u00b7 ",
          tags$a("Our World in Data", href = "https://ourworldindata.org", target = "_blank")
        )
      )
    )
  ),

  # ── Tab 4: Regions ────────────────────────────────────────────────────────────
  nav_panel(
    title = "Regions",
    icon  = icon("layer-group"),

    layout_sidebar(
      sidebar = sidebar(
        width = 310,
        bg    = "#f8f9fa",
        open  = "desktop",

        tags$h6("FILTERS"),

        selectInput("region_sex", "Sex",
                    choices = c("Both", "Male", "Female"), selected = "Both"),

        selectInput("region_ncd", "NCD outcome",
                    choices = ncd_choices, selected = ncd_choices[1]),

        selectInput("region_year", "Year",
                    choices = rev(years), selected = max(years)),

        tags$hr(),
        tags$h6("TREND CHART"),

        selectizeInput("region_show", "Regions to compare",
                       choices  = sort(unique(df$region[!is.na(df$region)])),
                       selected = sort(unique(df$region[!is.na(df$region)])),
                       multiple = TRUE,
                       options  = list(plugins = list("remove_button"),
                                       placeholder = "Pick regions..."))
      ),

      tagList(

        # Value boxes
        layout_columns(
          col_widths = breakpoints(xs = c(12, 12, 12),
                                   sm = c(4,  4,  4)),
          class = "mb-3",

          value_box(
            title    = "Most inactive region",
            value    = textOutput("region_most_inactive", inline = TRUE),
            showcase = icon("couch"),
            theme    = value_box_theme(bg = "#E63946", fg = "white")
          ),
          value_box(
            title    = "Least inactive region",
            value    = textOutput("region_least_inactive", inline = TRUE),
            showcase = icon("person-running"),
            theme    = value_box_theme(bg = "#2A9D8F", fg = "white")
          ),
          value_box(
            title    = textOutput("region_ncd_box_title", inline = TRUE),
            value    = textOutput("region_highest_ncd", inline = TRUE),
            showcase = icon("heart-pulse"),
            theme    = value_box_theme(bg = "#264653", fg = "white")
          )
        ),

        # Regional scatter: GDP vs Inactivity
        card(
          card_header("Regional Overview: GDP vs. Inactivity"),
          card_body(
            padding = "0.5rem",
            withSpinner(
              plotlyOutput("region_scatter_plot", height = "400px"),
              type = 6, color = "#2E75B6", size = 0.8
            )
          ),
          card_footer(
            tags$small(class = "text-muted",
                       "Each bubble = one World Bank region \u00b7 ",
                       "Size = number of countries \u00b7 ",
                       "Colour = region")
          )
        ),

        # Bar charts side by side
        layout_columns(
          col_widths = breakpoints(sm = 12, lg = c(6, 6)),
          fill = FALSE,
          class = "mb-3",

          card(
            card_header("Average Physical Inactivity by Region"),
            card_body(
              padding = "0.5rem",
              withSpinner(
                plotlyOutput("region_inactivity_bar", height = "350px"),
                type = 6, color = "#2E75B6", size = 0.8
              )
            )
          ),

          card(
            card_header(textOutput("region_ncd_bar_title")),
            card_body(
              padding = "0.5rem",
              withSpinner(
                plotlyOutput("region_ncd_bar", height = "350px"),
                type = 6, color = "#E63946", size = 0.8
              )
            )
          )
        ),

        # Trend chart — dual axis
        card(
          card_header("Regional Trends Over Time"),
          card_body(
            padding = "0.5rem",
            withSpinner(
              plotlyOutput("region_trend_plot", height = "420px"),
              type = 6, color = "#2A9D8F", size = 0.8
            )
          ),
          card_footer(
            tags$small(class = "text-muted",
                       "Population-unweighted regional averages across all countries with data")
          )
        ),

        # Summary table
        card(
          card_header(
            class = "d-flex align-items-center justify-content-between",
            "Regional Summary",
            downloadButton("dl_region_csv", "",
                           class = "btn btn-sm btn-outline-secondary py-0 px-1",
                           icon = icon("download"))
          ),
          card_body(
            padding = "0.5rem",
            tableOutput("region_table")
          ),
          card_footer(
            tags$small(class = "text-muted",
                       "Averages for the selected year and sex filter")
          )
        ),

        tags$div(
          class = "app-footer",
          "Data: ",
          tags$a("WHO GHO", href = "https://www.who.int/data/gho", target = "_blank"),
          " \u00b7 ",
          tags$a("World Bank", href = "https://data.worldbank.org", target = "_blank"),
          " \u00b7 ",
          tags$a("Our World in Data", href = "https://ourworldindata.org", target = "_blank")
        )
      )
    )
  ),

  # ── Tab 5: About ─────────────────────────────────────────────────────────────
  nav_panel(
    title = "About",
    icon  = icon("circle-info"),

    layout_columns(
      col_widths = breakpoints(sm = 12, md = 10, lg = 8),
      card(
        card_header("About This Product"),
        card_body(
          tags$h5("The Paradox"),
          tags$p(
            "The world's wealthiest countries are often its most physically inactive. ",
            "High incomes bring sedentary jobs, car-dependent cities, and leisure screen time. ",
            "The result: chronic diseases that cost lives and health systems trillions. ",
            tags$em("The Couch Economy"), " makes this paradox visible and explorable."
          ),
          tags$h5("Data Sources"),
          tags$ul(
            tags$li(tags$strong("WHO GHO (NCD_PAC):"),
                    " Physical inactivity prevalence, adults 18+, age-standardised — 195 countries, 2000–2022"),
            tags$li(tags$strong("WHO GHO (NCD_DIABETES_PREVALENCE_AGESTD):"),
                    " Age-standardised diabetes prevalence, sex-stratified, annual"),
            tags$li(tags$strong("WHO GHO (NCDMORT3070):"),
                    " Probability (%) of dying ages 30–70 from CVD, cancer, diabetes, or CRD — the WHO SDG 3.4 indicator"),
            tags$li(tags$strong("World Bank (NY.GDP.PCAP.CD, SI.POV.GINI):"),
                    " GDP per capita and Gini inequality index via wbstats"),
            tags$li(tags$strong("Our World in Data:"),
                    " Obesity prevalence (%)")
          ),
          tags$h5("Features"),
          tags$ul(
            tags$li(tags$strong("Explorer (Overview):"),
                    " Interactive scatter plot of GDP vs. physical inactivity across 195 countries, ",
                    "with value boxes, income-group colour coding, and CSV data export."),
            tags$li(tags$strong("Explorer (Year-over-Year):"),
                    " Compare two time points with grouped bar charts \u2014 filter by income group ",
                    "or view the top 10 countries with the largest changes."),
            tags$li(tags$strong("Explorer (Country Detail):"),
                    " Country-level trend charts and AI-generated health briefs ",
                    "(policy or general audience) powered by Google Gemini."),
            tags$li(tags$strong("Compare:"),
                    " Select 2\u20133 countries for head-to-head trend charts, a data snapshot table, ",
                    "NCD comparison, and an AI-written comparative brief with CSV export."),
            tags$li(tags$strong("Regions:"),
                    " Aggregated views by World Bank region \u2014 value boxes, GDP-vs-inactivity scatter, ",
                    "side-by-side bar charts, dual-axis trend lines, a summary table, and CSV export. ",
                    "Multi-select filter to control which regions appear on the trend chart.")
          ),
          tags$h5("Pipeline"),
          tags$p(
            "Data is fetched from five APIs, joined on ISO-3 country code \u00d7 year \u00d7 sex, ",
            "and saved as a single processed file. ",
            "A GitHub Actions workflow refreshes the data every Monday at 06:00 UTC ",
            "and redeploys the app to shinyapps.io automatically."
          ),
          tags$p(
            "Country briefs are generated on demand by the Google Gemini API ",
            "(gemini-2.5-flash-lite) and cached by country + data hash + audience tone."
          ),
          tags$h5("Course"),
          tags$p("GLHLTH 562: Data Science and Visualization with R \u2014 Final Project, Spring 2026"),
          tags$h5("Source Code"),
          tags$p(
            tags$a("github.com/aayange/couch-economy",
                   href = "https://github.com/aayange/couch-economy",
                   target = "_blank")
          )
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Navigate to Explorer from landing page ────────────────────────────────

  observeEvent(input$go_explorer, {
    nav_select("main_nav", selected = "Explorer")
  })

  # ── Filtered data ──────────────────────────────────────────────────────────

  filtered <- reactive({
    d <- df |>
      filter(sex == input$sex_filter, year == as.numeric(input$year_filter))
    if (input$income_filter != "All")
      d <- d |> filter(income_level == input$income_filter)
    d |> filter(!is.na(inactivity_pct), !is.na(gdp_per_capita), !is.na(income_level))
  })

  # ── Value boxes ───────────────────────────────────────────────────────────

  output$n_countries <- renderText(n_distinct(filtered()$iso3))

  output$avg_inactivity <- renderText({
    paste0(round(mean(filtered()$inactivity_pct, na.rm = TRUE), 1), "%")
  })

  output$ncd_box_title <- renderText({
    paste0("Avg ", ncd_label_for(input$ncd_outcome))
  })

  output$avg_ncd <- renderText({
    ncd_col <- input$ncd_outcome
    d <- filtered()
    if (!ncd_col %in% names(d)) return("\u2014")
    val <- mean(d[[ncd_col]], na.rm = TRUE)
    if (is.nan(val)) "\u2014" else paste0(round(val, 1), "%")
  })

  output$gdp_cor <- renderText({
    d <- filtered()
    if (nrow(d) < 3) return("\u2014")
    r <- cor(log10(d$gdp_per_capita), d$inactivity_pct, use = "complete.obs")
    if (is.na(r)) return("\u2014")
    strength <- if (abs(r) < 0.2) "weak"
                else if (abs(r) < 0.5) "moderate"
                else if (abs(r) < 0.7) "strong"
                else "very strong"
    direction <- if (r > 0) "positive" else "negative"
    paste0("r = ", sprintf("%+.2f", r), " (", strength, " ", direction, ")")
  })

  # ── Scatter plot ──────────────────────────────────────────────────────────

  scatter_gg <- reactive({
    d         <- filtered()
    ncd_col   <- input$ncd_outcome
    ncd_label <- ncd_label_for(ncd_col)

    validate(
      need(ncd_col %in% names(d), paste("Column", ncd_col, "not available.")),
      need(nrow(d) > 0, "No data for current filters.")
    )

    d_ncd <- d |> filter(!is.na(.data[[ncd_col]]))

    validate(need(nrow(d_ncd) > 0,
      paste0("No ", ncd_label, " data for the selected year. Try a different year.")))

    d_ncd |>
      ggplot(aes(
        x     = gdp_per_capita,
        y     = inactivity_pct,
        color = income_level,
        size  = .data[[ncd_col]],
        text  = glue(
          "<b>{country_name}</b><br>",
          "GDP: {label_dollar()(gdp_per_capita)}<br>",
          "Inactivity: {inactivity_pct}%<br>",
          "{ncd_label}: {round(.data[[ncd_col]], 1)}%"
        )
      )) +
      geom_point(alpha = 0.72) +
      geom_smooth(aes(group = 1), method = "lm", formula = y ~ x,
                  color = "#495057", linewidth = 0.7, linetype = "dashed",
                  se = FALSE, inherit.aes = TRUE, show.legend = FALSE) +
      scale_x_log10(labels = label_dollar(scale_cut = cut_short_scale())) +
      scale_color_manual(values = income_colors, na.value = "grey70",
                         name = NULL) +
      scale_size(range = c(2, 11), guide = "none") +
      labs(x = "GDP per capita (log scale)", y = "Physical inactivity (%)",
           title = paste("Physical Inactivity vs. GDP per Capita \u2014", input$year_filter)) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        axis.title       = element_text(size = 11, color = "#495057"),
        legend.text      = element_text(size = 10),
        plot.background  = element_rect(fill = "white", colour = NA)
      )
  })

  output$scatter_plot <- renderPlotly({
    p <- scatter_gg()
    ggplotly(p + labs(title = NULL), tooltip = "text") |>
      layout(
        legend = list(orientation = "h", y = -0.2, font = list(size = 10)),
        margin = list(b = 60, t = 10, l = 10, r = 10),
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent"
      ) |>
      config(displayModeBar = FALSE)
  })

  # ── Selected country ─────────────────────────────────────────────────────

  selected_country <- reactiveVal(NULL)

  observeEvent(input$brief_country, {
    selected_country(NULL)
  })

  observeEvent(event_data("plotly_click"), {
    click <- event_data("plotly_click")
    if (!is.null(click)) {
      d   <- filtered()
      idx <- which.min(abs(log10(d$gdp_per_capita) - click$x) +
                       abs(d$inactivity_pct - click$y))
      selected_country(d$country_name[idx])
      updateSelectInput(session, "brief_country", selected = d$country_name[idx])
      nav_select("explorer_sub", selected = "Country Detail")
    }
  })

  # ── Trend chart ───────────────────────────────────────────────────────────

  output$trend_title <- renderText({
    country <- if (!is.null(selected_country())) selected_country() else input$brief_country
    paste("Trend \u2014", country)
  })

  trend_data <- reactive({
    country <- if (!is.null(selected_country())) selected_country() else input$brief_country
    d <- df |>
      filter(country_name == country, sex == input$sex_filter) |>
      arrange(year)
    list(data = d, country = country)
  })

  output$trend_plot <- renderPlotly({
    td        <- trend_data()
    d         <- td$data
    ncd_col   <- input$ncd_outcome
    ncd_label <- ncd_label_for(ncd_col)

    validate(
      need(nrow(d) > 0, "No data for selected country."),
      need(ncd_col %in% names(d), paste("Column", ncd_col, "not available."))
    )

    # Split into non-NA subsets for each series
    d_inact <- d |> filter(!is.na(inactivity_pct))
    d_ncd   <- d |> filter(!is.na(.data[[ncd_col]]))

    plot_ly() |>
      add_lines(data = d_inact, x = ~year, y = ~inactivity_pct,
                name = "Inactivity (%)", yaxis = "y",
                line = list(color = "#2E75B6", width = 2.5),
                hovertemplate = paste0(
                  "<b>%{x}</b><br>Inactivity: %{y:.1f}%<extra></extra>"
                )) |>
      add_markers(data = d_inact, x = ~year, y = ~inactivity_pct,
                  showlegend = FALSE, yaxis = "y",
                  marker = list(color = "#2E75B6", size = 7),
                  hoverinfo = "skip") |>
      add_lines(data = d_ncd, x = ~year, y = as.formula(paste0("~", ncd_col)),
                name = ncd_label, yaxis = "y2",
                line = list(color = "#E63946", width = 2.5, dash = "dash"),
                hovertemplate = paste0(
                  "<b>%{x}</b><br>", ncd_label, ": %{y:.1f}%<extra></extra>"
                )) |>
      add_markers(data = d_ncd, x = ~year, y = as.formula(paste0("~", ncd_col)),
                  showlegend = FALSE, yaxis = "y2",
                  marker = list(color = "#E63946", size = 6, symbol = "triangle-up"),
                  hoverinfo = "skip") |>
      layout(
        xaxis = list(title = "", gridcolor = "#e9ecef", zeroline = FALSE),
        yaxis = list(
          title = list(text = "Physical inactivity (%)",
                       font = list(size = 12, color = "#2E75B6")),
          gridcolor = "#e9ecef", zeroline = FALSE,
          tickfont = list(color = "#2E75B6")
        ),
        yaxis2 = list(
          title = list(text = ncd_label,
                       font = list(size = 12, color = "#E63946")),
          overlaying = "y", side = "right", zeroline = FALSE,
          tickfont = list(color = "#E63946"),
          showgrid = FALSE
        ),
        legend = list(orientation = "h", y = -0.2, font = list(size = 11)),
        margin = list(b = 60, t = 10, l = 60, r = 60),
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent",
        hovermode = "x unified"
      ) |>
      config(displayModeBar = FALSE)
  })

  # ── Country brief ─────────────────────────────────────────────────────────

  brief_text <- eventReactive(input$generate_brief, {
    country      <- input$brief_country
    tone         <- input$brief_tone
    sex_filter   <- input$sex_filter
    ncd_outcome  <- input$ncd_outcome
    api_key      <- Sys.getenv("GEMINI_API_KEY")
    country_data <- df |> filter(country_name == country)

    validate(need(nrow(country_data) > 0, paste("No data available for", country)))

    withProgress(message = paste("Generating brief for", country, "..."), value = 0.5, {
      tryCatch(
        generate_country_brief(country_data, tone = tone,
                               sex_filter = sex_filter,
                               ncd_outcome = ncd_outcome,
                               api_key = api_key),
        error = function(e) paste("Error generating brief:", e$message)
      )
    })
  })

  format_brief_html <- function(txt) {
    txt <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", txt)
    paragraphs <- strsplit(txt, "\n{2,}")[[1]]
    paragraphs <- trimws(paragraphs)
    paragraphs <- paragraphs[nchar(paragraphs) > 0]
    paste0("<p style='line-height:1.78; font-size:0.95em; margin-bottom:0.8em;'>",
           gsub("\n", "<br>", paragraphs), "</p>", collapse = "\n")
  }

  output$brief_output <- renderUI({
    if (is.null(brief_text()) || brief_text() == "") {
      tags$p(
        class = "text-muted p-3 mb-0",
        icon("circle-info", class = "me-1"),
        'Select a country and click \u201cGenerate Brief\u201d for an AI-written health analysis.'
      )
    } else {
      tags$div(
        class = "p-3",
        HTML(format_brief_html(brief_text())),
        tags$hr(class = "my-2"),
        tags$small(
          class = "text-muted",
          icon("triangle-exclamation", class = "me-1"),
          "AI-generated \u2014 always verify against primary sources."
        )
      )
    }
  })

  # ── Downloads ─────────────────────────────────────────────────────────────

  output$dl_scatter <- downloadHandler(
    filename = function() {
      paste0("scatter_", input$year_filter, "_", input$sex_filter, ".png")
    },
    content = function(file) {
      ggsave(file, plot = scatter_gg(), width = 10, height = 6, dpi = 300, bg = "white")
    }
  )

  output$dl_trend <- downloadHandler(
    filename = function() {
      td <- trend_data()
      paste0("trend_", gsub(" ", "_", td$country), ".png")
    },
    content = function(file) {
      td        <- trend_data()
      d         <- td$data
      country   <- td$country
      ncd_col   <- input$ncd_outcome
      ncd_label <- ncd_label_for(ncd_col)

      d_long <- d |>
        filter(!is.na(inactivity_pct) | !is.na(.data[[ncd_col]])) |>
        tidyr::pivot_longer(
          cols = c(inactivity_pct, all_of(ncd_col)),
          names_to = "metric", values_to = "value"
        ) |>
        filter(!is.na(value)) |>
        mutate(metric = ifelse(metric == "inactivity_pct", "Inactivity (%)", ncd_label))

      p <- ggplot(d_long, aes(x = year, y = value, color = metric, linetype = metric)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2.5) +
        scale_color_manual(values = c("Inactivity (%)" = "#2E75B6", setNames("#E63946", ncd_label))) +
        scale_linetype_manual(values = c("Inactivity (%)" = "solid", setNames("dashed", ncd_label))) +
        labs(x = NULL, y = "%", color = NULL, linetype = NULL,
             title = paste("Trend \u2014", country)) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", colour = NA)
        )

      ggsave(file, plot = p, width = 8, height = 5, dpi = 300, bg = "white")
    }
  )

  output$dl_brief <- downloadHandler(
    filename = function() {
      paste0("brief_", gsub(" ", "_", input$brief_country), "_", input$brief_tone, ".txt")
    },
    content = function(file) {
      txt <- brief_text()
      if (is.null(txt) || txt == "") txt <- "No brief generated yet."
      writeLines(txt, file)
    }
  )

  # ── Compare tab ────────────────────────────────────────────────────────────

  compare_colors <- c("#2E75B6", "#E63946", "#2A9D8F")

  compare_data <- reactive({
    req(length(input$compare_countries) >= 2)
    df |>
      filter(country_name %in% input$compare_countries,
             sex == input$compare_sex)
  })

  output$compare_table <- renderTable({
    req(length(input$compare_countries) >= 2)
    ncd_col   <- input$compare_ncd
    ncd_label <- ncd_label_for(ncd_col)

    d <- compare_data() |>
      filter(!is.na(inactivity_pct), !is.na(gdp_per_capita)) |>
      group_by(country_name) |>
      slice_max(year, n = 1) |>
      ungroup() |>
      transmute(
        Country          = country_name,
        `Income Group`   = as.character(income_level),
        Year             = as.integer(year),
        `GDP per Capita` = scales::dollar(gdp_per_capita, accuracy = 1),
        `Inactivity (%)`  = round(inactivity_pct, 1),
        !!ncd_label      := if (ncd_col %in% names(df))
                              round(.data[[ncd_col]], 1) else NA_real_
      )

    d
  }, striped = TRUE, hover = TRUE, spacing = "s", width = "100%", na = "\u2014")

  output$compare_ncd_title <- renderText({
    ncd_label_for(input$compare_ncd)
  })

  output$compare_inactivity_plot <- renderPlotly({
    d <- compare_data() |>
      filter(!is.na(inactivity_pct))

    validate(need(nrow(d) > 0, "No inactivity data for selected countries."))

    cnames <- unique(d$country_name)
    colors <- setNames(compare_colors[seq_along(cnames)], cnames)

    p <- plot_ly()
    for (i in seq_along(cnames)) {
      cn <- cnames[i]
      dd <- d |> filter(country_name == cn) |> arrange(year)
      p <- p |>
        add_lines(data = dd, x = ~year, y = ~inactivity_pct,
                  name = cn, line = list(color = colors[cn], width = 2.5),
                  hovertemplate = paste0("<b>", cn, "</b><br>%{x}: %{y:.1f}%<extra></extra>")) |>
        add_markers(data = dd, x = ~year, y = ~inactivity_pct,
                    showlegend = FALSE,
                    marker = list(color = colors[cn], size = 6),
                    hoverinfo = "skip")
    }

    p |> layout(
      xaxis = list(title = "", gridcolor = "#e9ecef", zeroline = FALSE),
      yaxis = list(title = list(text = "Physical inactivity (%)",
                                font = list(size = 12)),
                   gridcolor = "#e9ecef", zeroline = FALSE),
      legend = list(orientation = "h", y = -0.2, font = list(size = 11)),
      margin = list(b = 60, t = 10, l = 60, r = 10),
      paper_bgcolor = "transparent", plot_bgcolor = "transparent",
      hovermode = "x unified"
    ) |> config(displayModeBar = FALSE)
  })

  output$compare_ncd_plot <- renderPlotly({
    ncd_col   <- input$compare_ncd
    ncd_label <- ncd_label_for(ncd_col)

    d <- compare_data() |> filter(!is.na(.data[[ncd_col]]))
    validate(need(nrow(d) > 0, paste0("No ", ncd_label, " data for selected countries.")))

    cnames <- unique(d$country_name)
    colors <- setNames(compare_colors[seq_along(cnames)], cnames)

    p <- plot_ly()
    for (i in seq_along(cnames)) {
      cn <- cnames[i]
      dd <- d |> filter(country_name == cn) |> arrange(year)
      p <- p |>
        add_lines(data = dd, x = ~year, y = as.formula(paste0("~", ncd_col)),
                  name = cn, line = list(color = colors[cn], width = 2.5, dash = "dash"),
                  hovertemplate = paste0("<b>", cn, "</b><br>%{x}: %{y:.1f}%<extra></extra>")) |>
        add_markers(data = dd, x = ~year, y = as.formula(paste0("~", ncd_col)),
                    showlegend = FALSE,
                    marker = list(color = colors[cn], size = 6, symbol = "triangle-up"),
                    hoverinfo = "skip")
    }

    p |> layout(
      xaxis = list(title = "", gridcolor = "#e9ecef", zeroline = FALSE),
      yaxis = list(title = list(text = ncd_label, font = list(size = 12)),
                   gridcolor = "#e9ecef", zeroline = FALSE),
      legend = list(orientation = "h", y = -0.2, font = list(size = 11)),
      margin = list(b = 60, t = 10, l = 60, r = 10),
      paper_bgcolor = "transparent", plot_bgcolor = "transparent",
      hovermode = "x unified"
    ) |> config(displayModeBar = FALSE)
  })

  # ── Comparison brief ─────────────────────────────────────────────────────

  compare_brief_text <- eventReactive(input$generate_compare_brief, {
    req(length(input$compare_countries) >= 2)
    api_key      <- Sys.getenv("GEMINI_API_KEY")
    country_data <- df |> filter(country_name %in% input$compare_countries)

    validate(need(nrow(country_data) > 0, "No data available for selected countries."))

    withProgress(message = "Generating comparison brief...", value = 0.5, {
      tryCatch(
        generate_comparison_brief(country_data, tone = input$compare_tone,
                                  sex_filter = input$compare_sex,
                                  ncd_outcome = input$compare_ncd,
                                  api_key = api_key),
        error = function(e) paste("Error generating brief:", e$message)
      )
    })
  })

  output$compare_brief_output <- renderUI({
    if (is.null(compare_brief_text()) || compare_brief_text() == "") {
      tags$p(
        class = "text-muted p-3 mb-0",
        icon("circle-info", class = "me-1"),
        'Select 2\u20133 countries and click \u201cGenerate Comparison\u201d for an AI-written analysis.'
      )
    } else {
      tags$div(
        class = "p-3",
        HTML(format_brief_html(compare_brief_text())),
        tags$hr(class = "my-2"),
        tags$small(
          class = "text-muted",
          icon("triangle-exclamation", class = "me-1"),
          "AI-generated \u2014 always verify against primary sources."
        )
      )
    }
  })

  output$dl_compare_brief <- downloadHandler(
    filename = function() {
      cnames <- paste(gsub(" ", "_", input$compare_countries), collapse = "_vs_")
      paste0("compare_", cnames, "_", input$compare_tone, ".txt")
    },
    content = function(file) {
      txt <- compare_brief_text()
      if (is.null(txt) || txt == "") txt <- "No comparison brief generated yet."
      writeLines(txt, file)
    }
  )

  # ── CSV export of filtered data ─────────────────────────────────────────────

  output$dl_compare_csv <- downloadHandler(
    filename = function() {
      cnames <- paste(gsub(" ", "_", input$compare_countries), collapse = "_vs_")
      paste0("compare_", cnames, "_", input$compare_sex, ".csv")
    },
    content = function(file) {
      d <- compare_data() |>
        select(country_name, iso3, region, income_level, year, sex,
               gdp_per_capita, inactivity_pct, any_of(unname(ncd_choices)))
      readr::write_csv(d, file)
    }
  )

  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0("couch_economy_", input$year_filter, "_", input$sex_filter, ".csv")
    },
    content = function(file) {
      d <- filtered() |>
        select(country_name, iso3, region, income_level, year, sex,
               gdp_per_capita, inactivity_pct, any_of(unname(ncd_choices)))
      readr::write_csv(d, file)
    }
  )

  # ── Historical comparison ───────────────────────────────────────────────────

  filtered_year2 <- reactive({
    req(input$year_filter_2)
    d <- df |>
      filter(sex == input$sex_filter, year == as.numeric(input$year_filter_2))
    if (input$income_filter != "All")
      d <- d |> filter(income_level == input$income_filter)
    d |> filter(!is.na(inactivity_pct), !is.na(gdp_per_capita), !is.na(income_level))
  })

  output$hist_title <- renderText({
    paste0("Historical Comparison: ", input$year_filter, " vs. ", input$year_filter_2)
  })

  output$hist_slope_plot <- renderPlotly({
    req(input$year_filter_2)
    validate(need(input$year_filter != input$year_filter_2,
                  "Select two different years to compare."))

    y1 <- as.numeric(input$year_filter)
    y2 <- as.numeric(input$year_filter_2)

    d1 <- filtered() |> select(country_name, income_level, inactivity_y1 = inactivity_pct)
    d2 <- filtered_year2() |> select(country_name, inactivity_y2 = inactivity_pct)

    change <- inner_join(d1, d2, by = "country_name")

    if (input$hist_view == "top10") {
      change <- change |> slice_max(abs(inactivity_y1 - inactivity_y2), n = 10)
    } else {
      change <- change |> filter(income_level == input$hist_view)
    }

    change <- change |>
      arrange(inactivity_y1) |>
      mutate(country_name = factor(country_name, levels = country_name))

    validate(need(nrow(change) > 0, "No countries match the current filters."))

    chart_height <- max(450, nrow(change) * 35 + 80)

    plot_ly(change, y = ~country_name, height = chart_height) |>
      add_bars(x = ~inactivity_y2, name = as.character(y2),
               marker = list(color = "#2E75B6"),
               hovertemplate = paste0("<b>%{y}</b><br>", y2, ": %{x:.1f}%<extra></extra>")) |>
      add_bars(x = ~inactivity_y1, name = as.character(y1),
               marker = list(color = "#E9C46A"),
               hovertemplate = paste0("<b>%{y}</b><br>", y1, ": %{x:.1f}%<extra></extra>")) |>
      layout(
        barmode = "group",
        xaxis = list(
          title = list(text = "Physical inactivity (%)", font = list(size = 12)),
          gridcolor = "#e9ecef", zeroline = FALSE
        ),
        yaxis = list(title = "", tickfont = list(size = 11)),
        legend = list(orientation = "h", y = 1.04, x = 0.5, xanchor = "center",
                      font = list(size = 11)),
        margin = list(l = 140, r = 20, t = 30, b = 50),
        paper_bgcolor = "transparent", plot_bgcolor = "transparent"
      ) |> config(displayModeBar = FALSE)
  })

  output$dl_hist_csv <- downloadHandler(
    filename = function() {
      paste0("historical_comparison_", input$year_filter, "_vs_", input$year_filter_2, ".csv")
    },
    content = function(file) {
      y1 <- as.numeric(input$year_filter)
      y2 <- as.numeric(input$year_filter_2)
      d1 <- filtered() |> select(country_name, iso3, region, income_level,
                                  gdp_per_capita, inactivity_pct) |>
        mutate(year = y1)
      d2 <- filtered_year2() |> select(country_name, iso3, region, income_level,
                                        gdp_per_capita, inactivity_pct) |>
        mutate(year = y2)
      readr::write_csv(bind_rows(d1, d2), file)
    }
  )

  # ── Regional aggregation ─────────────────────────────────────────────────────

  region_data <- reactive({
    df |>
      filter(sex == input$region_sex,
             year == as.numeric(input$region_year),
             !is.na(region))
  })

  region_summary <- reactive({
    ncd_col   <- input$region_ncd
    ncd_label <- ncd_label_for(ncd_col)

    region_data() |>
      group_by(region) |>
      summarise(
        n_countries    = n_distinct(iso3),
        avg_gdp        = mean(gdp_per_capita, na.rm = TRUE),
        avg_inactivity = mean(inactivity_pct, na.rm = TRUE),
        avg_ncd        = mean(.data[[ncd_col]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(avg_inactivity))
  })

  region_colors <- c(
    "East Asia & Pacific"        = "#264653",
    "Europe & Central Asia"      = "#2A9D8F",
    "Latin America & Caribbean"  = "#E9C46A",
    "Middle East & North Africa" = "#F4A261",
    "North America"              = "#E63946",
    "South Asia"                 = "#2E75B6",
    "Sub-Saharan Africa"         = "#6A4C93"
  )

  output$region_most_inactive <- renderText({
    d <- region_summary()
    if (nrow(d) == 0) return("—")
    top <- d |> slice_max(avg_inactivity, n = 1)
    paste0(top$region, " (", round(top$avg_inactivity, 1), "%)")
  })

  output$region_least_inactive <- renderText({
    d <- region_summary()
    if (nrow(d) == 0) return("—")
    bot <- d |> slice_min(avg_inactivity, n = 1)
    paste0(bot$region, " (", round(bot$avg_inactivity, 1), "%)")
  })

  output$region_ncd_box_title <- renderText({
    paste0("Highest ", ncd_label_for(input$region_ncd))
  })

  output$region_highest_ncd <- renderText({
    d <- region_summary()
    if (nrow(d) == 0 || all(is.nan(d$avg_ncd))) return("—")
    top <- d |> filter(!is.nan(avg_ncd)) |> slice_max(avg_ncd, n = 1)
    paste0(top$region, " (", round(top$avg_ncd, 1), "%)")
  })

  output$region_scatter_plot <- renderPlotly({
    d <- region_summary() |> filter(!is.nan(avg_gdp), !is.nan(avg_inactivity))
    validate(need(nrow(d) > 0, "No data for selected filters."))

    plot_ly(d, x = ~avg_gdp, y = ~avg_inactivity, type = "scatter", mode = "markers+text",
            marker = list(
              size = ~sqrt(n_countries) * 12,
              color = region_colors[as.character(d$region)],
              opacity = 0.8,
              line = list(color = "white", width = 1.5)
            ),
            text = ~region,
            textposition = "top center",
            textfont = list(size = 10),
            hovertemplate = paste0(
              "<b>%{text}</b><br>",
              "Avg GDP/capita: %{x:$,.0f}<br>",
              "Avg inactivity: %{y:.1f}%<br>",
              "Countries: %{customdata}",
              "<extra></extra>"
            ),
            customdata = ~n_countries) |>
      layout(
        xaxis = list(
          title = list(text = "Avg GDP per capita ($)", font = list(size = 12)),
          gridcolor = "#e9ecef", zeroline = FALSE,
          type = "log"
        ),
        yaxis = list(
          title = list(text = "Avg physical inactivity (%)", font = list(size = 12)),
          gridcolor = "#e9ecef", zeroline = FALSE
        ),
        margin = list(l = 60, r = 20, t = 40, b = 50),
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        showlegend = FALSE
      ) |> config(displayModeBar = FALSE)
  })

  output$region_inactivity_bar <- renderPlotly({
    d <- region_summary() |> arrange(avg_inactivity) |>
      mutate(region = factor(region, levels = region))

    validate(need(nrow(d) > 0, "No data for selected filters."))

    plot_ly(d, y = ~region, x = ~avg_inactivity, type = "bar",
            orientation = "h",
            marker = list(color = region_colors[as.character(d$region)]),
            text = ~paste0(round(avg_inactivity, 1), "%"),
            textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Avg inactivity: %{x:.1f}%<br>Countries: %{customdata}<extra></extra>",
            customdata = ~n_countries) |>
      layout(
        xaxis = list(title = list(text = "Avg physical inactivity (%)", font = list(size = 12)),
                     gridcolor = "#e9ecef", zeroline = FALSE),
        yaxis = list(title = "", tickfont = list(size = 11)),
        margin = list(l = 180, r = 40, t = 10, b = 50),
        paper_bgcolor = "transparent", plot_bgcolor = "transparent"
      ) |> config(displayModeBar = FALSE)
  })

  output$region_ncd_bar_title <- renderText({
    paste0("Average ", ncd_label_for(input$region_ncd), " by Region")
  })

  output$region_ncd_bar <- renderPlotly({
    ncd_col   <- input$region_ncd
    ncd_label <- ncd_label_for(ncd_col)

    d <- region_summary() |> arrange(avg_ncd) |>
      mutate(region = factor(region, levels = region))

    validate(need(nrow(d) > 0 && any(!is.nan(d$avg_ncd)),
                  paste0("No ", ncd_label, " data for selected filters.")))

    plot_ly(d, y = ~region, x = ~avg_ncd, type = "bar",
            orientation = "h",
            marker = list(color = region_colors[as.character(d$region)]),
            text = ~paste0(round(avg_ncd, 1), "%"),
            textposition = "outside",
            hovertemplate = paste0("<b>%{y}</b><br>Avg ", ncd_label, ": %{x:.1f}%<extra></extra>")) |>
      layout(
        xaxis = list(title = list(text = paste0("Avg ", ncd_label), font = list(size = 12)),
                     gridcolor = "#e9ecef", zeroline = FALSE),
        yaxis = list(title = "", tickfont = list(size = 11)),
        margin = list(l = 180, r = 40, t = 10, b = 50),
        paper_bgcolor = "transparent", plot_bgcolor = "transparent"
      ) |> config(displayModeBar = FALSE)
  })

  output$region_trend_plot <- renderPlotly({
    ncd_col   <- input$region_ncd
    ncd_label <- ncd_label_for(ncd_col)
    req(input$region_show)

    d <- df |>
      filter(sex == input$region_sex, !is.na(region),
             region %in% input$region_show) |>
      group_by(region, year) |>
      summarise(
        avg_inactivity = mean(inactivity_pct, na.rm = TRUE),
        avg_ncd = mean(.data[[ncd_col]], na.rm = TRUE),
        .groups = "drop"
      )

    validate(need(nrow(d) > 0, "No trend data available."))

    regions <- unique(d$region)

    p <- plot_ly()
    for (r in regions) {
      dd <- d |> filter(region == r, !is.nan(avg_inactivity)) |> arrange(year)
      p <- p |>
        add_lines(data = dd, x = ~year, y = ~avg_inactivity,
                  name = r, legendgroup = r,
                  line = list(color = region_colors[r], width = 2.5),
                  hovertemplate = paste0("<b>", r, "</b><br>Inactivity: %{y:.1f}%<extra></extra>")) |>
        add_markers(data = dd, x = ~year, y = ~avg_inactivity,
                    showlegend = FALSE, legendgroup = r,
                    marker = list(color = region_colors[r], size = 5),
                    hoverinfo = "skip")

      dd_ncd <- d |> filter(region == r, !is.nan(avg_ncd)) |> arrange(year)
      if (nrow(dd_ncd) > 0) {
        p <- p |>
          add_lines(data = dd_ncd, x = ~year, y = ~avg_ncd,
                    name = r, legendgroup = r, showlegend = FALSE,
                    yaxis = "y2",
                    line = list(color = region_colors[r], width = 1.5, dash = "dash"),
                    hovertemplate = paste0("<b>", r, "</b><br>", ncd_label, ": %{y:.1f}%<extra></extra>"))
      }
    }

    p |> layout(
      xaxis = list(title = "", gridcolor = "#e9ecef", zeroline = FALSE),
      yaxis = list(
        title = list(text = "Avg physical inactivity (%)",
                     font = list(size = 12, color = "#495057")),
        gridcolor = "#e9ecef", zeroline = FALSE
      ),
      yaxis2 = list(
        title = list(text = paste0("Avg ", ncd_label),
                     font = list(size = 12, color = "#adb5bd")),
        overlaying = "y", side = "right", zeroline = FALSE,
        showgrid = FALSE
      ),
      legend = list(orientation = "h", y = -0.2, font = list(size = 10)),
      margin = list(b = 80, t = 10, l = 60, r = 80),
      paper_bgcolor = "transparent", plot_bgcolor = "transparent",
      hovermode = "x unified"
    ) |> config(displayModeBar = FALSE)
  })

  output$region_table <- renderTable({
    ncd_col   <- input$region_ncd
    ncd_label <- ncd_label_for(ncd_col)

    region_summary() |>
      transmute(
        Region             = region,
        Countries          = as.integer(n_countries),
        `Avg GDP/Capita`   = scales::dollar(avg_gdp, accuracy = 1),
        `Avg Inactivity (%)` = round(avg_inactivity, 1),
        !!ncd_label        := round(avg_ncd, 1)
      )
  }, striped = TRUE, hover = TRUE, spacing = "s", width = "100%", na = "—")

  output$dl_region_csv <- downloadHandler(
    filename = function() {
      paste0("regional_summary_", input$region_year, "_", input$region_sex, ".csv")
    },
    content = function(file) {
      readr::write_csv(region_summary(), file)
    }
  )
}

# ── Run ───────────────────────────────────────────────────────────────────────

shinyApp(ui, server)
