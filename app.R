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

  # ── Tab 1: Explorer ──────────────────────────────────────────────────────────
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

        selectInput("ncd_outcome", "NCD outcome",
                    choices = ncd_choices, selected = ncd_choices[1]),

        sliderInput("year_filter", "Year",
                    min = min(years), max = max(years),
                    value = max(years), step = 1, sep = ""),

        hr(),

        tags$h6("COUNTRY BRIEF"),

        selectInput("brief_country", "Select a country",
                    choices = countries, selected = "United States"),

        radioButtons("brief_tone", "Audience",
                     choices  = c("Policy" = "policy", "General public" = "general"),
                     selected = "policy", inline = TRUE),

        actionButton("generate_brief", "Generate Brief",
                     class = "btn-primary w-100 mt-2",
                     icon  = icon("wand-magic-sparkles")),

        tags$small(
          class = "text-muted d-block mt-2",
          "Cached — first call hits the Gemini API, repeats are instant."
        )
      ),

      # ── Main content ──────────────────────────────────────────────────────────
      tagList(

        # Row 1: Four value boxes — responsive (1 col → 2 col → 4 col)
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
            showcase = icon("person-walking"),
            theme    = value_box_theme(bg = "#2E75B6", fg = "white")
          ),
          value_box(
            title    = textOutput("ncd_box_title", inline = TRUE),
            value    = textOutput("avg_ncd", inline = TRUE),
            showcase = icon("heart-pulse"),
            theme    = value_box_theme(bg = "#2A9D8F", fg = "white")
          ),
          value_box(
            title    = "Most inactive country",
            value    = textOutput("most_inactive", inline = TRUE),
            showcase = icon("couch"),
            theme    = value_box_theme(bg = "#E63946", fg = "white")
          )
        ),

        # Row 2: Scatter + Trend — side-by-side on desktop, stacked on mobile
        layout_columns(
          col_widths = breakpoints(sm = 12, lg = c(7, 5)),
          fill = FALSE,
          class = "mb-3",

          card(
            card_header("Physical Inactivity vs. GDP per Capita"),
            card_body(
              padding = "0.5rem",
              withSpinner(
                plotlyOutput("scatter_plot", height = "420px"),
                type = 6, color = "#2E75B6", size = 0.8
              )
            ),
            card_footer(
              class = "d-flex align-items-center justify-content-between",
              tags$small(
                class = "text-muted",
                "Bubble size = NCD outcome \u00b7 Colour = income group \u00b7 ",
                tags$strong("Click a point"), " to load its trend"
              ),
              downloadButton("dl_scatter", "",
                             class = "btn btn-sm btn-outline-secondary py-0 px-1",
                             icon = icon("download"))
            )
          ),

          card(
            card_header(textOutput("trend_title")),
            card_body(
              padding = "0.5rem",
              withSpinner(
                plotlyOutput("trend_plot", height = "420px"),
                type = 6, color = "#E63946", size = 0.8
              )
            ),
            card_footer(
              class = "d-flex align-items-center justify-content-between",
              tags$small(
                class = "text-muted",
                "Solid line = physical inactivity \u00b7 Dashed = selected NCD outcome"
              ),
              downloadButton("dl_trend", "",
                             class = "btn btn-sm btn-outline-secondary py-0 px-1",
                             icon = icon("download"))
            )
          )
        ),

        # Row 3: Country brief — full width
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

        # Footer
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
    )
  ),

  # ── Tab 2: About ─────────────────────────────────────────────────────────────
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

  # ── Filtered data ──────────────────────────────────────────────────────────

  filtered <- reactive({
    d <- df |>
      filter(sex == input$sex_filter, year == input$year_filter)
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
    paste0("Avg ", names(ncd_choices)[ncd_choices == input$ncd_outcome])
  })

  output$avg_ncd <- renderText({
    ncd_col <- input$ncd_outcome
    d <- filtered()
    if (!ncd_col %in% names(d)) return("\u2014")
    val <- mean(d[[ncd_col]], na.rm = TRUE)
    if (is.nan(val)) "\u2014" else paste0(round(val, 1), "%")
  })

  output$most_inactive <- renderText({
    d <- filtered()
    if (nrow(d) == 0) return("\u2014")
    d |> slice_max(inactivity_pct, n = 1, with_ties = FALSE) |> pull(country_name)
  })

  # ── Scatter plot ──────────────────────────────────────────────────────────

  scatter_gg <- reactive({
    d         <- filtered()
    ncd_col   <- input$ncd_outcome
    ncd_label <- names(ncd_choices)[ncd_choices == ncd_col]

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

  observeEvent(event_data("plotly_click"), {
    click <- event_data("plotly_click")
    if (!is.null(click)) {
      d   <- filtered()
      idx <- which.min(abs(log10(d$gdp_per_capita) - click$x) +
                       abs(d$inactivity_pct - click$y))
      selected_country(d$country_name[idx])
      updateSelectInput(session, "brief_country", selected = d$country_name[idx])
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
    ncd_label <- names(ncd_choices)[ncd_choices == ncd_col]

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
    api_key      <- Sys.getenv("GEMINI_API_KEY")
    country_data <- df |> filter(country_name == country)

    validate(need(nrow(country_data) > 0, paste("No data available for", country)))

    withProgress(message = paste("Generating brief for", country, "..."), value = 0.5, {
      tryCatch(
        generate_country_brief(country_data, tone = tone, api_key = api_key),
        error = function(e) paste("Error generating brief:", e$message)
      )
    })
  })

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
        tags$p(style = "line-height: 1.78; font-size: 0.95em;", brief_text()),
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
      ncd_label <- names(ncd_choices)[ncd_choices == ncd_col]

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
}

# ── Run ───────────────────────────────────────────────────────────────────────

shinyApp(ui, server)
