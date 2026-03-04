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

# Load .env for local development (ignored in GitHub Actions where secrets are set)
if (file.exists(here(".env"))) {
  readRenviron(here(".env"))
}

# ── Lookup tables ─────────────────────────────────────────────────────────────

countries <- sort(unique(df$country_name))
years     <- sort(unique(df$year))

# Only offer NCD outcomes that are actually present in the data
# obesity_prevalence is listed first as it has the broadest year coverage (1990–2022)
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

# Colour palette for income groups
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
)

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title = tags$span(
    tags$strong("The Couch Economy"),
    tags$span(" — Wealth, Inactivity & NCD Burden",
              style = "font-size: 0.85em; color: #6c757d;")
  ),
  theme = app_theme,
  navbar_options = navbar_options(bg = "#1A3C5E", theme = "dark"),

  # ── Tab 1: Explorer ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Explorer",
    icon  = icon("globe"),

    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        bg    = "#f8f9fa",

        tags$h6("Filters", class = "text-muted text-uppercase fw-bold mt-2"),

        selectInput("income_filter", "Income group",
                    choices = income_levels, selected = "All"),

        selectInput("sex_filter", "Sex",
                    choices = c("Both", "Male", "Female"), selected = "Both"),

        selectInput("ncd_outcome", "NCD outcome",
                    choices = ncd_choices,
                    selected = ncd_choices[1]),

        sliderInput("year_filter", "Year",
                    min = min(years), max = max(years),
                    value = max(years), step = 1, sep = ""),

        hr(),

        tags$h6("Country Brief", class = "text-muted text-uppercase fw-bold"),

        selectInput("brief_country", "Select a country",
                    choices = countries, selected = "United States"),

        radioButtons("brief_tone", "Audience",
                     choices  = c("Policy" = "policy", "General public" = "general"),
                     selected = "policy", inline = TRUE),

        actionButton("generate_brief", "Generate Brief",
                     class = "btn-primary w-100 mt-1",
                     icon  = icon("wand-magic-sparkles")),

        tags$small(
          class = "text-muted",
          "Briefs are cached — first call hits the Gemini API, repeats are instant."
        )
      ),

      # Main panel
      layout_columns(
        col_widths = c(12, 7, 5),
        fill = FALSE,

        # Row 1: headline value boxes
        layout_columns(
          col_widths = c(4, 4, 4),
          value_box(
            title    = "Countries with data",
            value    = textOutput("n_countries", inline = TRUE),
            showcase = icon("flag"),
            theme    = value_box_theme(bg = "#1A3C5E", fg = "white")
          ),
          value_box(
            title    = "Avg inactivity rate",
            value    = textOutput("avg_inactivity", inline = TRUE),
            showcase = icon("person-walking"),
            theme    = value_box_theme(bg = "#2E75B6", fg = "white")
          ),
          value_box(
            title    = "Most inactive (shown year)",
            value    = textOutput("most_inactive", inline = TRUE),
            showcase = icon("couch"),
            theme    = value_box_theme(bg = "#E63946", fg = "white")
          )
        ),

        # Row 2: Scatter plot (wider)
        card(
          card_header("Physical Inactivity vs. GDP per Capita"),
          plotlyOutput("scatter_plot", height = "400px"),
          card_footer(tags$small(class = "text-muted",
            "Each point is one country. Bubble size = NCD outcome value. ",
            "Colour = income group. Click a point to select it for the trend chart."
          ))
        ),

        # Row 2: Trend chart (narrower)
        card(
          card_header(textOutput("trend_title")),
          plotlyOutput("trend_plot", height = "400px"),
          card_footer(tags$small(class = "text-muted",
            "Left axis: physical inactivity (%). Right axis: selected NCD outcome."
          ))
        ),

        # Row 3: Country brief (full width)
        card(
          card_header(
            "Country Brief",
            tags$span(
              class = "badge bg-success ms-2",
              "Powered by Gemini 2.5 Flash Lite"
            )
          ),
          uiOutput("brief_output"),
          min_height = "150px"
        )
      )
    )
  ),

  # ── Tab 2: About ─────────────────────────────────────────────────────────────
  nav_panel(
    title = "About",
    icon  = icon("circle-info"),

    layout_columns(
      col_widths = c(8),
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
            tags$li(tags$strong("WHO Global Health Observatory (GHO):"),
                    " Physical inactivity prevalence, adults 18+, age-standardised (NCD_PAC)"),
            tags$li(tags$strong("World Bank:"),
                    " GDP per capita (NY.GDP.PCAP.CD) and Gini index (SI.POV.GINI)"),
            tags$li(tags$strong("Our World in Data:"),
                    " Cardiovascular mortality, diabetes prevalence, obesity prevalence")
          ),
          tags$h5("Pipeline"),
          tags$p(
            "Data refreshes automatically every Monday via a GitHub Actions workflow. ",
            "Country briefs are generated on demand via the Google Gemini API (gemini-2.5-flash-lite) ",
            "and cached locally to avoid redundant API calls."
          ),
          tags$h5("Course"),
          tags$p("GLHLTH 562: Data Science and Visualization with R — Final Project")
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Filtered data (reactive) ─────────────────────────────────────────────────

  filtered <- reactive({
    d <- df |>
      filter(sex == input$sex_filter, year == input$year_filter)

    if (input$income_filter != "All") {
      d <- d |> filter(income_level == input$income_filter)
    }

    d |> filter(!is.na(inactivity_pct), !is.na(gdp_per_capita))
  })

  # ── Value boxes ──────────────────────────────────────────────────────────────

  output$n_countries <- renderText({
    n_distinct(filtered()$iso3)
  })

  output$avg_inactivity <- renderText({
    avg <- mean(filtered()$inactivity_pct, na.rm = TRUE)
    paste0(round(avg, 1), "%")
  })

  output$most_inactive <- renderText({
    d <- filtered()
    if (nrow(d) == 0) return("—")
    d |> slice_max(inactivity_pct, n = 1) |> pull(country_name)
  })

  # ── Scatter plot ─────────────────────────────────────────────────────────────

  output$scatter_plot <- renderPlotly({
    d <- filtered()
    ncd_col   <- input$ncd_outcome
    ncd_label <- names(ncd_choices)[ncd_choices == ncd_col]

    validate(
      need(ncd_col %in% names(d),
           paste("Column", ncd_col, "not available in data.")),
      need(nrow(d) > 0, "No data for current filters.")
    )

    d_ncd <- d |> filter(!is.na(.data[[ncd_col]]))

    validate(
      need(nrow(d_ncd) > 0,
           paste0("No ", names(ncd_choices)[ncd_choices == ncd_col],
                  " data for the selected year. Try a different year or NCD outcome."))
    )

    p <- d_ncd |>
      ggplot(aes(
        x     = gdp_per_capita,
        y     = inactivity_pct,
        color = income_level,
        size  = .data[[ncd_col]],
        text  = glue("{country_name}\nGDP: {dollar(gdp_per_capita, accuracy=1)}\nInactivity: {inactivity_pct}%\n{ncd_label}: {round(.data[[ncd_col]],1)}")
      )) +
      geom_point(alpha = 0.75) +
      scale_x_log10(labels = dollar_format(scale = 1e-3, suffix = "k")) +
      scale_color_manual(values = income_colors, na.value = "grey60") +
      scale_size(range = c(2, 10), guide = "none") +
      labs(
        x     = "GDP per capita (log scale, USD)",
        y     = "Physical inactivity (%)",
        color = "Income group"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")

    ggplotly(p, tooltip = "text") |>
      layout(legend = list(orientation = "h", y = -0.2))
  })

  # ── Selected country (from scatter click or dropdown) ────────────────────────

  selected_country <- reactiveVal(NULL)

  observeEvent(event_data("plotly_click"), {
    click <- event_data("plotly_click")
    if (!is.null(click)) {
      # Find country nearest to clicked point
      d <- filtered()
      idx <- which.min(abs(log10(d$gdp_per_capita) - click$x) +
                       abs(d$inactivity_pct - click$y))
      selected_country(d$country_name[idx])
      updateSelectInput(session, "brief_country", selected = d$country_name[idx])
    }
  })

  # ── Trend chart ──────────────────────────────────────────────────────────────

  output$trend_title <- renderText({
    country <- if (!is.null(selected_country())) selected_country() else input$brief_country
    paste("Trend:", country)
  })

  output$trend_plot <- renderPlotly({
    country <- if (!is.null(selected_country())) selected_country() else input$brief_country
    ncd_col  <- input$ncd_outcome
    ncd_label <- names(ncd_choices)[ncd_choices == ncd_col]

    d <- df |>
      filter(country_name == country, sex == input$sex_filter) |>
      arrange(year)

    validate(
      need(nrow(d) > 0, "No data for selected country."),
      need(ncd_col %in% names(d), paste("Column", ncd_col, "not available."))
    )

    ncd_max      <- max(d[[ncd_col]], na.rm = TRUE)
    inact_max    <- max(d$inactivity_pct, na.rm = TRUE)
    scale_factor <- if (is.finite(ncd_max) && is.finite(inact_max) && ncd_max > 0)
                      inact_max / ncd_max else 1

    p <- d |>
      ggplot(aes(x = year)) +
      geom_line(aes(y = inactivity_pct, color = "Inactivity (%)"), linewidth = 1.2) +
      geom_point(aes(y = inactivity_pct, color = "Inactivity (%)"), size = 2.5) +
      geom_line(aes(y = .data[[ncd_col]] * scale_factor,
                    color = ncd_label), linewidth = 1.2, linetype = "dashed") +
      scale_color_manual(
        values = c("Inactivity (%)" = "#2E75B6", setNames("#E63946", ncd_label))
      ) +
      labs(x = NULL, y = "Physical inactivity (%)", color = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")

    ggplotly(p) |> layout(legend = list(orientation = "h", y = -0.2))
  })

  # ── Country brief ─────────────────────────────────────────────────────────────

  brief_text <- eventReactive(input$generate_brief, {
    country  <- input$brief_country
    tone     <- input$brief_tone
    api_key  <- Sys.getenv("GEMINI_API_KEY")

    country_data <- df |> filter(country_name == country)

    validate(need(nrow(country_data) > 0,
                  paste("No data available for", country)))

    withProgress(message = paste("Generating brief for", country, "..."),
                 value = 0.5, {
      tryCatch(
        generate_country_brief(country_data, tone = tone, api_key = api_key),
        error = function(e) paste("Error generating brief:", e$message)
      )
    })
  })

  output$brief_output <- renderUI({
    if (is.null(brief_text()) || brief_text() == "") {
      tags$p(class = "text-muted p-3",
             "Select a country and click \"Generate Brief\" to get an AI-written health analysis.")
    } else {
      tags$div(
        class = "p-3",
        tags$p(style = "line-height: 1.7; font-size: 0.95em;", brief_text()),
        tags$small(class = "text-muted",
          icon("robot"), " Generated by Gemini 2.5 Flash Lite. Always verify against primary sources."
        )
      )
    }
  })
}

# ── Run ───────────────────────────────────────────────────────────────────────

shinyApp(ui, server)
