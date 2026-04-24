# R/generate_brief.R
# ─────────────────────────────────────────────────────────────────────────────
# Generates a plain-language country brief using the Google Gemini API (free).
# Get a free key at: https://aistudio.google.com → Get API key (no card needed)
# Add GEMINI_API_KEY=... to your .env file.
# Briefs are cached by country + data hash so repeat calls are free.
# ─────────────────────────────────────────────────────────────────────────────

library(httr2)
library(jsonlite)
library(dplyr)
library(digest)
library(here)

cache_dir <- here("data", "briefs")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

# ── Build the prompt ──────────────────────────────────────────────────────────

ncd_labels <- c(
  obesity_prevalence  = "Obesity prevalence",
  diabetes_prevalence = "Diabetes prevalence",
  ncd_mortality       = "Premature NCD mortality (ages 30-70)"
)

build_prompt <- function(country_data, tone = "policy",
                         sex_filter = "Both",
                         ncd_outcome = "obesity_prevalence") {

  d <- country_data |>
    filter(sex == sex_filter) |>
    arrange(year)

  country  <- unique(d$country_name)
  region   <- unique(d$region)
  income   <- unique(d$income_level)

  latest   <- d |> slice_tail(n = 1)
  earliest <- d |> slice_head(n = 1)

  inact_now   <- latest$inactivity_pct
  inact_then  <- earliest$inactivity_pct
  inact_delta <- round(inact_now - inact_then, 1)
  inact_dir   <- ifelse(inact_delta > 0, "increased", "decreased")

  gdp <- scales::dollar(latest$gdp_per_capita, accuracy = 1)

  ncd_label <- ncd_labels[[ncd_outcome]]
  ncd_val   <- if (ncd_outcome %in% names(latest) && !is.na(latest[[ncd_outcome]]))
                 round(latest[[ncd_outcome]], 1) else NA

  tone_instruction <- if (tone == "policy") {
    "Write for a public health policymaker. Be analytical and evidence-focused. Use professional language."
  } else {
    "Write for a general audience. Be clear, engaging, and avoid jargon. Use plain language."
  }

  ncd_line <- if (!is.na(ncd_val)) glue::glue("\n- {ncd_label}: {ncd_val}%") else ""

  sex_note <- if (sex_filter != "Both") glue::glue(" (data for {tolower(sex_filter)}s)") else ""

  prompt <- glue::glue(
    "You are a global health analyst. Write a 200-word country health brief about {country}.",
    "\n\n{tone_instruction}",
    "\n\nData summary{sex_note}:",
    "\n- Region: {region}",
    "\n- Income group: {income}",
    "\n- GDP per capita: {gdp}",
    "\n- Physical inactivity (adults): {inact_now}% ({inact_dir} by {abs(inact_delta)} pp since {earliest$year})",
    "{ncd_line}",
    "\n\nFocus your analysis on physical inactivity and {ncd_label}.",
    " Interpret these trends in 200 words. Identify patterns, explain the wealth-inactivity paradox",
    " if relevant, and suggest one concrete policy implication. Do not use bullet points.",
    .sep = ""
  )

  prompt
}

# ── Call the Gemini API ───────────────────────────────────────────────────────

call_gemini <- function(prompt, api_key) {
  url <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    "gemini-2.5-flash-lite:generateContent?key=", api_key
  )

  resp <- request(url) |>
    req_headers(`Content-Type` = "application/json") |>
    req_body_json(list(
      contents = list(list(
        parts = list(list(text = prompt))
      )),
      generationConfig = list(
        maxOutputTokens = 400,
        temperature     = 0.5
      )
    )) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()

  if (resp_status(resp) != 200) {
    body <- resp_body_json(resp)
    stop("Gemini API error (", resp_status(resp), "): ",
         body$error$message)
  }

  resp_body_json(resp)$candidates[[1]]$content$parts[[1]]$text
}

# ── Main function (called from app.R) ────────────────────────────────────────

generate_country_brief <- function(country_data, tone = "policy",
                                   sex_filter = "Both",
                                   ncd_outcome = "obesity_prevalence",
                                   api_key = Sys.getenv("GEMINI_API_KEY")) {

  if (api_key == "") {
    env_file <- file.path(here(), ".env")
    if (file.exists(env_file)) {
      readRenviron(env_file)
      api_key <- Sys.getenv("GEMINI_API_KEY")
    }
  }

  if (api_key == "") stop("GEMINI_API_KEY not set. Add it to your .env file.")

  country <- unique(country_data$country_name)

  data_hash  <- digest(country_data, algo = "md5")
  cache_key  <- paste0(gsub("[^a-zA-Z0-9]", "_", country), "_", tone, "_",
                        gsub("[^a-zA-Z0-9]", "_", sex_filter), "_",
                        ncd_outcome, "_", substr(data_hash, 1, 8))
  cache_file <- file.path(cache_dir, paste0(cache_key, ".txt"))

  if (file.exists(cache_file)) {
    message("Using cached brief for: ", country)
    return(readLines(cache_file, warn = FALSE) |> paste(collapse = "\n"))
  }

  message("Generating brief for: ", country)
  prompt <- build_prompt(country_data, tone, sex_filter, ncd_outcome)
  brief  <- call_gemini(prompt, api_key)

  writeLines(brief, cache_file)
  brief
}

# ── Comparison brief ─────────────────────────────────────────────────────────

build_comparison_prompt <- function(countries_data, tone = "policy",
                                    sex_filter = "Both",
                                    ncd_outcome = "obesity_prevalence") {

  tone_instruction <- if (tone == "policy") {
    "Write for a public health policymaker. Be analytical and evidence-focused. Use professional language."
  } else {
    "Write for a general audience. Be clear, engaging, and avoid jargon. Use plain language."
  }

  ncd_label     <- ncd_labels[[ncd_outcome]]
  country_names <- unique(countries_data$country_name)

  summaries <- lapply(country_names, function(cn) {
    d <- countries_data |>
      filter(country_name == cn, sex == sex_filter) |>
      arrange(year)
    latest   <- d |> slice_tail(n = 1)
    earliest <- d |> slice_head(n = 1)

    inact_now   <- latest$inactivity_pct
    inact_then  <- earliest$inactivity_pct
    inact_delta <- round(inact_now - inact_then, 1)
    inact_dir   <- ifelse(inact_delta > 0, "increased", "decreased")

    gdp     <- scales::dollar(latest$gdp_per_capita, accuracy = 1)
    ncd_val <- if (ncd_outcome %in% names(latest) && !is.na(latest[[ncd_outcome]]))
                 round(latest[[ncd_outcome]], 1) else NA

    lines <- c(
      glue::glue("\n\n### {cn}"),
      glue::glue("- Region: {unique(d$region)}"),
      glue::glue("- Income group: {unique(d$income_level)}"),
      glue::glue("- GDP per capita: {gdp}"),
      glue::glue("- Physical inactivity: {inact_now}% ({inact_dir} by {abs(inact_delta)} pp since {earliest$year})")
    )
    if (!is.na(ncd_val)) lines <- c(lines, glue::glue("- {ncd_label}: {ncd_val}%"))
    paste(lines, collapse = "\n")
  })

  sex_note <- if (sex_filter != "Both") glue::glue(" (data for {tolower(sex_filter)}s)") else ""

  glue::glue(
    "You are a global health analyst. Write a 300-word comparative brief ",
    "about these {length(country_names)} countries: {paste(country_names, collapse = ', ')}.",
    "\n\n{tone_instruction}",
    "\n\nData summary{sex_note}:",
    "{paste(summaries, collapse = '')}",
    "\n\nFocus your comparison on physical inactivity and {ncd_label}.",
    " Highlight the most striking differences ",
    "and similarities. Explain what ",
    "accounts for divergent outcomes. End with one concrete policy lesson that ",
    "emerges from the comparison. Do not use bullet points.",
    .sep = ""
  )
}

generate_comparison_brief <- function(countries_data, tone = "policy",
                                      sex_filter = "Both",
                                      ncd_outcome = "obesity_prevalence",
                                      api_key = Sys.getenv("GEMINI_API_KEY")) {

  if (api_key == "") {
    env_file <- file.path(here(), ".env")
    if (file.exists(env_file)) {
      readRenviron(env_file)
      api_key <- Sys.getenv("GEMINI_API_KEY")
    }
  }

  if (api_key == "") stop("GEMINI_API_KEY not set. Add it to your .env file.")

  country_names <- sort(unique(countries_data$country_name))

  data_hash  <- digest(countries_data, algo = "md5")
  cache_key  <- paste0("compare_", paste(gsub("[^a-zA-Z0-9]", "_", country_names), collapse = "_"),
                        "_", tone, "_", gsub("[^a-zA-Z0-9]", "_", sex_filter), "_",
                        ncd_outcome, "_", substr(data_hash, 1, 8))
  cache_file <- file.path(cache_dir, paste0(cache_key, ".txt"))

  if (file.exists(cache_file)) {
    message("Using cached comparison brief")
    return(readLines(cache_file, warn = FALSE) |> paste(collapse = "\n"))
  }

  message("Generating comparison brief for: ", paste(country_names, collapse = ", "))
  prompt <- build_comparison_prompt(countries_data, tone, sex_filter, ncd_outcome)
  brief  <- call_gemini(prompt, api_key)

  writeLines(brief, cache_file)
  brief
}

# ── Invalidate stale cache ────────────────────────────────────────────────────
# Called by GitHub Actions after a data refresh to clear outdated briefs.

invalidate_brief_cache <- function() {
  cached <- list.files(cache_dir, pattern = "\\.txt$", full.names = TRUE)
  file.remove(cached)
  message("Cleared ", length(cached), " cached briefs.")
}
