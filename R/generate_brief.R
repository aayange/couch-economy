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

build_prompt <- function(country_data, tone = "policy") {

  d <- country_data |>
    filter(sex == "Both") |>
    arrange(year)

  country  <- unique(d$country_name)
  region   <- unique(d$region)
  income   <- unique(d$income_level)

  # Latest available values
  latest <- d |> slice_tail(n = 1)
  earliest <- d |> slice_head(n = 1)

  inact_now   <- latest$inactivity_pct
  inact_then  <- earliest$inactivity_pct
  inact_delta <- round(inact_now - inact_then, 1)
  inact_dir   <- ifelse(inact_delta > 0, "increased", "decreased")

  gdp    <- scales::dollar(latest$gdp_per_capita, accuracy = 1)
  cvd    <- if ("ncd_mortality" %in% names(latest) && !is.na(latest$ncd_mortality))
              round(latest$ncd_mortality, 1) else NA
  diab   <- if (!is.na(latest$diabetes_prevalence)) round(latest$diabetes_prevalence, 1) else NA
  obese  <- if (!is.na(latest$obesity_prevalence)) round(latest$obesity_prevalence, 1) else NA

  tone_instruction <- if (tone == "policy") {
    "Write for a public health policymaker. Be analytical and evidence-focused. Use professional language."
  } else {
    "Write for a general audience. Be clear, engaging, and avoid jargon. Use plain language."
  }

  cvd_line  <- if (!is.na(cvd))  glue::glue("\n- Premature NCD mortality (ages 30-70): {cvd}%") else ""
  diab_line <- if (!is.na(diab)) glue::glue("\n- Diabetes prevalence: {diab}%") else ""
  obese_line <- if (!is.na(obese)) glue::glue("\n- Obesity prevalence: {obese}%") else ""

  prompt <- glue::glue(
    "You are a global health analyst. Write a 200-word country health brief about {country}.",
    "\n\n{tone_instruction}",
    "\n\nData summary:",
    "\n- Region: {region}",
    "\n- Income group: {income}",
    "\n- GDP per capita: {gdp}",
    "\n- Physical inactivity (adults): {inact_now}% ({inact_dir} by {abs(inact_delta)} pp since {earliest$year})",
    "{cvd_line}",
    "{diab_line}",
    "{obese_line}",
    "\n\nInterpret these trends in 200 words. Identify patterns, explain the wealth-inactivity paradox",
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
                                   api_key = Sys.getenv("GEMINI_API_KEY")) {

  # If not in environment, try loading from .env in the project root
  if (api_key == "") {
    env_file <- file.path(here(), ".env")
    if (file.exists(env_file)) {
      readRenviron(env_file)
      api_key <- Sys.getenv("GEMINI_API_KEY")
    }
  }

  if (api_key == "") stop("GEMINI_API_KEY not set. Add it to your .env file.")

  country <- unique(country_data$country_name)

  # Cache key: country + data hash + tone
  data_hash  <- digest(country_data, algo = "md5")
  cache_key  <- paste0(gsub("[^a-zA-Z0-9]", "_", country), "_", tone, "_", substr(data_hash, 1, 8))
  cache_file <- file.path(cache_dir, paste0(cache_key, ".txt"))

  # Return cached brief if available
  if (file.exists(cache_file)) {
    message("Using cached brief for: ", country)
    return(readLines(cache_file, warn = FALSE) |> paste(collapse = "\n"))
  }

  # Build prompt and call API
  message("Generating brief for: ", country)
  prompt <- build_prompt(country_data, tone)
  brief  <- call_gemini(prompt, api_key)

  # Cache the result
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
