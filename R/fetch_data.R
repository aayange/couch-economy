# R/fetch_data.R
# ─────────────────────────────────────────────────────────────────────────────
# Fetches raw data from WHO GHO, World Bank, and Our World in Data.
# Run manually during development or automatically via GitHub Actions.
# Outputs saved to data/raw/ as .rds files.
# ─────────────────────────────────────────────────────────────────────────────

library(httr2)
library(jsonlite)
library(wbstats)
library(readr)
library(dplyr)
library(purrr)
library(here)

raw_dir <- here("data", "raw")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
message("Saving raw data to: ", raw_dir)

# ── Helper: paginated WHO GHO fetch ──────────────────────────────────────────
# The WHO OData API returns results in pages (default 1000 rows per page).
# fromJSON(simplifyDataFrame=TRUE) handles NULL->NA automatically.

fetch_who <- function(indicator) {
  base_url  <- paste0("https://ghoapi.azureedge.net/api/", indicator)
  all_pages <- list()
  url       <- base_url

  repeat {
    resp <- request(url) |>
      req_timeout(60) |>
      req_retry(max_tries = 3, backoff = ~ 5) |>
      req_perform()

    parsed <- fromJSON(resp_body_string(resp), simplifyDataFrame = TRUE)

    if (!is.null(parsed$value) && nrow(parsed$value) > 0) {
      all_pages <- c(all_pages, list(parsed$value))
    }

    # Follow OData pagination
    next_link <- parsed[["@odata.nextLink"]]
    if (is.null(next_link) || length(next_link) == 0) break
    url <- next_link
  }

  if (length(all_pages) == 0) return(data.frame())
  bind_rows(all_pages)
}

# ── 1. WHO GHO: Physical Inactivity (age-standardised) ───────────────────────
# Indicator: NCD_PAC

message("[1/3] Fetching WHO GHO physical inactivity data (NCD_PAC)...")

inactivity_raw <- tryCatch({
  raw_df <- fetch_who("NCD_PAC")

  # ── Diagnostics — remove once working ──
  message("  Raw rows returned: ", nrow(raw_df))
  message("  Columns: ", paste(names(raw_df), collapse = ", "))
  if ("Dim1" %in% names(raw_df)) {
    message("  Dim1 unique values: ", paste(unique(raw_df$Dim1), collapse = ", "))
  } else {
    message("  WARNING: Dim1 column not found in response!")
  }
  if ("NumericValue" %in% names(raw_df)) {
    message("  NumericValue non-NA count: ", sum(!is.na(raw_df$NumericValue)))
  }
  # ───────────────────────────────────────

  raw_df |>
    filter(
      Dim1 %in% c("SEX_MLE", "SEX_FMLE", "SEX_BTSX"),
      !is.na(NumericValue),
      !is.na(SpatialDim),
      nchar(as.character(SpatialDim)) == 3
    ) |>
    mutate(sex = sub("SEX_", "", Dim1)) |>   # strip prefix: SEX_BTSX -> BTSX
    select(
      iso3            = SpatialDim,
      year            = TimeDim,
      sex,
      inactivity_pct  = NumericValue,
      inactivity_low  = Low,
      inactivity_high = High
    )
}, error = function(e) {
  message("  ERROR fetching WHO inactivity: ", e$message)
  NULL
})

if (!is.null(inactivity_raw) && nrow(inactivity_raw) > 0) {
  saveRDS(inactivity_raw, file.path(raw_dir, "who_inactivity.rds"))
  message("  Saved ", nrow(inactivity_raw), " rows -> data/raw/who_inactivity.rds")
} else {
  message("  WARNING: 0 rows returned for NCD_PAC — check API or indicator code")
}

# ── 2. World Bank: GDP per capita + Gini ─────────────────────────────────────

message("[2/3] Fetching World Bank indicators...")

wb_raw <- tryCatch({
  wb_econ <- wb_data(
    indicator   = c(gdp_per_capita = "NY.GDP.PCAP.CD",
                    gini           = "SI.POV.GINI"),
    start_date  = 2000,
    end_date    = 2023,
    return_wide = TRUE
  ) |>
    select(iso3 = iso3c, country, year = date, gdp_per_capita, gini)

  wb_meta <- wb_countries() |>
    select(iso3 = iso3c, income_level)

  left_join(wb_econ, wb_meta, by = "iso3")
}, error = function(e) {
  message("  ERROR fetching World Bank data: ", e$message)
  NULL
})

if (!is.null(wb_raw)) {
  saveRDS(wb_raw, file.path(raw_dir, "wb_indicators.rds"))
  message("  Saved ", nrow(wb_raw), " rows -> data/raw/wb_indicators.rds")
}

# ── 3. WHO GHO: Diabetes prevalence (age-standardised) ───────────────────────
# Indicator: NCD_DIABETES_PREVALENCE_AGESTD — annual, sex-stratified like inactivity

message("[3/5] Fetching WHO GHO diabetes prevalence (NCD_DIABETES_PREVALENCE_AGESTD)...")

diabetes_raw <- tryCatch({
  raw_df <- fetch_who("NCD_DIABETES_PREVALENCE_AGESTD")
  message("  Raw rows returned: ", nrow(raw_df))

  raw_df |>
    filter(
      Dim1 %in% c("SEX_MLE", "SEX_FMLE", "SEX_BTSX"),
      Dim2 == "AGEGROUP_YEARS18-PLUS",
      !is.na(NumericValue),
      !is.na(SpatialDim),
      nchar(as.character(SpatialDim)) == 3
    ) |>
    mutate(sex = sub("SEX_", "", Dim1)) |>
    select(
      iso3               = SpatialDim,
      year               = TimeDim,
      sex,
      diabetes_prevalence = NumericValue
    ) |>
    mutate(
      sex = recode(sex, "BTSX" = "Both", "MLE" = "Male", "FMLE" = "Female"),
      diabetes_prevalence = round(diabetes_prevalence, 1)
    )
}, error = function(e) {
  message("  ERROR fetching WHO diabetes: ", e$message)
  NULL
})

if (!is.null(diabetes_raw) && nrow(diabetes_raw) > 0) {
  saveRDS(diabetes_raw, file.path(raw_dir, "who_diabetes.rds"))
  message("  Saved ", nrow(diabetes_raw), " rows -> data/raw/who_diabetes.rds")
} else {
  message("  WARNING: 0 rows returned for diabetes — check indicator code")
}

# ── 4. WHO GHO: Premature NCD mortality (SDG 3.4) ────────────────────────────
# Indicator: NCDMORT3070 — probability (%) of dying between 30 and 70 from
# CVD, cancer, diabetes, or chronic respiratory disease. Sex-stratified.
# (WHS2_161 cardiovascular-only returns 0 rows; OWID CVD is non-redistributable)

message("[4/5] Fetching WHO GHO premature NCD mortality (NCDMORT3070)...")

ncd_mortality_raw <- tryCatch({
  raw_df <- fetch_who("NCDMORT3070")
  message("  Raw rows returned: ", nrow(raw_df))

  raw_df |>
    filter(
      Dim1 %in% c("SEX_MLE", "SEX_FMLE", "SEX_BTSX"),
      !is.na(NumericValue),
      !is.na(SpatialDim),
      nchar(as.character(SpatialDim)) == 3
    ) |>
    mutate(sex = sub("SEX_", "", Dim1)) |>
    select(
      iso3            = SpatialDim,
      year            = TimeDim,
      sex,
      ncd_mortality   = NumericValue
    ) |>
    mutate(
      sex          = recode(sex, "BTSX" = "Both", "MLE" = "Male", "FMLE" = "Female"),
      ncd_mortality = round(ncd_mortality, 1)
    )
}, error = function(e) {
  message("  ERROR fetching NCD mortality: ", e$message)
  NULL
})

if (!is.null(ncd_mortality_raw) && nrow(ncd_mortality_raw) > 0) {
  saveRDS(ncd_mortality_raw, file.path(raw_dir, "who_ncd_mortality.rds"))
  message("  Saved ", nrow(ncd_mortality_raw), " rows -> data/raw/who_ncd_mortality.rds")
} else {
  message("  WARNING: 0 rows returned for NCDMORT3070")
}

# ── 5. OWID: Obesity ─────────────────────────────────────────────────────────

message("[5/5] Fetching OWID obesity data...")

fetch_owid_single <- function(url, val_hint, indicator_name) {
  tryCatch({
    df <- read_csv(url, show_col_types = FALSE, progress = FALSE)
    names(df) <- tolower(gsub("[[:space:]]+", "_", names(df)))
    id_cols  <- c("entity", "code", "year")
    val_cols <- setdiff(names(df), id_cols)
    best_col <- val_cols[grepl(val_hint, val_cols, ignore.case = TRUE)][1]
    if (is.na(best_col)) best_col <- val_cols[1]
    df |>
      filter(!is.na(code), nchar(code) == 3) |>
      select(iso3 = code, year, value = all_of(best_col)) |>
      mutate(indicator = indicator_name)
  }, error = function(e) {
    message("  WARNING: failed to fetch ", indicator_name, " — ", e$message)
    NULL
  })
}

owid_raw <- list(
  obesity_prevalence = fetch_owid_single(
    "https://ourworldindata.org/grapher/share-of-adults-defined-as-obese.csv",
    "obese|obesity|share",
    "obesity_prevalence"
  )
) |> compact()

if (length(owid_raw) > 0) {
  saveRDS(owid_raw, file.path(raw_dir, "owid_ncd.rds"))
  message("  Saved ", length(owid_raw), " indicators -> data/raw/owid_ncd.rds")
}

message("\nFetch complete. Run R/process_data.R next.")
