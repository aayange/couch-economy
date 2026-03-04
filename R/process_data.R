# R/process_data.R
# ─────────────────────────────────────────────────────────────────────────────
# Cleans, joins, and validates the three raw datasets into one analysis-ready
# file. Outputs saved to data/processed/couch_economy.rds.
# ─────────────────────────────────────────────────────────────────────────────

library(dplyr)
library(tidyr)
library(purrr)
library(countrycode)   # ISO code standardisation
library(here)

raw_dir  <- here("data", "raw")
proc_dir <- here("data", "processed")
dir.create(proc_dir, showWarnings = FALSE, recursive = TRUE)

message("Loading raw data...")
message("  Looking in: ", raw_dir)

# Check required files exist
required_files <- c("who_inactivity.rds", "wb_indicators.rds", "owid_ncd.rds")
missing_files  <- required_files[!file.exists(file.path(raw_dir, required_files))]

if (length(missing_files) > 0) {
  stop(
    "Missing raw data files: ", paste(missing_files, collapse = ", "),
    "\nRun R/fetch_data.R first.",
    call. = FALSE
  )
}

who_raw      <- readRDS(file.path(raw_dir, "who_inactivity.rds"))
wb_raw       <- readRDS(file.path(raw_dir, "wb_indicators.rds"))
owid_raw     <- readRDS(file.path(raw_dir, "owid_ncd.rds"))
diabetes_raw     <- if (file.exists(file.path(raw_dir, "who_diabetes.rds")))
                      readRDS(file.path(raw_dir, "who_diabetes.rds")) else NULL
ncd_mort_raw     <- if (file.exists(file.path(raw_dir, "who_ncd_mortality.rds")))
                      readRDS(file.path(raw_dir, "who_ncd_mortality.rds")) else NULL


# ── 1. Clean WHO data ─────────────────────────────────────────────────────────
# Sex codes: BTSX = both, MLE = male, FMLE = female

if (nrow(who_raw) == 0) {
  message("  WARNING: WHO data is empty — inactivity columns will be NA")
  message("  Check fetch_data.R diagnostic output above for the API response structure")
  # Build a stub using World Bank country list so other data is still explorable
  who_stub_countries <- wb_raw |> distinct(iso3) |> pull(iso3)
  who_raw <- expand.grid(
    iso3 = who_stub_countries,
    year = c(2010, 2016, 2022),
    sex  = c("BTSX", "MLE", "FMLE"),
    stringsAsFactors = FALSE
  ) |>
    mutate(inactivity_pct = NA_real_,
           inactivity_low = NA_real_,
           inactivity_high = NA_real_)
}

who <- who_raw |>
  mutate(
    sex = recode(sex,
      "BTSX" = "Both",
      "MLE"  = "Male",
      "FMLE" = "Female"
    ),
    inactivity_pct  = round(inactivity_pct, 1),
    inactivity_low  = round(inactivity_low, 1),
    inactivity_high = round(inactivity_high, 1)
  )

message("  WHO: ", nrow(who), " rows, ", n_distinct(who$iso3), " countries")

# ── 2. Clean World Bank data ──────────────────────────────────────────────────
# GDP is log-transformed for scatter plot scaling.
# Gini is sparse — fill forward within country where available.

wb <- wb_raw |>
  arrange(iso3, year) |>
  group_by(iso3) |>
  fill(gini, .direction = "downup") |>   # fill Gini gaps within country
  ungroup() |>
  mutate(
    log_gdp = log10(gdp_per_capita),
    income_level = factor(
      income_level,
      levels = c("Low income", "Lower middle income",
                 "Upper middle income", "High income")
    )
  ) |>
  filter(!is.na(gdp_per_capita))

message("  World Bank: ", nrow(wb), " rows, ", n_distinct(wb$iso3), " countries")

# ── 3. Clean and pivot OWID data ─────────────────────────────────────────────

owid <- imap(owid_raw, function(df, name) {
  df |>
    filter(!is.na(value)) |>
    mutate(indicator = name, value = round(value, 2))
}) |>
  bind_rows() |>
  pivot_wider(names_from = indicator, values_from = value)

message("  OWID: ", nrow(owid), " rows, ", n_distinct(owid$iso3), " countries")

# ── 4. Join all sources ───────────────────────────────────────────────────────
# WHO years: typically 2000, 2010, 2015, 2016, 2022.

who_years <- sort(unique(who$year))

joined <- who |>
  left_join(wb,   by = c("iso3", "year")) |>
  left_join(owid, by = c("iso3", "year"))

if (!is.null(diabetes_raw)) {
  joined <- left_join(joined, diabetes_raw, by = c("iso3", "year", "sex"))
}

if (!is.null(ncd_mort_raw)) {
  joined <- left_join(joined, ncd_mort_raw, by = c("iso3", "year", "sex"))
}

joined <- joined |>
  mutate(
    country_name = countrycode(iso3, origin = "iso3c", destination = "country.name"),
    region       = countrycode(iso3, origin = "iso3c", destination = "region"),
    continent    = countrycode(iso3, origin = "iso3c", destination = "continent")
  ) |>
  filter(!is.na(country_name))

# Relocate only columns that actually exist (some OWID indicators may be missing)
desired_order <- c(
  "iso3", "country_name", "region", "continent", "year", "sex",
  "income_level", "gdp_per_capita", "log_gdp", "gini",
  "inactivity_pct", "inactivity_low", "inactivity_high",
  "ncd_mortality", "diabetes_prevalence", "obesity_prevalence"
)
joined <- joined |>
  select(-any_of("country")) |>   # drop WB 'country' col — shadows variables in filter()
  relocate(any_of(desired_order))

message("  Joined: ", nrow(joined), " rows, ",
        n_distinct(joined$iso3), " countries, ",
        n_distinct(joined$year), " years")

# ── 5. Validate ───────────────────────────────────────────────────────────────

# Flag countries missing inactivity estimates
missing_inactivity <- joined |>
  filter(is.na(inactivity_pct), sex == "Both") |>
  distinct(iso3, country_name)

if (nrow(missing_inactivity) > 0) {
  message("  WARNING: ", nrow(missing_inactivity),
          " countries missing inactivity estimates")
}

# Flag anomalies: inactivity > 3 SD from mean
anomalies <- joined |>
  filter(sex == "Both") |>
  mutate(z = scale(inactivity_pct)) |>
  filter(abs(z) > 3)

if (nrow(anomalies) > 0) {
  message("  WARNING: ", nrow(anomalies), " anomalous inactivity values flagged")
  print(anomalies |> select(iso3, country_name, year, inactivity_pct))
}

# ── 6. Save ───────────────────────────────────────────────────────────────────

saveRDS(joined, file.path(proc_dir, "couch_economy.rds"))
message("\nProcessed data saved -> data/processed/couch_economy.rds")
message("Run app.R to launch the Shiny app.")
