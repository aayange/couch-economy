# The Couch Economy

**Wealth, Inactivity, and the Global NCD Burden**

An interactive Shiny dashboard exploring why the world's wealthiest countries are often its most physically inactive — and what that means for obesity, diabetes, and premature NCD mortality globally.

**Live app:** [ayange.shinyapps.io/couch-economy](https://ayange.shinyapps.io/couch-economy/)

---

## Pipeline Documentation

### 1. Data Origins

| Source | Indicator | Access Method |
|--------|-----------|---------------|
| WHO Global Health Observatory | Physical inactivity prevalence, adults 18+, age-standardised (`NCD_PAC`) | OData REST API — `ghoapi.azureedge.net/api/` |
| WHO Global Health Observatory | Diabetes prevalence, adults 18+, age-standardised (`NCD_DIABETES_PREVALENCE_AGESTD`) | OData REST API |
| WHO Global Health Observatory | Premature NCD mortality, ages 30–70 (`NCDMORT3070`) — WHO SDG 3.4 indicator | OData REST API |
| World Bank | GDP per capita (`NY.GDP.PCAP.CD`), Gini coefficient (`SI.POV.GINI`), income group classification | `wbstats` R package |
| Our World in Data | Adult obesity prevalence (%) | Public CSV download |
| Google Gemini | On-demand country health briefs | REST API — `generativelanguage.googleapis.com` (gemini-2.5-flash-lite) |

All sources are publicly available aggregate national statistics — no individual-level or protected health information is involved.

### 2. Ingestion

`R/fetch_data.R` handles all data ingestion:

- **WHO GHO (3 indicators)** — fetched via `httr2` with paginated OData requests. The `fetch_who()` helper follows `@odata.nextLink` until all pages are retrieved. Results are filtered by sex dimension (`SEX_BTSX`, `SEX_MLE`, `SEX_FMLE`) and, for diabetes, by age group (`AGEGROUP_YEARS18-PLUS`) to avoid duplicate rows. No authentication required.
- **World Bank** — queried via the `wbstats` package (`wb_data()` and `wb_countries()`). Returns tidy data frames by country-year. No authentication required.
- **OWID** — CSV fetched with `readr::read_csv()` from the public chart URL. No authentication required.
- **Gemini** — called on demand from the Shiny app via `httr2` POST to the Gemini REST API. Requires `GEMINI_API_KEY` in `.env`.

Raw outputs are saved to `data/raw/` as `.rds` files.

### 3. Processing

`R/process_data.R` cleans and joins all sources:

- WHO sex codes recoded to readable labels (`BTSX` → Both, `MLE` → Male, `FMLE` → Female)
- World Bank Gini gaps filled forward/backward within country using `tidyr::fill()` (Gini data is ~15% sparse)
- GDP per capita log-transformed for scatter plot scaling
- ISO-3166 country codes standardised across all sources using the `countrycode` package
- Five datasets joined on `iso3 × year × sex` into a single long-format data frame (13,455 rows, 195 countries, 23 years)
- WHO regional codes (`AFR`, `AMR`, `EMR`, `EUR`, `WPR`) filtered out — these are not countries
- Country display names, regions, and continents added via `countrycode`
- Anomaly detection: inactivity values more than 3 SD from the mean are flagged in the console log
- Output saved to `data/processed/couch_economy.rds`

`R/generate_brief.R` handles AI-generated briefs:

- **Country briefs** — data for the selected country is summarised into a structured prompt with key metrics (inactivity trend, GDP, NCD outcomes, sex filter). Users choose between "policy" (analytical, evidence-focused) and "general public" (plain language, no jargon) tone.
- **Comparison briefs** — when 2–3 countries are selected in the Compare tab, a comparative prompt is built with metrics for all selected countries, asking Gemini to highlight differences, explain divergent outcomes, and suggest a policy lesson.
- All prompts sent to Google Gemini (gemini-2.5-flash-lite) via `httr2`
- Responses cached to `data/briefs/` keyed by `country(s) + data hash + tone + sex + NCD outcome` — repeat calls are instant
- Cache invalidated automatically after each weekly data refresh

### 4. Output

A Shiny app (`app.R`) built with `bslib` (Bootstrap 5) and `plotly`, with five tabs:

**Home tab:** Landing page with a visual guide to the dashboard, key stats (195 countries, 2000–2022, 5 data sources), audience descriptions, feature overview cards, and a glossary of key terms.

**Explorer tab** (three sub-tabs):

- **Overview** — value boxes (country count, average inactivity, average NCD outcome, GDP-inactivity correlation with qualitative label), interactive scatter plot (GDP vs. inactivity, sized by NCD outcome, coloured by income group, OLS trend line). Click any bubble to jump to Country Detail.
- **Year-over-Year** — select two years and view a grouped bar chart showing how inactivity changed. Filter by World Bank income group or view the top 10 countries with the largest absolute change. CSV export included.
- **Country Detail** — country-level dual-axis trend chart (inactivity + selected NCD outcome over time) and AI-generated 200-word health brief (policy or general audience) powered by Google Gemini. Downloads available for trend chart (PNG) and brief (TXT).

**Compare tab:**

- Select 2–3 countries for head-to-head inactivity trend lines, a data snapshot table, NCD outcome comparison bar chart, and an AI-generated comparative brief via Gemini. CSV export of comparison data included.

**Regions tab:**

- Three value boxes (most/least inactive region, highest NCD burden region)
- Regional scatter plot (average GDP vs. average inactivity, bubble size = country count)
- Side-by-side bar charts for inactivity and selected NCD outcome by region
- Dual-axis trend chart (solid lines for inactivity, dashed for NCD) with a multi-select filter to control which of the seven World Bank regions are displayed
- Summary table with CSV export

**About tab:** data sources, pipeline description, full feature list, methodology, and course context.

All tabs include CSV data export. The Explorer sidebar provides global filters for income group, sex, NCD outcome, and year.

### 5. How to Reproduce

**Requirements:** R 4.2+, RStudio (recommended)

**Step 1 — Install packages**

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "ggplot2", "plotly", "scales", "here", "glue",
  "httr2", "jsonlite", "wbstats", "readr", "tidyr", "purrr",
  "countrycode", "digest", "shinycssloaders"
))
```

**Step 2 — Set up environment**

Create a `.env` file in the project root:

```
GEMINI_API_KEY=your_key_here
```

Get a free API key at [aistudio.google.com](https://aistudio.google.com/) (no credit card required).

**Step 3 — Fetch and process data**

```r
source("R/fetch_data.R")    # ~30 sec, fetches from WHO, World Bank, and OWID
source("R/process_data.R")  # ~5 sec, cleans, joins, and validates
```

**Step 4 — Launch the app**

```r
shiny::runApp()
```

**Deployment to shinyapps.io:**

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "...", token = "...", secret = "...")
rsconnect::deployApp(appName = "couch-economy")
```

For automated deployment, add `SHINYAPPS_NAME`, `SHINYAPPS_TOKEN`, `SHINYAPPS_SECRET`, and `GEMINI_API_KEY` as GitHub repository secrets.

---

## Automation

A GitHub Actions workflow (`.github/workflows/refresh.yml`) runs every Monday at 06:00 UTC and:

1. Fetches fresh data from all five API sources
2. Reprocesses and joins the data
3. Invalidates stale cached briefs
4. Commits updated `.rds` files back to the repository
5. Redeploys the app to shinyapps.io

The workflow can also be triggered manually from the GitHub Actions tab.

---

## Project Structure

```
couch-economy/
├── app.R                          # Shiny app (UI + server)
├── R/
│   ├── fetch_data.R               # API ingestion (WHO, World Bank, OWID)
│   ├── process_data.R             # Cleaning, joining, validation
│   └── generate_brief.R           # Gemini brief generation + caching
├── data/
│   ├── raw/                       # Raw API responses (.rds)
│   ├── processed/                 # Joined analysis-ready data
│   └── briefs/                    # Cached LLM country briefs
├── deck/                          # Presentation slides (Quarto)
├── .github/
│   └── workflows/
│       └── refresh.yml            # Weekly GitHub Actions cron job
├── .env.example                   # Environment variable template
├── .gitignore
└── README.md                      # Pipeline documentation (this file)
```

---

## Data Notes

- WHO inactivity data covers 195 countries across 23 years (2000–2022), with annual estimates for both sexes combined, male, and female.
- WHO diabetes data is filtered to the 18+ age group to match the inactivity indicator's population. The API also returns 30+ estimates, which are excluded to prevent duplicate rows on join.
- World Bank Gini data is sparse (~15% missing after forward/backward fill). This is a known limitation of the Gini index.
- Venezuela and Ethiopia lack World Bank income group classifications and are excluded from the scatter plot (which colours by income group) but remain available in trend charts and country briefs.
- The 18 countries flagged as anomalous (inactivity > 3 SD) — notably Kuwait, UAE, Cuba, and South Korea — are real WHO estimates, not data errors.

---

*GLHLTH 562: Data Science and Visualization with R — Final Project, Spring 2026*
*Duke University*
