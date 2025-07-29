library(tidyverse)

# 1) Read raw quarterly data
exports_raw <- read_csv("Exports.csv", show_col_types = FALSE)
imports_raw <- read_csv("Imports.csv", show_col_types = FALSE)
gdp_raw     <- read_csv("GDP.csv",     show_col_types = FALSE)

# 2) Filter for P6=Exports, P7=Imports; Australia & South Korea
exports_clean <- exports_raw %>%
  filter(TRANSACTION == "P6", REF_AREA %in% c("AUS", "KOR")) %>%
  select(
    country    = REF_AREA,
    Quarter    = TIME_PERIOD,
    Export_val = OBS_VALUE
  )

imports_clean <- imports_raw %>%
  filter(TRANSACTION == "P7", REF_AREA %in% c("AUS", "KOR")) %>%
  select(
    country    = REF_AREA,
    Quarter    = TIME_PERIOD,
    Import_val = OBS_VALUE
  )

gdp_clean <- gdp_raw %>%
  filter(REF_AREA %in% c("AUS", "KOR")) %>%
  select(
    country = REF_AREA,
    Quarter = TIME_PERIOD,
    GDP_val = OBS_VALUE
  )

# 3) Compute openness = (Exports + Imports) / GDP
openness_all <- exports_clean %>%
  inner_join(imports_clean, by = c("country", "Quarter")) %>%
  inner_join(gdp_clean,     by = c("country", "Quarter")) %>%
  mutate(
    exports_pctGDP = 100 * Export_val / GDP_val,
    imports_pctGDP = 100 * Import_val  / GDP_val,
    openness       = exports_pctGDP + imports_pctGDP
  )

# 4) Select exactly the two pre-crisis quarters per country
selected <- openness_all %>%
  filter(
    Quarter %in% c("1997-Q2", "2019-Q4"),
    country %in% c("AUS", "KOR")
  ) %>%
  distinct(country, Quarter, .keep_all = TRUE)

# 5) Show the final 4 rows
print(selected)

# 6) ... and save them for Dynare
write_csv(selected, "openness_crisis_pre.csv")

