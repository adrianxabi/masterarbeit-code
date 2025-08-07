# =======================================================================
#  compute_dY.R
#  ----------------------------------------------------------
#  Calculates ΔY_t for Australia and South Korea
#  (1997-Q2 and 2019-Q4) using
#    • OECD quarterly M1, G, GDP series (in df_full.csv)
#    • Leightner (2024) elasticities
#  Output: tibble with country, quarter, dY_pct
# =======================================================================

library(tidyverse)

# 1. ---------------------------------------------------------------------
# Load data
# df_full.csv must contain at least:
#   country | quarter | M1 | dM1 | G | dG
# If dM1 / dG are missing, the script will compute them from lags.
# ------------------------------------------------------------------------
df <- read_csv("df_full.csv")

# 2. ---------------------------------------------------------------------
# Leightner (2024) elasticities: money (eps_M1) and fiscal (dGDP_dG)
# ------------------------------------------------------------------------
multis <- tribble(
  ~country,   ~quarter,  ~eps_M1, ~dGDP_dG,
  "Australia","1997-Q2",  1.47,    5.91,
  "Australia","2019-Q4",  1.13,    4.85,
  "Korea",    "1997-Q2",  1.00,   14.41,
  "Korea",    "2019-Q4",  1.23,    6.64
)

# 3. ---------------------------------------------------------------------
# Nominal GDP for the same quarters (national currency)
# ------------------------------------------------------------------------
gdps <- tribble(
  ~country,   ~quarter,                ~GDP,
  "Australia","1997-Q2",  1221451000000,
  "Korea",    "1997-Q2",   861573300000000,
  "Australia","2019-Q4",  2359119000000,
  "Korea",    "2019-Q4",  2072982400000000
)

# 4. ---------------------------------------------------------------------
# Compute relative quarterly changes if dM1 / dG are absent
# ------------------------------------------------------------------------
df_rates <- df %>%
  arrange(country, quarter) %>%
  group_by(country) %>%
  mutate(
    dM1_rel = if_else(
      "dM1" %in% names(.),
      dM1 / lag(M1),
      (M1 - lag(M1)) / lag(M1)
    ),
    dG_rel  = if_else(
      "dG" %in% names(.),
      dG / lag(G),
      (G  - lag(G )) / lag(G )
    )
  ) %>%
  ungroup()

# 5. ---------------------------------------------------------------------
# Calculate ΔY_t in percentage points
# ------------------------------------------------------------------------
results_dY <- df_rates %>%
  semi_join(multis, by = c("country", "quarter")) %>%      # select 4 obs
  left_join(multis, by = c("country", "quarter")) %>%
  left_join(gdps,   by = c("country", "quarter")) %>%
  mutate(
    eps_G_pct = (G / GDP) * dGDP_dG,                       # fiscal multiplier in %
    dY_pct    = eps_M1 * dM1_rel + eps_G_pct * dG_rel      # total output impulse
  ) %>%
  select(country, quarter, dY_pct)

print(results_dY)

# Optional: write to CSV
# write_csv(results_dY, "dY_results.csv")
