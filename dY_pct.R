# =======================================================================
#  compute_dY.R
#  ----------------------------------------------------------
#  Calculates ΔY_t for Australia and South Korea
#  (1997-Q2 and 2019-Q4) using
#    • df_full.csv  (quarterly M1, dM1, G, dG, …)
#    • Leightner (2024) elasticities
#    • nominal GDP levels (hard-coded below)
#  Output: tibble with country, quarter, dY_pct
# =======================================================================

library(tidyverse)

# 1. Load data -----------------------------------------------------------
df <- read_csv("df_full.csv")            # must contain: country, quarter, M1, dM1, G, dG

# 2. Elasticities (Leightner 2024) --------------------------------------
multis <- tribble(
  ~country,   ~quarter,  ~eps_M1, ~dGDP_dG,
  "Australia","1997-Q2",  1.47,    5.91,
  "Australia","2019-Q4",  1.13,    4.85,
  "Korea",    "1997-Q2",  1.00,   14.41,
  "Korea",    "2019-Q4",  1.23,    6.64
)

# 3. Nominal GDP levels --------------------------------------------------
gdps <- tribble(
  ~country,   ~quarter,                ~GDP,
  "Australia","1997-Q2",  1221451000000,
  "Korea",    "1997-Q2",   861573300000000,
  "Australia","2019-Q4",  2359119000000,
  "Korea",    "2019-Q4",  2072982400000000
)

# 4. ΔY_t calculation ----------------------------------------------------
results_dY <- df %>% 
  arrange(country, quarter) %>% 
  group_by(country) %>% 
  mutate(
    dM1_rel = dM1 / lag(M1),     # ΔM1_t / M1_{t-1}
    dG_rel  = dG  / lag(G)       # ΔG_t  / G_{t-1}
  ) %>% 
  ungroup() %>% 
  inner_join(multis, by = c("country","quarter")) %>% 
  inner_join(gdps,   by = c("country","quarter")) %>% 
  mutate(
    eps_G_pct = (G / GDP) * dGDP_dG,              # fiscal multiplier in %
    dY_pct    = eps_M1 * dM1_rel + eps_G_pct * dG_rel
  ) %>% 
  select(country, quarter, dY_pct)

print(results_dY)
