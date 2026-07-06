#
# FILE_03_UNWEIGHTED_BALANCED_DESCRIPTIVES_08052026.R
#
# PURPOSE: Producing the DiD descriptive table restricted to the BALANCED PANEL
#          (households observed in BOTH Wave 12 AND Wave 18)
#
# BALANCED PANEL LOGIC:
#   Keeping only HH_IDs that appear in BOTH Wave 12 (pre) and Wave 18 (post).
#   This mirrors the within-household identification used in the DiD models
#   (FEOLS with HH_ID FE in FILE_04)
#


library(tidyverse)
library(arrow)
library(here)
library(readr)

# CONFIG 
parquet_dir  <- here("arrow_partitions_CORRECTING_HHS_08052026")
analysis_dir <- here("CORRECTING_HHS_analysis_outputs_08052026")
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)


# LOADING DATA (Waves 12 & 18 only) 
ds <- arrow::open_dataset(parquet_dir, format = "parquet")

cols_of_interest <- c(
  "HH_ID", "STATE", "WAVE_NO",
  "HH_SIZE",
  # Composition
  "ANY_16_59", "ANY_MALE_16_59",
  "D2_NO_ADULT_16_59", "D3_NO_ADULT_MALE_16_59",
  # Health outcomes
  "HHS1", "HHS2", "n_unhealthy",
  "any_hospitalised", "any_on_medication",
  # Income (OECD-MODIFIED-equivalised)
  "equivincafterhealth",
  # Standard income (for poverty threshold comparison)
  "percap_after_health",
  # OOP
  "OOP_HEALTH",
  # Poverty
  "poverty_tendulkar", "poverty_rangarajan",
  # Region (for poverty threshold)
  "REGION_TYPE"
)

cols_existing <- intersect(cols_of_interest, names(ds))

hh_raw <- ds %>%
  filter(WAVE_NO %in% c(12L, 18L)) %>%
  select(all_of(cols_existing)) %>%
  collect() %>%
  as_tibble()

cat("Rows loaded (Waves 12 & 18):", nrow(hh_raw), "\n")
cat("Columns available:", ncol(hh_raw), "\n\n")

# DATA CLEANING 
if ("OOP_HEALTH" %in% names(hh_raw))
  hh_raw <- hh_raw %>%
  mutate(OOP_HEALTH = ifelse(OOP_HEALTH < 0, NA_real_, OOP_HEALTH))

if ("equivincafterhealth" %in% names(hh_raw))
  hh_raw <- hh_raw %>%
  mutate(equivincafterhealth = ifelse(equivincafterhealth < 0, NA_real_, equivincafterhealth))

# Convenience flags
has_oop      <- "OOP_HEALTH"          %in% names(hh_raw) && any(!is.na(hh_raw$OOP_HEALTH))
has_equiv    <- "equivincafterhealth" %in% names(hh_raw) && any(!is.na(hh_raw$equivincafterhealth))
has_pov_rang <- "poverty_rangarajan"  %in% names(hh_raw) && any(!is.na(hh_raw$poverty_rangarajan))

cat("OOP available:",             has_oop,      "\n")
cat("OECD-MODIFIED-equiv avail:", has_equiv,    "\n")
cat("Rangarajan poverty avail:",  has_pov_rang, "\n\n")

# TREATMENT / PERIOD FLAGS 
treated_states <- c(
  "Andhra Pradesh", "Kerala", "Meghalaya", "Haryana", "Punjab", "Rajasthan",
  "Andaman and Nicobar Islands", "Arunachal Pradesh", "Assam", "Bihar",
  "Chandigarh", "Chhattisgarh", "DNH and DD", "Goa", "Gujarat",
  "Himachal Pradesh", "Jammu & Kashmir", "Jharkhand", "Karnataka",
  "Ladakh", "Lakshadweep", "Madhya Pradesh", "Maharashtra", "Manipur",
  "Mizoram", "Nagaland", "Puducherry", "Sikkim", "Tamil Nadu", "Tripura",
  "Uttar Pradesh", "Uttarakhand"
)
control_states <- c("Delhi", "Odisha", "Telangana")

hh_flagged <- hh_raw %>%
  mutate(
    STATE            = as.character(STATE),
    treat_state_simple = case_when(
      STATE %in% treated_states ~ 1L,
      STATE %in% control_states ~ 0L,
      TRUE                      ~ NA_integer_
    ),
    post_wave = case_when(
      WAVE_NO == 12L ~ 0L,
      WAVE_NO == 18L ~ 1L,
      TRUE           ~ NA_integer_
    )
  ) %>%
  filter(!is.na(treat_state_simple), !is.na(post_wave))

cat("After treatment/period flag filter:", nrow(hh_flagged), "rows\n\n")

# BALANCED PANEL CONSTRUCTION 
balanced_ids <- hh_flagged %>%
  group_by(HH_ID) %>%
  summarise(
    n_waves      = n_distinct(WAVE_NO),
    waves_seen   = paste(sort(unique(WAVE_NO)), collapse = "-"),
    n_treat_vals = n_distinct(treat_state_simple),
    treat_vals   = paste(sort(unique(treat_state_simple)), collapse = "-"),
    .groups = "drop"
  ) %>%
  # present in BOTH waves and same treatment-state in both waves
  filter(n_waves == 2L, waves_seen == "12-18", n_treat_vals == 1L) %>%
  pull(HH_ID)

cat("Households in UNBALANCED panel:", n_distinct(hh_flagged$HH_ID), "\n")
cat("Households in BALANCED panel:  ", length(balanced_ids), "\n")
cat("Attrition (dropped):           ",
    n_distinct(hh_flagged$HH_ID) - length(balanced_ids), "\n\n")

hh_balanced <- hh_flagged %>%
  filter(HH_ID %in% balanced_ids)

cat("Rows in balanced panel (household × wave):", nrow(hh_balanced), "\n\n")

# BALANCE DIAGNOSTIC (counting unique households per cell) 
balance_check <- hh_balanced %>%
  distinct(HH_ID, treat_state_simple, post_wave) %>%   # one row per household × period
  count(treat_state_simple, post_wave, name = "n_households") %>%
  mutate(
    group  = if_else(treat_state_simple == 1L, "Treated", "Control"),
    period = if_else(post_wave == 0L, "Pre (Wave 12)", "Post (Wave 18)")
  ) %>%
  arrange(group, period)

cat("BALANCE CHECK (unique households per cell):\n")
print(balance_check)
cat("\n")

write_csv(
  balance_check,
  file.path(analysis_dir, "balance_check_n_households_08052026.csv")
)

# DESCRIPTIVE TABLE (BALANCED PANEL) 
descriptive_balanced <- hh_balanced %>%
  group_by(treat_state_simple, post_wave) %>%
  summarise(
    # Sample size: unique households in each cell
    N_households = n_distinct(HH_ID),
    
    # Household size (continuous)
    hh_size_mean = mean(HH_SIZE, na.rm = TRUE),
    hh_size_sd   = sd(HH_SIZE,   na.rm = TRUE),
    
    # Composition flags (binary 0/1)
    any_16_59_mean          = mean(ANY_16_59,           na.rm = TRUE),
    any_16_59_sd            = sd(ANY_16_59,             na.rm = TRUE),
    
    any_male_16_59_mean     = mean(ANY_MALE_16_59,      na.rm = TRUE),
    any_male_16_59_sd       = sd(ANY_MALE_16_59,        na.rm = TRUE),
    
    no_adult_male_16_59_mean = mean(D3_NO_ADULT_MALE_16_59, na.rm = TRUE),
    no_adult_male_16_59_sd   = sd(D3_NO_ADULT_MALE_16_59,   na.rm = TRUE),
    
    # Health outcomes
    HHS1_mean = mean(HHS1, na.rm = TRUE),
    HHS1_sd   = sd(HHS1,   na.rm = TRUE),
    
    HHS2_mean = mean(HHS2, na.rm = TRUE),
    HHS2_sd   = sd(HHS2,   na.rm = TRUE),
    
    # Poverty
    pov_tendulkar_mean  = mean(poverty_tendulkar, na.rm = TRUE),
    pov_tendulkar_sd    = sd(poverty_tendulkar,   na.rm = TRUE),
    
    pov_rangarajan_mean = if (has_pov_rang) mean(poverty_rangarajan, na.rm = TRUE) else NA_real_,
    pov_rangarajan_sd   = if (has_pov_rang) sd(poverty_rangarajan,   na.rm = TRUE) else NA_real_,
    
    # Income: OECD-MODIFIED-equivalised after health
    equiv_inc_after_health_mean = if (has_equiv) mean(equivincafterhealth, na.rm = TRUE) else NA_real_,
    equiv_inc_after_health_sd   = if (has_equiv) sd(equivincafterhealth,   na.rm = TRUE) else NA_real_,
    
    # OOP health spending
    OOP_health_mean = if (has_oop) mean(OOP_HEALTH, na.rm = TRUE) else NA_real_,
    OOP_health_sd   = if (has_oop) sd(OOP_HEALTH,   na.rm = TRUE) else NA_real_,
    
    # Healthcare utilisation
    any_hospitalised_mean   = mean(any_hospitalised,  na.rm = TRUE),
    any_hospitalised_sd     = sd(any_hospitalised,    na.rm = TRUE),
    
    any_on_medication_mean  = mean(any_on_medication, na.rm = TRUE),
    any_on_medication_sd    = sd(any_on_medication,   na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    group  = if_else(treat_state_simple == 1L, "Treated", "Control"),
    period = if_else(post_wave == 0L, "Pre (Wave 12)", "Post (Wave 18)")
  ) %>%
  arrange(group, period)

cat("DESCRIPTIVE TABLE — BALANCED PANEL (Wave 12 & 18):\n")
print(descriptive_balanced, n = Inf)

write_csv(
  descriptive_balanced,
  file.path(analysis_dir, "Descriptive_table_w12_w18_BALANCED_unweighted_08052026.csv")
)

cat("\nSaved: Descriptive_table_w12_w18_BALANCED_unweighted_08052026.csv\n")
cat("Saved: balance_check_n_households_08052026.csv\n")
cat("\nSCRIPT COMPLETE\n")
