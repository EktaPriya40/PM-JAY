# FILE_05_SIMPLE_2X2_DiD_ADJUSTED_UNWEIGHTED_08052026.R
#
# COVARIATE-ADJUSTED 2x2 DiD: Wave 12 (pre) vs Wave 18 (post)
# Unweighted, OECD-modified equivalised income
#

library(dplyr)
library(arrow)
library(here)
library(fixest)
library(broom)
library(readr)

cat("PMJAY DiD ANALYSIS - COVARIATE-ADJUSTED (OECD MODIFIED, UNWEIGHTED)\n")

# Directories 
parquet_dir <- here("arrow_partitions_CORRECTING_HHS_08052026")
analysis_dir <- here("CORRECTING_HHS_analysis_outputs_08052026")

cat("Loading data from:", parquet_dir, "\n\n")

# 1. LOAD UPRATED POVERTY LINES (UNWEIGHTED)

poverty_tendulkar_uprated <- read_csv(
  file.path(analysis_dir, "tendulkar_poverty_lines_uprated_unweighted_08052026.csv"),
  show_col_types = FALSE
) %>%
  select(WAVE_NO, REGION_TYPE, poverty_line_uprated) %>%
  rename(poverty_line_tendulkar = poverty_line_uprated)

poverty_rangarajan_uprated <- read_csv(
  file.path(analysis_dir, "rangarajan_poverty_lines_uprated_unweighted_08052026.csv"),
  show_col_types = FALSE
) %>%
  select(WAVE_NO, REGION_TYPE, poverty_line_uprated) %>%
  rename(poverty_line_rangarajan = poverty_line_uprated)

cat("Uprated poverty lines loaded (unweighted).\n\n")

# 2. LOAD HOUSEHOLD DATA FOR WAVES 12 AND 18 (UNWEIGHTED)

ds <- arrow::open_dataset(parquet_dir, format = "parquet")

hh_core <- ds %>%
  select(
    STATE, WAVE_NO, HH_ID, HH_SIZE, REGION_TYPE,
    # Health outcomes
    n_unhealthy, HHS1, HHS2, health_bad_any,
    any_hospitalised, any_on_medication,
    # Financial outcomes (OECD-modified)
    equivincafterhealth, equivincmonth,
    net_income_month, percap_after_health, OOP_HEALTH,
    # Poverty flags (if present)
    poverty_tendulkar, poverty_rangarajan,
    # Composition covariates
    ANY_16_59, ANY_MALE_16_59,
    D2_NO_ADULT_16_59, D3_NO_ADULT_MALE_16_59,
    # Policy exposure (from pipeline)
    PMJAY_STATUS
  ) %>%
  filter(
    WAVE_NO %in% c(12L, 18L),
    !is.na(HH_SIZE),
    HH_SIZE > 0
  ) %>%
  collect()

cat("Total observations loaded:", nrow(hh_core), "\n\n")

# 3. STANDARDISE REGION NAMES AND MERGE UPRATED POVERTY LINES

hh_core <- hh_core %>%
  mutate(
    REGION_TYPE_clean = case_when(
      tolower(trimws(REGION_TYPE)) == "rural" ~ "Rural",
      tolower(trimws(REGION_TYPE)) == "urban" ~ "Urban",
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(
    poverty_tendulkar_uprated,
    by = c("WAVE_NO", "REGION_TYPE_clean" = "REGION_TYPE")
  ) %>%
  left_join(
    poverty_rangarajan_uprated,
    by = c("WAVE_NO", "REGION_TYPE_clean" = "REGION_TYPE")
  ) %>%
  mutate(
    poverty_tendulkar_uprated = case_when(
      !is.na(percap_after_health) & !is.na(poverty_line_tendulkar) &
        percap_after_health < poverty_line_tendulkar ~ 1L,
      !is.na(percap_after_health) & !is.na(poverty_line_tendulkar) &
        percap_after_health >= poverty_line_tendulkar ~ 0L,
      TRUE ~ NA_integer_
    ),
    poverty_rangarajan_uprated = case_when(
      !is.na(percap_after_health) & !is.na(poverty_line_rangarajan) &
        percap_after_health < poverty_line_rangarajan ~ 1L,
      !is.na(percap_after_health) & !is.na(poverty_line_rangarajan) &
        percap_after_health >= poverty_line_rangarajan ~ 0L,
      TRUE ~ NA_integer_
    )
  )

cat("Poverty indicators (uprated) constructed.\n\n")

# 4. DEFINE TREATED AND CONTROL STATES 

treated_states <- c(
  "Andhra Pradesh", "Kerala", "Meghalaya", "Haryana", "Punjab", "Rajasthan",
  "Andaman and Nicobar Islands", "Arunachal Pradesh", "Assam", "Bihar",
  "Chandigarh", "Chhattisgarh", "DNH and DD", "Goa", "Gujarat",
  "Himachal Pradesh", "Jammu & Kashmir", "Jharkhand", "Karnataka", "Ladakh",
  "Lakshadweep", "Madhya Pradesh", "Maharashtra", "Manipur", "Mizoram",
  "Nagaland", "Puducherry", "Sikkim", "Tamil Nadu", "Tripura",
  "Uttar Pradesh", "Uttarakhand"
)

control_states <- c("Delhi", "Odisha", "Telangana")

# 5. BUILD COVARIATE-ADJUSTED PANEL (UNWEIGHTED)

phase1_cov <- hh_core %>%
  mutate(
    STATE = factor(STATE),
    treat_state_simple = case_when(
      STATE %in% treated_states ~ 1L,
      STATE %in% control_states ~ 0L,
      TRUE ~ NA_integer_
    ),
    post_wave = case_when(
      WAVE_NO == 12L ~ 0L,
      WAVE_NO == 18L ~ 1L,
      TRUE ~ NA_integer_
    ),
    OOP_share = ifelse(
      !is.na(OOP_HEALTH) & !is.na(net_income_month) & net_income_month > 0,
      OOP_HEALTH / net_income_month,
      NA_real_
    ),
    log_equivinc = log1p(equivincmonth),
    urban = if_else(REGION_TYPE_clean == "Urban", 1L, 0L)
  ) %>%
  filter(
    !is.na(treat_state_simple),
    !is.na(post_wave)
  )

cat("PHASE 1 COVARIATE PANEL SUMMARY\n")
cat("Total observations:", nrow(phase1_cov), "\n\n")

# 6. DEFINE COVARIATES AND RHS

covars <- ~ HH_SIZE + ANY_16_59 + ANY_MALE_16_59 +
  D2_NO_ADULT_16_59 + D3_NO_ADULT_MALE_16_59 +
  log_equivinc + urban

covar_terms <- attr(terms(covars), "term.labels")
covar_rhs <- paste(c("treat_state_simple * post_wave", covar_terms), collapse = " + ")

cat("Covariates included:\n")
print(covar_terms)
cat("\n")

cat("ESTIMATING COVARIATE-ADJUSTED DiD MODELS (UNWEIGHTED)\n\n")

# 7. HHS1 (Binary health outcome) - covariate-adjusted LPM

phase1_hhs1 <- phase1_cov %>%
  filter(!is.na(HHS1))

form_hhs1 <- as.formula(paste("HHS1 ~", covar_rhs, "| HH_ID + WAVE_NO"))

lpm_hhs1_cov <- feols(
  form_hhs1,
  data = phase1_hhs1,
  cluster = ~ STATE
)

cat("HHS1 (binary health index) - Covariate-adjusted LPM\n")
print(summary(lpm_hhs1_cov))
cat("\n")

# 8. POVERTY (TENDULKAR, UPRATED) - covariate-adjusted LPM

phase1_pov_tend <- phase1_cov %>%
  filter(!is.na(poverty_tendulkar_uprated))

form_pov_tend <- as.formula(
  paste("poverty_tendulkar_uprated ~", covar_rhs, "| HH_ID + WAVE_NO")
)

lpm_pov_tend_cov <- feols(
  form_pov_tend,
  data = phase1_pov_tend,
  cluster = ~ STATE
)

cat("Poverty (Tendulkar, uprated) - Covariate-adjusted LPM\n")
print(summary(lpm_pov_tend_cov))
cat("\n")

# 9. OECD-MODIFIED EQUIVALISED INCOME AFTER HEALTH - covariate-adjusted linear DiD

phase1_eqinc <- phase1_cov %>%
  filter(!is.na(equivincafterhealth))

form_eqinc <- as.formula(
  paste("equivincafterhealth ~", covar_rhs, "| HH_ID + WAVE_NO")
)

lin_eqinc_cov <- feols(
  form_eqinc,
  data = phase1_eqinc,
  cluster = ~ STATE
)

cat("OECD Modified Equivalised Income After Health - Covariate-adjusted Linear DiD\n")
print(summary(lin_eqinc_cov))
cat("\n")

# 10. OOP HEALTH SPENDING - covariate-adjusted linear DiD

phase1_oop <- phase1_cov %>%
  filter(!is.na(OOP_HEALTH))

form_oop <- as.formula(
  paste("OOP_HEALTH ~", covar_rhs, "| HH_ID + WAVE_NO")
)

lin_oop_cov <- feols(
  form_oop,
  data = phase1_oop,
  cluster = ~ STATE
)

cat("OOP Health Spending - Covariate-adjusted Linear DiD\n")
print(summary(lin_oop_cov))
cat("\n")

# 11. OOP SHARE OF INCOME - covariate-adjusted linear DiD

phase1_oopshare <- phase1_cov %>%
  filter(!is.na(OOP_share))

form_oopshare <- as.formula(
  paste("OOP_share ~", covar_rhs, "| HH_ID + WAVE_NO")
)

lin_oopshare_cov <- feols(
  form_oopshare,
  data = phase1_oopshare,
  cluster = ~ STATE
)

cat("OOP Share of Income - Covariate-adjusted Linear DiD\n")
print(summary(lin_oopshare_cov))
cat("\n")

# 12. ANY HOSPITALISED - covariate-adjusted LPM

phase1_hosp <- phase1_cov %>%
  filter(!is.na(any_hospitalised))

form_hosp <- as.formula(
  paste("any_hospitalised ~", covar_rhs, "| HH_ID + WAVE_NO")
)

lpm_hosp_cov <- feols(
  form_hosp,
  data = phase1_hosp,
  cluster = ~ STATE
)

cat("Any Hospitalised - Covariate-adjusted LPM\n")
print(summary(lpm_hosp_cov))
cat("\n")

# 13. ANY ON MEDICATION - covariate-adjusted LPM

phase1_med <- phase1_cov %>%
  filter(!is.na(any_on_medication))

form_med <- as.formula(
  paste("any_on_medication ~", covar_rhs, "| HH_ID + WAVE_NO")
)

lpm_med_cov <- feols(
  form_med,
  data = phase1_med,
  cluster = ~ STATE
)

cat("Any on Medication - Covariate-adjusted LPM\n")
print(summary(lpm_med_cov))
cat("\n")

# 14. SUMMARY TABLE OF COVARIATE-ADJUSTED DiD EFFECTS
# Binary/LPM outcomes reported in percentage points (pp), 
# Continuous outcomes in original units (₹/month)

did_term <- "treat_state_simple:post_wave"

results_summary_cov <- tibble::tibble(
  outcome = c(
    "HHS1 (LPM, covariate-adjusted)",
    "Poverty (Tendulkar, covariate-adjusted)",
    "OECD Modified Eqinc After Health (₹, covariate-adjusted)",
    "OOP Health Spending (₹, covariate-adjusted)",
    "OOP Share (covariate-adjusted)",
    "Any Hospitalised (covariate-adjusted)",
    "Any on Medication (covariate-adjusted)"
  ),
  did_effect = c(
    round(coef(lpm_hhs1_cov)[did_term] * 100, 2),
    round(coef(lpm_pov_tend_cov)[did_term] * 100, 2),
    round(coef(lin_eqinc_cov)[did_term], 2),
    round(coef(lin_oop_cov)[did_term], 2),
    round(coef(lin_oopshare_cov)[did_term], 4),
    round(coef(lpm_hosp_cov)[did_term] * 100, 2),
    round(coef(lpm_med_cov)[did_term] * 100, 2)
  ),
  unit = c(
    "pp", "pp",
    "₹/month", "₹/month",
    "proportion", "pp", "pp"
  ),
  specification = c(
    "Linear Probability Model (covariate-adjusted)",
    "Linear Probability Model (covariate-adjusted)",
    "Linear (covariate-adjusted)",
    "Linear (covariate-adjusted)",
    "Linear (covariate-adjusted)",
    "Linear Probability (covariate-adjusted)",
    "Linear Probability (covariate-adjusted)"
  )
)

print(results_summary_cov)
cat("\n")

# 15. SAVE PANEL AND MODELS (UNWEIGHTED, COVARIATE-ADJUSTED)

if (!dir.exists(analysis_dir)) dir.create(analysis_dir, recursive = TRUE)

write_rds(
  phase1_cov,
  file.path(
    analysis_dir,
    "phase1_panel_covariates_OECD_Modified_V18_unweighted_08052026.rds"
  )
)
cat(" Saved: phase1_panel_covariates_OECD_Modified_V18_unweighted_08052026.rds\n")

write_rds(
  list(
    lpm_hhs1_cov = lpm_hhs1_cov,
    lpm_pov_tend_cov = lpm_pov_tend_cov,
    lin_eqinc_cov = lin_eqinc_cov,
    lin_oop_cov = lin_oop_cov,
    lin_oopshare_cov = lin_oopshare_cov,
    lpm_hosp_cov = lpm_hosp_cov,
    lpm_med_cov = lpm_med_cov,
    results_summary_cov = results_summary_cov
  ),
  file.path(
    analysis_dir,
    "phase1_covariate_models_OECD_Modified_V18_unweighted_08052026.rds"
  )
)
cat(" Saved: phase1_covariate_models_OECD_Modified_V18_unweighted_08052026.rds\n")

# 16. SAVE CSV SUMMARIES (UNWEIGHTED)

write_csv(
  results_summary_cov,
  file.path(
    analysis_dir,
    "phase1_results_summary_covariate_adjusted_OECD_Modified_V18_unweighted_08052026.csv"
  )
)
cat(" Saved: phase1_results_summary_covariate_adjusted_OECD_Modified_V18_unweighted_08052026.csv\n\n")

# 17. COMPACT RESULTS TABLE WITH CIs IN PERCENTAGE POINTS
# Binary outcomes: coeff, SE, CI all multiplied by 100 (pp)
# Continuous outcomes: rounded to 2dp in original units

results_cov_table <- tibble::tibble(
  Outcome = c(
    "HHS1 (Any Unhealthy)",
    "Poverty (Tendulkar)",
    "Eqvinc After Health (₹/mo)",
    "OOP Health (₹/mo)",
    "Hospitalised",
    "Medication Use"
  ),
  DiD_Coef = c(
    coef(lpm_hhs1_cov)[did_term] * 100,
    coef(lpm_pov_tend_cov)[did_term] * 100,
    coef(lin_eqinc_cov)[did_term],
    coef(lin_oop_cov)[did_term],
    coef(lpm_hosp_cov)[did_term] * 100,
    coef(lpm_med_cov)[did_term] * 100
  ),
  SE = c(
    sqrt(vcov(lpm_hhs1_cov)[did_term, did_term]) * 100,
    sqrt(vcov(lpm_pov_tend_cov)[did_term, did_term]) * 100,
    sqrt(vcov(lin_eqinc_cov)[did_term, did_term]),
    sqrt(vcov(lin_oop_cov)[did_term, did_term]),
    sqrt(vcov(lpm_hosp_cov)[did_term, did_term]) * 100,
    sqrt(vcov(lpm_med_cov)[did_term, did_term]) * 100
  )
) %>%
  mutate(
    t_stat = DiD_Coef / SE,
    p_value = 2 * pt(-abs(t_stat), Inf),
    CI_Lower = DiD_Coef - 1.96 * SE,
    CI_Upper = DiD_Coef + 1.96 * SE,
    
    DiD_Coef = round(DiD_Coef, 2),
    SE       = round(SE, 2),
    t_stat   = round(t_stat, 2),
    p_value  = round(p_value, 4),
    CI       = paste0("[", round(CI_Lower, 2), ", ", round(CI_Upper, 2), "]"),
    
    Sig = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1  ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(Outcome, DiD_Coef, SE, t_stat, p_value, CI, Sig)

write.csv(
  results_cov_table,
  file.path(
    analysis_dir,
    "Summary_ADJUSTED_DiD_Results_V18_unweighted_08052026.csv"
  ),
  row.names = FALSE
)

cat("\n")
cat("ADJUSTED DiD RESULTS (Wave 12 vs 18, OECD Modified, Unweighted)\n")
cat("Binary outcomes reported in percentage points (pp); continuous in ₹/month\n\n")
print(results_cov_table)
cat("\nSignificance: *** p<0.01, ** p<0.05, * p<0.10\n")
cat("CI: 95% Confidence Interval\n")

cat(" Saved: Summary_ADJUSTED_DiD_Results_V18_unweighted_08052026.csv\n\n")

cat("COVARIATE-ADJUSTED ANALYSIS COMPLETE (UNWEIGHTED)\n")
cat("Output directory:", analysis_dir, "\n")
