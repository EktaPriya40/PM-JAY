# File_04_UNWEIGHTED_Simple_DiD_OECD_Modified_EQINC_V18_UNADJUSTED_08052026.R
#
# SIMPLE 2x2 DiD: Wave 12 (pre) vs Wave 18 (post)
# Treatment: states that adopted PMJAY by Wave 18
# Control: states (Delhi, Odisha, Telangana) that never adopted PMJAY
#
# Both Tendulkar & Rangarajan poverty thresholds
# with uprated poverty lines by wave & region

library(dplyr)
library(arrow)
library(here)
library(fixest)
library(broom)
library(marginaleffects)
library(tidyr)
library(readr)

cat("PMJAY DiD ANALYSIS\n")


parquet_dir <- here("arrow_partitions_CORRECTING_HHS_08052026")
ds <- arrow::open_dataset(parquet_dir, format = "parquet")

cat("Loading data from:", parquet_dir, "\n\n")

# 1. LOAD UPRATED POVERTY LINES

analysis_dir <- here("CORRECTING_HHS_analysis_outputs_08052026")

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

cat("Uprated poverty lines loaded:\n")
cat("  - Tendulkar (Wave 12 Rural): Rs",
    poverty_tendulkar_uprated %>%
      filter(WAVE_NO == 12, REGION_TYPE == "Rural") %>%
      pull(poverty_line_tendulkar),
    "\n")
cat("  - Rangarajan (Wave 12 Rural): Rs",
    poverty_rangarajan_uprated %>%
      filter(WAVE_NO == 12, REGION_TYPE == "Rural") %>%
      pull(poverty_line_rangarajan),
    "\n\n")

# 2. EXTRACTING DATA FOR WAVES 12 AND 18

hh_core <- ds %>%
  select(
    STATE, WAVE_NO, HH_ID, HH_SIZE, REGION_TYPE,
    n_unhealthy, HHS1, HHS2, health_bad_any,
    equivincafterhealth, net_income_month, percap_after_health,
    poverty_tendulkar, OOP_HEALTH,
    any_hospitalised, any_on_medication
  ) %>%
  filter(
    WAVE_NO %in% c(12L, 18L),
    !is.na(HH_SIZE),
    HH_SIZE > 0
  ) %>%
  collect()

cat("Total observations loaded:", nrow(hh_core), "\n\n")

# 3. STANDARDISE REGION NAMES AND ADD UPRATED POVERTY LINES

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
  )

hh_core <- hh_core %>%
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

cat("Poverty indicators created using uprated thresholds:\n")
cat("  - Tendulkar (uprated)\n")
cat("  - Rangarajan (uprated, robustness check)\n\n")

# 4. CREATE DiD PANEL

treated_states <- c(
  "Andhra Pradesh", "Kerala", "Meghalaya", "Haryana", "Punjab", "Rajasthan",
  "Andaman and Nicobar Islands", "Arunachal Pradesh", "Assam", "Bihar",
  "Chandigarh", "Chhattisgarh", "DNH and DD", "Goa", "Gujarat",
  "Himachal Pradesh", "Jammu & Kashmir", "Jharkhand", "Karnataka", "Ladakh",
  "Lakshadweep", "Madhya Pradesh", "Maharashtra", "Manipur", "Mizoram", "Nagaland",
  "Puducherry", "Sikkim", "Tamil Nadu", "Tripura", "Uttar Pradesh", "Uttarakhand"
)

control_states <- c("Delhi", "Odisha", "Telangana")

cat("Treatment states :", length(treated_states), "\n")
cat("Control states :", paste(control_states, collapse = ", "), "\n\n")

phase1_panel <- hh_core %>%
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
    )
  ) %>%
  filter(
    !is.na(treat_state_simple),
    !is.na(post_wave)
  )

cat("PHASE 1 PANEL SUMMARY \n")
cat("Total observations:", nrow(phase1_panel), "\n")
cat("Treated state-waves:", sum(phase1_panel$treat_state_simple == 1), "\n")
cat("Control state-waves:", sum(phase1_panel$treat_state_simple == 0), "\n")
cat("Pre-period (Wave 12):", sum(phase1_panel$post_wave == 0), "\n")
cat("Post-period (Wave 18):", sum(phase1_panel$post_wave == 1), "\n\n")

# 5. HEALTH OUTCOMES: HHS1 (Binary: Any Unhealthy Member)

cat("HEALTH OUTCOME: HHS1 (Any Unhealthy Member)\n")

phase1_hhs1 <- phase1_panel %>% filter(!is.na(HHS1))

cat("HHS1 prevalence:\n")
print(table(phase1_hhs1$HHS1, useNA = "ifany"))
cat("\n")

glm_hhs1 <- glm(
  HHS1 ~ treat_state_simple * post_wave,
  family = binomial(link = "logit"),
  data = phase1_hhs1
)

cat("Logistic Regression DiD: HHS1 \n")
print(summary(glm_hhs1))

cat("\nDiD Effect on Probability Scale (Logit) \n")

pred_hhs1 <- predictions(
  glm_hhs1,
  newdata = datagrid(
    treat_state_simple = 0:1,
    post_wave = 0:1
  )
)

pred_table_hhs1 <- pred_hhs1 %>%
  tibble::as_tibble() %>%
  select(treat_state_simple, post_wave, estimate) %>%
  arrange(post_wave, treat_state_simple)

cat("\nPredicted probabilities:\n")
print(pred_table_hhs1)

p_treat_post <- pred_table_hhs1 %>% filter(treat_state_simple == 1, post_wave == 1) %>% pull(estimate)
p_treat_pre  <- pred_table_hhs1 %>% filter(treat_state_simple == 1, post_wave == 0) %>% pull(estimate)
p_control_post <- pred_table_hhs1 %>% filter(treat_state_simple == 0, post_wave == 1) %>% pull(estimate)
p_control_pre  <- pred_table_hhs1 %>% filter(treat_state_simple == 0, post_wave == 0) %>% pull(estimate)

did_logit_hhs1 <- (p_treat_post - p_treat_pre) - (p_control_post - p_control_pre)
did_logit_hhs1_pp <- did_logit_hhs1 * 100

cat("\nDiD Effect (Logit, Probability Scale):", round(did_logit_hhs1_pp, 2), "pp\n\n")

lpm_hhs1 <- fixest::feols(
  HHS1 ~ treat_state_simple * post_wave | HH_ID + WAVE_NO,
  data = phase1_hhs1,
  cluster = ~ STATE
)

cat("Linear Probability Model: HHS1 \n")
print(summary(lpm_hhs1))

did_lpm_hhs1_pp <- coef(lpm_hhs1)["treat_state_simple:post_wave"] * 100
cat("\nDiD Effect (LPM):", round(did_lpm_hhs1_pp, 2), "pp\n\n")

# 6. HEALTH OUTCOMES: HHS2 (Proportion of Unhealthy Members)

cat("HEALTH OUTCOME: HHS2 (Proportion Unhealthy)\n")

phase1_hhs2 <- phase1_panel %>%
  filter(
    !is.na(n_unhealthy),
    !is.na(HH_SIZE),
    HH_SIZE > 0
  ) %>%
  mutate(n_healthy = pmax(HH_SIZE - n_unhealthy, 0L))

cat("HHS2 summary:\n")
print(summary(phase1_hhs2$HHS2))
cat("\n")

glm_hhs2 <- glm(
  cbind(n_unhealthy, n_healthy) ~ treat_state_simple * post_wave,
  family = binomial(link = "logit"),
  data = phase1_hhs2
)

cat("Binomial Logistic Regression DiD: HHS2 \n")
print(summary(glm_hhs2))

cat("\nDiD Effect on Probability Scale (Binomial Logit) \n")

pred_hhs2 <- predictions(
  glm_hhs2,
  newdata = datagrid(
    treat_state_simple = 0:1,
    post_wave = 0:1
  )
)

pred_table_hhs2 <- pred_hhs2 %>%
  tibble::as_tibble() %>%
  select(treat_state_simple, post_wave, estimate) %>%
  arrange(post_wave, treat_state_simple)

cat("\nPredicted probabilities:\n")
print(pred_table_hhs2)

p_treat_post_2 <- pred_table_hhs2 %>% filter(treat_state_simple == 1, post_wave == 1) %>% pull(estimate)
p_treat_pre_2  <- pred_table_hhs2 %>% filter(treat_state_simple == 1, post_wave == 0) %>% pull(estimate)
p_control_post_2 <- pred_table_hhs2 %>% filter(treat_state_simple == 0, post_wave == 1) %>% pull(estimate)
p_control_pre_2  <- pred_table_hhs2 %>% filter(treat_state_simple == 0, post_wave == 0) %>% pull(estimate)

did_logit_hhs2 <- (p_treat_post_2 - p_treat_pre_2) - (p_control_post_2 - p_control_pre_2)
did_logit_hhs2_pp <- did_logit_hhs2 * 100

cat("\nDiD Effect (Binomial Logit, Probability Scale):", round(did_logit_hhs2_pp, 2), "pp\n\n")

lpm_hhs2 <- fixest::feols(
  HHS2 ~ treat_state_simple * post_wave | HH_ID + WAVE_NO,
  data = phase1_hhs2,
  cluster = ~ STATE
)

cat("Linear Model: HHS2 (Proportion Unhealthy)\n")
print(summary(lpm_hhs2))

did_lpm_hhs2_pp <- coef(lpm_hhs2)["treat_state_simple:post_wave"] * 100
cat("\nDiD Effect (HHS2 LPM):", round(did_lpm_hhs2_pp, 2), "pp\n\n")

# 7. POVERTY STATUS - MAIN SPECIFICATION (TENDULKAR - UPRATED)

cat("POVERTY STATUS: TENDULKAR (UPRATED BY WAVE & REGION) - MAIN\n")

phase1_pov_tendulkar <- phase1_panel %>%
  filter(!is.na(poverty_tendulkar_uprated))

cat("Poverty prevalence (Tendulkar, uprated):\n")
print(table(phase1_pov_tendulkar$poverty_tendulkar_uprated, useNA = "ifany"))
cat("\n")

lpm_pov_tendulkar <- fixest::feols(
  poverty_tendulkar_uprated ~ treat_state_simple * post_wave | HH_ID + WAVE_NO,
  data = phase1_pov_tendulkar,
  cluster = ~ STATE
)

cat("Linear Probability Model: Poverty (Tendulkar, Uprated) \n")
print(summary(lpm_pov_tendulkar))
cat("\n")

did_lpm_pov_tendulkar_pp <- coef(lpm_pov_tendulkar)["treat_state_simple:post_wave"] * 100
cat("DiD Effect (Tendulkar):", round(did_lpm_pov_tendulkar_pp, 2), "pp\n\n")

# 8. POVERTY STATUS - ROBUSTNESS CHECK (RANGARAJAN - UPRATED)

cat("POVERTY STATUS: RANGARAJAN (UPRATED BY WAVE & REGION) - ROBUSTNESS\n")

phase1_pov_rangarajan <- phase1_panel %>%
  filter(!is.na(poverty_rangarajan_uprated))

cat("Poverty prevalence (Rangarajan, uprated):\n")
print(table(phase1_pov_rangarajan$poverty_rangarajan_uprated, useNA = "ifany"))
cat("\n")

lpm_pov_rangarajan <- fixest::feols(
  poverty_rangarajan_uprated ~ treat_state_simple * post_wave | HH_ID + WAVE_NO,
  data = phase1_pov_rangarajan,
  cluster = ~ STATE
)

cat("Linear Probability Model: Poverty (Rangarajan, Uprated - Robustness) \n")
print(summary(lpm_pov_rangarajan))
cat("\n")

did_lpm_pov_rangarajan_pp <- coef(lpm_pov_rangarajan)["treat_state_simple:post_wave"] * 100
cat("DiD Effect (Rangarajan):", round(did_lpm_pov_rangarajan_pp, 2), "pp\n\n")

# 9. OECD MODIFIED EQUIVALISED INCOME AFTER HEALTH SPENDING

cat("OECD MODIFIED EQUIVALISED INCOME AFTER HEALTH SPENDING\n")

phase1_eqinc <- phase1_panel %>% filter(!is.na(equivincafterhealth))

cat("Equivalised income summary:\n")
print(summary(phase1_eqinc$equivincafterhealth))
cat("\n")

lin_eqinc <- fixest::feols(
  equivincafterhealth ~ treat_state_simple * post_wave | HH_ID + WAVE_NO,
  data = phase1_eqinc,
  cluster = ~ STATE
)

cat("Linear DiD: OECD Modified Equivalised Income After Health \n")
print(summary(lin_eqinc))
cat("\n")

# 10. OUT-OF-POCKET HEALTH SPENDING

cat("OUT-OF-POCKET HEALTH SPENDING\n")

phase1_oop <- phase1_panel %>% filter(!is.na(OOP_HEALTH))

cat("OOP health spending summary:\n")
print(summary(phase1_oop$OOP_HEALTH))
cat("\n")

lin_oop <- fixest::feols(
  OOP_HEALTH ~ treat_state_simple * post_wave | HH_ID + WAVE_NO,
  data = phase1_oop,
  cluster = ~ STATE
)

cat("Linear DiD: OOP Health Spending \n")
print(summary(lin_oop))
cat("\n")

# 11. OOP SHARE OF INCOME

cat("OOP SHARE OF INCOME\n")

phase1_oopshare <- phase1_panel %>% filter(!is.na(OOP_share))

cat("OOP share summary:\n")
print(summary(phase1_oopshare$OOP_share))
cat("\n")

lin_oopshare <- fixest::feols(
  OOP_share ~ treat_state_simple * post_wave | HH_ID + WAVE_NO,
  data = phase1_oopshare,
  cluster = ~ STATE
)

cat("Linear DiD: OOP Share of Income \n")
print(summary(lin_oopshare))
cat("\n")

# 12. HEALTHCARE UTILISATION: HOSPITALISATION

cat("HEALTHCARE UTILISATION: HOSPITALISATION\n")
options(digits = 10)

phase1_hosp <- phase1_panel %>% filter(!is.na(any_hospitalised))

hosp_pre_treat <- mean(phase1_hosp$any_hospitalised[
  phase1_hosp$treat_state_simple == 1 & phase1_hosp$post_wave == 0
], na.rm = TRUE)
hosp_post_treat <- mean(phase1_hosp$any_hospitalised[
  phase1_hosp$treat_state_simple == 1 & phase1_hosp$post_wave == 1
], na.rm = TRUE)
hosp_pre_ctrl <- mean(phase1_hosp$any_hospitalised[
  phase1_hosp$treat_state_simple == 0 & phase1_hosp$post_wave == 0
], na.rm = TRUE)
hosp_post_ctrl <- mean(phase1_hosp$any_hospitalised[
  phase1_hosp$treat_state_simple == 0 & phase1_hosp$post_wave == 1
], na.rm = TRUE)

cat("Hospitalisation rates:\n")
cat("Treated Pre:  ", format(hosp_pre_treat, digits = 10), "\n")
cat("Treated Post: ", format(hosp_post_treat, digits = 10), "\n")
cat("Control Pre:  ", format(hosp_pre_ctrl, digits = 10), "\n")
cat("Control Post: ", format(hosp_post_ctrl, digits = 10), "\n\n")

lpm_hosp <- fixest::feols(
  any_hospitalised ~ treat_state_simple * post_wave | HH_ID + WAVE_NO,
  data = phase1_hosp,
  cluster = ~ STATE
)

cat("Linear Probability Model: Any Hospitalised \n")
print(summary(lpm_hosp))
cat("\n")

# 13. HEALTHCARE UTILISATION: MEDICATION

cat("HEALTHCARE UTILISATION: MEDICATION USE\n")

phase1_med <- phase1_panel %>% filter(!is.na(any_on_medication))

cat("Medication use prevalence:\n")
print(table(phase1_med$any_on_medication, useNA = "ifany"))
cat("\n")

lpm_med <- fixest::feols(
  any_on_medication ~ treat_state_simple * post_wave | HH_ID + WAVE_NO,
  data = phase1_med,
  cluster = ~ STATE
)

cat("Linear Probability Model: Any on Medication \n")
print(summary(lpm_med))
cat("\n")

# 14. SUMMARY RESULTS TABLE: MAIN vs ROBUSTNESS
# Binary/LPM outcomes in percentage points (pp), rounded to 2dp
# Continuous outcomes in original units, rounded to 2dp

cat("SUMMARY: DiD EFFECTS - TENDULKAR vs RANGARAJAN\n")

results_summary <- tibble::tibble(
  outcome = c(
    "HHS1 (Logit)",
    "HHS1 (LPM)",
    "HHS2 (Binomial Logit)",
    "HHS2 (LPM)",
    "Poverty (Tendulkar, Main)",
    "Poverty (Rangarajan, Robustness)",
    "OECD Modified Eqinc After Health (Rs)",
    "OOP Health Spending (Rs)",
    "OOP Share (%)",
    "Any Hospitalised",
    "Any on Medication"
  ),
  did_effect = c(
    round(did_logit_hhs1_pp, 2),
    round(did_lpm_hhs1_pp, 2),
    round(did_logit_hhs2_pp, 2),
    round(did_lpm_hhs2_pp, 2),
    round(did_lpm_pov_tendulkar_pp, 2),
    round(did_lpm_pov_rangarajan_pp, 2),
    round(coef(lin_eqinc)["treat_state_simple:post_wave"], 2),
    round(coef(lin_oop)["treat_state_simple:post_wave"], 2),
    round(coef(lin_oopshare)["treat_state_simple:post_wave"], 4),
    round(coef(lpm_hosp)["treat_state_simple:post_wave"] * 100, 2),
    round(coef(lpm_med)["treat_state_simple:post_wave"] * 100, 2)
  ),
  unit = c(
    "pp", "pp", "pp", "pp",
    "pp", "pp",
    "Rs/month", "Rs/month", "proportion", "pp", "pp"
  ),
  specification = c(
    "Logit (via marginaleffects)",
    "Linear Probability Model",
    "Binomial Logit",
    "Linear Probability Model",
    "MAIN: Tendulkar (uprated)",
    "ROBUSTNESS: Rangarajan (uprated)",
    "Linear (OECD Modified equivalised)",
    "Linear",
    "Linear",
    "Linear Probability",
    "Linear Probability"
  )
)

print(results_summary)
cat("\n")

# 15. SAVE RESULTS

cat("SAVING RESULTS\n")

if (!dir.exists(analysis_dir)) dir.create(analysis_dir, recursive = TRUE)

write_rds(
  phase1_panel,
  file.path(analysis_dir, "phase1_panel_OECD_Modified_V18_unweighted_08052026.rds")
)
cat(" Saved: phase1_panel_OECD_Modified_V18_unweighted_08052026.rds\n")

write_rds(
  list(
    glm_hhs1 = glm_hhs1,
    glm_hhs2 = glm_hhs2,
    lpm_hhs1 = lpm_hhs1,
    lpm_hhs2 = lpm_hhs2,
    did_logit_hhs1_pp = did_logit_hhs1_pp,
    did_logit_hhs2_pp = did_logit_hhs2_pp,
    did_lpm_hhs1_pp = did_lpm_hhs1_pp,
    did_lpm_hhs2_pp = did_lpm_hhs2_pp,
    lpm_pov_tendulkar = lpm_pov_tendulkar,
    lpm_pov_rangarajan = lpm_pov_rangarajan,
    did_lpm_pov_tendulkar_pp = did_lpm_pov_tendulkar_pp,
    did_lpm_pov_rangarajan_pp = did_lpm_pov_rangarajan_pp,
    lin_eqinc = lin_eqinc,
    lin_oop = lin_oop,
    lin_oopshare = lin_oopshare,
    lpm_hosp = lpm_hosp,
    lpm_med = lpm_med,
    results_summary = results_summary
  ),
  file.path(analysis_dir, "phase1_models_OECD_Modified_V18_unweighted_08052026.rds")
)
cat(" Saved: phase1_models_OECD_Modified_V18_unweighted_08052026.rds\n")

write_csv(
  results_summary,
  file.path(analysis_dir, "phase1_results_summary_OECD_Modified_V18_unweighted_08052026.csv")
)
cat(" Saved: phase1_results_summary_OECD_Modified_V18_unweighted_08052026.csv\n")

comparison_table <- tibble::tibble(
  outcome = c("HHS1 (Any Unhealthy)", "HHS2 (Proportion Unhealthy)"),
  logit_pp = c(
    round(did_logit_hhs1_pp, 2),
    round(did_logit_hhs2_pp, 2)
  ),
  lpm_pp = c(
    round(did_lpm_hhs1_pp, 2),
    NA_real_
  ),
  difference = c(
    round(did_logit_hhs1_pp - did_lpm_hhs1_pp, 2),
    NA_real_
  ),
  note = c(
    "Logit via marginaleffects",
    "Binomial logit (uses HH structure)"
  )
)

write_csv(
  comparison_table,
  file.path(analysis_dir, "logit_vs_lpm_comparison_OECD_Modified_unweighted_08052026.csv")
)
cat("Saved: logit_vs_lpm_comparison_OECD_Modified_unweighted_08052026.csv\n\n")

cat("ANALYSIS COMPLETE\n")

cat("KEY DiD EFFECTS (OECD MODIFIED EQUIVALISED INCOME):\n\n")

cat("HEALTH OUTCOMES:\n")
cat("  - HHS1 (Logit):                           ", round(did_logit_hhs1_pp, 2), "pp\n")
cat("  - HHS1 (LPM):                             ", round(did_lpm_hhs1_pp, 2), "pp\n")
cat("  - HHS2 (Binomial Logit):                  ", round(did_logit_hhs2_pp, 2), "pp\n\n")

cat("POVERTY STATUS:\n")
cat("  - Tendulkar (MAIN):                       ", round(did_lpm_pov_tendulkar_pp, 2), "pp\n")
cat("  - Rangarajan (ROBUSTNESS CHECK):          ", round(did_lpm_pov_rangarajan_pp, 2), "pp\n\n")

cat("FINANCIAL OUTCOMES:\n")
cat("  - OECD Modified Eqinc After Health:       Rs", round(coef(lin_eqinc)["treat_state_simple:post_wave"], 2), "/month\n")
cat("  - OOP Health Spending:                    Rs", round(coef(lin_oop)["treat_state_simple:post_wave"], 2), "/month\n")
cat("  - OOP Share of Income:                    ", round(coef(lin_oopshare)["treat_state_simple:post_wave"] * 100, 2), "%\n\n")

cat("UTILISATION:\n")
cat("  - Any Hospitalised:                       ", round(coef(lpm_hosp)["treat_state_simple:post_wave"] * 100, 2), "pp\n")
cat("  - Any on Medication:                      ", round(coef(lpm_med)["treat_state_simple:post_wave"] * 100, 2), "pp\n\n")

cat("Output directory:", analysis_dir, "\n")
cat("SCRIPT COMPLETE\n")


# COMPACT RESULTS TABLE WITH CIs IN PERCENTAGE POINTS
# Binary outcomes: coeff, SE, CI all multiplied by 100 (pp), rounded to 2dp
# Continuous outcomes: rounded to 2dp in original units

simple_models <- readRDS(
  here("CORRECTING_HHS_analysis_outputs_08052026", "phase1_models_OECD_Modified_V18_unweighted_08052026.rds")
)

did_term <- "treat_state_simple:post_wave"

results <- tibble(
  Outcome = c(
    "HHS1 (Any Unhealthy)",
    "Poverty (Tendulkar)",
    "Eqvinc After Health (Rs/mo)",
    "OOP Health (Rs/mo)",
    "Hospitalised",
    "Medication Use"
  ),
  
  DiD_Coef = c(
    coef(simple_models$lpm_hhs1)[did_term] * 100,
    coef(simple_models$lpm_pov_tendulkar)[did_term] * 100,
    coef(simple_models$lin_eqinc)[did_term],
    coef(simple_models$lin_oop)[did_term],
    coef(simple_models$lpm_hosp)[did_term] * 100,
    coef(simple_models$lpm_med)[did_term] * 100
  ),
  
  SE = c(
    sqrt(vcov(simple_models$lpm_hhs1)[did_term, did_term]) * 100,
    sqrt(vcov(simple_models$lpm_pov_tendulkar)[did_term, did_term]) * 100,
    sqrt(vcov(simple_models$lin_eqinc)[did_term, did_term]),
    sqrt(vcov(simple_models$lin_oop)[did_term, did_term]),
    sqrt(vcov(simple_models$lpm_hosp)[did_term, did_term]) * 100,
    sqrt(vcov(simple_models$lpm_med)[did_term, did_term]) * 100
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
  results,
  here("CORRECTING_HHS_analysis_outputs_08052026",
       "Summary_UNADJUSTED_DiD_Results_V18_unweighted_08052026.csv"),
  row.names = FALSE
)

cat("\n")
cat("UNADJUSTED (SIMPLE) DiD RESULTS (Wave 12 vs 18, OECD Modified)\n")
cat("Binary outcomes reported in percentage points (pp); continuous in Rs/month\n\n")
print(results)
cat("\nSignificance: *** p<0.01, ** p<0.05, * p<0.10\n")
cat("CI: 95% Confidence Interval\n")