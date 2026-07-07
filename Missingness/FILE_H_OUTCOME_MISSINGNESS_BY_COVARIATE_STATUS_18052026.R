# FILE_H_OUTCOME_MISSINGNESS_BY_COVARIATE_STATUS_18052026.R
#
# Purpose:
#   Produce simple, aligned diagnostics showing outcome missingness by
#   covariate status (Observed vs Missing), split by treatment group and period.
#
# Output:
#   1) one CSV per outcome
#   2) one combined CSV
#
# Outputs include only:
#   - n_total
#   - n_outcome_nonmissing
#   - n_outcome_missing
#   - pct_outcome_nonmissing
#   - pct_outcome_missing
#
# Date stamp:
#   18052026

library(dplyr)
library(tidyr)
library(readr)
library(arrow)
library(here)
library(purrr)

# ── CONFIG ────────────────────────────────────────────────────────────────────
parquet_dir <- here("arrow_partitions_CORRECTING_HHS_08052026")
out_dir     <- here("ANALYSIS_DIAGNOSTICS_OUTCOME_MISSINGNESS_BY_COVSTATUS_18052026")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stamp <- "18052026"

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

outcomes <- c(
  "HHS1",
  "HHS2",
  "OOP_HEALTH",
  "poverty_tendulkar",
  "poverty_rangarajan",
  "equivincmonth",
  "equivincafterhealth"
)

covariates <- c(
  "HH_SIZE",
  "log_equivinc",
  "urban",
  "ANY_16_59",
  "ANY_MALE_16_59",
  "D2_NO_ADULT_16_59",
  "D3_NO_ADULT_MALE_16_59"
)

needed_cols <- c(
  "HH_ID", "STATE", "WAVE_NO",
  "HH_SIZE", "REGION_TYPE",
  "equivincmonth", "equivincafterhealth",
  "ANY_16_59", "ANY_MALE_16_59",
  "D2_NO_ADULT_16_59", "D3_NO_ADULT_MALE_16_59",
  "HHS1", "HHS2", "OOP_HEALTH",
  "poverty_tendulkar", "poverty_rangarajan"
)

# ── HELPERS ───────────────────────────────────────────────────────────────────
first_nonmissing <- function(x, row_id) {
  idx <- which(!is.na(x))
  if (length(idx) == 0) return(NA)
  x[idx[which.min(row_id[idx])]]
}

first_nonmissing_num <- function(x, row_id) {
  val <- first_nonmissing(x, row_id)
  if (length(val) == 0 || is.na(val)) return(NA_real_)
  as.numeric(val)
}

first_nonmissing_int <- function(x, row_id) {
  val <- first_nonmissing(x, row_id)
  if (length(val) == 0 || is.na(val)) return(NA_integer_)
  as.integer(val)
}

# ── LOAD DATA ─────────────────────────────────────────────────────────────────
ds <- open_dataset(parquet_dir, format = "parquet")

cols_present <- intersect(needed_cols, names(ds))

raw <- ds %>%
  filter(WAVE_NO %in% c(12L, 18L)) %>%
  select(all_of(cols_present)) %>%
  collect() %>%
  mutate(
    HH_ID    = as.character(HH_ID),
    STATE   = as.character(STATE),
    WAVE_NO = as.integer(WAVE_NO),
    .row_id = row_number(),
    treat = case_when(
      STATE %in% treated_states ~ 1L,
      STATE %in% control_states ~ 0L,
      TRUE ~ NA_integer_
    ),
    post = case_when(
      WAVE_NO == 12L ~ 0L,
      WAVE_NO == 18L ~ 1L,
      TRUE ~ NA_integer_
    ),
    urban = case_when(
      tolower(trimws(REGION_TYPE)) == "urban" ~ 1L,
      tolower(trimws(REGION_TYPE)) == "rural" ~ 0L,
      TRUE ~ NA_integer_
    ),
    log_equivinc = ifelse(
      !is.na(equivincmonth) & equivincmonth >= 0,
      log1p(equivincmonth),
      NA_real_
    )
  ) %>%
  filter(!is.na(treat), !is.na(post))

# ── BALANCED PANEL ────────────────────────────────────────────────────────────
balanced_ids <- raw %>%
  group_by(HH_ID) %>%
  summarise(
    n_waves = n_distinct(WAVE_NO),
    waves_present = paste(sort(unique(WAVE_NO)), collapse = "-"),
    n_treat = n_distinct(treat),
    .groups = "drop"
  ) %>%
  filter(
    n_waves == 2L,
    waves_present == "12-18",
    n_treat == 1L
  ) %>%
  pull(HH_ID)

panel <- raw %>%
  filter(HH_ID %in% balanced_ids)

# ── DEDUPLICATE TO ONE ROW PER HH_ID × WAVE ───────────────────────────────────
panel_unique <- panel %>%
  group_by(HH_ID, STATE, WAVE_NO, treat, post) %>%
  summarise(
    HH_SIZE = first_nonmissing_num(HH_SIZE, .row_id),
    log_equivinc = first_nonmissing_num(log_equivinc, .row_id),
    urban = first_nonmissing_int(urban, .row_id),
    ANY_16_59 = first_nonmissing_int(ANY_16_59, .row_id),
    ANY_MALE_16_59 = first_nonmissing_int(ANY_MALE_16_59, .row_id),
    D2_NO_ADULT_16_59 = first_nonmissing_int(D2_NO_ADULT_16_59, .row_id),
    D3_NO_ADULT_MALE_16_59 = first_nonmissing_int(D3_NO_ADULT_MALE_16_59, .row_id),
    HHS1 = first_nonmissing_int(HHS1, .row_id),
    HHS2 = first_nonmissing_num(HHS2, .row_id),
    OOP_HEALTH = first_nonmissing_num(OOP_HEALTH, .row_id),
    poverty_tendulkar = first_nonmissing_int(poverty_tendulkar, .row_id),
    poverty_rangarajan = first_nonmissing_int(poverty_rangarajan, .row_id),
    equivincmonth = first_nonmissing_num(equivincmonth, .row_id),
    equivincafterhealth = first_nonmissing_num(equivincafterhealth, .row_id),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(treat == 1L, "Treated", "Control"),
    period = if_else(post == 1L, "Post (W18)", "Pre (W12)")
  )

cat("Balanced panel rows (deduplicated):", nrow(panel_unique), "\n")

# ── FUNCTION: OUTCOME MISSINGNESS BY COVARIATE STATUS ────────────────────────
make_outcome_missingness_table <- function(df, outcome_var, covariate_var) {
  
  if (!(outcome_var %in% names(df))) return(NULL)
  if (!(covariate_var %in% names(df))) return(NULL)
  
  df %>%
    mutate(
      cov_status = if_else(
        is.na(.data[[covariate_var]]),
        "Covariate Missing",
        "Covariate Observed"
      ),
      outcome_missing = is.na(.data[[outcome_var]])
    ) %>%
    group_by(group, period, cov_status) %>%
    summarise(
      n_total = n(),
      n_outcome_nonmissing = sum(!outcome_missing),
      n_outcome_missing = sum(outcome_missing),
      pct_outcome_nonmissing = round(100 * n_outcome_nonmissing / n_total, 2),
      pct_outcome_missing = round(100 * n_outcome_missing / n_total, 2),
      .groups = "drop"
    ) %>%
    mutate(
      outcome = outcome_var,
      covariate = covariate_var,
      .before = 1
    ) %>%
    select(
      outcome,
      covariate,
      group,
      period,
      cov_status,
      n_total,
      n_outcome_nonmissing,
      n_outcome_missing,
      pct_outcome_nonmissing,
      pct_outcome_missing
    )
}

# ── BUILD ALL TABLES ──────────────────────────────────────────────────────────
results <- map_dfr(outcomes, function(ov) {
  map_dfr(covariates, function(cv) {
    make_outcome_missingness_table(panel_unique, ov, cv)
  })
})

# ── SAVE COMBINED LONG FILE ───────────────────────────────────────────────────
combined_file <- file.path(
  out_dir,
  paste0("outcome_missingness_by_covariate_status_ALL_", stamp, ".csv")
)
write_csv(results, combined_file)
cat("Wrote:", combined_file, "\n")

# ── SAVE ONE FILE PER OUTCOME ─────────────────────────────────────────────────
walk(outcomes, function(ov) {
  this_tbl <- results %>% filter(outcome == ov)
  if (nrow(this_tbl) == 0) return(NULL)
  
  file_name <- file.path(
    out_dir,
    paste0("outcome_missingness_by_covariate_status_", ov, "_", stamp, ".csv")
  )
  
  write_csv(this_tbl, file_name)
  cat("Wrote:", file_name, "\n")
})

cat("All output saved to:", out_dir, "\n")