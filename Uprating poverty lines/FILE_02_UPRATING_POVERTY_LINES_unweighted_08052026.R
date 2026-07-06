# 
# FILE_02_UPRATING_POVERTY_LINES_unweighted_08052026.R
# POVERTY LINE UPRATING BY WAVE & REGION
# 
# PURPOSE: Uprate BOTH Tendulkar & Rangarajan (2011-12) poverty thresholds 
#          to other waves using wave-specific CPI indices (rural & urban separately)
#          with OECD MODIFIED equivalised income

library(dplyr)
library(arrow)
library(here)
library(tidyr)
library(readr)

cat("POVERTY LINE UPRATING BY WAVE & REGION (TENDULKAR & RANGARAJAN, OECD MODIFIED)\n\n")

# 1. POVERTY LINE BASELINES (2011-12)

# Tendulkar thresholds
tendulkar_base <- tibble::tibble(
  poverty_method = "Tendulkar",
  REGION_TYPE = c("Rural", "Urban"),
  poverty_line_2011_12 = c(816, 1000)
)

# Rangarajan thresholds
rangarajan_base <- tibble::tibble(
  poverty_method = "Rangarajan",
  REGION_TYPE = c("Rural", "Urban"),
  poverty_line_2011_12 = c(972, 1407)
)

cat("Poverty Line Baselines (2011-12):\n\n")
cat("TENDULKAR:\n")
print(tendulkar_base)
cat("\nRANGARAJAN:\n")
print(rangarajan_base)
cat("\n")

# 2. LOADING CPI DATA FROM DATASETS FOLDER

datasets_dir <- file.path(here(), "datasets")

# Load with all columns, then select what we need
cpi_2011_2013 <- read_csv(
  file.path(datasets_dir, "general_index_base_2012_CPIndex_Jan11-To-May13.csv"),
  skip = 1,
  show_col_types = FALSE
) %>%
  select(Year, Month, State, Rural, Urban)

cpi_2013_2020 <- read_csv(
  file.path(datasets_dir, "general_index_base_2012_CPIndex_Jan13-To-Mar20.csv"),
  skip = 1,
  show_col_types = FALSE
) %>%
  select(Year, Month, State, Rural, Urban)

cat("CPI files loaded from datasets folder\n\n")

# Combine and keep only ALL India
cpi_all <- bind_rows(cpi_2011_2013, cpi_2013_2020) %>%
  filter(State == "ALL India") %>%
  mutate(
    Month_num = match(Month, month.name)
  ) %>%
  filter(!is.na(Month_num)) %>%
  select(Year, Month_num, Rural, Urban)

cat("CPI Data Summary (2012 base, General Index):\n")
cat("Years:", min(cpi_all$Year), "to", max(cpi_all$Year), "\n")
cat("Observations:", nrow(cpi_all), "\n\n")

# 3. CALCULATING AVERAGE CPI BY WAVE

wave_def <- tibble::tribble(
  ~WAVE_NO, ~wave_year, ~month_start, ~month_end,
  9L,  2015, 1, 4,
  10L, 2015, 5, 8,
  11L, 2015, 9, 12,
  12L, 2016, 1, 4,
  13L, 2016, 5, 8,
  14L, 2016, 9, 12,
  18L, 2018, 1, 4
)

# Join without name conflicts
wave_cpi <- wave_def %>%
  left_join(
    cpi_all %>% rename(wave_year = Year, month_num = Month_num),
    by = "wave_year",
    relationship = "many-to-many"
  ) %>%
  filter(month_num >= month_start & month_num <= month_end) %>%
  group_by(WAVE_NO) %>%
  summarise(
    CPI_Rural = mean(Rural, na.rm = TRUE),
    CPI_Urban = mean(Urban, na.rm = TRUE),
    .groups = "drop"
  )

cat("Average CPI by Wave (Base 2012=100):\n")
print(wave_cpi)
cat("\n")

# 4. UPRATING TENDULKAR THRESHOLDS

cpi_2011_12 <- 92.5

poverty_tendulkar_uprated <- wave_cpi %>%
  crossing(tendulkar_base) %>%
  mutate(
    cpi_base = cpi_2011_12,
    cpi_wave = ifelse(REGION_TYPE == "Rural", CPI_Rural, CPI_Urban),
    inflation_factor = cpi_wave / cpi_base,
    poverty_line_uprated = poverty_line_2011_12 * inflation_factor
  ) %>%
  select(
    WAVE_NO, poverty_method, REGION_TYPE, poverty_line_2011_12, cpi_base, cpi_wave,
    inflation_factor, poverty_line_uprated
  ) %>%
  mutate(
    poverty_line_uprated = round(poverty_line_uprated, 2)
  )

cat("TENDULKAR Poverty Lines (Uprated by Wave & Region):\n\n")
print(poverty_tendulkar_uprated)
cat("\n")

# 5. UPRATING RANGARAJAN THRESHOLDS

poverty_rangarajan_uprated <- wave_cpi %>%
  crossing(rangarajan_base) %>%
  mutate(
    cpi_base = cpi_2011_12,
    cpi_wave = ifelse(REGION_TYPE == "Rural", CPI_Rural, CPI_Urban),
    inflation_factor = cpi_wave / cpi_base,
    poverty_line_uprated = poverty_line_2011_12 * inflation_factor
  ) %>%
  select(
    WAVE_NO, poverty_method, REGION_TYPE, poverty_line_2011_12, cpi_base, cpi_wave,
    inflation_factor, poverty_line_uprated
  ) %>%
  mutate(
    poverty_line_uprated = round(poverty_line_uprated, 2)
  )

cat("RANGARAJAN Poverty Lines (Uprated by Wave & Region):\n\n")
print(poverty_rangarajan_uprated)
cat("\n")

# Combine both methods
poverty_uprated_combined <- bind_rows(
  poverty_tendulkar_uprated,
  poverty_rangarajan_uprated
)


# 6. CALCULATE MEAN INCOME & EQUIVALISED INCOME (OECD MODIFIED) BY WAVE & REGION

parquet_dir <- here("arrow_partitions_CORRECTING_HHS_08052026")
ds <- arrow::open_dataset(parquet_dir, format = "parquet")


# Load income stats using OECD MODIFIED column names from pipeline
income_stats <- ds %>%
  select(
    WAVE_NO, HH_ID, REGION_TYPE,
    net_income_month, equivincmonth, equivincafterhealth
  ) %>%
  filter(WAVE_NO %in% c(9L, 10L, 11L, 12L, 13L, 14L, 18L)) %>%
  collect() %>%
  # Standardise region names to match poverty baselines
  mutate(
    REGION_TYPE_clean = case_when(
      tolower(trimws(REGION_TYPE)) == "rural" ~ "Rural",
      tolower(trimws(REGION_TYPE)) == "urban" ~ "Urban",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(REGION_TYPE_clean)) %>%  # Remove NA regions
  group_by(WAVE_NO, REGION_TYPE_clean) %>%
  summarise(
    mean_income = mean(net_income_month, na.rm = TRUE),
    mean_eqinc = mean(equivincmonth, na.rm = TRUE),
    mean_eqinc_afterhealth = mean(equivincafterhealth, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  rename(REGION_TYPE = REGION_TYPE_clean) %>%
  mutate(
    mean_income = round(mean_income, 2),
    mean_eqinc = round(mean_eqinc, 2),
    mean_eqinc_afterhealth = round(mean_eqinc_afterhealth, 2)
  )

cat("Mean Income & Equivalised Income (OECD MODIFIED) by Wave & Region:\n\n")
print(income_stats)
cat("\n")

# 7. FINAL OUTPUT TABLE - TENDULKAR

final_table_tendulkar <- poverty_tendulkar_uprated %>%
  left_join(income_stats, by = c("WAVE_NO", "REGION_TYPE")) %>%
  select(
    WAVE_NO, REGION_TYPE, n,
    poverty_line_2011_12, poverty_line_uprated,
    mean_income, mean_eqinc, mean_eqinc_afterhealth, cpi_wave
  ) %>%
  rename(
    Wave = WAVE_NO,
    Region = REGION_TYPE,
    N_Households = n,
    Poverty_Line_2011_12 = poverty_line_2011_12,
    Poverty_Line_Uprated = poverty_line_uprated,
    Mean_Income = mean_income,
    Mean_Eqinc_OECD_Modified = mean_eqinc,
    Mean_Eqinc_After_Health = mean_eqinc_afterhealth,
    CPI_Wave = cpi_wave
  )

cat("FINAL TABLE: TENDULKAR - Poverty Lines & OECD MODIFIED Income Statistics by Wave & Region\n\n")
print(final_table_tendulkar)
cat("\n")

# 8. FINAL OUTPUT TABLE - RANGARAJAN

final_table_rangarajan <- poverty_rangarajan_uprated %>%
  left_join(income_stats, by = c("WAVE_NO", "REGION_TYPE")) %>%
  select(
    WAVE_NO, REGION_TYPE, n,
    poverty_line_2011_12, poverty_line_uprated,
    mean_income, mean_eqinc, mean_eqinc_afterhealth, cpi_wave
  ) %>%
  rename(
    Wave = WAVE_NO,
    Region = REGION_TYPE,
    N_Households = n,
    Poverty_Line_2011_12 = poverty_line_2011_12,
    Poverty_Line_Uprated = poverty_line_uprated,
    Mean_Income = mean_income,
    Mean_Eqinc_OECD_Modified = mean_eqinc,
    Mean_Eqinc_After_Health = mean_eqinc_afterhealth,
    CPI_Wave = cpi_wave
  )

cat("FINAL TABLE: RANGARAJAN - Poverty Lines & OECD MODIFIED Income Statistics by Wave & Region\n\n")
print(final_table_rangarajan)
cat("\n")

# 9. COMPARISON TABLE: TENDULKAR vs RANGARAJAN (WAVE 12 & 18)

comparison_table <- bind_rows(
  final_table_tendulkar %>% mutate(Method = "Tendulkar"),
  final_table_rangarajan %>% mutate(Method = "Rangarajan")
) %>%
  filter(Wave %in% c(12L, 18L)) %>%
  select(Method, Wave, Region, N_Households, Poverty_Line_Uprated, Mean_Income, Mean_Eqinc_After_Health) %>%
  arrange(Wave, Region, Method)

cat("COMPARISON: TENDULKAR vs RANGARAJAN (Key Waves)\n\n")
print(comparison_table)
cat("\n")

# 10. SAVE OUTPUT

output_dir <- here("CORRECTING_HHS_analysis_outputs_08052026")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Save Tendulkar results
write_csv(final_table_tendulkar, file.path(output_dir, "tendulkar_uprated_by_wave_region_oecd_modified_unweighted_08052026.csv"))
write_csv(poverty_tendulkar_uprated, file.path(output_dir, "tendulkar_poverty_lines_uprated_unweighted_08052026.csv"))

# Save Rangarajan results
write_csv(final_table_rangarajan, file.path(output_dir, "rangarajan_uprated_by_wave_region_oecd_modified_unweighted_08052026.csv"))
write_csv(poverty_rangarajan_uprated, file.path(output_dir, "rangarajan_poverty_lines_uprated_unweighted_08052026.csv"))

# Save combined results
write_csv(poverty_uprated_combined, file.path(output_dir, "poverty_lines_both_methods_uprated_unweighted_08052026.csv"))
write_csv(comparison_table, file.path(output_dir, "poverty_methods_comparison_wave12_18_unweighted_08052026.csv"))

# Save income stats
write_csv(income_stats, file.path(output_dir, "mean_income_oecd_modified_by_wave_region_unweighted_08052026.csv"))

cat(" Saved: tendulkar_uprated_by_wave_region_oecd_modified_unweighted_08052026.csv\n")
cat(" Saved: tendulkar_poverty_lines_uprated_unweighted_08052026.csv\n")
cat(" Saved: rangarajan_uprated_by_wave_region_oecd_modified_unweighted_08052026.csv\n")
cat(" Saved: rangarajan_poverty_lines_uprated_unweighted_08052026.csv\n")
cat(" Saved: poverty_lines_both_methods_uprated_unweighted_08052026.csv\n")
cat(" Saved: poverty_methods_comparison_wave12_18_unweighted_08052026.csv\n")
cat(" Saved: mean_income_oecd_modified_by_wave_region_unweighted_08052026.csv\n\n")

cat("SCRIPT COMPLETE\n")