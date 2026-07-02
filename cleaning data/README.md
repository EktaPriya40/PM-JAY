README DOCUMENT for 002_Household_Structure_NONEQ_V1_09012026.R

0. Setup and Configuration
library(tidyverse)
library(arrow)
library(here)
library(ggplot2)
library(scales)

# Config
parquet_dir  <- here("arrow_partitions_clean_noneq")
analysis_dir <- here("analysis_outputs_noneq", "household_structure")

dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)


What this does?

Loads required packages for:
Data manipulation (tidyverse)
Reading parquet datasets (arrow)
Project-root paths (here)
Plotting & scales (ggplot2, scales) 


Defines where to read data from:

parquet_dir: the NONEQ pipeline output folder, produced by 001_CMIE_WAVE_PIPELINE_NONEQ_V2_11012026.R.


Defines where to write outputs:
analysis_dir: analysis_outputs_noneq/household_structure.
Ensures analysis_dir exists (dir.create(..., recursive = TRUE)).


1. Load the Arrow Dataset

# Load Section 1 household-level dataset 
ds <- arrow::open_dataset(parquet_dir, format = "parquet")


What this does?

Opens a lazy Arrow dataset over all parquet files in arrow_partitions_clean_noneq/.
No rows are read into memory yet; all operations are deferred until collect().

The dataset ds contains one row per household-wave with variables created in Section 001. The variables are:

HHIDs and timing,
Health aggregates,
Income and OOP health spending,
Poverty and PMJAY exposure.



2. STEP 1 – Define variables of interest

cols_of_interest <- c(
  "HH_ID", "STATE", "WAVE_NO", "HH_SIZE",
  "ANY_16_59", "ANY_MALE_16_59",
  "D2_NO_ADULT_16_59", "D3_NO_ADULT_MALE_16_59",
  "n_unhealthy", "HHS1", "HHS2", "health_bad_any",
  "any_hospitalised", "any_on_medication",
  "net_income_month", "percap_after_health", "percap_month",
  # not for NONEQ income script "equivincmonth", "equivincafterhealth",   
  "OOP_HEALTH", "OOP_share", "poverty_tendulkar",
  "tot_exp_month",
  "REGION_TYPE", "REGION_TYPE_clean", "PMJAY_ACTIVE", "PMJAY_STATUS"
)


What this does?

Creates a character vector of column names-a list of all variables needed for this household structure script. Note: this is a broader list for convenience


3. Variable breakdown by category
3.1 Household Identifiers

Variable	Description	Example	Source
HH_ID	Unique household ID	HH_001	People of India (POI)
STATE	Indian state / UT	"Bihar"	Aspirational India / Cons. Pyramids
WAVE_NO	CMIE survey wave index	9, 12, 18	Derived from file dates in the pipeline
3.2 Household Composition Flags (from People of India aggregation)

Variable	Description	Example	Calculation
ANY_16_59	HH has ≥ 1 member aged 16–59	TRUE/FALSE	From member AGE_YRS / AGE_MTHS
ANY_MALE_16_59	HH has ≥ 1 male aged 16–59	TRUE/FALSE	From member GENDER + AGE
D2_NO_ADULT_16_59	Deprivation flag: no adult 16–59	1 or 0	!ANY_16_59
D3_NO_ADULT_MALE_16_59	Deprivation flag: no male 16–59	1 or 0	!ANY_MALE_16_59


Derived from member-level People of India data (AGE_YEARS_EXACT, GENDER).
Aggregated to HH-level (one row per HH) in the pipeline.
Stored as 0/1 flags.


3.3 Health Status Variables (from People of India aggregation)

Variable	Description	Type	Range
n_unhealthy	Count of unhealthy members in HH	Integer	0 to HH_SIZE
HHS1	HH health status – binary	0 or 1	0/1
HHS2	HH health status – proportion	Numeric	0.0–1.0
health_bad_any	Any health issue indicator	0 or 1	0/1
any_hospitalised	≥ 1 member (was/is) hospitalised	0 or 1	0/1
any_on_medication	≥ 1 member on regular medication	0 or 1	0/1
			


HHS1: binary outcome for logit/LPM (any unhealthy in HH).
HHS2: continuous measure for linear models (intensity).
any_hospitalised / any_on_medication: healthcare utilisation outcomes.

•	Built in aggregate_poi_health():
•	Normalises health flags (Y/YES/TRUE/1 → TRUE, N/NO/FALSE/0 → FALSE).
•	Aggregates by HH_ID + time (WAVE_NO or MONTH).


3.4 Income Variables (Non-equivalised)


Variable	Description	Type	Unit
net_income_month	Total HH monthly income	Numeric	₹ per month
percap_month	Per-capita monthly income	Numeric	₹ per person per month
percap_after_health	Per-capita income after health spending	Numeric	₹ per person per month
tot_exp_month	Total HH monthly expenditure	Numeric	₹ per month


Assessing financial risk related to health status

percap_after_health is central for poverty classification (Tendulkar line (not uprated yet)) and evaluating if health shocks push households below the poverty line.


For the pipeline-
Negative values are NA.
percap_after_health, percap_month computed in derive_outcomes()

Note: This is for non-equalised income- NONEQ (OECD equalised income is in the other pipeline).

3.5 Out-of-Pocket (OOP) Health spending variables

Variable	Description	Type	Unit
OOP_HEALTH	Total OOP health spending (monthly)	Numeric	₹ per month
OOP_share	OOP as share of household income	Numeric	0.0–1.0+


OOP_HEALTH: absolute spending for health expenditure, 
OOP_share: relative burden for fraction of income spent on health

These are only defined when net_income_month > 0 and not NA.


3.6 Poverty Variables (Tendulkar line (not uprated))

Variable	Description	Type	Values
poverty_tendulkar	Poverty status	Binary	0 = non-poor, 1 = poor
			percap_after_health < threshold(region)



Thresholds:
Rural: ₹816 per person per month
Urban: ₹1000 per person per month


3.7 Geographic and PMJAY Variables

Variable	Description	Type	Values
REGION_TYPE	Raw rural/urban classification	Character	"urban", "rural"
REGION_TYPE_clean	Standardised version (lowercased, trimmed)	Character	"urban", "rural"
PMJAY_ACTIVE	PMJAY active in this state & wave?	Binary	0 = no, 1 = yes
PMJAY_STATUS	PMJAY adoption status	Character	"active", "inactive", "not_adopted", "unknown"


PMJAY variables defined for DiD:

Treated if PMJAY_ACTIVE == 1 in that state-wave.
Control if PMJAY was never adopted (e.g., Delhi, Odisha, Telangana).



4. STEP 2 – finding column names

cols_existing <- intersect(cols_of_interest, names(ds))

What this does?

Takes the intersection of:
cols_of_interest, and names(ds) (actual columns in parquet).

Result: cols_existing contains only columns present in both sets (intersect)

If cols_of_interest has 24 names but parquet only has 22, cols_existing will have 22 names, and the script proceeds using those.


5. STEP 3 – Extract and load data into hh_core

hh_core <- ds %>%
  dplyr::select(dplyr::all_of(cols_existing)) %>%
  dplyr::collect() %>%
  tibble::as_tibble()


Requests only the columns in cols_existing and then converts the result to a tibble.


Result: hh_core is a tibble (in-memory) containing one row per household-wave and exactly the selected column names (from col_existing).


6. STEP 4 – Diagnostic Checks

print(names(hh_core))
summary(hh_core$OOP_HEALTH)

What this does?

Verifies the available variable names.
Prints min, max, quartiles, mean, and count of NA’s.
Helps in early data-quality assurance-looking for NA or outliers etc.


7. Cleaning: Drop Negative Income / OOP

# drop any negative income / OOP if the columns exist
if ("net_income_month" %in% names(hh_core)) {
  hh_core <- hh_core %>%
    mutate(net_income_month = ifelse(net_income_month < 0, NA_real_, net_income_month))
}
if ("OOP_HEALTH" %in% names(hh_core)) {
  hh_core <- hh_core %>%
    mutate(OOP_HEALTH = ifelse(OOP_HEALTH < 0, NA_real_, OOP_HEALTH))
}


What this does?

Any remaining negative net_income_month is assigned NA.
Any remaining negative OOP_HEALTH is assigned NA.


8. Convenience Flags (Presence of OOP and Total Expenditure)

# convenience flags
has_oop   <- "OOP_HEALTH"   %in% names(hh_core) && any(!is.na(hh_core$OOP_HEALTH))
has_totexp<- "tot_exp_month"%in% names(hh_core) && any(!is.na(hh_core$tot_exp_month))

has_oop: TRUE if OOP_HEALTH exists and has non-NA values.
has_totexp: TRUE if tot_exp_month exists and has non-NA values.


9. Household-Level summaries by Wave

hh_by_wave <- hh_core %>%
  dplyr::group_by(WAVE_NO) %>%
  dplyr::summarise(
    n_households = dplyr::n(),
    mean_hh_size = mean(HH_SIZE, na.rm = TRUE),
    median_hh_size = stats::median(HH_SIZE, na.rm = TRUE),
    
    # health
    mean_unhealthy = mean(n_unhealthy, na.rm = TRUE),
    median_unhealthy = stats::median(n_unhealthy, na.rm = TRUE),
    share_HHS1_bad = mean(HHS1 == 1L, na.rm = TRUE),
    share_health_bad = mean(health_bad_any == 1L, na.rm = TRUE),
    share_any_hosp = mean(any_hospitalised == 1L, na.rm = TRUE),
    share_any_med = mean(any_on_medication == 1L, na.rm = TRUE),
    
    # poverty / income
    share_poor_tendulkar = mean(poverty_tendulkar == 1L, na.rm = TRUE),
    mean_percap_after = mean(percap_after_health, na.rm = TRUE),
    median_percap_after = stats::median(percap_after_health, na.rm = TRUE),
    mean_hh_income = mean(net_income_month, na.rm = TRUE),
    median_hh_income = stats::median(net_income_month, na.rm = TRUE),
    
    
    mean_OOP = if (has_oop) mean(OOP_HEALTH, na.rm = TRUE) else NA_real_,
    median_OOP = if (has_oop) stats::median(OOP_HEALTH, na.rm = TRUE) else NA_real_,
    mean_tot_exp = if (has_totexp) mean(tot_exp_month, na.rm = TRUE) else NA_real_,
    median_tot_exp = if (has_totexp) stats::median(tot_exp_month, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  dplyr::arrange(WAVE_NO)

readr::write_csv(hh_by_wave, file.path(analysis_dir, "summary_by_wave.csv"))
saveRDS(hh_by_wave, file.path(analysis_dir, "summary_by_wave.rds"))


Household size: n_households, mean_hh_size, median_hh_size

Health burden: mean_unhealthy, med_unhealthy, share_HHS1_bad, share_health_bad, share_any_hosp, share_any_med

Poverty & income: share_poor_tendulkar, mean/median percap_after_health, mean/median net_income_month

Financial variables: mean/median OOP_HEALTH, mean/median tot_exp_month 

Outputs: summary_by_wave.csv and .rds 


10. Household Size Distribution by Wave

hh_size_dist <- hh_core %>%
  dplyr::group_by(WAVE_NO, HH_SIZE) %>%
  dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
  dplyr::group_by(WAVE_NO) %>%
  dplyr::mutate(pct = count / sum(count) * 100)

readr::write_csv(hh_size_dist, file.path(analysis_dir, "hh_size_distribution_by_wave.csv"))

Distribution of household sizes for each wave: counts and percentages.

11. Composition flags distribution

comp_flags_by_wave <- hh_core %>%
  dplyr::group_by(WAVE_NO) %>%
  dplyr::summarise(
    share_any_16_59           = mean(ANY_16_59, na.rm = TRUE),
    share_any_male_16_59      = mean(ANY_MALE_16_59, na.rm = TRUE),
    share_no_adult_16_59      = mean(D2_NO_ADULT_16_59, na.rm = TRUE),
    share_no_adult_male_16_59 = mean(D3_NO_ADULT_MALE_16_59, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(WAVE_NO)

readr::write_csv(comp_flags_by_wave, file.path(analysis_dir, "composition_flags_by_wave.csv"))

This is for wave-level prevalence of households with/without working-age adults and males.


12. Health status distributions

health_dist <- hh_core %>%
  dplyr::group_by(WAVE_NO) %>%
  dplyr::summarise(
    households_with_unhealthy = sum(n_unhealthy > 0, na.rm = TRUE),
    households_all_healthy    = sum(n_unhealthy == 0 & !is.na(n_unhealthy), na.rm = TRUE),
    pct_with_unhealthy        = households_with_unhealthy / dplyr::n() * 100,
    .groups = "drop"
  )

readr::write_csv(health_dist, file.path(analysis_dir, "health_summary_by_wave.csv"))

This is for how many HHs have any unhealthy members and what percentage that is.

14. Region / PMJAY Breakdown

region_pmjay_by_wave <- hh_core %>%
  dplyr::group_by(WAVE_NO, REGION_TYPE_clean, PMJAY_STATUS) %>%
  dplyr::summarise(
    n_households      = dplyr::n(),
    share_poor        = mean(poverty_tendulkar == 1L, na.rm = TRUE),
    mean_percap_after = mean(percap_after_health, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(region_pmjay_by_wave, file.path(analysis_dir, "region_pmjay_by_wave.csv"))

Stratifies poverty and welfare by wave x region x PMJAY status.

15. Printed summaries

summarise_waves <- function(waves = NULL) {
  df <- hh_core
  if (!is.null(waves)) {
    df <- df %>% dplyr::filter(WAVE_NO %in% waves)
  }
  cat("\n=== Summary for waves:", if (is.null(waves)) "ALL" else paste(waves, collapse = ", "), "===\n")
  
  df %>%
    dplyr::group_by(WAVE_NO) %>%
    dplyr::summarise(
      n_households    = dplyr::n(),
      mean_HH_SIZE    = mean(HH_SIZE, na.rm = TRUE),
      median_HH_SIZE  = stats::median(HH_SIZE, na.rm = TRUE),
      mean_n_unhealthy= mean(n_unhealthy, na.rm = TRUE),
      mean_HHS1       = mean(HHS1, na.rm = TRUE),
      mean_HHS2       = mean(HHS2, na.rm = TRUE),
      mean_healthbad  = mean(health_bad_any, na.rm = TRUE),
      mean_any_hosp   = mean(any_hospitalised, na.rm = TRUE),
      mean_any_med    = mean(any_on_medication, na.rm = TRUE),
      mean_percap     = mean(percap_after_health, na.rm = TRUE),
      mean_income     = mean(net_income_month, na.rm = TRUE),
      mean_OOP        = if (has_oop) mean(OOP_HEALTH, na.rm = TRUE) else NA_real_,
      mean_pov        = mean(poverty_tendulkar, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    print(n = Inf)
}


summarise_waves() – all waves.
summarise_waves(c(12, 18)) – DiD pre/post waves.

16. Final diagnostics and quick ratios

print(names(hh_core))
summary(hh_core$OOP_HEALTH)

# summarise_waves()
summarise_waves(c(12, 18))
with(hh_by_wave, mean_OOP / mean_tot_exp)

Re-checks:
Column names.
OOP_HEALTH summary.

Prints detailed summary for waves 12 and 18 (aligning to simple DiD analysis).

with(hh_by_wave, mean_OOP / mean_tot_exp):

Gives wave-wise ratio of OOP to total spending.









 

