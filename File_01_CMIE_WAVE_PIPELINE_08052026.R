# File_01_CMIE_WAVE_PIPELINE_08052026.R
# OECD Modified Equivalence Scale (1st adult=1.0, extra adult=0.5, child<16=0.3)

# Purpose: Read raw CMIE CSVs for waves 9-14 and 18, deriving household-level outcomes,
#          and writing partitioned Parquet files for downstream scripts (unweighted).
# Output: arrow_partitions_CORRECTING_HHS_08052026

library(tidyverse)
library(lubridate)
library(arrow)
library(stringr)
library(purrr)
library(here)

# 1 Config
base_dir <- here("datasets")
out_dir <- here("arrow_partitions_CORRECTING_HHS_08052026")
datasets <- c(
  "Aspirational India",
  "Consumption pyramids",
  "Household income",
  "People of India",
  "Member income"
)
wave_mode <- "intersection"
limit_waves <- c(9:14, 18)

# 2 Helpers
read_safe <- function(path) {
  readr::read_csv(path, guess_max = 50000,
                  show_col_types = FALSE, progress = FALSE)
}

discover_files <- function(ds) {
  dir(
    file.path(base_dir, ds),
    full.names = TRUE,
    recursive = TRUE,
    pattern = "(csv|csv\\.gz|csv\\.zip)$",
    ignore.case = TRUE
  )
}

extract_date <- function(parent, base, grandparent = NA_character_) {
  candidate <- paste(grandparent, parent, base, sep = "_")
  tok <- str_extract(candidate, "20\\d{2}[-_]?\\d{2}[-_]?\\d{2}")
  if (!is.na(tok)) {
    tok <- gsub("[-_]", "", tok)
    dt <- suppressWarnings(ymd(tok))
    if (!is.na(dt)) return(dt)
  }
  tok2 <- str_extract(candidate, "20\\d{4}")
  if (!is.na(tok2)) {
    dt2 <- suppressWarnings(ymd(paste0(tok2, "01")))
    if (!is.na(dt2)) return(dt2)
  }
  as.Date(NA)
}

compute_wave <- function(date) {
  if (is.na(date)) return(NA_integer_)
  yr <- year(date); mo <- month(date)
  ceiling(mo / 4) + (yr - 2014) * 3
}

safe_left_join <- function(x, y, by) {
  if (!is.data.frame(y) || nrow(y) == 0) return(x)
  dplyr::left_join(x, y, by = by)
}

standardise_month <- function(df) {
  if (!("MONTH" %in% names(df))) return(df)
  if (is.numeric(df$MONTH)) {
    df$MONTH <- as.integer(df$MONTH); return(df)
  }
  df$MONTH <- trimws(as.character(df$MONTH))
  suppressWarnings({
    parsed <- parse_date_time(
      df$MONTH,
      orders = c("b-y","b-Y","b","B-y","B-Y","m","m-y","ym","Y-m","Y-m-d"),
      quiet = TRUE
    )
  })
  if (exists("parsed") && any(!is.na(parsed))) {
    df$MONTH <- ifelse(!is.na(parsed), month(parsed), NA_integer_)
    return(df)
  }
  df$MONTH <- suppressWarnings(as.integer(df$MONTH))
  df
}

coerce_keys <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  if ("HH_ID" %in% names(df)) df <- df %>% mutate(HH_ID = as.character(HH_ID))
  if ("WAVE_NO" %in% names(df)) df <- df %>% mutate(WAVE_NO = as.integer(WAVE_NO))
  if ("MONTH" %in% names(df)) df <- df %>% mutate(MONTH = as.integer(MONTH))
  df
}

backfill_var <- function(combined, var, ...) {
  sources <- list(...)
  for (src in sources) {
    if (is.data.frame(src) && var %in% names(src)) {
      combined[[var]] <- dplyr::coalesce(
        combined[[var]],
        src[[var]][match(combined$HH_ID, src$HH_ID)]
      )
    }
  }
  combined
}

# PMJAY mapping 
pmjay_state_wave <- tribble(
  ~STATE, ~PMJAY_start_wave,
  "Andhra Pradesh", 19, "Assam", 15, "Bihar", 15, "Chandigarh", 15,
  "Chhattisgarh", 15, "Delhi", 99, "Goa", 15, "Gujarat", 15,
  "Haryana", 15, "Himachal Pradesh", 15, "Jammu & Kashmir", 17,
  "Jharkhand", 15, "Karnataka", 16, "Kerala", 18, "Madhya Pradesh", 15,
  "Maharashtra", 15, "Odisha", 99, "Puducherry", 15, "Punjab", 18,
  "Rajasthan", 19, "Tamil Nadu", 15, "Telangana", 27,
  "Uttar Pradesh", 15, "Uttarakhand", 15, "West Bengal", 99,
  "Tripura", 15, "Meghalaya", 17, "Sikkim", 15, "Mizoram", 16,
  "Ladakh", 17
)

# Normalise STATE keys
clean_state <- function(x) {
  x2 <- as.character(x)
  x2 <- URLdecode(x2)
  x2 <- trimws(x2)
  x2
}

# OECD Modified Equivalence Scale Helper
oecd_modified_scale <- function(hh_size, n_children) {
  # OECD modified equivalence scale:
  # 1st adult: 1.0
  # Each additional adult: 0.5
  # Each child (age < 16): 0.3
  #
  # Formula: 1.0 + 0.5 * (adults - 1) + 0.3 * children
  if (is.na(hh_size) || hh_size <= 0) return(NA_real_)
  n_children <- ifelse(is.na(n_children) || n_children < 0, 0, n_children)
  n_adults <- pmax(hh_size - n_children, 1) # at least 1 adult
  scale <- 1.0 + 0.5 * pmax(n_adults - 1, 0) + 0.3 * n_children
  return(scale)
}

aggregate_poi_health <- function(poi_members) {
  if (!is.data.frame(poi_members) || nrow(poi_members) == 0) return(tibble())
  normalize_flag <- function(x) {
    x2 <- toupper(trimws(as.character(x)))
    dplyr::case_when(
      x2 %in% c("Y","YES","TRUE","1") ~ TRUE,
      x2 %in% c("N","NO","FALSE","0") ~ FALSE,
      x2 %in% c("DATA NOT AVAILABLE","NOT APPLICABLE") ~ NA,
      TRUE ~ NA
    )
  }
  poi_health <- poi_members %>%
    mutate(
      IS_HEALTHY_norm = if ("IS_HEALTHY" %in% names(.)) normalize_flag(IS_HEALTHY) else NA,
      IS_HOSP_norm = if ("IS_HOSPITALISED" %in% names(.)) normalize_flag(IS_HOSPITALISED) else NA,
      WAS_HOSP_norm = if ("WAS_HOSPITALISED" %in% names(.)) normalize_flag(WAS_HOSPITALISED) else NA,
      ON_MED_norm = if ("IS_ON_REGULAR_MEDICATION" %in% names(.)) normalize_flag(IS_ON_REGULAR_MEDICATION) else NA
    )
  keys <- intersect(c("WAVE_NO","MONTH"), names(poi_health))
  grp <- c("HH_ID", keys)
  poi_health %>%
    group_by(across(all_of(grp))) %>%
    summarise(
      n_is_obs    = sum(!is.na(IS_HEALTHY_norm)),
      n_unhealthy = sum(IS_HEALTHY_norm == FALSE, na.rm = TRUE),
      HHS1 = case_when(
        n_is_obs == 0 & n_unhealthy == 0 ~ 0L,          # routing zero
        n_is_obs == 0                    ~ NA_integer_, # truly missing
        n_unhealthy > 0                  ~ 1L,
        TRUE                             ~ 0L
      ),
      any_hospitalised = as.integer(
        any(IS_HOSP_norm  == TRUE, na.rm = TRUE) |
          any(WAS_HOSP_norm == TRUE, na.rm = TRUE)
      ),
      any_on_medication = as.integer(any(ON_MED_norm == TRUE, na.rm = TRUE)),
      .groups = "drop"
    )
}

# 3 Output prep
if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
stopifnot(dir.exists(base_dir))

files_tbl <- tibble(dataset = datasets) %>%
  mutate(filename = map(dataset, discover_files)) %>%
  tidyr::unnest_longer(filename)

if (nrow(files_tbl) == 0) stop("No files found under the dataset folders.")

files_with_dates <- files_tbl %>%
  mutate(
    base = basename(filename),
    parent = basename(dirname(filename)),
    grandparent = basename(dirname(dirname(filename))),
    start_date = as_date(mapply(extract_date, parent, base, grandparent, SIMPLIFY = TRUE)),
    wave = vapply(start_date, compute_wave, numeric(1))
  )

manifest <- files_with_dates %>%
  filter(!is.na(wave)) %>%
  select(dataset, filename, start_date, wave)

waves_by_dataset <- manifest %>% distinct(dataset, wave)

if (wave_mode == "intersection") {
  waves_to_do <- waves_by_dataset %>%
    count(wave) %>%
    filter(n == length(datasets)) %>%
    pull(wave)
} else {
  waves_to_do <- sort(unique(waves_by_dataset$wave))
}

if (!all(is.na(limit_waves))) {
  waves_to_do <- intersect(waves_to_do, limit_waves)
}

if (length(waves_to_do) == 0) stop("No waves selected to run.")
waves_to_do <- intersect(waves_to_do, 1:18)

cat("\nWAVES TO PROCESS (OECD MODIFIED EQUIVALISED) \n")
cat("Selected waves:", paste(waves_to_do, collapse = ", "), "\n")
cat("Waves 9–14: Parallel trends\n")
cat("Wave 18: DiD post-period\n")
cat("Equivalence scale: OECD Modified (1st adult=1.0, +0.5 per adult, +0.3 per child<16)\n\n")

# 4 PMJAY thresholds and poverty lines
tendulkar_rural <- 816
tendulkar_urban <- 1000
rangarajan_rural <- 972
rangarajan_urban <- 1407

# 5 Outcome derivation with OECD Modified scale
derive_outcomes <- function(combined, poi_members, pmjay_state_wave) {
  if (is.data.frame(poi_members) && nrow(poi_members) > 0) {
    poi_members <- poi_members %>% standardise_month() %>% coerce_keys()
  }
  
  rename_if_exists <- function(df, old, new) {
    if (old %in% names(df) && !(new %in% names(df))) {
      df <- dplyr::rename(df, !!new := !!rlang::sym(old))
    }
    df
  }
  
  combined <- combined %>%
    rename_if_exists("M_EXP_HEALTH.x", "M_EXP_HEALTH") %>%
    rename_if_exists("M_EXP_HEALTH.y", "M_EXP_HEALTH") %>%
    rename_if_exists("M_EXP_MEDICINES.x", "M_EXP_MEDICINES") %>%
    rename_if_exists("M_EXP_MEDICINES.y", "M_EXP_MEDICINES") %>%
    rename_if_exists("M_EXP_DOCTORS_PHYSIO_FEE.x", "M_EXP_DOCTORS_PHYSIO_FEE") %>%
    rename_if_exists("M_EXP_DOCTORS_PHYSIO_FEE.y", "M_EXP_DOCTORS_PHYSIO_FEE") %>%
    rename_if_exists("M_EXP_MEDICAL_TESTS.x", "M_EXP_MEDICAL_TESTS") %>%
    rename_if_exists("M_EXP_MEDICAL_TESTS.y", "M_EXP_MEDICAL_TESTS") %>%
    rename_if_exists("M_EXP_HOSPITALISATION_FEES.x", "M_EXP_HOSPITALISATION_FEES") %>%
    rename_if_exists("M_EXP_HOSPITALISATION_FEES.y", "M_EXP_HOSPITALISATION_FEES") %>%
    rename_if_exists("M_EXP_HEALTH_INS_PREMIUM.x", "M_EXP_HEALTH_INS_PREMIUM") %>%
    rename_if_exists("M_EXP_HEALTH_INS_PREMIUM.y", "M_EXP_HEALTH_INS_PREMIUM")
  
  # HH_SIZE from POI
  if (is.data.frame(poi_members) && nrow(poi_members) > 0 && "MEM_ID" %in% names(poi_members)) {
    if ("MONTH" %in% names(poi_members)) {
      hh_size_df <- poi_members %>%
        group_by(HH_ID, MONTH) %>%
        summarise(HH_SIZE = n_distinct(MEM_ID, na.rm = TRUE), .groups = "drop")
      combined <- combined %>% left_join(hh_size_df, by = c("HH_ID", "MONTH"))
    } else {
      hh_size_df <- poi_members %>%
        group_by(HH_ID) %>%
        summarise(HH_SIZE = n_distinct(MEM_ID, na.rm = TRUE), .groups = "drop")
      combined <- combined %>% left_join(hh_size_df, by = "HH_ID")
    }
  } else {
    combined$HH_SIZE <- NA_integer_
  }
  
  # Count children (< 16 years) from POI for OECD modified scale
  if (is.data.frame(poi_members) && nrow(poi_members) > 0 && "AGE_YEARS_EXACT" %in% names(poi_members)) {
    if ("MONTH" %in% names(poi_members)) {
      n_children_df <- poi_members %>%
        filter(!is.na(AGE_YEARS_EXACT) & AGE_YEARS_EXACT < 16) %>%
        group_by(HH_ID, MONTH) %>%
        summarise(N_CHILDREN = n(), .groups = "drop")
      combined <- combined %>%
        left_join(n_children_df, by = c("HH_ID", "MONTH")) %>%
        mutate(N_CHILDREN = coalesce(N_CHILDREN, 0L))
    } else {
      n_children_df <- poi_members %>%
        filter(!is.na(AGE_YEARS_EXACT) & AGE_YEARS_EXACT < 16) %>%
        group_by(HH_ID) %>%
        summarise(N_CHILDREN = n(), .groups = "drop")
      combined <- combined %>%
        left_join(n_children_df, by = "HH_ID") %>%
        mutate(N_CHILDREN = coalesce(N_CHILDREN, 0L))
    }
  } else {
    combined$N_CHILDREN <- 0L
  }
  
  # OOP_HEALTH
  health_items <- intersect(
    c(
      "M_EXP_HEALTH", "M_EXP_MEDICINES", "M_EXP_DOCTORS_PHYSIO_FEE",
      "M_EXP_MEDICAL_TESTS", "M_EXP_HOSPITALISATION_FEES", "M_EXP_HEALTH_INS_PREMIUM"
    ),
    names(combined)
  )
  
  combined <- combined %>%
    mutate(
      M_EXP_HEALTH_num = if ("M_EXP_HEALTH" %in% names(.)) suppressWarnings(as.numeric(M_EXP_HEALTH)) else NA_real_,
      health_components_sum = if (length(health_items) > 0) {
        rowSums(across(setdiff(health_items, "M_EXP_HEALTH"),
                       ~ suppressWarnings(as.numeric(.))),
                na.rm = TRUE)
      } else NA_real_,
      OOP_HEALTH = as.numeric(coalesce(M_EXP_HEALTH_num, health_components_sum))
    )
  
  # Income, per-capita & OECD MODIFIED equivalised
  combined <- combined %>%
    mutate(
      TOT_EXP_UNI = coalesce(!!!select(., any_of(c("TOT_EXP", "TOT_EXP.x", "TOT_EXP.y"))))
    ) %>%
    mutate(
      TOT_INC_num_raw = if ("TOT_INC" %in% names(.)) suppressWarnings(as.numeric(TOT_INC)) else NA_real_,
      INC_ALL_num_raw = if ("INC_OF_HH_FRM_ALL_SRCS" %in% names(.)) suppressWarnings(as.numeric(INC_OF_HH_FRM_ALL_SRCS)) else NA_real_,
      ADJ_EXP_num_raw = if ("ADJ_TOT_EXP" %in% names(.)) suppressWarnings(as.numeric(ADJ_TOT_EXP)) else NA_real_,
      TOT_EXP_num_raw = if ("TOT_EXP_UNI" %in% names(.)) suppressWarnings(as.numeric(TOT_EXP_UNI)) else NA_real_,
      TOT_INC_num = ifelse(TOT_INC_num_raw < 0, NA_real_, TOT_INC_num_raw),
      INC_ALL_num = ifelse(INC_ALL_num_raw < 0, NA_real_, INC_ALL_num_raw),
      ADJ_EXP_num = ifelse(ADJ_EXP_num_raw < 0, NA_real_, ADJ_EXP_num_raw),
      TOT_EXP_num = ifelse(TOT_EXP_num_raw < 0, NA_real_, TOT_EXP_num_raw),
      net_income_month = coalesce(TOT_INC_num, INC_ALL_num),
      tot_exp_month = coalesce(ADJ_EXP_num, TOT_EXP_num),
      percap_after_health = ifelse(
        !is.na(net_income_month) & !is.na(OOP_HEALTH) &
          !is.na(HH_SIZE) & HH_SIZE > 0,
        (net_income_month - OOP_HEALTH) / HH_SIZE,
        NA_real_
      ),
      percap_month = ifelse(
        !is.na(net_income_month) & !is.na(HH_SIZE) & HH_SIZE > 0,
        net_income_month / HH_SIZE,
        NA_real_
      ),
      HH_SIZE = as.numeric(HH_SIZE),
      # OECD MODIFIED scale using helper function
      eq_scale = mapply(oecd_modified_scale, HH_SIZE, N_CHILDREN),
      equivincmonth = if_else(
        !is.na(net_income_month) & !is.na(eq_scale) & eq_scale > 0,
        net_income_month / eq_scale,
        NA_real_
      ),
      equivincafterhealth = if_else(
        !is.na(net_income_month) & !is.na(OOP_HEALTH) &
          !is.na(eq_scale) & eq_scale > 0,
        (net_income_month - OOP_HEALTH) / eq_scale,
        NA_real_
      ),
      OOP_share = ifelse(
        !is.na(OOP_HEALTH) & !is.na(net_income_month) & net_income_month > 0,
        OOP_HEALTH / net_income_month,
        NA_real_
      )
    )
  
  # Poverty (Tendulkar)
  if ("REGION_TYPE" %in% names(combined)) {
    combined <- combined %>%
      mutate(
        REGION_TYPE_clean = tolower(trimws(coalesce(as.character(REGION_TYPE), ""))),
        tendulkar_threshold = case_when(
          REGION_TYPE_clean == "urban" ~ tendulkar_urban,
          REGION_TYPE_clean == "rural" ~ tendulkar_rural,
          TRUE ~ NA_real_
        ),
        poverty_tendulkar = case_when(
          !is.na(percap_after_health) & !is.na(tendulkar_threshold) &
            percap_after_health < tendulkar_threshold ~ 1L,
          !is.na(percap_after_health) & !is.na(tendulkar_threshold) &
            percap_after_health >= tendulkar_threshold ~ 0L,
          TRUE ~ NA_integer_
        )
      )
  } else {
    combined <- combined %>%
      mutate(
        REGION_TYPE_clean = NA_character_,
        tendulkar_threshold = NA_real_,
        poverty_tendulkar = NA_integer_
      )
  }
  
  # Poverty (Rangarajan)
  if ("REGION_TYPE" %in% names(combined)) {
    combined <- combined %>%
      mutate(
        REGION_TYPE_clean = tolower(trimws(coalesce(as.character(REGION_TYPE), ""))),
        rangarajan_threshold = case_when(
          REGION_TYPE_clean == "urban" ~ rangarajan_urban,
          REGION_TYPE_clean == "rural" ~ rangarajan_rural,
          TRUE ~ NA_real_
        ),
        poverty_rangarajan = case_when(
          !is.na(percap_after_health) & !is.na(rangarajan_threshold) &
            percap_after_health < rangarajan_threshold ~ 1L,
          !is.na(percap_after_health) & !is.na(rangarajan_threshold) &
            percap_after_health >= rangarajan_threshold ~ 0L,
          TRUE ~ NA_integer_
        )
      )
  } else {
    combined <- combined %>%
      mutate(
        REGION_TYPE_clean = NA_character_,
        rangarajan_threshold = NA_real_,
        poverty_rangarajan = NA_integer_
      )
  }
  
  # Health/utilisation from POI
  poi_health <- aggregate_poi_health(poi_members)
  if (nrow(poi_health) == 0) {
    combined$n_unhealthy <- NA_integer_
    combined$HHS1 <- NA_integer_
    combined$HHS2 <- NA_real_
    combined$health_bad_any <- NA_integer_
    combined$any_hospitalised <- NA_integer_
    combined$any_on_medication <- NA_integer_
  } else {
    poi_health <- coerce_keys(poi_health)
    combined   <- coerce_keys(combined)
    if (all(c("HH_ID","WAVE_NO") %in% names(combined)) &&
        all(c("HH_ID","WAVE_NO") %in% names(poi_health))) {
      by_keys <- c("HH_ID","WAVE_NO")
    } else if (all(c("HH_ID","MONTH") %in% names(combined)) &&
               all(c("HH_ID","MONTH") %in% names(poi_health))) {
      by_keys <- c("HH_ID","MONTH")
    } else {
      by_keys <- "HH_ID"
    }
    combined <- combined %>%
      left_join(poi_health %>% select(-n_is_obs), by = by_keys) %>%
      mutate(
        HHS2 = ifelse(
          !is.na(HH_SIZE) & HH_SIZE > 0 & !is.na(n_unhealthy),
          n_unhealthy / HH_SIZE,
          NA_real_
        ),
        health_bad_any = coalesce(
          HHS1,
          as.integer(any_hospitalised == 1L | any_on_medication == 1L)
        ),
        any_hospitalised = coalesce(any_hospitalised, 0L),
        any_on_medication = coalesce(any_on_medication, 0L)
      )
  }
  
  # PMJAY exposure
  if ("STATE" %in% names(combined)) {
    combined <- combined %>%
      mutate(STATE = as.character(STATE)) %>%
      left_join(pmjay_state_wave, by = "STATE") %>%
      mutate(
        PMJAY_ACTIVE = as.integer(
          !is.na(PMJAY_start_wave) &
            PMJAY_start_wave != 99 &
            WAVE_NO >= PMJAY_start_wave
        ),
        PMJAY_STATUS = case_when(
          PMJAY_ACTIVE == 1L ~ "active",
          !is.na(PMJAY_start_wave) & PMJAY_start_wave == 99 ~ "not_adopted",
          TRUE ~ "inactive"
        )
      )
  } else {
    combined <- combined %>%
      mutate(
        STATE = NA_character_,
        PMJAY_start_wave = NA_integer_,
        PMJAY_ACTIVE = NA_integer_,
        PMJAY_STATUS = NA_character_
      )
  }
  
  combined
}

# 6 Per-wave processing
process_and_write_wave <- function(w) {
  gc(); cat("\n--- Processing wave", w, "---\n")
  wave_data <- manifest %>% filter(wave == w)
  
  # Member income 
  ds_mi <- wave_data %>% filter(dataset == "Member income")
  mi_members <- if (nrow(ds_mi) == 0) tibble() else {
    map_dfr(ds_mi$filename, ~ read_safe(.x) %>%
              select(
                any_of(c("HH_ID","MONTH","MEM_ID")),
                matches("(^|_)inc($|_)|income", ignore.case = TRUE)
              )) %>%
      standardise_month() %>%
      coerce_keys()
  }
  
  # Consumption pyramids
  ds_cp <- wave_data %>% filter(dataset == "Consumption pyramids")
  cp_df <- if (nrow(ds_cp) == 0) tibble() else {
    map_dfr(ds_cp$filename, ~ read_safe(.x) %>%
              select(
                any_of(c("HH_ID","MONTH","TOT_EXP","STATE")),
                starts_with("M_EXP")
              )) %>%
      standardise_month() %>%
      coerce_keys()
  }
  
  # Household income
  ds_inc <- wave_data %>% filter(dataset == "Household income")
  inc_df <- if (nrow(ds_inc) == 0) tibble() else {
    map_dfr(ds_inc$filename, ~ read_safe(.x) %>%
              select(
                any_of(c(
                  "HH_ID","MONTH","TOT_INC","INC_OF_HH_FRM_ALL_SRCS",
                  "ADJ_TOT_EXP","STATE"
                ))
              )) %>%
      standardise_month() %>%
      coerce_keys()
  }
  
  # Aspirational India (household covariates)
  ds_ai <- wave_data %>% filter(dataset == "Aspirational India")
  ai_df <- if (nrow(ds_ai) == 0) tibble() else {
    map_dfr(ds_ai$filename, ~ read_safe(.x) %>%
              select(
                any_of(c(
                  "HH_ID","STATE","REGION_TYPE",
                  "TYPE_OF_ROOF","TYPE_OF_WALL",
                  "HAS_TOILET_IN_HOUSE","HAS_ACCESS_TO_WATER_IN_HOUSE",
                  "MONTH"
                ))
              )) %>%
      distinct(HH_ID, .keep_all = TRUE) %>%
      standardise_month() %>%
      coerce_keys()
  }
  
  # People of India
  ds_poi <- wave_data %>% filter(dataset == "People of India")
  poi_members <- if (nrow(ds_poi) == 0) tibble() else {
    map_dfr(ds_poi$filename, ~ read_safe(.x) %>%
              select(any_of(c(
                "HH_ID","WAVE_NO","MEM_ID","STATE","REGION_TYPE",
                "RESPONSE_STATUS","RELATION_WITH_HOH","GENDER",
                "AGE_YRS","AGE_MTHS",
                "IS_HEALTHY","IS_HOSPITALISED","WAS_HOSPITALISED",
                "IS_ON_REGULAR_MEDICATION","MONTH"
              )))) %>%
      mutate(
        AGE_YEARS_EXACT =
          suppressWarnings(as.numeric(AGE_YRS)) +
          suppressWarnings(as.numeric(coalesce(AGE_MTHS, 0))) / 12
      ) %>%
      standardise_month() %>%
      coerce_keys()
  }
  
  poi_hoh <- if (nrow(poi_members) == 0) tibble() else {
    poi_members %>%
      filter(RELATION_WITH_HOH %in% c("HOH","Head","Head of Household")) %>%
      distinct(HH_ID, .keep_all = TRUE)
  }
  
  poi_comp <- if (nrow(poi_members) == 0) tibble() else {
    poi_members %>%
      group_by(HH_ID) %>%
      summarise(
        ANY_16_59 = any(
          !is.na(AGE_YEARS_EXACT) &
            AGE_YEARS_EXACT >= 16 &
            AGE_YEARS_EXACT < 60
        ),
        ANY_MALE_16_59 = any(
          GENDER %in% c("M","Male") &
            !is.na(AGE_YEARS_EXACT) &
            AGE_YEARS_EXACT >= 16 &
            AGE_YEARS_EXACT < 60
        ),
        .groups = "drop"
      ) %>%
      mutate(
        D2_NO_ADULT_16_59 = !ANY_16_59,
        D3_NO_ADULT_MALE_16_59 = !ANY_MALE_16_59
      )
  }
  
  # Member income aggregation
  mi_hh <- if (nrow(mi_members) == 0) tibble() else {
    inc_cols <- setdiff(names(mi_members), c("HH_ID","MONTH","MEM_ID"))
    if (length(inc_cols) > 0) {
      mi_members <- mi_members %>%
        mutate(across(all_of(inc_cols),
                      ~ suppressWarnings(as.numeric(.))))
    }
    tmp <- if (length(inc_cols) == 0) {
      mi_members %>% mutate(MEMBER_INC_TOTAL = NA_real_)
    } else {
      mi_members %>%
        mutate(MEMBER_INC_TOTAL =
                 rowSums(across(all_of(inc_cols)), na.rm = TRUE))
    }
    if ("MONTH" %in% names(tmp)) {
      tmp %>%
        group_by(HH_ID, MONTH) %>%
        summarise(
          MI_TOT_MEMBER_INC = sum(MEMBER_INC_TOTAL, na.rm = TRUE),
          MI_N_MEMBERS_WITH_ROWS = n(),
          .groups = "drop"
        )
    } else {
      tmp %>%
        group_by(HH_ID) %>%
        summarise(
          MI_TOT_MEMBER_INC = sum(MEMBER_INC_TOTAL, na.rm = TRUE),
          MI_N_MEMBERS_WITH_ROWS = n(),
          .groups = "drop"
        )
    }
  }
  
  mi_hh <- coerce_keys(mi_hh)
  
  pick_first_nonempty <- function(...) {
    for (x in list(...)) if (nrow(x) > 0) return(x)
    tibble()
  }
  
  base_df <- pick_first_nonempty(cp_df, inc_df, ai_df, poi_hoh)
  if (nrow(base_df) == 0) {
    message("Wave ", w, ": no base data. Skipping.")
    return(invisible(FALSE))
  }
  
  base_df <- coerce_keys(base_df)
  
  by_cp <- if (all(c("HH_ID","MONTH") %in% names(base_df)) &&
               all(c("HH_ID","MONTH") %in% names(cp_df))) c("HH_ID","MONTH") else "HH_ID"
  by_inc <- if (all(c("HH_ID","MONTH") %in% names(base_df)) &&
                all(c("HH_ID","MONTH") %in% names(inc_df))) c("HH_ID","MONTH") else "HH_ID"
  by_mi <- if (all(c("HH_ID","MONTH") %in% names(base_df)) &&
               all(c("HH_ID","MONTH") %in% names(mi_hh))) c("HH_ID","MONTH") else "HH_ID"
  
  combined <- base_df %>%
    safe_left_join(cp_df, by = by_cp) %>%
    safe_left_join(inc_df, by = by_inc) %>%
    safe_left_join(mi_hh, by = by_mi) %>%
    safe_left_join(ai_df, by = "HH_ID") %>%
    safe_left_join(poi_hoh,by = "HH_ID") %>%
    safe_left_join(poi_comp,by = "HH_ID")
  
  if (!("MONTH" %in% names(combined))) {
    combined <- combined %>%
      mutate(MONTH = coalesce(
        ai_df$MONTH[match(HH_ID, ai_df$HH_ID)],
        cp_df$MONTH[match(HH_ID, cp_df$HH_ID)],
        inc_df$MONTH[match(HH_ID, inc_df$HH_ID)],
        mi_hh$MONTH[match(HH_ID, mi_hh$HH_ID)],
        NA_integer_
      ))
  }
  
  combined <- coerce_keys(combined) %>%
    mutate(WAVE_NO = as.integer(w))
  
  if (!("STATE" %in% names(combined))) combined$STATE <- NA_character_
  
  combined <- backfill_var(combined, "STATE", ai_df, cp_df, inc_df, mi_hh, poi_members) %>%
    mutate(
      STATE = if_else(
        is.na(STATE) | STATE == "" | STATE == "__HIVE_DEFAULT_PARTITION__",
        "UNKNOWN_STATE",
        as.character(STATE)
      )
    )
  
  if (!("REGION_TYPE" %in% names(combined))) combined$REGION_TYPE <- NA_character_
  combined <- backfill_var(combined, "REGION_TYPE", ai_df, cp_df, inc_df, poi_members)
  
  combined <- derive_outcomes(combined, poi_members, pmjay_state_wave)
  
  if (!("PMJAY_STATUS" %in% names(combined))) {
    combined$PMJAY_STATUS <- NA_character_
  }
  
  print(
    combined %>%
      count(WAVE_NO, STATE, PMJAY_STATUS) %>%
      arrange(WAVE_NO, STATE, PMJAY_STATUS),
    n = 200
  )
  
  combined <- combined %>%
    mutate(
      PMJAY_STATUS = if_else(
        is.na(PMJAY_STATUS) |
          PMJAY_STATUS == "" |
          PMJAY_STATUS == "__HIVE_DEFAULT_PARTITION__",
        "unknown",
        as.character(PMJAY_STATUS)
      )
    )
  
  # Final column order
  combined <- combined %>%
    select(
      HH_ID, WAVE_NO, MONTH, STATE, REGION_TYPE, REGION_TYPE_clean,
      net_income_month, tot_exp_month,
      percap_after_health, percap_month,
      equivincmonth, equivincafterhealth,
      OOP_HEALTH, OOP_share,
      HH_SIZE, N_CHILDREN, eq_scale,
      poverty_tendulkar, poverty_rangarajan,
      HHS1, HHS2, n_unhealthy, health_bad_any,
      any_hospitalised, any_on_medication,
      ANY_16_59, ANY_MALE_16_59,
      D2_NO_ADULT_16_59, D3_NO_ADULT_MALE_16_59,
      PMJAY_STATUS
    )
  
  gc()
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  partition_cols <- intersect(c("WAVE_NO","STATE","PMJAY_STATUS"), names(combined))
  if (length(partition_cols) == 0) partition_cols <- "WAVE_NO"
  
  write_dataset(
    combined,
    path = out_dir,
    format = "parquet",
    partitioning = partition_cols
  )
  
  cat("Wrote wave", w, "to", out_dir, "\n")
  invisible(TRUE)
}

# 7 Run waves
results <- vector("list", length(waves_to_do))
names(results) <- as.character(waves_to_do)

for (i in seq_along(waves_to_do)) {
  w <- waves_to_do[i]; cat("\n Starting wave", w, "===\n")
  res <- tryCatch(
    {
      process_and_write_wave(w)
      list(success = TRUE, error = NULL)
    },
    error = function(e) {
      list(success = FALSE, error = e)
    }
  )
  results[[i]] <- res
  if (isTRUE(res$success)) {
    cat(" Wrote wave", w, "to", out_dir, "\n")
  } else {
    cat(" Wave", w, "FAILED:", conditionMessage(res$error), "\n")
  }
}

errs <- purrr::map(results, "error")
if (any(!purrr::map_lgl(errs, is.null))) {
  cat("\nSome waves failed:\n")
  fail_idx <- which(!purrr::map_lgl(errs, is.null))
  for (i in fail_idx) {
    cat("- Wave", names(results)[i], ":", conditionMessage(errs[[i]]), "\n")
  }
} else {
  cat("\n All selected waves completed without errors.\n")
}

# Save manifest and pmjay mapping for reproducibility
analysis_dir <- here("CORRECTING_HHS_analysis_outputs_08052026")
if (!dir.exists(analysis_dir)) dir.create(analysis_dir, recursive = TRUE)

readr::write_rds(files_with_dates, file.path(analysis_dir, "CORRECTING_HHS_files_with_dates_oecd_modified_08052026.rds"))
readr::write_rds(pmjay_state_wave, file.path(analysis_dir, "CORRECTING_HHS_pmjay_state_wave_oecd_modified_08052026.rds"))

cat("\n OECD MODIFIED EQUIVALISED PIPELINE COMPLETE ===\n")
cat("Output saved to:", out_dir, "\n")
cat("Waves processed:", paste(waves_to_do, collapse = ", "), "\n")
cat("SCRIPT COMPLETE\n")