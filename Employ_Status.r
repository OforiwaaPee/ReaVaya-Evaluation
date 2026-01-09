# =========================
# Rea Vaya SDID — EMPLOY_STATUS end-to-end
# Standardized name: EMPLOY_STATUS
# Description: Employment status (3-category)
# Type: Integer (plus optional factor label)
# =========================

#To save panels on Mac: At the top of your EMPLOY_STATUS script:
source("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Processed Data/Panels/panel_utils.R")  # adjust path as needed
outcome_name <- "EMPLOY_STATUS"


# ---- 0) PACKAGES ----
# Run this ONCE manually if needed:
# install.packages(c("readxl", "dplyr", "janitor", "stringr", "data.table", "fixest"))

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(janitor)
  library(stringr)
  library(data.table)   # fast IO + aggregations
  library(fixest)       # TWFE + sunab()
  # library(sf)
  # library(terra)
  # library(tidyverse)
  # library(geodata)
  # library(rnaturalearth)
  # library(osmdata)
  # library(leaflet)
  # library(rayshader)
  # library(gganimate)
  # library(arrow)      # parquet
})

# Speed knobs
data.table::setDTthreads(percent = 90)
options(fixest.ci_level = 0.95)


# ---- 1) IMPORT AND SAVE RAW HTS, HZONE LOOKUP, TREATMENT STATUS FILES ----
## 2000
#PERSON2000
library(readxl)
PERSON2000 <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Raw Data/HTS_surveys/2000/PERSON.xlsx")

#TRIP2000
library(readxl)
TRIP2000 <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Raw Data/HTS_surveys/2000/TRIP.xlsx")

#HOUSE2000
library(readxl)
HOUSE2000 <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Raw Data/HTS_surveys/2000/HOUSE.xlsx")

## 2014
#PERSON2014
library(readxl)
PERSON2014 <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Raw Data/HTS_surveys/2014/COJpersonfile.xlsx")

#TRIP2014
library(readxl)
TRIP2014 <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Raw Data/HTS_surveys/2014/COJtripfile.xlsx")

#HOUSE2014
library(readxl)
HOUSE2014 <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Raw Data/HTS_surveys/2014/COJhousefile.xls")

#ATTITUDE2014
library(readxl)
ATTITUDE2014 <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Raw Data/HTS_surveys/2014/COJattitudefile.xlsx")

## 2019
#PERSON2019
library(readxl)
PERSON2019 <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Raw Data/HTS_surveys/2019/COJ_Person_Data.xlsx")

#HOUSE2019
library(readxl)
HOUSE2019 <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Raw Data/HTS_surveys/2019/COJ_Household_Data.xlsx")

#WEIGHTS2019
library(readxl)
WEIGHT2019 <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Raw Data/HTS_surveys/2019/COJ_Data_Weights.xlsx")

#PERSONIMPUTED2019
library(readxl)
PERSONIMPUTED2019 <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Raw Data/HTS_surveys/2019/Imputed_persons.xlsx")

## Treatment status (Phase 1A, 1B, and combined)
#PHASE1A_TREATSTATUS
library(readxl)
TREATED_1A <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Processed Data/1a_TreatedAreas.xlsx")

#PHASE1B_TREATSTATUS
library(readxl)
TREATED_1B <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Processed Data/1b_TreatedAreas.xlsx")

#PHASE1A1B_TREATSTATUS
library(readxl)
TREATED_1A1B <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Processed Data/1a1b_TreatedAreas.xlsx")

## Home TAZ lookups
# Excel look-ups home TAZ by year 2000, 2014, 2019
#HZONE2000_LOOKUP
library(readxl)
HZONE2000 <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Processed Data/hh_home_2000.xlsx")

#HZONE2014_LOOKUP
library(readxl)
HZONE2014 <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Processed Data/HZONE_lookup_2014.xlsx")

#HZONE2019_LOOKUP
library(readxl)
HZONE2019 <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Processed Data/HZONE_lookup_2019.xlsx")

## Optionally save all raw objects as an RData snapshot
save(PERSON2000, TRIP2000, HOUSE2000, PERSON2014, TRIP2014, HOUSE2014, ATTITUDE2014, PERSON2019, HOUSE2019, WEIGHT2019, PERSONIMPUTED2019, TREATED_1A, TREATED_1B, TREATED_1A1B, HZONE2000, HZONE2014, HZONE2019, file = "ReaVaya.RData")

# (Helper: string normalization if needed later)
norm_str <- function(x) {
  x <- as.character(x)
  tolower(str_squish(x))
}

###############################################################################
# HTS 2000 – Employment status (EMPLOY_BIN / EMPLOY_STATUS),
# TAZ-specific treatment (Phase 1A, Phase 1B, Phase 1A & 1B), descriptives,
# skewness, and boxplot
###############################################################################

# ---- 2) BUILD TAZ_TREAT_MASTER (Phase 1A + 1B, Option C staggered) ----
encode_status <- function(x) {
  dplyr::case_when(
    is.na(x)                               ~ NA_real_,
    stringr::str_to_lower(x) %in% c("Treated", "yes", "full")    ~ 1,
    stringr::str_to_lower(x) %in% c("Partial")                   ~ 0.5,
    stringr::str_to_lower(x) %in% c("Not_treated", "none", "no") ~ 0,
    TRUE                                                         ~ NA_real_
  )
}

## 2.1 Keep your original names but create clean alias columns
# Phase 1A: create clean aliases
TREATED_1A <- TREATED_1A %>%
  mutate(
    TAZ_ID   = ZONEID,
    Trt_2km  = `Treatment Status 2km (TrtSts_2km)`,
    Trt_1km  = `Treatment Status 1km (TrtSts_1km)`,
    Trt_500m = `Treatment Status 500m (TrtSts500m)`
  )

# Phase 1B: same idea
TREATED_1B <- TREATED_1B %>%
  mutate(
    TAZ_ID   = ZONEID,
    Trt_2km  = `Treatment Status 2km (TrtSts_2km)`,
    Trt_1km  = `Treatment Status 1km (TrtSts_1km)`,
    Trt_500m = `Treatment Status 500m (TrtSts500m)`
  )

# Phase 1A+1B combined
TREATED_1A1B <- TREATED_1A1B %>%
  mutate(
    TAZ_ID   = ZONEID,
    Trt_2km  = `Treatment Status 2km (TrtSts_2km)`,
    Trt_1km  = `Treatment Status 1km (TrtSts_1km)`,
    Trt_500m = `Treatment Status 500m (TrtSts500m)`
  )

## 2.2 Robust encoding function using the actual strings,define encode_status() to match typical English labels
encode_status <- function(x) {
  x_clean <- stringr::str_squish(stringr::str_to_lower(as.character(x)))
  
  dplyr::case_when(
    x_clean %in% c("treated", "treated area", "yes", "full")          ~ 1,
    x_clean %in% c("partial", "partially treated", "partially")       ~ 0.5,
    x_clean %in% c("not treated", "not_treated", "no", "none",
                   "untreated", "0", "")                              ~ 0,
    TRUE                                                              ~ NA_real_
  )
}

# 1 = fully treated within 1km; 0.5 = partially treated; 0 = not treated; NA = no data / outside corridor / etc.


## 2.3 Build Cleaned Treatment tables for Phases 1a/1b/1ab
## Phase 1A
treat_1a_clean <- TREATED_1A %>%
  transmute(
    TAZ_ID,
    phase1a_500m_cat  = Trt_500m,
    phase1a_1000m_cat = Trt_1km,
    phase1a_2000m_cat = Trt_2km,
    phase1a_500m      = encode_status(Trt_500m),
    phase1a_1000m     = encode_status(Trt_1km),
    phase1a_2000m     = encode_status(Trt_2km)
  )

## Phase 1B
treat_1b_clean <- TREATED_1B %>%
  transmute(
    TAZ_ID,
    phase1b_500m_cat  = Trt_500m,
    phase1b_1000m_cat = Trt_1km,
    phase1b_2000m_cat = Trt_2km,
    phase1b_500m      = encode_status(Trt_500m),
    phase1b_1000m     = encode_status(Trt_1km),
    phase1b_2000m     = encode_status(Trt_2km)
  )

## Phase 1A+1B combined
treat_1ab_clean <- TREATED_1A1B %>%
  transmute(
    TAZ_ID,
    phase1ab_500m_cat  = Trt_500m,
    phase1ab_1000m_cat = Trt_1km,
    phase1ab_2000m_cat = Trt_2km,
    phase1ab_500m      = encode_status(Trt_500m),
    phase1ab_1000m     = encode_status(Trt_1km),
    phase1ab_2000m     = encode_status(Trt_2km)
  )

#Quick check (should show 0/0.5/1, not all NA)
table(treat_1a_clean$phase1a_500m, useNA = "ifany")
table(treat_1b_clean$phase1b_500m, useNA = "ifany")


## 2.4 Build TAZ_TREAT_MASTER with 1km as main band
# Using Main treatment definition = 1km band and then 0.5km (500m) and 2km = robustness bands 

TAZ_TREAT_MASTER <- treat_1a_clean %>%
  full_join(treat_1b_clean,  by = "TAZ_ID") %>%
  full_join(treat_1ab_clean, by = "TAZ_ID") %>%
  mutate(
    # ==========================
    # PHASE 1A – 1km band (main phase-specific spec)
    # ==========================
    phase1a_treat_1km   = case_when(
      phase1a_1000m == 1   ~ 1L,
      TRUE                 ~ 0L
    ),
    phase1a_partial_1km = case_when(
      phase1a_1000m == 0.5 ~ 1L,
      TRUE                 ~ 0L
    ),
    phase1a_treat_year_1km = case_when(
      phase1a_treat_1km == 1L ~ 2009L,  # Phase 1A opening year
      TRUE                    ~ NA_integer_
    ),
    
    # ==========================
    # PHASE 1B – 1km band
    # ==========================
    phase1b_treat_1km   = case_when(
      phase1b_1000m == 1   ~ 1L,
      TRUE                 ~ 0L
    ),
    phase1b_partial_1km = case_when(
      phase1b_1000m == 0.5 ~ 1L,
      TRUE                 ~ 0L
    ),
    phase1b_treat_year_1km = case_when(
      phase1b_treat_1km == 1L ~ 2013L,  # Phase 1B opening year
      TRUE                    ~ NA_integer_
    ),
    
    # ==========================
    # COMBINED Phase 1A+1B – 1km band (corridor)
    # ==========================
    phase1ab_treat_1km = case_when(
      phase1a_1000m == 1 | phase1b_1000m == 1 ~ 1L,
      TRUE                                    ~ 0L
    ),
    phase1ab_partial_1km = case_when(
      phase1ab_treat_1km == 1L ~ 0L,
      phase1a_1000m == 0.5 | phase1b_1000m == 0.5 ~ 1L,
      TRUE                                       ~ 0L
    ),
    phase1ab_treat_year_1km = case_when(
      phase1a_1000m == 1                          ~ 2009L, # served in 1A
      phase1a_1000m != 1 & phase1b_1000m == 1     ~ 2013L, # only served in 1B
      TRUE                                        ~ NA_integer_
    ),
    
    # ==========================
    # Robustness – corridor-style bands (500m & 2km)
    # ==========================
    
    # 500m corridor: full treatment in either phase
    treatment_year_500 = case_when(
      phase1a_500m == 1                         ~ 2009,
      phase1a_500m != 1 & phase1b_500m == 1     ~ 2013,
      TRUE                                      ~ NA_real_
    ),
    treat_main_500 = case_when(
      phase1a_500m == 1 | phase1b_500m == 1 ~ 1L,
      TRUE                                  ~ 0L
    ),
    treat_partial_500 = case_when(
      treat_main_500 == 1                                        ~ 0L,
      phase1a_500m == 0.5 | phase1b_500m == 0.5                 ~ 1L,
      TRUE                                                       ~ 0L
    ),
    
    # 2km corridor
    treatment_year_2km = case_when(
      phase1a_2000m == 1                         ~ 2009,
      phase1a_2000m != 1 & phase1b_2000m == 1    ~ 2013,
      TRUE                                       ~ NA_real_
    ),
    treat_main_2km = case_when(
      phase1a_2000m == 1 | phase1b_2000m == 1 ~ 1L,
      TRUE                                    ~ 0L
    ),
    treat_partial_2km = case_when(
      treat_main_2km == 1                                        ~ 0L,
      phase1a_2000m == 0.5 | phase1b_2000m == 0.5               ~ 1L,
      TRUE                                                       ~ 0L
    )
  )

TAZ_TREAT_MASTER <- TAZ_TREAT_MASTER %>%
  mutate(
    # Combined corridor 1km aliases
    treatment_year_1km   = phase1ab_treat_year_1km,
    treat_main_1km       = phase1ab_treat_1km,
    treat_partial_1km    = phase1ab_partial_1km
  )

# At this point you have:
# - Phase 1A, 1B, 1AB treatment at 1km (phase1a_treat_1km, phase1b_treat_1km, phase1ab_treat_1km...)
# - Corridor-style robustness bands at 500m and 2km (treat_main_500, treat_main_2km, etc.)

# Quick check
table(is.na(TAZ_TREAT_MASTER$treatment_year_1km))
table(TAZ_TREAT_MASTER$phase1a_treat_1km,  useNA = "ifany")  # 1A-only treated
table(TAZ_TREAT_MASTER$phase1b_treat_1km,  useNA = "ifany")  # 1B-only treated
table(TAZ_TREAT_MASTER$phase1ab_treat_1km, useNA = "ifany")  # union = corridor


# ---- 3) EMPLOYMENT RECODE (WRKSTAT → EMPLOY_BIN / EMPLOY_STATUS) ----
employment_var_2000 <- "WRKSTAT"

# TODO: confirm via table(PERSON2000$WRKSTAT, useNA = "ifany")
employed_codes_2000     <- c(1)      # employed
not_employed_codes_2000 <- c(2, 3, 4, 5, 6, 7, 8)      # not employed
missing_codes_2000      <- c(-100)   # missing/unknown

PERSON2000 <- PERSON2000 %>%
  mutate(
    WRKSTAT_NUM = suppressWarnings(as.numeric(.data[[employment_var_2000]])),
    
    EMPLOY_BIN = case_when(
      WRKSTAT_NUM %in% employed_codes_2000     ~ 1L,
      WRKSTAT_NUM %in% not_employed_codes_2000 ~ 2L,
      is.na(WRKSTAT_NUM) |
        WRKSTAT_NUM %in% missing_codes_2000    ~ 3L,
      TRUE                                     ~ 3L
    ),
    
    EMPLOY_STATUS = case_when(
      EMPLOY_BIN == 1L ~ 1L,
      EMPLOY_BIN == 2L ~ 0L,
      TRUE             ~ NA_integer_
    ),
    
    EMPLOY_BIN_lbl = factor(
      EMPLOY_BIN,
      levels = c(1L, 2L, 3L),
      labels = c("Employed", "Not employed", "Missing/Unknown")
    )
  )


# ---- 4) ATTACH HOME TAZ TO PERSON2000 >> Didn't need to do this for 2000 but will need it for 2014 and 2019----
# TODO: verify names(PERSON2000) and names(HZONE2000)
# Assumption:
#   PERSON2000: PINDEX, QNO, HZONE, WRKSTAT, AGE, WEIGHT
#   HZONE2000 : QNO, home_taz_2000, WEIGHT

PERSON2000 <- PERSON2000 %>%
  left_join(
    HZONE2000 %>% select(QNO, HZONE),
    by = "QNO"
  )


# ---- 5) MERGE TAZ_TREAT_MASTER INTO PERSON2000 + TIME VARIABLES (Phase 1AB 1km, Phase 1A 1km, Phase 1B 1km) ----
# Attach all phase-specific treatment info to PERSON2000 using Gauteng TAZ
PERSON2000 <- PERSON2000 %>%
  left_join(
    TAZ_TREAT_MASTER,
    by = c("HZONE" = "TAZ_ID")   # PERSON2000$HZONE ↔ TAZ_TREAT_MASTER$TAZ_ID
  )

# Quick sanity check:
names(PERSON2000)[grepl("phase1", names(PERSON2000))]

#Build three variants: Phase 1AB, Phase 1A, Phase 1B (1km band)
# Common survey year
survey_year_2000 <- 2000L

# ----- (a) Combined Phase 1AB corridor, 1km -----
survey_year_2000 <- 2000L

PERSON2000_1AB <- PERSON2000 %>%
  mutate(
    survey_year        = 2000L,
    treatment_year     = phase1ab_treat_year_1km,
    treat_main         = phase1ab_treat_1km,
    treat_partial_main = phase1ab_partial_1km,
    
    time_since_treat = case_when(
      !is.na(treatment_year) ~ survey_year - treatment_year,
      TRUE                   ~ NA_real_
    ),
    post_treat = case_when(
      !is.na(treatment_year) & survey_year >= treatment_year ~ 1L,
      !is.na(treatment_year) & survey_year <  treatment_year ~ 0L,
      TRUE                                                   ~ 0L
    ),
    
    # ---- Cleaned treatment labels ----
    treat_group = case_when(
      treat_main         == 1L ~ "Treated",
      treat_partial_main == 1L ~ "Partial treated",
      TRUE                     ~ "Not treated"
    ),
    
    w_per = WEIGHT
  )

# ----- (b) Phase 1A only, 1km -----
PERSON2000_1A <- PERSON2000 %>%
  mutate(
    survey_year        = 2000L,
    treatment_year     = phase1a_treat_year_1km,
    treat_main         = phase1a_treat_1km,
    treat_partial_main = phase1a_partial_1km,
    
    time_since_treat = case_when(
      !is.na(treatment_year) ~ survey_year - treatment_year,
      TRUE                   ~ NA_real_
    ),
    post_treat = case_when(
      !is.na(treatment_year) & survey_year >= treatment_year ~ 1L,
      !is.na(treatment_year) & survey_year <  treatment_year ~ 0L,
      TRUE                                                   ~ 0L
    ),
    
    treat_group = case_when(
      treat_main         == 1L ~ "Treated",
      treat_partial_main == 1L ~ "Partial treated",
      TRUE                     ~ "Not treated"
    ),
    
    w_per = WEIGHT
  )

# ----- (c) Phase 1B only, 1km -----
PERSON2000_1B <- PERSON2000 %>%
  mutate(
    survey_year        = 2000L,
    treatment_year     = phase1b_treat_year_1km,
    treat_main         = phase1b_treat_1km,
    treat_partial_main = phase1b_partial_1km,
    
    time_since_treat = case_when(
      !is.na(treatment_year) ~ survey_year - treatment_year,
      TRUE                   ~ NA_real_
    ),
    post_treat = case_when(
      !is.na(treatment_year) & survey_year >= treatment_year ~ 1L,
      !is.na(treatment_year) & survey_year <  treatment_year ~ 0L,
      TRUE                                                   ~ 0L
    ),
    
    treat_group = case_when(
      treat_main         == 1L ~ "Treated",
      treat_partial_main == 1L ~ "Partial treated",
      TRUE                     ~ "Not treated"
    ),
    
    w_per = WEIGHT
  )

#Sanity Checks
with(PERSON2000_1AB, table(treat_group, useNA = "ifany"))
with(PERSON2000_1A,  table(treat_group, useNA = "ifany"))
with(PERSON2000_1B,  table(treat_group, useNA = "ifany"))


# ---- 6) SUMMARY STATISTICS FOR EMPLOYMENT (2000) ----
library(dplyr)
library(e1071)
library(ggplot2)

#6.1 Overall descriptives for EMPLOY_STATUS: Working-age subset from the CORRECT dataset
person2000_working <- PERSON2000_1AB %>%
  filter(AGE >= 15, AGE <= 64)

emp_desc_2000_overall <- person2000_working %>%
  summarise(
    # counts
    n_total    = n(),
    n_missing  = sum(is.na(EMPLOY_STATUS)),
    n_nonmiss  = sum(!is.na(EMPLOY_STATUS)),
   
    # weighted sample size (non-missing only)
    w_n        = sum(w_per[!is.na(EMPLOY_STATUS)], na.rm = TRUE),
    
    # weighted mean
    w_mean     = sum(EMPLOY_STATUS * w_per, na.rm = TRUE) / w_n,
    
    # weighted variance & sd (population-style)
    w_var      = sum(w_per * (EMPLOY_STATUS - w_mean)^2, na.rm = TRUE) / w_n,
    w_sd       = sqrt(w_var),
    
    # range
    min_val    = min(EMPLOY_STATUS, na.rm = TRUE),
    max_val    = max(EMPLOY_STATUS, na.rm = TRUE)
  )

emp_desc_2000_overall

#To explicitly check for out-of-range values:
table(person2000_working_working$EMPLOY_STATUS, useNA = "ifany")


## 6.2 Descriptives by treatment group
library(dplyr)
emp_desc_2000_by_treat <- person2000_working %>%
  group_by(treat_group) %>%   # "Treated", "Partial treated", "Not treated"
  summarise(
    # Unweighted counts
    n_total   = n(),
    n_missing = sum(is.na(EMPLOY_STATUS)),
    n_nonmiss = sum(!is.na(EMPLOY_STATUS)),
    
    # Weighted N (only for non-missing EMPLOY_STATUS)
    w_n = sum(if_else(!is.na(EMPLOY_STATUS), w_per, 0), na.rm = TRUE),
    
    # Weighted mean (employment rate)
    w_mean = if_else(
      w_n > 0,
      sum(EMPLOY_STATUS * w_per, na.rm = TRUE) / w_n,
      NA_real_
    ),
    
    # Weighted variance & sd (population-style)
    w_var = if_else(
      w_n > 0,
      sum(w_per * (EMPLOY_STATUS - w_mean)^2, na.rm = TRUE) / w_n,
      NA_real_
    ),
    w_sd  = sqrt(w_var),
    
    # Range (to check for any weird values)
    min_val = suppressWarnings(min(EMPLOY_STATUS, na.rm = TRUE)),
    max_val = suppressWarnings(max(EMPLOY_STATUS, na.rm = TRUE)),
    
    .groups = "drop"
  )

emp_desc_2000_by_treat


## 6.2 "Employed vs Not employed" descriptives: Overall(working age)
library(dplyr)

person2000_working <- PERSON2000_1AB %>%
  filter(AGE >= 15, AGE <= 64)

# Overall counts + weighted shares for Employed vs Not employed
emp_overall_2000 <- person2000_working %>%
  mutate(
    EMPLOY_CAT = case_when(
      EMPLOY_STATUS == 1 ~ "Employed",
      EMPLOY_STATUS == 0 ~ "Not employed",
      TRUE               ~ "Missing"
    )
  ) %>%
  group_by(EMPLOY_CAT) %>%
  summarise(
    n_unweighted = n(),
    w_n          = sum(w_per, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  mutate(
    w_share = w_n / sum(w_n[EMPLOY_CAT != "Missing"])
  )

emp_overall_2000

## 6.3 "Employed vs Not employed" descriptives: By treatment group (also including missing counts)
emp_by_treat_2000 <- person2000_working %>%
  mutate(
    EMPLOY_CAT = case_when(
      EMPLOY_STATUS == 1 ~ "Employed",
      EMPLOY_STATUS == 0 ~ "Not employed",
      TRUE               ~ "Missing"
    )
  ) %>%
  group_by(treat_group, EMPLOY_CAT) %>%
  summarise(
    n_unweighted = n(),
    w_n          = sum(w_per, na.rm = TRUE),
    .groups      = "drop_last"
  ) %>%
  group_by(treat_group) %>%
  mutate(
    w_share = w_n / sum(w_n[EMPLOY_CAT != "Missing"])
  ) %>%
  ungroup()

emp_by_treat_2000


###############################################################################
# HTS 2014 – Employment status (EMPLOY_BIN / EMPLOY_STATUS),
# TAZ-specific treatment (Phase 1A, Phase 1B, Phase 1A & 1B), descriptives
###############################################################################
#STEP 1 — Recode Employment for 2014
#Code to recode 2014 employment
library(dplyr)

# Recode main occupation (q2_10) into EMPLOY_BIN (3-category) and EMPLOY_STATUS (binary)
PERSON2014 <- PERSON2014 %>%
  mutate(
    EMPLOY_BIN = case_when(
      q2_10 %in% c(1, 2)      ~ 1L,  # Employed (full-time or part-time)
      q2_10 %in% 3:12         ~ 2L,  # Not employed (all other non-workers)
      TRUE                    ~ 3L   # Missing/blank/NA
    ),
    EMPLOY_STATUS = case_when(
      q2_10 %in% c(1, 2)      ~ 1L,      # Employed
      q2_10 %in% 3:12         ~ 0L,      # Not employed
      TRUE                    ~ NA_real_ # Missing
    )
  )

#STEP 2 — Attach HOME TAZ (HZONE lookup)
#2014 does NOT have TAZ directly.
HZONE2014_LOOKUP = HZONE2014

# inspect the columns quickly (you can run this):
  names(HZONE2014)
head(HZONE2014, 10)

# Attach home TAZ (HZONE) to each person in 2014
PERSON2014_M <- PERSON2014 %>%
  left_join(
    HZONE2014 %>% select(q_id, HZONE),
    by = "q_id"   # <- if your person id is named differently, change here
  )

#sanity check:
table(is.na(PERSON2014_M$HZONE))

#STEP 3 — Attach treatment from TAZ_TREAT_MASTER (1km, 1AB)
#join via HZONE → TAZ_ID:
PERSON2014_T <- PERSON2014_M %>%
  left_join(
    TAZ_TREAT_MASTER,
    by = c("HZONE" = "TAZ_ID")
  )

#Check the treatment fields are now there:
names(PERSON2014_T)[grepl("phase1", names(PERSON2014_T))]

#STEP 4 — Build Phase 1AB (1km) treatment fields for 2014
library(dplyr)
# Example structure — adjust treatment vars & weight column names
# Phase 1AB – 2014 (1km)
PERSON2014_1AB <- PERSON2014_T %>%
  mutate(
    survey_year        = 2014L,
    treatment_year     = phase1ab_treat_year_1km,
    treat_main         = phase1ab_treat_1km,
    treat_partial_main = phase1ab_partial_1km,
    time_since_treat = if_else(
      !is.na(treatment_year),
      survey_year - treatment_year,
      NA_real_
    ),
    post_treat = case_when(
      !is.na(treatment_year) & survey_year >= treatment_year ~ 1L,
      !is.na(treatment_year) & survey_year <  treatment_year ~ 0L,
      TRUE                                                   ~ 0L
    ),
    treat_group = case_when(
      treat_main         == 1L ~ "Treated",
      treat_partial_main == 1L ~ "Partial treated",
      TRUE                     ~ "Not treated"
    ),
    w_per = PP_Weight   
  )

# Phase 1A – 2014 (1km)
PERSON2014_1A <- PERSON2014_T %>%
  mutate(
    survey_year        = 2014L,
    treatment_year     = phase1a_treat_year_1km,
    treat_main         = phase1a_treat_1km,
    treat_partial_main = phase1a_partial_1km,
    time_since_treat = if_else(
      !is.na(treatment_year),
      survey_year - treatment_year,
      NA_real_
    ),
    post_treat = case_when(
      !is.na(treatment_year) & survey_year >= treatment_year ~ 1L,
      !is.na(treatment_year) & survey_year <  treatment_year ~ 0L,
      TRUE                                                   ~ 0L
    ),
    treat_group = case_when(
      treat_main         == 1L ~ "Treated",
      treat_partial_main == 1L ~ "Partial treated",
      TRUE                     ~ "Not treated"
    ),
    w_per = PP_Weight
  )

# Phase 1B – 2014 (1km)
PERSON2014_1B <- PERSON2014_T %>%
  mutate(
    survey_year        = 2014L,
    treatment_year     = phase1b_treat_year_1km,
    treat_main         = phase1b_treat_1km,
    treat_partial_main = phase1b_partial_1km,
    time_since_treat = if_else(
      !is.na(treatment_year),
      survey_year - treatment_year,
      NA_real_
    ),
    post_treat = case_when(
      !is.na(treatment_year) & survey_year >= treatment_year ~ 1L,
      !is.na(treatment_year) & survey_year <  treatment_year ~ 0L,
      TRUE                                                   ~ 0L
    ),
    treat_group = case_when(
      treat_main         == 1L ~ "Treated",
      treat_partial_main == 1L ~ "Partial treated",
      TRUE                     ~ "Not treated"
    ),
    w_per = PP_Weight
  )

# Optional sanity check:
table(PERSON2014_1AB$treat_group, useNA = "ifany")

#STEP 5 - Restrict to working-age (15–64), non-missing employment + weight
DATA2014 <- PERSON2014_1AB %>%
  filter(
    age >= 15,
    age <= 64,
    !is.na(EMPLOY_STATUS),
    !is.na(w_per)
  )

#STEP 6 - Overall employment rate (weighted)
overall2014 <- DATA2014 %>%
  summarise(
    w_n           = sum(w_per, na.rm = TRUE),
    employment_rt = sum(EMPLOY_STATUS * w_per, na.rm = TRUE) / w_n
  )

overall2014

# 6B. Employment rate by treatment group (Treated/Partial/Not treated)
by_treat2014 <- DATA2014 %>%
  group_by(treat_group) %>%
  summarise(
    w_n           = sum(w_per, na.rm = TRUE),
    employment_rt = sum(EMPLOY_STATUS * w_per, na.rm = TRUE) / w_n,
    .groups       = "drop"
  )

by_treat2014

# 6C. Employed vs Not employed (overall)
emp_vs_not_2014 <- DATA2014 %>%
  mutate(emp_cat = if_else(EMPLOY_STATUS == 1L, "Employed", "Not employed")) %>%
  group_by(emp_cat) %>%
  summarise(
    w_n   = sum(w_per, na.rm = TRUE),
    share = w_n / sum(w_n),
    .groups = "drop"
  )

emp_vs_not_2014

# 6D. Employed vs Not employed by treatment group
emp_vs_not_by_treat_2014 <- DATA2014 %>%
  mutate(emp_cat = if_else(EMPLOY_STATUS == 1L, "Employed", "Not employed")) %>%
  group_by(treat_group, emp_cat) %>%
  summarise(
    w_n = sum(w_per, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  group_by(treat_group) %>%
  mutate(share = w_n / sum(w_n)) %>%
  ungroup()

emp_vs_not_by_treat_2014

#6E. Summary block (total observations, weighted N, mean, sd, min/max)
summary_block <- function(df, var, weight) {
  
  y <- df[[var]]
  w <- df[[weight]]
  
  # Handle missing weights
  w[is.na(w)] <- 0
  
  # Weighted N
  w_n <- sum(w[!is.na(y)], na.rm = TRUE)
  
  tibble::tibble(
    Metric = c(
      "Total observations",
      "Missing",
      "Non-missing",
      "Weighted N",
      "Mean",
      "Std. deviation",
      "Minimum",
      "Maximum"
    ),
    Value = c(
      length(y),
      sum(is.na(y)),
      sum(!is.na(y)),
      w_n,
      ifelse(w_n > 0,
             sum(y * w, na.rm = TRUE) / w_n,
             NA_real_
      ),
      ifelse(w_n > 0,
             sqrt( sum(w * (y - (sum(y * w) / w_n))^2, na.rm = TRUE) / w_n ),
             NA_real_
      ),
      ifelse(all(is.na(y)), NA_real_, min(y, na.rm = TRUE)),
      ifelse(all(is.na(y)), NA_real_, max(y, na.rm = TRUE))
    )
  )
}

summary2014 <- summary_block(DATA2014, var = "EMPLOY_STATUS", weight = "w_per")
summary2014

#6F. Data counts for Treated / Partial / Not Treated (1AB, 1A, 1B)
library(dplyr)
library(tibble)

combined_counts <- bind_rows(
  # -------------------- PHASE 1AB --------------------
  PERSON2014_T %>%
    mutate(
      Phase = "1AB",
      treat_group = case_when(
        phase1ab_treat_1km == 1 ~ "Treated",
        phase1ab_partial_1km == 1 ~ "Partial treated",
        TRUE ~ "Not treated"
      )
    ),
  
  # -------------------- PHASE 1A --------------------
  PERSON2014_T %>%
    mutate(
      Phase = "1A",
      treat_group = case_when(
        phase1a_treat_1km == 1 ~ "Treated",
        phase1a_partial_1km == 1 ~ "Partial treated",
        TRUE ~ "Not treated"
      )
    ),
  
  # -------------------- PHASE 1B --------------------
  PERSON2014_T %>%
    mutate(
      Phase = "1B",
      treat_group = case_when(
        phase1b_treat_1km == 1 ~ "Treated",
        phase1b_partial_1km == 1 ~ "Partial treated",
        TRUE ~ "Not treated"
      )
    )
) %>%
  group_by(Phase, treat_group) %>%
  summarise(
    n_unweighted = n(),
    w_total      = sum(PP_Weight, na.rm = TRUE),     # Weighted population count
    .groups      = "drop"
  ) %>%
  arrange(factor(Phase, levels = c("1AB", "1A", "1B")),
          factor(treat_group, levels = c("Treated", "Partial treated", "Not treated")))

combined_counts


###############################################################################
# HTS 2019 – Employment status (EMPLOY_BIN / EMPLOY_STATUS),
# TAZ-specific treatment (Phase 1A, Phase 1B, Phase 1A & 1B), descriptives
###############################################################################
#Step A — Recode based on occupation category
PERSON2019 <- PERSON2019 %>%
  mutate(
    EMPLOY_BIN = case_when(
      # 1 = Employed
      Occupation_Description %in% c("Fulltime worker", "Parttime worker") ~ 1L,
      
      # 2 = Not employed
      Occupation_Description %in% c(
        "Child staying at home",
        "Housewife or Husband",
        "Learner High school learner",
        "Learner Pre school child",
        "Learner Primary school",
        "Learner University or College st",
        "Other",
        "Pensioner or Retired",
        "Unable to work handicapped or il",
        "Unemployed would like to work"
      ) ~ 2L,
      
      # 3 = Missing / Unspecified / NA
      TRUE ~ 3L
    ),
    
    EMPLOY_STATUS = case_when(
      Occupation_Description %in% c("Fulltime worker", "Parttime worker") ~ 1L,
      Occupation_Description %in% c(
        "Child staying at home",
        "Housewife or Husband",
        "Learner High school learner",
        "Learner Pre school child",
        "Learner Primary school",
        "Learner University or College st",
        "Other",
        "Pensioner or Retired",
        "Unable to work handicapped or il",
        "Unemployed would like to work"
      ) ~ 0L,
      TRUE ~ NA_real_
    )
  )

#Check results:
table(PERSON2019$EMPLOY_STATUS, useNA = "ifany")
table(PERSON2019$EMPLOY_BIN, useNA = "ifany")

# Inspect HZONE2019 to confirm join keys and columns
names(HZONE2019)
head(HZONE2019, 10)

#Step B - Attach home TAZ to HZONE2019 
library(dplyr)

#Attach home TAZ from HZONE2019
PERSON2019_M <- PERSON2019 %>%
  left_join(
    HZONE2019 %>% select(H_ID, HZONE),
    by = "H_ID"
  )

# Check the merge
table(is.na(PERSON2019_M$HZONE))


#Step C - Attach treatment fields (Phase 1AB, 1A, 1B) from TAZ_TREAT_MASTER
PERSON2019_T <- PERSON2019_M %>%
  filter(!is.na(HZONE)) %>%              # keep only those with home TAZ
  left_join(
    TAZ_TREAT_MASTER,
    by = c("HZONE" = "TAZ_ID")
  )

# Check treatment columns exist
grep("phase1", names(PERSON2019_T), value = TRUE)


#Step D - Build 2019 Phase 1AB / 1A / 1B variants
#1️⃣ Load the correct sheet (Person_Data_Weights) from the correct file
library(readxl)
library(dplyr)

weights2019_path <- "Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Data/HTS/Mathetha_JoburgSurveys/2019/COJ_Data_Weights.xlsx"

WEIGHT2019_person <- read_excel(
  weights2019_path,
  sheet = "Person_Data_Weights"
) %>%
  select(MP_CODE, weights)

# Quick sanity check (optional)
summary(WEIGHT2019_person$weights)

#2️⃣ Attach final_weight onto PERSON2019_T using only MP_CODE
PERSON2019_T <- PERSON2019_T %>%
  select(-any_of(c("weights", "w_per")), everything()) %>%
  left_join(WEIGHT2019_person, by = "MP_CODE") %>%
  mutate(
    w_per = weights
  )

# Sanity check: weights should be mostly non-missing and reasonable in size
summary(PERSON2019_T$w_per)
sum(is.na(PERSON2019_T$w_per))

#3️⃣ Use final_weight in your Step D code
# 1. Phase 1AB (Main 1KM)
PERSON2019_1AB <- PERSON2019_T %>%
  mutate(
    survey_year        = 2019L,
    treatment_year     = phase1ab_treat_year_1km,
    treat_main         = phase1ab_treat_1km,
    treat_partial_main = phase1ab_partial_1km,
    
    time_since_treat = if_else(
      !is.na(treatment_year),
      survey_year - treatment_year,
      NA_real_
    ),
    post_treat = case_when(
      !is.na(treatment_year) & survey_year >= treatment_year ~ 1L,
      !is.na(treatment_year) & survey_year <  treatment_year ~ 0L,
      TRUE                                                   ~ 0L
    ),
    treat_group = case_when(
      treat_main         == 1L ~ "Treated",
      treat_partial_main == 1L ~ "Partial treated",
      TRUE                     ~ "Not treated"
    )
    # w_per already present from PERSON2019_T
  )

# 2. Phase 1A (1KM)
PERSON2019_1A <- PERSON2019_T %>%
  mutate(
    survey_year        = 2019L,
    treatment_year     = phase1a_treat_year_1km,
    treat_main         = phase1a_treat_1km,
    treat_partial_main = phase1a_partial_1km,
    
    time_since_treat = if_else(
      !is.na(treatment_year),
      survey_year - treatment_year,
      NA_real_
    ),
    post_treat = case_when(
      !is.na(treatment_year) & survey_year >= treatment_year ~ 1L,
      !is.na(treatment_year) & survey_year <  treatment_year ~ 0L,
      TRUE                                                   ~ 0L
    ),
    treat_group = case_when(
      treat_main         == 1L ~ "Treated",
      treat_partial_main == 1L ~ "Partial treated",
      TRUE                     ~ "Not treated"
    )
    # w_per already present
  )

# 3. Phase 1B (1KM)
PERSON2019_1B <- PERSON2019_T %>%
  mutate(
    survey_year        = 2019L,
    treatment_year     = phase1b_treat_year_1km,
    treat_main         = phase1b_treat_1km,
    treat_partial_main = phase1b_partial_1km,
    
    time_since_treat = if_else(
      !is.na(treatment_year),
      survey_year - treatment_year,
      NA_real_
    ),
    post_treat = case_when(
      !is.na(treatment_year) & survey_year >= treatment_year ~ 1L,
      !is.na(treatment_year) & survey_year <  treatment_year ~ 0L,
      TRUE                                                   ~ 0L
    ),
    treat_group = case_when(
      treat_main         == 1L ~ "Treated",
      treat_partial_main == 1L ~ "Partial treated",
      TRUE                     ~ "Not treated"
    )
    # w_per already present
  )


#Step E - 2019 employment descriptives for main spec (Phase 1AB)
#Working-age, non-missing:
DATA2019_1AB <- PERSON2019_1AB %>%
  filter(
    Age >= 15,
    Age <= 64,
    !is.na(EMPLOY_STATUS),
    !is.na(w_per)
  )

#Overall employment rate:
overall2019 <- DATA2019_1AB %>%
  summarise(
    w_n           = sum(w_per, na.rm = TRUE),
    employment_rt = sum(EMPLOY_STATUS * w_per, na.rm = TRUE) / w_n
  )

overall2019

#By treatment group:
by_treat2019 <- DATA2019_1AB %>%
  group_by(treat_group) %>%
  summarise(
    w_n           = sum(w_per, na.rm = TRUE),
    employment_rt = sum(EMPLOY_STATUS * w_per, na.rm = TRUE) / w_n,
    .groups       = "drop"
  )

by_treat2019

#Summary block (total observations, weighted N, mean, sd, min/max)
summary_2019_block <- DATA2019_1AB %>%
  summarise(
    total_obs     = n(),
    missing       = sum(is.na(EMPLOY_STATUS)),
    non_missing   = sum(!is.na(EMPLOY_STATUS)),
    
    # weighted N
    weighted_N    = sum(w_per, na.rm = TRUE),
    
    # weighted mean
    w_mean        = sum(EMPLOY_STATUS * w_per, na.rm = TRUE) / weighted_N,
    
    # weighted variance
    w_var         = sum(w_per * (EMPLOY_STATUS - w_mean)^2, na.rm = TRUE) / weighted_N,
    
    # weighted SD
    w_sd          = sqrt(w_var),
    
    # min/max
    min_value     = min(EMPLOY_STATUS, na.rm = TRUE),
    max_value     = max(EMPLOY_STATUS, na.rm = TRUE)
  )

summary_2019_block

#Employed vs unemployed by treatment
emp_vs_not_by_treat_2019 <- DATA2019_1AB %>%
  mutate(
    emp_cat = if_else(EMPLOY_STATUS == 1L, "Employed", "Not employed")
  ) %>%
  group_by(treat_group, emp_cat) %>%
  summarise(
    n_unweighted = n(),
    w_n          = sum(w_per, na.rm = TRUE),
    .groups      = "drop_last"
  ) %>%
  group_by(treat_group) %>%
  mutate(
    w_share = w_n / sum(w_n)   # weighted share within each treatment group
  ) %>%
  ungroup()

emp_vs_not_by_treat_2019


#Overall Employed vs Not Employed table (not split by treatment),
emp_vs_not_overall_2019 <- DATA2019_1AB %>%
  mutate(
    emp_cat = if_else(EMPLOY_STATUS == 1L, "Employed", "Not employed")
  ) %>%
  group_by(emp_cat) %>%
  summarise(
    n_unweighted = n(),
    w_n          = sum(w_per, na.rm = TRUE),
    share        = w_n / sum(w_n),
    .groups      = "drop"
  )

emp_vs_not_overall_2019


##############################################################################
### Cross-sectional descriptive breakdown Employment status
###############################
#Reusable Function to Compute Descriptives Per Year & Phase
library(dplyr)
compute_cross_sectional <- function(df, survey_year, phase_label) {
  
  df %>%
    mutate(
      Phase = phase_label,
      TreatGroup = case_when(
        treated_main == 1 ~ "Treated",
        partial_main == 1 ~ "Partial",
        TRUE ~ "Not treated"
      )
    ) %>%
    group_by(Phase, TreatGroup) %>%
    summarise(
      sample_n   = n(),
      weighted_N = sum(w_per, na.rm = TRUE),
      employ_rate = weighted.mean(EMPLOY_STATUS, w_per, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Year = survey_year) %>%
    select(Year, Phase, TreatGroup, sample_n, weighted_N, employ_rate)
}

#Define a tiny new function with no case_when at all
library(dplyr)
#Compute_cross_sectional()
compute_cross_sectional <- function(df, year_label, phase_label) {
  df %>%
    group_by(
      Year       = survey_year,      # should already be 2000/2014/2019
      Phase      = phase_label,
      TreatGroup = treat_group
    ) %>%
    summarise(
      # how many *rows* in group (regardless of missing EMPLOY_STATUS)
      sample_n = n(),
      
      # total weighted population in group (regardless of employment)
      weighted_N = sum(w_per, na.rm = TRUE),
      
      # weighted employment rate
      employ_rate = {
        ok <- !is.na(EMPLOY_STATUS) & !is.na(w_per)
        
        if (!any(ok)) {
          NA_real_   # no usable data in this group
        } else {
          w  <- w_per[ok]
          y  <- EMPLOY_STATUS[ok] == 1
          
          if (sum(w) == 0) {
            NA_real_
          } else {
            sum(y * w) / sum(w)
          }
        }
      },
      .groups = "drop"
    )
}

#Run your 9 cross-section objects and bind them:
# 2000
cx_2000_1A  <- compute_cross_sectional(PERSON2000_1A,  2000, "1A")
cx_2000_1B  <- compute_cross_sectional(PERSON2000_1B,  2000, "1B")
cx_2000_1AB <- compute_cross_sectional(PERSON2000_1AB, 2000, "1AB")

# 2014
cx_2014_1A  <- compute_cross_sectional(PERSON2014_1A,  2014, "1A")
cx_2014_1B  <- compute_cross_sectional(PERSON2014_1B,  2014, "1B")
cx_2014_1AB <- compute_cross_sectional(PERSON2014_1AB, 2014, "1AB")

# 2019
cx_2019_1A  <- compute_cross_sectional(PERSON2019_1A,  2019, "1A")
cx_2019_1B  <- compute_cross_sectional(PERSON2019_1B,  2019, "1B")
cx_2019_1AB <- compute_cross_sectional(PERSON2019_1AB, 2019, "1AB")

cross_sectional_table <- bind_rows(
  cx_2000_1A, cx_2000_1B, cx_2000_1AB,
  cx_2014_1A, cx_2014_1B, cx_2014_1AB,
  cx_2019_1A, cx_2019_1B, cx_2019_1AB
)

#Quick optional sanity checks you can run
# Check raw 2019 employment rate (no weights)
PERSON2019_T %>%
  filter(!is.na(EMPLOY_STATUS)) %>%
  summarise(
    raw_rate = mean(EMPLOY_STATUS == 1)
  )

# Check weighted 2019 employment rate overall
PERSON2019_T %>%
  filter(!is.na(EMPLOY_STATUS), !is.na(w_per)) %>%
  summarise(
    weighted_rate = sum((EMPLOY_STATUS == 1) * w_per) / sum(w_per)
  )

cross_sectional_table
print(cross_sectional_table, n = Inf)



###############################################################################
# Employment status Panel
# TAZ-specific treatment (Phase 1A, Phase 1B, Phase 1A & 1B), 
###############################################################################
#Build panels for each phase (1AB, 1A, 1B)
## Create DATA2000_1AB
DATA2000_1AB <- PERSON2000_1AB %>%
  filter(
    AGE >= 15,
    AGE <= 64,
    !is.na(EMPLOY_STATUS),
    !is.na(w_per),
    !is.na(HZONE)
  )

## 2000 1A
DATA2000_1A <- PERSON2000_1A %>%
  filter(
    AGE >= 15,
    AGE <= 64,
    !is.na(EMPLOY_STATUS),
    !is.na(w_per),
    !is.na(HZONE)
  )

## 2000 1B
DATA2000_1B <- PERSON2000_1B %>%
  filter(
    AGE >= 15,
    AGE <= 64,
    !is.na(EMPLOY_STATUS),
    !is.na(w_per),
    !is.na(HZONE)
  )

####-------------------------
#### 2014 – Phase 1AB / 1A / 1B
#### (note: age variable is `age`)
####-------------------------
library(dplyr)

# -------------------------------------------------------
# 1) Attach HOME TAZ from HZONE2014
# -------------------------------------------------------
PERSON2014_M <- PERSON2014 %>%
  left_join(
    HZONE2014 %>% select(q_id, HZONE),
    by = "q_id"
  )

# -------------------------------------------------------
# 2) Attach TAZ treatment master
# -------------------------------------------------------
PERSON2014_T <- PERSON2014_M %>%
  filter(!is.na(HZONE)) %>%
  left_join(
    TAZ_TREAT_MASTER,
    by = c("HZONE" = "TAZ_ID")
  )

# -------------------------------------------------------
# 3) Create Phase 1AB dataset
# -------------------------------------------------------
PERSON2014_1AB <- PERSON2014_T %>%
  mutate(
    survey_year        = 2014L,
    
    treatment_year     = phase1ab_treat_year_1km,
    treat_main         = phase1ab_treat_1km,
    treat_partial_main = phase1ab_partial_1km,
    
    time_since_treat = if_else(
      !is.na(treatment_year),
      survey_year - treatment_year,
      NA_real_
    ),
    
    post_treat = case_when(
      !is.na(treatment_year) & survey_year >= treatment_year ~ 1L,
      !is.na(treatment_year) & survey_year <  treatment_year ~ 0L,
      TRUE                                                   ~ 0L
    ),
    
    treat_group = case_when(
      treat_main         == 1L ~ "Treated",
      treat_partial_main == 1L ~ "Partial treated",
      TRUE                     ~ "Not treated"
    ),
    
    w_per = PP_Weight
  )

# -------------------------------------------------------
# 4) Create Phase 1A dataset
# -------------------------------------------------------
PERSON2014_1A <- PERSON2014_T %>%
  mutate(
    survey_year        = 2014L,
    
    treatment_year     = phase1a_treat_year_1km,
    treat_main         = phase1a_treat_1km,
    treat_partial_main = phase1a_partial_1km,
    
    time_since_treat = if_else(
      !is.na(treatment_year),
      survey_year - treatment_year,
      NA_real_
    ),
    
    post_treat = case_when(
      !is.na(treatment_year) & survey_year >= treatment_year ~ 1L,
      !is.na(treatment_year) & survey_year <  treatment_year ~ 0L,
      TRUE                                                   ~ 0L
    ),
    
    treat_group = case_when(
      treat_main         == 1L ~ "Treated",
      treat_partial_main == 1L ~ "Partial treated",
      TRUE                     ~ "Not treated"
    ),
    
    w_per = PP_Weight
  )

# -------------------------------------------------------
# 5) Create Phase 1B dataset
# -------------------------------------------------------
PERSON2014_1B <- PERSON2014_T %>%
  mutate(
    survey_year        = 2014L,
    
    treatment_year     = phase1b_treat_year_1km,
    treat_main         = phase1b_treat_1km,
    treat_partial_main = phase1b_partial_1km,
    
    time_since_treat = if_else(
      !is.na(treatment_year),
      survey_year - treatment_year,
      NA_real_
    ),
    
    post_treat = case_when(
      !is.na(treatment_year) & survey_year >= treatment_year ~ 1L,
      !is.na(treatment_year) & survey_year <  treatment_year ~ 0L,
      TRUE                                                   ~ 0L
    ),
    
    treat_group = case_when(
      treat_main         == 1L ~ "Treated",
      treat_partial_main == 1L ~ "Partial treated",
      TRUE                     ~ "Not treated"
    ),
    
    w_per = PP_Weight
  )


DATA2014_1AB <- PERSON2014_1AB %>%
  filter(age >= 15, age <= 64,
         !is.na(EMPLOY_STATUS),
         !is.na(w_per),
         !is.na(HZONE))

DATA2014_1A <- PERSON2014_1A %>%
  filter(age >= 15, age <= 64,
         !is.na(EMPLOY_STATUS),
         !is.na(w_per),
         !is.na(HZONE))

DATA2014_1B <- PERSON2014_1B %>%
  filter(age >= 15, age <= 64,
         !is.na(EMPLOY_STATUS),
         !is.na(w_per),
         !is.na(HZONE))

####-------------------------
#### 2019 – Phase 1AB / 1A / 1B
#### (note: age variable is `Age`)
####-------------------------
DATA2019_1AB <- PERSON2019_1AB %>%
  filter(
    Age >= 15,
    Age <= 64,
    !is.na(EMPLOY_STATUS),
    !is.na(w_per),
    !is.na(HZONE)
  )

DATA2019_1A <- PERSON2019_1A %>%
  filter(
    Age >= 15,
    Age <= 64,
    !is.na(EMPLOY_STATUS),
    !is.na(w_per),
    !is.na(HZONE)
  )

DATA2019_1B <- PERSON2019_1B %>%
  filter(
    Age >= 15,
    Age <= 64,
    !is.na(EMPLOY_STATUS),
    !is.na(w_per),
    !is.na(HZONE)
  )


## ---- Phase 1AB panel ----
panel_1AB <- bind_rows(
  DATA2000_1AB %>% mutate(survey_year = 2000L),
  DATA2014_1AB %>% mutate(survey_year = 2014L),
  DATA2019_1AB %>% mutate(survey_year = 2019L)
) %>%
  filter(!is.na(EMPLOY_STATUS),
         !is.na(w_per),
         !is.na(HZONE))

## ---- Phase 1A panel ----
panel_1A <- bind_rows(
  DATA2000_1A %>% mutate(survey_year = 2000L),
  DATA2014_1A %>% mutate(survey_year = 2014L),
  DATA2019_1A %>% mutate(survey_year = 2019L)
) %>%
  filter(!is.na(EMPLOY_STATUS),
         !is.na(w_per),
         !is.na(HZONE))

## ---- Phase 1B panel ----
panel_1B <- bind_rows(
  DATA2000_1B %>% mutate(survey_year = 2000L),
  DATA2014_1B %>% mutate(survey_year = 2014L),
  DATA2019_1B %>% mutate(survey_year = 2019L)
) %>%
  filter(!is.na(EMPLOY_STATUS),
         !is.na(w_per),
         !is.na(HZONE))

## Make sure treatment dummies exist in each panel
library(dplyr)

panel_1AB <- panel_1AB %>%
  mutate(
    treated_main = as.integer(treat_group == "Treated"),
    partial_main = as.integer(treat_group == "Partial treated")
  )

panel_1A <- panel_1A %>%
  mutate(
    treated_main = as.integer(treat_group == "Treated"),
    partial_main = as.integer(treat_group == "Partial treated")
  )

panel_1B <- panel_1B %>%
  mutate(
    treated_main = as.integer(treat_group == "Treated"),
    partial_main = as.integer(treat_group == "Partial treated")
  )

#Quick check:
table(panel_1AB$treated_main, panel_1AB$partial_main)


###############################################################################
# Employment status Pre-period (2000) check
# TAZ-specific treatment (Phase 1A, Phase 1B, Phase 1A & 1B)
# This isn’t a full “parallel trends test” (we only have one pre), but it lets you see if treated vs control look wildly different already in 2000.
###############################################################################
library(dplyr)

# Pre-period table for Phase 1AB
pre2000_1AB <- panel_1AB %>%
  filter(survey_year == 2000) %>%
  mutate(
    group = case_when(
      treated_main == 1L ~ "Treated",
      partial_main == 1L ~ "Partial",
      TRUE               ~ "Not treated"
    )
  ) %>%
  group_by(group) %>%
  summarise(
    n_unweighted = n(),
    w_N          = sum(w_per, na.rm = TRUE),
    emp_rate     = sum(EMPLOY_STATUS * w_per, na.rm = TRUE) / w_N,
    .groups      = "drop"
  )

pre2000_1AB
#table shows pre-BRT employment levels by treatment status 1AB


# Pre-period table for Phase 1A
pre2000_1A <- panel_1A %>%
  filter(survey_year == 2000) %>%
  mutate(
    group = case_when(
      treated_main == 1L ~ "Treated",
      partial_main == 1L ~ "Partial",
      TRUE               ~ "Not treated"
    )
  ) %>%
  group_by(group) %>%
  summarise(
    n_unweighted = n(),
    w_N          = sum(w_per, na.rm = TRUE),
    emp_rate     = sum(EMPLOY_STATUS * w_per, na.rm = TRUE) / w_N,
    .groups      = "drop"
  )

pre2000_1A

# Pre-period table for Phase 1AB
pre2000_1B <- panel_1B %>%
  filter(survey_year == 2000) %>%
  mutate(
    group = case_when(
      treated_main == 1L ~ "Treated",
      partial_main == 1L ~ "Partial",
      TRUE               ~ "Not treated"
    )
  ) %>%
  group_by(group) %>%
  summarise(
    n_unweighted = n(),
    w_N          = sum(w_per, na.rm = TRUE),
    emp_rate     = sum(EMPLOY_STATUS * w_per, na.rm = TRUE) / w_N,
    .groups      = "drop"
  )

pre2000_1B


###############################################################################
# Employment status Model
# TAZ-specific treatment (Phase 1A, Phase 1B, Phase 1A & 1B), 
###############################################################################
# Multi-period DiD for Phase 1AB (2000 baseline, 2014 & 2019 post) 
# to get separate impacts for 2014 and 2019, with: TAZ fixed effects (HZONE); Year fixed effects (survey_year); Clustered by TAZ

# Repeat for Phase 1AB
library(fixest)

did_1AB <- feols(
  EMPLOY_STATUS ~ 
    i(survey_year, treated_main, ref = 2000) +
    i(survey_year, partial_main, ref = 2000) |
    HZONE + survey_year,
  data    = panel_1AB,
  weights = ~ w_per,
  cluster = ~ HZONE
)

summary(did_1AB)

# Repeat for Phase 1A and 1B
# Quick check for 1A and 1B
table(panel_1A$treated_main, panel_1A$partial_main)
table(panel_1B$treated_main, panel_1B$partial_main)

# Phase 1A
did_1A <- feols(
  EMPLOY_STATUS ~ 
    i(survey_year, treated_main, ref = 2000) +
    i(survey_year, partial_main, ref = 2000) |
    HZONE + survey_year,
  data    = panel_1A,
  weights = ~ w_per,
  cluster = ~ HZONE
)

summary(did_1A)

# Phase 1B
did_1B <- feols(
  EMPLOY_STATUS ~ 
    i(survey_year, treated_main, ref = 2000) +
    i(survey_year, partial_main, ref = 2000) |
    HZONE + survey_year,
  data    = panel_1B,
  weights = ~ w_per,
  cluster = ~ HZONE
)

summary(did_1B)


## Present Simple baseline model: present this compactly in the dissertation: pre-period table + 3×4 coefficient table
library(broom)
library(dplyr)

extract_did <- function(model, phase_label) {
  tidy(model) %>%
    filter(grepl("survey_year::", term)) %>%
    mutate(
      Phase = phase_label,
      Effect = case_when(
        term == "survey_year::2014:treated_main"  ~ "Treated × 2014",
        term == "survey_year::2019:treated_main"  ~ "Treated × 2019",
        term == "survey_year::2014:partial_main"  ~ "Partial × 2014",
        term == "survey_year::2019:partial_main"  ~ "Partial × 2019",
        TRUE                                      ~ term
      )
    ) %>%
    select(Phase, Effect, estimate, std.error, p.value)
}

results_all <- bind_rows(
  extract_did(did_1A,  "Phase 1A"),
  extract_did(did_1B,  "Phase 1B"),
  extract_did(did_1AB, "Phase 1AB")
)

results_all


###############################################################################
# Pre-Post Table for Employment status
# TAZ-specific treatment (Phase 1A, Phase 1B, Phase 1A & 1B),
###############################################################################
# Simple pre–post table for Phase 1AB
library(dplyr)

# Helper to get weighted mean
w_mean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)

make_prepost_phase <- function(panel, phase_label, post_year) {
  
  dat <- panel %>%
    filter(
      survey_year %in% c(2000L, post_year),
      !is.na(EMPLOY_STATUS),
      !is.na(w_per)
    )
  
  # Treated = fully treated TAZs
  pre_T <- dat %>%
    filter(survey_year == 2000L, treated_main == 1L)
  post_T <- dat %>%
    filter(survey_year == post_year, treated_main == 1L)
  
  # Control = not treated (exclude partial)
  pre_C <- dat %>%
    filter(survey_year == 2000L, treated_main == 0L, partial_main == 0L)
  post_C <- dat %>%
    filter(survey_year == post_year, treated_main == 0L, partial_main == 0L)
  
  Treated_Pre   <- w_mean(pre_T$EMPLOY_STATUS,   pre_T$w_per)
  Treated_Post  <- w_mean(post_T$EMPLOY_STATUS,  post_T$w_per)
  Control_Pre   <- w_mean(pre_C$EMPLOY_STATUS,   pre_C$w_per)
  Control_Post  <- w_mean(post_C$EMPLOY_STATUS,  post_C$w_per)
  
  Delta_T <- Treated_Post  - Treated_Pre
  Delta_C <- Control_Post  - Control_Pre
  DID     <- Delta_T - Delta_C
  
  tibble::tibble(
    Phase           = phase_label,
    Post_Year       = post_year,
    Outcome         = "Employment Status",
    Standardised    = "EMPLOY_STATUS",
    Treated_Pre     = Treated_Pre,
    Treated_Post    = Treated_Post,
    Delta_T         = Delta_T,
    Control_Pre     = Control_Pre,
    Control_Post    = Control_Post,
    Delta_C         = Delta_C,
    DID_DeltaT_DeltaC = DID
  )
}

# Build the full table for all phases & post years
prepost_all <- dplyr::bind_rows(
  make_prepost_phase(panel_1A,  "Phase 1A",  2014L),
  make_prepost_phase(panel_1A,  "Phase 1A",  2019L),
  make_prepost_phase(panel_1B,  "Phase 1B",  2014L),
  make_prepost_phase(panel_1B,  "Phase 1B",  2019L),
  make_prepost_phase(panel_1AB, "Phase 1AB", 2014L),
  make_prepost_phase(panel_1AB, "Phase 1AB", 2019L)
)

prepost_all

options(tibble.width = Inf)
prepost_all


###############################################################################
# Parallel trends for Phase 1B
###############################################################################
library(fixest)

table(panel_1B$survey_year)
table(panel_1B$treated_main)

#Keep ONLY pre-period years (2000 & 2014)
pretrend_1B <- panel_1B %>%
  filter(survey_year %in% c(2000, 2014))
#Sanity:
table(pretrend_1B$survey_year)

#Estimate the Parallel Trends Test Model
library(fixest)

pt_1B <- feols(
  EMPLOY_STATUS ~ i(survey_year, treated_main, ref = 2000) |
    HZONE + survey_year,
  data    = pretrend_1B,
  weights = ~ w_per,
  cluster = ~ HZONE
)

summary(pt_1B)

#Produce a Plot (optional but recommended for dissertation) 
library(dplyr)
library(ggplot2)

plot_df <- pretrend_1B %>%
  group_by(survey_year, treated_main) %>%
  summarise(w_mean = weighted.mean(EMPLOY_STATUS, w_per, na.rm = TRUE)) %>%
  ungroup()

ggplot(plot_df, aes(x = survey_year, y = w_mean, color = factor(treated_main))) +
  geom_point(size = 3) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Control", "Treated")) +
  labs(
    title = "Parallel Trends Check — Phase 1B Only (Employment Status)",
    x = "Survey Year",
    y = "Weighted Employment Rate",
    color = "Group"
  ) +
  theme_minimal(base_size = 8.5)

#Create the Pre-period Summary Table 2000 & 2014 for Treated vs Control, only for Phase 1B.
library(dplyr)
pretrend_1B <- panel_1B %>%
  filter(survey_year %in% c(2000, 2014))
# Weighted means by group & year
pre_table_1B <- pretrend_1B %>%
  group_by(treated_main, survey_year) %>%
  summarise(
    n_unweighted = n(),
    w_N          = sum(w_per, na.rm = TRUE),
    w_mean       = weighted.mean(EMPLOY_STATUS, w_per, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(group = ifelse(treated_main == 1, "Treated", "Control"))

#Format into a nice 2000–2014 Δ table:
pre_delta_1B <- pre_table_1B %>%
  select(group, survey_year, w_mean) %>%
  tidyr::pivot_wider(
    names_from = survey_year,
    values_from = w_mean,
    names_prefix = "y_"
  ) %>%
  mutate(
    delta = y_2014 - y_2000
  )

pre_delta_1B

#Compute the DiD-style pre-period difference
pre_DiD_1B <- pre_delta_1B %>%
  select(group, delta) %>%
  tidyr::pivot_wider(names_from = group, values_from = delta) %>%
  mutate(ATT_pretrend = Treated - Control)

pre_DiD_1B

#Parallel Trends Regression (statistical test)
library(fixest)

pt_1B <- feols(
  EMPLOY_STATUS ~ i(survey_year, treated_main, ref = 2000) |
    HZONE + survey_year,
  data    = pretrend_1B,
  weights = ~ w_per,
  cluster = ~ HZONE
)

summary(pt_1B)

#Parallel Trends Plot (Dissertation-ready)
library(ggplot2)

plot_df <- pretrend_1B %>%
  group_by(survey_year, treated_main) %>%
  summarise(
    w_mean = weighted.mean(EMPLOY_STATUS, w_per, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Group = ifelse(treated_main == 1, "Treated", "Control"))

ggplot(plot_df, aes(x = survey_year, y = w_mean, color = Group)) +
  geom_point(size = 4) +
  geom_line(size = 1.4) +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "black") +
  labs(
    title = "Parallel Trends Check – Phase 1B Only (2000–2014)",
    subtitle = "Employment Status (Weighted Means)",
    x = "Survey Year (Pre-treatment only)",
    y = "Employment Rate (Weighted)",
    color = "Group"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Control" = "steelblue",  "Treated" = "firebrick"))
  
  

###############################################################################
# Save panels to Macbook documents
# TAZ-specific treatment (Phase 1A, Phase 1B, Phase 1A & 1B)
###############################################################################
#1️⃣ Folder + naming convention (simple + clear)
# Let’s pick a single root folder for all panels, e.g.:
  base_panel_dir <- "~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Processed Data/Panels"

#2️⃣ One reusable helper you can call from ANY script
#Put this in a small utility file, e.g. panel_utils.R, and source() it at the top of each outcome script.
# panel_utils.R
save_phase_panels <- function(outcome_name,
                              panel_1AB = NULL,
                              panel_1A  = NULL,
                              panel_1B  = NULL,
                              base_dir  = "~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Processed Data/Panels/") {
  
  # 1. Make outcome-specific folder
  out_dir <- file.path(base_dir, outcome_name)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # 2. Date tag for versioning
  date_tag <- format(Sys.Date(), "%Y-%m-%d")
  
  # 3. Helper to save one panel safely
  save_one <- function(panel, phase) {
    if (!is.null(panel)) {
      fname <- paste0("panel_", outcome_name, "_", phase, "_", date_tag, ".rds")
      fpath <- file.path(out_dir, fname)
      saveRDS(panel, fpath)
      message("Saved: ", fpath)
    }
  }
  
  save_one(panel_1AB, "1AB")
  save_one(panel_1A,  "1A")
  save_one(panel_1B,  "1B")
}

#3️⃣ How you’ll actually use it in each outcome script
# At the end of the script, after you’ve built panels:
  save_phase_panels(
    outcome_name = outcome_name,
    panel_1AB    = panel_1AB,
    panel_1A     = panel_1A,
    panel_1B     = panel_1B
  )
  
  