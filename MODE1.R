# =========================
# Rea Vaya SDID — MODE1c end-to-end
# Standardized name: MODE1c
# Description: Primary Mode - transit/Non-transit
# Type: Integer (plus optional factor label)
# =========================

#To save panels on Mac: At the top of your MAIN_PURP script:
source("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Processed Data/Panels/panel_utils.R")  # adjust path as needed
outcome_name <- "MODE1c"


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
# HTS 2000 – *Primary Trip Mode Conditional (MODE1c),
# TAZ-specific treatment (Phase 1A, Phase 1B, Phase 1A & 1B), descriptives
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


# ==========================
# ---- 3) Build person-level MODE1 from TRIP2000 ----
# ==========================
#MODE1 = 1 if the person uses public transit is Train, Bus, or Minibus Taxi
#MODE1 = 0 otherwise (including other transport mode)
library(dplyr)
# Build person-level MODE1 (conditional) from TRIP2000
build_person_mode1_conditional_2000 <- function(TRIP2000) {
  
  trips_clean <- TRIP2000 %>%
    mutate(
      MAINMODE = as.integer(MAINMODE),
      MAINMODE = if_else(MAINMODE == -100L, NA_integer_, MAINMODE)
    ) %>%
    filter(!is.na(ID), !is.na(QNO)) %>%
    filter(!is.na(MAINMODE))
  
  person_primary_mode <- trips_clean %>%
    group_by(ID, QNO) %>%
    count(MAINMODE, name = "n_mode") %>%
    arrange(desc(n_mode), MAINMODE) %>%
    slice(1) %>%
    ungroup() %>%
    rename(MODE1_raw = MAINMODE) %>%
    mutate(
      MODE1 = if_else(MODE1_raw %in% c(1L, 2L, 3L), 1L, 0L)
    )
  
  person_primary_mode
}

#Create MODE1_COND_2000 from TRIP2000
library(dplyr)

build_mode1_cond_2000 <- function(TRIP2000) {
  
  trips_clean <- TRIP2000 %>%
    mutate(
      MAINMODE = as.integer(MAINMODE),
      MAINMODE = if_else(MAINMODE == -100L, NA_integer_, MAINMODE)
    ) %>%
    filter(!is.na(ID), !is.na(QNO)) %>%
    filter(!is.na(MAINMODE))  # conditional: must have ≥1 valid mode
  
  # Person-level "primary mode" = modal MAINMODE across trips
  # Tie-breaker: lowest MAINMODE code
  mode1_cond <- trips_clean %>%
    group_by(ID, QNO) %>%
    count(MAINMODE, name = "n_mode") %>%
    arrange(desc(n_mode), MAINMODE) %>%
    slice(1) %>%
    ungroup() %>%
    rename(MODE1_raw = MAINMODE) %>%
    mutate(
      # Public transport = Train(1), Bus(2), Taxi(3)
      MODE1 = if_else(MODE1_raw %in% c(1L, 2L, 3L), 1L, 0L)
    )
  
  mode1_cond
}

MODE1_COND_2000 <- build_mode1_cond_2000(TRIP2000)

# quick check
MODE1_COND_2000 %>% count(MODE1_raw, sort = TRUE)
MODE1_COND_2000 %>% summarise(share_public = mean(MODE1))

#Join MODE1 to PERSON2000 (home HZONE + person weights)
PERSON_MODE1_2000 <- PERSON2000 %>%
  filter(!is.na(WEIGHT)) %>%                 # valid person weights
  select(ID, QNO, HZONE, WEIGHT, everything()) %>%
  left_join(
    MODE1_COND_2000 %>% select(ID, QNO, MODE1, MODE1_raw),
    by = c("ID", "QNO")
  ) %>%
  filter(!is.na(MODE1)) %>%                  # keep CONDITIONAL universe
  mutate(
    survey_year = 2000,
    outcome = "MODE1_public_transport"
  )

#Attach treatment (by HZONE)
PERSON_MODE1_2000 <- PERSON_MODE1_2000 %>%
  left_join(
    TAZ_TREAT_2000 %>% select(HZONE, TREATED_1A, TREATED_1B, TREATED_1A1B),
    by = "HZONE"
  )











# =========================
# Join MODE1 onto PERSON2000 and keep home HZONE + person weight + treatments
# (This is your analysis dataset for 2000 conditional MODE1)
# =========================
PERSON_MODE1_2000 <- PERSON2000 %>%
  filter(!is.na(WEIGHT)) %>%                           # unconditional base requires valid person weights
  select(
    ID, QNO, HZONE, WEIGHT,
    TREATED_1A1B, TREATED_1A, TREATED_1B,
    everything()
  ) %>%
  left_join(
    MODE1_COND_2000 %>% select(ID, QNO, MODE1, MODE1_raw),
    by = c("ID", "QNO")
  ) %>%
  filter(!is.na(MODE1)) %>%                            # enforce conditional universe
  mutate(
    survey_year = 2000,
    outcome     = "MODE1_public_transport",
    universe    = "Conditional"
  )

# sanity checks: missing treatments?
PERSON_MODE1_2000 %>%
  summarise(
    n = n(),
    miss_1A1B = mean(is.na(TREATED_1A1B)),
    miss_1A   = mean(is.na(TREATED_1A)),
    miss_1AB  = mean(is.na(TREATED_1AB))
  )


