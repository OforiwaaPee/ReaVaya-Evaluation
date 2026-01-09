# =========================
# Rea Vaya SDID — MAIN_PURPc end-to-end
# Standardized name: MAIN_PURPc
# Description: Main Trip purpose - Conditional
# Type: Integer (plus optional factor label)
# =========================

#To save panels on Mac: At the top of your MAIN_PURP script:
source("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Processed Data/Panels/panel_utils.R")  # adjust path as needed
outcome_name <- "MAIN_PURPc"


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
# HTS 2000 – Main Trip Purpose (MAIN_PURP_BIN/MAIN_PURP),
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


# ---- 3) Build person-level MAIN_PURP from TRIP2000 ----
#MAIN_PURP = 1 if the person made at least ONE work-related trip (PURPOSE 2 or 3)
#MAIN_PURP = 0 otherwise (including people with only non-work trips or only NA PURPOSE)
library(dplyr)
# Start from TRIP2000 exactly as you have it
trip2000 <- TRIP2000
# Create trip-level work vs non-work
trip2000 <- trip2000 %>%
  mutate(
    MAIN_PURP_TRIP = case_when(
      PURPOSE %in% c(2, 3) ~ 1L,                # work-related
      PURPOSE %in% c(1, 4, 5, 6, 7, 8, 9) ~ 0L, # non-work
      TRUE ~ NA_integer_
    )
  )

# Collapse to person-level: 1 if person has ANY work-related trip
main_purp_person2000 <- trip2000 %>%
  group_by(QNO, PINDEX) %>%
  summarise(
    main_purp = as.integer(any(MAIN_PURP_TRIP == 1, na.rm = TRUE)),
    .groups   = "drop"
  )
# Attach MAIN_PURP to PERSON2000 + define weight_person
person2000 <- PERSON2000

PERSON2000_MAINPURP <- person2000 %>%
  left_join(main_purp_person2000, by = c("QNO", "PINDEX")) %>%
  mutate(
    weight_person = WEIGHT   # alias person weight
  )
# Quick sanity check(before filtering)
table(PERSON2000_MAINPURP$main_purp, useNA = "ifany")

## ✅ CHANGED: restrict to CONDITIONAL (travelers only)
PERSON2000_MAINPURP_cond <- PERSON2000_MAINPURP %>%
  filter(!is.na(main_purp))

# Check conditional version
table(PERSON2000_MAINPURP_cond$main_purp, useNA = "ifany")

#Standardize treatments
library(janitor)
treat2000_1ab <- TREATED_1A1B %>% clean_names()
treat2000_1a  <- TREATED_1A  %>% clean_names()
treat2000_1b  <- TREATED_1B %>% clean_names()

#Build Phase 1AB / 1A / 1B panels (1km band only)
# Phase 1AB (1km)
panel2000_1ab <- PERSON2000_MAINPURP_cond %>%      ## ✅ CHANGED: _cond
  left_join(
    treat2000_1ab %>% select(taz_id, trt_1km),
    by = c("HZONE" = "taz_id")
  ) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(
    phase = "1AB",
    year  = 2000
  )

# Phase 1A (1km)
panel2000_1a <- PERSON2000_MAINPURP_cond %>%       ## ✅ CHANGED: _cond
  left_join(
    treat2000_1a %>% select(taz_id, trt_1km),
    by = c("HZONE" = "taz_id")
  ) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(
    phase = "1A",
    year  = 2000
  )

# Phase 1B (1km)
panel2000_1b <- PERSON2000_MAINPURP_cond %>%       ## ✅ CHANGED: _cond
  left_join(
    treat2000_1b %>% select(taz_id, trt_1km),
    by = c("HZONE" = "taz_id")
  ) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(
    phase = "1B",
    year  = 2000
  )

# Quick checks
table(panel2000_1ab$treat_1km, useNA = "ifany")
table(panel2000_1ab$main_purp, useNA = "ifany")   # should be 0/1, NO NA now


## Summary stats function (now using main_purp + weight_person)
summaries_mainpurp <- function(df, phase_label = "1AB (1km)") {
  
  cat("\n==============================\n")
  cat(" SUMMARY FOR PHASE:", phase_label, "\n")
  cat("==============================\n\n")
  
  # 0. Basic counts
  cat("Total persons:", nrow(df), "\n")
  cat("Non-missing MAIN_PURP:", sum(!is.na(df$main_purp)), "\n\n")
  
  # 1. Treated / Partial / Not Treated
  cat("Treatment group counts (unweighted):\n")
  print(table(df$treat_1km, useNA = "ifany"))
  cat("\n")
  
  cat("Treatment group counts (weighted):\n")
  print(
    df %>%
      group_by(treat_1km) %>%
      summarise(
        weighted_n = sum(weight_person, na.rm = TRUE),
        .groups    = "drop"
      )
  )
  cat("\n")
  
  # 2. Overall work vs not-work
  cat("Work vs Not work (MAIN_PURP, unweighted counts):\n")
  print(table(df$main_purp, useNA = "ifany"))
  cat("\n")
  
  cat("Work share (MAIN_PURP = 1, weighted):\n")
  print(
    df %>%
      summarise(
        work_share_wt = {
          v <- !is.na(main_purp) & !is.na(weight_person)
          if (any(v)) {
            sum(main_purp[v] * weight_person[v]) / sum(weight_person[v])
          } else {
            NA_real_
          }
        }
      )
  )
  cat("\n")
  
  # 3. Work purpose by treatment group
  cat("Work purpose by treatment group (unweighted proportion):\n")
  print(
    df %>%
      group_by(treat_1km) %>%
      summarise(
        work_rate = mean(main_purp, na.rm = TRUE),
        n         = n(),
        .groups   = "drop"
      )
  )
  cat("\n")
  
  cat("Work purpose by treatment group (weighted proportion):\n")
  print(
    df %>%
      group_by(treat_1km) %>%
      summarise(
        work_rate_wt = {
          v <- !is.na(main_purp) & !is.na(weight_person)
          if (any(v)) {
            sum(main_purp[v] * weight_person[v]) / sum(weight_person[v])
          } else {
            NA_real_
          }
        },
        weighted_n   = sum(weight_person, na.rm = TRUE),
        .groups      = "drop"
      )
  )
  cat("\n")
}


## Run summaries for 1AB, 1A, 1B
summaries_mainpurp(panel2000_1ab, "2000 — 1AB (1km)")
summaries_mainpurp(panel2000_1a,  "2000 — 1A (1km)")
summaries_mainpurp(panel2000_1b,  "2000 — 1B (1km)")


# ==========================
## Build 2014 TAZ_TREAT_MASTER with 1km as main band
# ==========================
    #Recode trip level
library(dplyr)
library(janitor)

trip2014 <- TRIP2014 %>%
  clean_names() %>%
  mutate(
    q_id        = as.character(q_id),
    pnumber     = as.character(pnumber),
    purpose_raw = q5_11,
    
    main_purp_trip = case_when(
      purpose_raw %in% c(1, 2) ~ 1L,      # Work-related
      purpose_raw %in% c(3:14) ~ 0L,      # Non-work
      purpose_raw == -1       ~ NA_integer_,
      TRUE                    ~ NA_integer_
    )
  )

#Collapse TRIP → PERSON (MAIN_PURP at person level)
main_purp_person2014 <- trip2014 %>%
  group_by(q_id, pnumber) %>%
  summarise(
    main_purp = as.integer(any(main_purp_trip == 1, na.rm = TRUE)),
    .groups   = "drop"
  )

#Prepare PERSON2014 (weights & person vars)
person2014 <- PERSON2014 %>%
  clean_names() %>%
  mutate(
    q_id          = as.character(q_id),
    pnumber       = as.character(pnumber),
    weight_person = pp_weight     # rename to standard label
  )

#Attach MAIN_PURP to PERSON2014
person2014_mainpurp <- person2014 %>%
  left_join(main_purp_person2014, by = c("q_id", "pnumber"))
#Check (before conditional filter)
table(person2014_mainpurp$main_purp, useNA = "ifany")

## ✅ CHANGED: CONDITIONAL FILTER for 2014
person2014_mainpurp_cond <- person2014_mainpurp %>%
  filter(!is.na(main_purp))

table(person2014_mainpurp_cond$main_purp, useNA = "ifany")

#Attach Home TAZ using HZONE2014 lookup
hzone2014 <- HZONE2014 %>%
  clean_names() %>%
  mutate(q_id = as.character(q_id)) %>%
  select(q_id, hzone)

person2014_mainpurp_cond <- person2014_mainpurp_cond %>%
  left_join(hzone2014, by = "q_id")

#Attach treatment (1km band, same TAZ files as 2000)
#Phase 1AB (1km)
panel2014_1ab <- person2014_mainpurp_cond %>%
  left_join(
    treat2000_1ab %>% select(taz_id, trt_1km),
    by = c("hzone" = "taz_id")
  ) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(
    phase = "1AB",
    year  = 2014
  )

#Phase 1A (1km)
panel2014_1a <- person2014_mainpurp_cond %>%
  left_join(
    treat2000_1a %>% select(taz_id, trt_1km),
    by = c("hzone" = "taz_id")
  ) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(
    phase = "1A",
    year  = 2014
  )

#Phase 1B (1km)
panel2014_1b <- person2014_mainpurp_cond %>%
  left_join(
    treat2000_1b %>% select(taz_id, trt_1km),
    by = c("hzone" = "taz_id")
  ) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(
    phase = "1B",
    year  = 2014
  )

#sanity checks:
table(panel2014_1ab$treat_1km, useNA = "ifany")
table(panel2014_1ab$main_purp, useNA = "ifany")  # should be 0/1, no NA

#Descriptives for 2014
summaries_mainpurp(panel2014_1ab, "2014 — Phase 1AB (1km)")
summaries_mainpurp(panel2014_1a,  "2014 — Phase 1A (1km)")
summaries_mainpurp(panel2014_1b,  "2014 — Phase 1B (1km)")


# ==========================
## Build 2019 TAZ_TREAT_MASTER with 1km as main band
# ==========================
#CLEAN & RECODE TRIP PURPOSE (PERSON2019) with correct weights for 2019
library(readxl)
library(dplyr)
library(janitor)
library(tibble)

# 1) Read 2019 person weights from Excel (Person_Data_Weights)
weights2019_path <- "Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Data/HTS/Mathetha_JoburgSurveys/2019/COJ_Data_Weights.xlsx"

WEIGHT2019_person <- read_excel(
  weights2019_path,
  sheet = "Person_Data_Weights"
) %>%
  mutate(
    MP_CODE = as.character(MP_CODE)   # ✅ make sure key is character
  ) %>%
  select(MP_CODE, weights)

# Sanity check
str(WEIGHT2019_person$MP_CODE)   # should be "chr"
summary(WEIGHT2019_person$weights)

# 2) Standardize treatment tables (reuse if already defined)
treat2000_1ab <- TREATED_1A1B %>% clean_names()  # taz_id, trt_1km, ...
treat2000_1a  <- TREATED_1A    %>% clean_names()
treat2000_1b  <- TREATED_1B    %>% clean_names()

# 3) Build PERSON2019 with MAIN_PURP and correct weights
person2019 <- PERSON2019 %>%
  clean_names() %>%                 # MP_CODE -> mp_code, etc.
  mutate(
    h_id        = as.character(h_id),
    p_id        = as.character(p_id),
    mp_code     = as.character(mp_code),   # ensure join key is character
    purpose_raw = trip_purpose
  ) %>%
  # Attach weights via MP_CODE
  left_join(WEIGHT2019_person, by = c("mp_code" = "MP_CODE")) %>%
  mutate(
    # MAIN_PURP coding (1 = any work-related main purpose, 0 = other)
    main_purp = case_when(
      purpose_raw %in% c(
        "Work at usual work place",
        "Work somewhere else",
        "Looking for work"
      ) ~ 1L,
      
      purpose_raw %in% c(
        "To go home",
        "Shopping",
        "Educational",
        "Visiting friend or relat",
        "Drop or Pickup someone",
        "Welfare offices",
        "Medical purposes",
        "Traditional healer",
        "Recreational",
        "Worship",
        "Other"
      ) ~ 0L,
      
      TRUE ~ NA_integer_    # empty, missing, or unmatched
    ),
    
    # Final person weight for analysis
    weight_person = weights
  )

# Sanity checks
cat("\n[2019] MAIN_PURP distribution (before conditional filter):\n")
print(table(person2019$main_purp, useNA = "ifany"))

cat("\n[2019] weight_person summary (before conditional filter):\n")
print(summary(person2019$weight_person))
cat("Missing weights:", sum(is.na(person2019$weight_person)), "\n")

# 4) Attach Home Zone (HZONE2019 → hzone by h_id)
hzone2019 <- HZONE2019 %>%
  clean_names() %>%
  mutate(
    h_id = as.character(h_id)
  ) %>%
  select(h_id, hzone)

person2019_mainpurp <- person2019 %>%
  left_join(hzone2019, by = "h_id")

cat("\n[2019] Missing hzone after merge:\n")
print(table(is.na(person2019_mainpurp$hzone)))

# 5) CONDITIONAL FILTER: keep only travelers (main_purp not NA)
person2019_mainpurp_cond <- person2019_mainpurp %>%
  filter(!is.na(main_purp))

cat("\n[2019] MAIN_PURP distribution (conditional only: travelers):\n")
print(table(person2019_mainpurp_cond$main_purp, useNA = "ifany"))

cat("\n[2019] weight_person summary (conditional only):\n")
print(summary(person2019_mainpurp_cond$weight_person))
cat("Missing weights (conditional):", sum(is.na(person2019_mainpurp_cond$weight_person)), "\n")

# 6) Attach treatment (1km band) to build 2019 panels:
#    Phase 1AB, Phase 1A, Phase 1B
# Phase 1AB (1km)
panel2019_1ab <- person2019_mainpurp_cond %>%
  left_join(
    treat2000_1ab %>% select(taz_id, trt_1km),
    by = c("hzone" = "taz_id")
  ) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(
    phase = "1AB",
    year  = 2019
  )

# Phase 1A (1km)
panel2019_1a <- person2019_mainpurp_cond %>%
  left_join(
    treat2000_1a %>% select(taz_id, trt_1km),
    by = c("hzone" = "taz_id")
  ) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(
    phase = "1A",
    year  = 2019
  )

# Phase 1B (1km)
panel2019_1b <- person2019_mainpurp_cond %>%
  left_join(
    treat2000_1b %>% select(taz_id, trt_1km),
    by = c("hzone" = "taz_id")
  ) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(
    phase = "1B",
    year  = 2019
  )

# Quick treatment sanity checks
cat("\n[2019] Treatment counts (Phase 1AB):\n")
print(table(panel2019_1ab$treat_1km, useNA = "ifany"))

cat("\n[2019] Treatment counts (Phase 1B):\n")
print(table(panel2019_1b$treat_1km, useNA = "ifany"))

cat("\n[2019] MAIN_PURP distribution in Phase 1AB panel:\n")
print(table(panel2019_1ab$main_purp, useNA = "ifany"))

# 7) Robust summary function (works for any year/phase)
summaries_mainpurp_robust <- function(df, phase_label = "2019") {
  
  cat("\n==============================\n")
  cat(" SUMMARY FOR PHASE:", phase_label, "\n")
  cat("==============================\n\n")
  
  cat("Total persons:", nrow(df), "\n")
  cat("Non-missing MAIN_PURP:", sum(!is.na(df$main_purp)), "\n\n")
  
  # 1. Treatment group counts
  cat("Treatment group counts (unweighted):\n")
  print(table(df$treat_1km, useNA = "ifany"))
  cat("\n")
  
  cat("Treatment group counts (weighted):\n")
  print(
    df %>%
      group_by(treat_1km) %>%
      summarise(
        weighted_n = sum(weight_person, na.rm = TRUE),
        .groups    = "drop"
      )
  )
  cat("\n")
  
  # 2. Work vs Not work (unweighted)
  cat("Work vs Not work (MAIN_PURP, unweighted counts):\n")
  print(table(df$main_purp, useNA = "ifany"))
  cat("\n")
  
  # 2b. Work share (weighted) – robust
  cat("Work share (MAIN_PURP = 1, weighted):\n")
  valid <- !is.na(df$main_purp) & !is.na(df$weight_person)
  if (any(valid)) {
    work_share_wt <- sum(df$main_purp[valid] * df$weight_person[valid]) /
      sum(df$weight_person[valid])
  } else {
    work_share_wt <- NA_real_
  }
  print(tibble(work_share_wt = work_share_wt))
  cat("\n")
  
  # 3. Work purpose by treatment group (unweighted)
  cat("Work purpose by treatment group (unweighted proportion):\n")
  print(
    df %>%
      group_by(treat_1km) %>%
      summarise(
        work_rate = mean(main_purp, na.rm = TRUE),
        n         = n(),
        .groups   = "drop"
      )
  )
  cat("\n")
  
  # 4. Work purpose by treatment group (weighted, robust)
  cat("Work purpose by treatment group (weighted proportion):\n")
  work_by_trt <- df %>%
    group_by(treat_1km) %>%
    summarise(
      work_share_wt = {
        v <- !is.na(main_purp) & !is.na(weight_person)
        if (any(v)) {
          sum(main_purp[v] * weight_person[v]) / sum(weight_person[v])
        } else {
          NA_real_
        }
      },
      weighted_n = sum(weight_person, na.rm = TRUE),
      .groups    = "drop"
    )
  print(work_by_trt)
  cat("\n")
}

# 8) Run 2019 summaries for each phase
summaries_mainpurp_robust(panel2019_1ab, "2019 — Phase 1AB (1km)")
summaries_mainpurp_robust(panel2019_1a,  "2019 — Phase 1A (1km)")
summaries_mainpurp_robust(panel2019_1b,  "2019 — Phase 1B (1km)")


#===========================================
#CROSS-SECTIONAL DESCRIPTIVES
#===========================================
library(dplyr)

#Before running cross-sectional descriptives, drop all rows with missing treatment, just as employment status outcome did
panel2014_1a_clean <- panel2014_1a %>% filter(!is.na(treat_1km))
panel2014_1b_clean <- panel2014_1b %>% filter(!is.na(treat_1km))
panel2014_1ab_clean <- panel2014_1ab %>% filter(!is.na(treat_1km))

panel2019_1a_clean <- panel2019_1a %>% filter(!is.na(treat_1km))
panel2019_1b_clean <- panel2019_1b %>% filter(!is.na(treat_1km))
panel2019_1ab_clean <- panel2019_1ab %>% filter(!is.na(treat_1km))

panel2000_1a_clean <- panel2000_1a %>% filter(!is.na(treat_1km))
panel2000_1b_clean <- panel2000_1b %>% filter(!is.na(treat_1km))
panel2000_1ab_clean <- panel2000_1ab %>% filter(!is.na(treat_1km))

#recode treatment groups on each panel
recode_treatgroup <- function(df) {
  df %>%
    mutate(
      treat_group = case_when(
        treat_1km == "Treated"     ~ "Treated",
        treat_1km == "Partial"     ~ "Partial",
        treat_1km == "Not Treated" ~ "Not treated",
        TRUE                       ~ NA_character_
      )
    ) %>%
    filter(!is.na(treat_group))     # drop NA treatment
}

panel2000_1a  <- recode_treatgroup(panel2000_1a)
panel2000_1b  <- recode_treatgroup(panel2000_1b)
panel2000_1ab <- recode_treatgroup(panel2000_1ab)

panel2014_1a  <- recode_treatgroup(panel2014_1a)
panel2014_1b  <- recode_treatgroup(panel2014_1b)
panel2014_1ab <- recode_treatgroup(panel2014_1ab)

panel2019_1a  <- recode_treatgroup(panel2019_1a)
panel2019_1b  <- recode_treatgroup(panel2019_1b)
panel2019_1ab <- recode_treatgroup(panel2019_1ab)

compute_cross_sectional_mainpurp <- function(df, phase_label) {
  
  df %>%
    mutate(
      Phase = phase_label,
      TreatGroup = case_when(
        treat_1km == "Treated"     ~ "Treated",
        treat_1km == "Partial"     ~ "Partial",
        treat_1km == "Not Treated" ~ "Not treated",
        TRUE                       ~ "No TAZ treatment info"  # treat_1km NA
      )
    ) %>%
    group_by(
      Year       = year,      # already 2000/2014/2019 in your panels
      Phase,
      TreatGroup
    ) %>%
    summarise(
      sample_n   = n(),
      weighted_N = sum(weight_person, na.rm = TRUE),
      work_share = {
        ok <- !is.na(main_purp) & !is.na(weight_person)
        if (!any(ok)) {
          NA_real_
        } else {
          w <- weight_person[ok]
          y <- main_purp[ok] == 1
          if (sum(w) == 0) NA_real_ else sum(y * w) / sum(w)
        }
      },
      .groups = "drop"
    ) %>%
    select(Year, Phase, TreatGroup, sample_n, weighted_N, work_share)
}


#Run it for all three years
# 2000
cx_mainpurp_2000_1A  <- compute_cross_sectional_mainpurp(panel2000_1a,  "1A")
cx_mainpurp_2000_1B  <- compute_cross_sectional_mainpurp(panel2000_1b,  "1B")
cx_mainpurp_2000_1AB <- compute_cross_sectional_mainpurp(panel2000_1ab, "1AB")

# 2014
cx_mainpurp_2014_1A  <- compute_cross_sectional_mainpurp(panel2014_1a,  "1A")
cx_mainpurp_2014_1B  <- compute_cross_sectional_mainpurp(panel2014_1b,  "1B")
cx_mainpurp_2014_1AB <- compute_cross_sectional_mainpurp(panel2014_1ab, "1AB")

# 2019
cx_mainpurp_2019_1A  <- compute_cross_sectional_mainpurp(panel2019_1a,  "1A")
cx_mainpurp_2019_1B  <- compute_cross_sectional_mainpurp(panel2019_1b,  "1B")
cx_mainpurp_2019_1AB <- compute_cross_sectional_mainpurp(panel2019_1ab, "1AB")

# Combine into one table
cross_sectional_mainpurp <- bind_rows(
  cx_mainpurp_2000_1A,  cx_mainpurp_2000_1B,  cx_mainpurp_2000_1AB,
  cx_mainpurp_2014_1A,  cx_mainpurp_2014_1B,  cx_mainpurp_2014_1AB,
  cx_mainpurp_2019_1A,  cx_mainpurp_2019_1B,  cx_mainpurp_2019_1AB
)

# Inspect
cross_sectional_mainpurp
print(cross_sectional_mainpurp, n = Inf)

    
###############################################################################==============================
#Pre-post for TAZ
############################################################################### ==============================
#MAIN_PURP Pre–Post Table Code
library(dplyr)

# weighted mean helper
w_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

make_prepost_phase_mainpurp <- function(df_2000, df_post, phase_label, post_year) {
  
  # Attach Year just for clarity
  pre  <- df_2000 %>%
    mutate(Year = 2000L) %>%
    filter(!is.na(main_purp), !is.na(weight_person))
  
  post <- df_post %>%
    mutate(Year = post_year) %>%
    filter(!is.na(main_purp), !is.na(weight_person))
  
  # Treated = fully treated TAZs
  pre_T  <- pre  %>% filter(treat_group == "Treated")
  post_T <- post %>% filter(treat_group == "Treated")
  
  # Control = Not treated only (exclude Partial)
  pre_C  <- pre  %>% filter(treat_group == "Not treated")
  post_C <- post %>% filter(treat_group == "Not treated")
  
  Treated_Pre   <- w_mean(pre_T$main_purp,   pre_T$weight_person)
  Treated_Post  <- w_mean(post_T$main_purp,  post_T$weight_person)
  Control_Pre   <- w_mean(pre_C$main_purp,   pre_C$weight_person)
  Control_Post  <- w_mean(post_C$main_purp,  post_C$weight_person)
  
  Delta_T <- Treated_Post  - Treated_Pre
  Delta_C <- Control_Post  - Control_Pre
  DID     <- Delta_T - Delta_C
  
  tibble::tibble(
    Phase             = phase_label,
    Post_Year         = post_year,
    Outcome           = "Main Trip Purpose: Work",
    Standardised      = "MAIN_PURP (1 = work)",
    Treated_Pre       = Treated_Pre,
    Treated_Post      = Treated_Post,
    Delta_T           = Delta_T,
    Control_Pre       = Control_Pre,
    Control_Post      = Control_Post,
    Delta_C           = Delta_C,
    DID_DeltaT_DeltaC = DID
  )
}

#Build the full pre–post table (all phases, all post years)
library(dplyr)

prepost_mainpurp <- bind_rows(
  # Phase 1A: 2000 vs 2014, 2000 vs 2019
  make_prepost_phase_mainpurp(panel2000_1a, panel2014_1a, "Phase 1A",  2014L),
  make_prepost_phase_mainpurp(panel2000_1a, panel2019_1a, "Phase 1A",  2019L),
  
  # Phase 1B
  make_prepost_phase_mainpurp(panel2000_1b, panel2014_1b, "Phase 1B",  2014L),
  make_prepost_phase_mainpurp(panel2000_1b, panel2019_1b, "Phase 1B",  2019L),
  
  # Phase 1AB
  make_prepost_phase_mainpurp(panel2000_1ab, panel2014_1ab, "Phase 1AB", 2014L),
  make_prepost_phase_mainpurp(panel2000_1ab, panel2019_1ab, "Phase 1AB", 2019L)
)

options(tibble.width = Inf)
print(prepost_mainpurp, n = Inf)


  ###############################################################################==============================
  #Stack panels from all years to build 3 master panels (for each phase 1AB, 1A, 1B: AFTER pre–post)
  ############################################################################### ==============================
  #Stack panels across years
  library(dplyr)
  library(janitor)

harmonize_types <- function(df) {
  df %>% mutate(across(everything(), as.character))
}

#Stack for Phase 1A
# Add Year & Phase, then harmonize all columns to character
panel2000_1a_c <- panel2000_1a %>%
  mutate(Year = "2000", Phase = "1A") %>%
  harmonize_types()

panel2014_1a_c <- panel2014_1a %>%
  mutate(Year = "2014", Phase = "1A") %>%
  harmonize_types()

panel2019_1a_c <- panel2019_1a %>%
  mutate(Year = "2019", Phase = "1A") %>%
  harmonize_types()

panel_mainpurp_1a <- bind_rows(
  panel2000_1a_c,
  panel2014_1a_c,
  panel2019_1a_c
) %>%
  mutate(
    Year          = as.integer(Year),
    main_purp     = as.numeric(main_purp),
    weight_person = as.numeric(weight_person),
    treat_group   = factor(treat_group,
                           levels = c("Not treated", "Partial", "Treated")),
    treated_main  = as.integer(treat_group == "Treated"),
    partial_main  = as.integer(treat_group == "Partial")
  )

#Stack for Phase 1B
panel2000_1b_c <- panel2000_1b %>%
  mutate(Year = "2000", Phase = "1B") %>%
  harmonize_types()

panel2014_1b_c <- panel2014_1b %>%
  mutate(Year = "2014", Phase = "1B") %>%
  harmonize_types()

panel2019_1b_c <- panel2019_1b %>%
  mutate(Year = "2019", Phase = "1B") %>%
  harmonize_types()

panel_mainpurp_1b <- bind_rows(
  panel2000_1b_c,
  panel2014_1b_c,
  panel2019_1b_c
) %>%
  mutate(
    Year          = as.integer(Year),
    main_purp     = as.numeric(main_purp),
    weight_person = as.numeric(weight_person),
    treat_group   = factor(treat_group,
                           levels = c("Not treated", "Partial", "Treated")),
    treated_main  = as.integer(treat_group == "Treated"),
    partial_main  = as.integer(treat_group == "Partial")
  )

#Stack for Phase 1AB
panel2000_1ab_c <- panel2000_1ab %>%
  mutate(Year = "2000", Phase = "1AB") %>%
  harmonize_types()

panel2014_1ab_c <- panel2014_1ab %>%
  mutate(Year = "2014", Phase = "1AB") %>%
  harmonize_types()

panel2019_1ab_c <- panel2019_1ab %>%
  mutate(Year = "2019", Phase = "1AB") %>%
  harmonize_types()

panel_mainpurp_1ab <- bind_rows(
  panel2000_1ab_c,
  panel2014_1ab_c,
  panel2019_1ab_c
) %>%
  mutate(
    Year          = as.integer(Year),
    main_purp     = as.numeric(main_purp),
    weight_person = as.numeric(weight_person),
    treat_group   = factor(treat_group,
                           levels = c("Not treated", "Partial", "Treated")),
    treated_main  = as.integer(treat_group == "Treated"),
    partial_main  = as.integer(treat_group == "Partial")
  )

 #quickly check:
  str(panel_mainpurp_1ab$main_purp)
  str(panel_mainpurp_1ab$weight_person)
  table(panel_mainpurp_1ab$main_purp, useNA = "ifany")
  


  ###############################################################################==============================
  #PARALLEL-TRENDS DESCRIPTIVES & PLOTS
  ############################################################################### ==============================
  #Parallel function
  library(dplyr)
  library(ggplot2)
  
  ##Parallel-trends style descriptive summaries
  # Helper to summarise for a stacked panel
  summarise_parallel_trends <- function(df) {
    df %>%
      group_by(Year, TreatGroup = treat_group) %>%
      summarise(
        sample_n   = n(),
        weighted_N = sum(weight_person, na.rm = TRUE),
        work_share = {
          ok <- !is.na(main_purp) & !is.na(weight_person)
          if (!any(ok)) NA_real_ else {
            w <- weight_person[ok]
            y <- main_purp[ok] == 1
            if (sum(w) == 0) NA_real_ else sum(y * w) / sum(w)
          }
        },
        .groups = "drop"
      )
  }
  
  pt_1ab <- summarise_parallel_trends(panel_mainpurp_1ab)
  pt_1a  <- summarise_parallel_trends(panel_mainpurp_1a)
  pt_1b  <- summarise_parallel_trends(panel_mainpurp_1b)
  
  
  ##Plot for Phase 1B (your “classic” BRT treatment)
  ggplot(pt_1b, aes(x = Year, y = work_share, colour = TreatGroup, group = TreatGroup)) +
    geom_line() +
    geom_point() +
    labs(
      title = "Parallel-trends style plot: Work main purpose, Phase 1B",
      y = "Weighted share with MAIN_PURP = Work",
      x = "Year"
    )
  
  
  # 1) Keep only 2000 & 2014, Phase 1B, and drop Partial
  pre_1b <- panel_mainpurp_1b %>%
    filter(Year %in% c(2000, 2014)) %>%
    filter(treat_group %in% c("Not treated", "Treated")) %>%
    mutate(
      # binary: 1 = treated, 0 = control (not treated)
      treated_main = as.integer(treat_group == "Treated")
    )
  
  # Quick sanity check
  pre_1b %>%
    count(Year, treat_group)
  
  # 2️⃣ Pre-period summary table (2000 & 2014, Treated vs Control)
  pre_summary_1b <- pre_1b %>%
    group_by(Year, Group = treat_group) %>%
    summarise(
      sample_n   = n(),
      weighted_N = sum(weight_person, na.rm = TRUE),
      work_share = {
        ok <- !is.na(main_purp) & !is.na(weight_person)
        if (!any(ok)) NA_real_ else {
          w <- weight_person[ok]
          y <- main_purp[ok] == 1
          if (sum(w) == 0) NA_real_ else sum(y * w) / sum(w)
        }
      },
      .groups = "drop"
    ) %>%
    arrange(Group, Year)
  
  pre_summary_1b
  
  
  #3️⃣ Format into a nice 2000–2014 Δ + DiD table
  library(tidyr)
  
  # Wide format: columns for 2000 and 2014
  pre_wide_1b <- pre_summary_1b %>%
    select(Group, Year, work_share) %>%
    pivot_wider(
      names_from  = Year,
      values_from = work_share,
      names_prefix = "work_"
    ) %>%
    mutate(
      delta_2014_2000 = work_2014 - work_2000
    )
  
  pre_wide_1b
  
  #4️⃣ Estimate the formal Parallel Trends Test Model (pre-period only)
  library(fixest)
  
  # treat_pre: indicator for 2014 (vs 2000) in pre-period
  pre_1b <- pre_1b %>%
    mutate(
      year2014 = as.integer(Year == 2014)
    )
  
  parallel_trends_1b <- feols(
    main_purp ~ treated_main * year2014,
    weights = ~ weight_person,
    cluster = ~ hzone,
    data = pre_1b
  )
  
  summary(parallel_trends_1b)
  
  
  ##5️⃣ Parallel-trends plot (Dissertation-ready, 2000–2014 Treated vs Control)
  library(ggplot2)
  
  ggplot(pre_summary_1b, aes(x = Year, y = work_share,
                             colour = Group, group = Group)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = c(2000, 2014)) +
    labs(
      title = "Pre-period Parallel Trends: Work Main Trip Purpose, Phase 1B",
      subtitle = "Weighted share of persons whose main trip purpose is work",
      x = "Year",
      y = "Work main-trip share",
      colour = "Group"
    ) +
    theme_minimal(base_size = 10)
  
  
  
  
  #Prep function that selects only the necessary columns
  library(dplyr)
  
  prep_conditional_min <- function(df) {
    df %>%
      # conditional sample: keep only observed main_purp (travelers)
      filter(!is.na(main_purp)) %>%
      # keep only people with treatment info
      filter(!is.na(treat_1km)) %>%
      transmute(
        year          = as.integer(year),
        hzone         = as.character(hzone),      # ensure same type across years
        main_purp     = as.numeric(main_purp),
        weight_person = as.numeric(weight_person),
        treat_group   = case_when(
          treat_1km == "Treated"     ~ "Treated",
          treat_1km == "Partial"     ~ "Partial",
          treat_1km == "Not Treated" ~ "Not treated",
          TRUE ~ NA_character_
        ),
        treated_main  = as.integer(treat_1km == "Treated"),
        partial_main  = as.integer(treat_1km == "Partial")
      ) %>%
      filter(!is.na(treat_group))
  }
  
  #Robust fix: coalesce any home-zone field into hzone
  library(dplyr)
  
  prep_conditional_min <- function(df) {
    
    # --- Harmonize home TAZ to a single column ---
    if ("hzone" %in% names(df)) {
      df$hzone_std <- as.character(df$hzone)
    } else if ("HZONE" %in% names(df)) {
      df$hzone_std <- as.character(df$HZONE)
    } else {
      stop("No home-zone field found (expected 'hzone' or 'HZONE').")
    }
    
    # --- Build minimal conditional dataset (avoids type conflicts) ---
    df %>%
      filter(!is.na(main_purp)) %>%     # conditional sample = travelers
      filter(!is.na(treat_1km)) %>%     # must have treatment group
      transmute(
        year          = as.integer(year),
        hzone         = hzone_std,
        main_purp     = as.numeric(main_purp),
        weight_person = as.numeric(weight_person),
        
        treat_group = case_when(
          treat_1km == "Treated"     ~ "Treated",
          treat_1km == "Partial"     ~ "Partial",
          treat_1km == "Not Treated" ~ "Not treated",
          TRUE ~ NA_character_
        ),
        
        treated_main = as.integer(treat_1km == "Treated"),
        partial_main = as.integer(treat_1km == "Partial")
      ) %>%
      filter(!is.na(treat_group), !is.na(hzone))
  }
  
  
  #2) Now stack Phase 1B conditional safely
  panelC_mainpurp_1B <- bind_rows(
    prep_conditional_min(panel2000_1b),
    prep_conditional_min(panel2014_1b),
    prep_conditional_min(panel2019_1b)
  )
  
  table(panelC_mainpurp_1B$year)
  table(panelC_mainpurp_1B$treat_group, useNA="ifany")
  
  
# Parallel trends summary table (conditional)
  summarise_parallel_trends_cond <- function(df) {
    df %>%
      group_by(Year = year, TreatGroup = treat_group) %>%
      summarise(
        sample_n   = n(),
        weighted_N = sum(weight_person, na.rm = TRUE),
        work_share = {
          ok <- !is.na(main_purp) & !is.na(weight_person)
          if (!any(ok)) NA_real_
          else sum((main_purp[ok] == 1) * weight_person[ok]) / sum(weight_person[ok])
        },
        .groups = "drop"
      ) %>%
      arrange(TreatGroup, Year)
  }
  
  ptC_1B <- summarise_parallel_trends_cond(panelC_mainpurp_1B)
  print(ptC_1B, n = Inf)
  
  
  #✅ Formal pre-trend test (2000 vs 2014 only)
  library(fixest)
  
  pretrendC_1B <- feols(
    main_purp ~ treated_main * (year == 2014) + partial_main * (year == 2014)
    | hzone,
    weights = ~ weight_person,
    cluster = ~ hzone,
    data = panelC_mainpurp_1B %>% filter(year %in% c(2000, 2014))
  )
  
  summary(pretrendC_1B)
  
  
  
  
  
  
  
###############################################################################==============================
#DID Model
############################################################################### ==============================
  library(fixest)
  
  ## --------------------------
  ## Phase 1AB — Baseline DID
  ## --------------------------
  
  did_mainpurp_1ab_base <- feols(
    main_purp ~ 
      i(Year, treated_main, ref = 2000) +   # Treated × (2014, 2019) relative to 2000
      i(Year, partial_main, ref = 2000)     # Partial × (2014, 2019) relative to 2000
    | hzone + Year,                         # TAZ FE + Year FE
    weights = ~ weight_person,
    cluster = ~ hzone,
    data = panel_mainpurp_1ab
  )
  
  
  ## --------------------------
  ## Phase 1A — Baseline DID
  ## --------------------------
  
  did_mainpurp_1a_base <- feols(
    main_purp ~ 
      i(Year, treated_main, ref = 2000) +
      i(Year, partial_main, ref = 2000)
    | hzone + Year,
    weights = ~ weight_person,
    cluster = ~ hzone,
    data = panel_mainpurp_1a
  )
  
  
  ## --------------------------
  ## Phase 1B — Baseline DID
  ## --------------------------
  
  did_mainpurp_1b_base <- feols(
    main_purp ~ 
      i(Year, treated_main, ref = 2000) +
      i(Year, partial_main, ref = 2000)
    | hzone + Year,
    weights = ~ weight_person,
    cluster = ~ hzone,
    data = panel_mainpurp_1b
  )
  
  
  ##Compact summary table for all three models
  etable(
    did_mainpurp_1ab_base,
    did_mainpurp_1a_base,
    did_mainpurp_1b_base,
    se.below = TRUE,
    dict = c(
      "i(Year, treated_main, ref = 2000)::2014"  = "Treated × 2014",
      "i(Year, treated_main, ref = 2000)::2019"  = "Treated × 2019",
      "i(Year, partial_main, ref = 2000)::2014"  = "Partial × 2014",
      "i(Year, partial_main, ref = 2000)::2019"  = "Partial × 2019"
    )
  )
  
#✅ Step 1 — Improved extractor: clearer labels, correct matching, compact output
  library(broom)
  library(dplyr)
  library(stringr)
  
  extract_did_phase <- function(model, phase_label) {
    
    tidy(model) %>%
      # Keep only DID interaction terms
      filter(str_detect(term, "Year::")) %>%
      
      # Create readable labels
      mutate(
        Effect = case_when(
          term == "Year::2014:treated_main"  ~ "Treated × 2014",
          term == "Year::2019:treated_main"  ~ "Treated × 2019",
          term == "Year::2014:partial_main"  ~ "Partial × 2014",
          term == "Year::2019:partial_main"  ~ "Partial × 2019",
          TRUE                               ~ term
        ),
        Phase = phase_label
      ) %>%
      
      # Select only the relevant fields
      select(
        Phase,
        Effect,
        estimate,
        std.error,
        statistic,
        p.value
      )
  }
  
  #✅ Step 2 — Generate compact results for each phase separately
  did_results_1ab <- extract_did_phase(did_mainpurp_1ab, "Phase 1AB")
  did_results_1a  <- extract_did_phase(did_mainpurp_1a,  "Phase 1A")
  did_results_1b  <- extract_did_phase(did_mainpurp_1b,  "Phase 1B")
  
  
  did_mainpurp_all_phases <- bind_rows(
    did_results_1ab,
    did_results_1a,
    did_results_1b
  )
  
  did_mainpurp_all_phases
  


###############################################################################
# Save panels to Macbook docs
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
