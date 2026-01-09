# =========================
# Rea Vaya SDID — MAIN_PURPu end-to-end
# Standardized name: MAIN_PURPu
# Description: Main Trip purpose - Unconditional
# Type: Integer (plus optional factor label)
# =========================

#To save panels on Mac: At the top of your MAIN_PURPu script:
source("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Processed Data/Panels/panel_utils.R")  # adjust path as needed
outcome_name <- "MAIN_PURPu"


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
# HTS 2000 – Main Trip Purpose Unconditional (MAIN_PURPu),
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
library(janitor)
library(readxl)

# Treatment tables (1km)
treat2000_1ab <- TREATED_1A1B %>% clean_names()
treat2000_1a  <- TREATED_1A   %>% clean_names()
treat2000_1b  <- TREATED_1B   %>% clean_names()

#1) YEAR 2000 — Build unconditional person-level MAIN_PURP from TRIP2000
# --------------------------
# 2000: Trip -> Person main_purp (UNCONDITIONAL)
# --------------------------

trip2000 <- TRIP2000 %>%
  mutate(
    MAIN_PURP_TRIP = case_when(
      PURPOSE %in% c(2, 3) ~ 1L,                          # work-related
      PURPOSE %in% c(1, 4, 5, 6, 7, 8, 9) ~ 0L,           # non-work
      TRUE ~ NA_integer_
    )
  )

# Collapse to person-level: 1 if ANY work trip
main_purp_person2000 <- trip2000 %>%
  group_by(QNO, PINDEX) %>%
  summarise(
    main_purp = as.integer(any(MAIN_PURP_TRIP == 1, na.rm = TRUE)),
    .groups   = "drop"
  )

# Attach to PERSON2000 (UNCONDITIONAL: persons with no trips become 0)
PERSON2000_MAINPURP_U <- PERSON2000 %>%
  left_join(main_purp_person2000, by = c("QNO", "PINDEX")) %>%
  mutate(
    main_purp     = if_else(is.na(main_purp), 0L, main_purp),
    weight_person = WEIGHT
  )

# Sanity checks
table(PERSON2000_MAINPURP_U$main_purp, useNA = "ifany")

#Build 2000 Phase panels (1km)
panel2000U_1ab <- PERSON2000_MAINPURP_U %>%
  left_join(treat2000_1ab %>% select(taz_id, trt_1km), by = c("HZONE" = "taz_id")) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(phase = "1AB", year = 2000)

panel2000U_1a <- PERSON2000_MAINPURP_U %>%
  left_join(treat2000_1a %>% select(taz_id, trt_1km), by = c("HZONE" = "taz_id")) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(phase = "1A", year = 2000)

panel2000U_1b <- PERSON2000_MAINPURP_U %>%
  left_join(treat2000_1b %>% select(taz_id, trt_1km), by = c("HZONE" = "taz_id")) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(phase = "1B", year = 2000)

table(panel2000U_1ab$treat_1km, useNA="ifany")

#2) YEAR 2014 — Unconditional MAIN_PURP from TRIP2014 + attach to PERSON2014
# --------------------------
# 2014: Trip -> Person main_purp (UNCONDITIONAL)
# --------------------------

trip2014 <- TRIP2014 %>%
  clean_names() %>%
  mutate(
    q_id    = as.character(q_id),
    pnumber = as.character(pnumber),
    purpose_raw = q5_11,
    main_purp_trip = case_when(
      purpose_raw %in% c(1, 2) ~ 1L,      # work-related
      purpose_raw %in% c(3:14) ~ 0L,      # non-work
      purpose_raw == -1        ~ NA_integer_,
      TRUE                     ~ NA_integer_
    )
  )

main_purp_person2014 <- trip2014 %>%
  group_by(q_id, pnumber) %>%
  summarise(
    main_purp = as.integer(any(main_purp_trip == 1, na.rm = TRUE)),
    .groups = "drop"
  )

person2014 <- PERSON2014 %>%
  clean_names() %>%
  mutate(
    q_id         = as.character(q_id),
    pnumber      = as.character(pnumber),
    weight_person = pp_weight
  )

# Attach main_purp (UNCONDITIONAL: persons with no trips become 0)
person2014_mainpurp_U <- person2014 %>%
  left_join(main_purp_person2014, by = c("q_id", "pnumber")) %>%
  mutate(main_purp = if_else(is.na(main_purp), 0L, main_purp))

# Attach HZONE (home TAZ)
hzone2014 <- HZONE2014 %>%
  clean_names() %>%
  mutate(q_id = as.character(q_id)) %>%
  select(q_id, hzone)

person2014_mainpurp_U <- person2014_mainpurp_U %>%
  left_join(hzone2014, by = "q_id")

# Sanity checks
table(person2014_mainpurp_U$main_purp, useNA="ifany")
table(is.na(person2014_mainpurp_U$hzone))


#Build 2014 Phase panels (1km)
panel2014U_1ab <- person2014_mainpurp_U %>%
  left_join(treat2000_1ab %>% select(taz_id, trt_1km), by = c("hzone" = "taz_id")) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(phase = "1AB", year = 2014)

panel2014U_1a <- person2014_mainpurp_U %>%
  left_join(treat2000_1a %>% select(taz_id, trt_1km), by = c("hzone" = "taz_id")) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(phase = "1A", year = 2014)

panel2014U_1b <- person2014_mainpurp_U %>%
  left_join(treat2000_1b %>% select(taz_id, trt_1km), by = c("hzone" = "taz_id")) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(phase = "1B", year = 2014)

#3) YEAR 2019 — Unconditional MAIN_PURP from PERSON2019 + correct weights via MP_CODE
## --------------------------
# 2019: Correct person weights (Person_Data_Weights)
# --------------------------

weights2019_path <- "Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Data/HTS/Mathetha_JoburgSurveys/2019/COJ_Data_Weights.xlsx"

WEIGHT2019_person <- read_excel(weights2019_path, sheet = "Person_Data_Weights") %>%
  select(MP_CODE, weights)

#Fix the weights table once, then join
WEIGHT2019_person <- read_excel(
  weights2019_path,
  sheet = "Person_Data_Weights"
) %>%
  mutate(MP_CODE = as.character(MP_CODE)) %>%   # 🔴 FIX
  select(MP_CODE, weights)

# --------------------------
# 2019: Person main_purp (UNCONDITIONAL)
# --------------------------
person2019 <- PERSON2019 %>%
  clean_names() %>%
  mutate(
    h_id    = as.character(h_id),
    mp_code = as.character(mp_code),
    purpose_raw = trip_purpose
  ) %>%
  left_join(WEIGHT2019_person, by = c("mp_code" = "MP_CODE")) %>%
  mutate(
    # Work-related labels based on YOUR observed 2019 strings
    main_purp = case_when(
      purpose_raw %in% c("Work at usual work place", "Work somewhere else", "Looking for work") ~ 1L,
      TRUE ~ 0L   # unconditional: everything else (incl Unspecified/NA) treated as not-work
    ),
    weight_person = weights
  )

# Attach HZONE
hzone2019 <- HZONE2019 %>%
  clean_names() %>%
  mutate(h_id = as.character(h_id)) %>%
  select(h_id, hzone)

person2019_mainpurp_U <- person2019 %>%
  left_join(hzone2019, by = "h_id")

# Sanity checks
table(person2019_mainpurp_U$main_purp, useNA="ifany")
summary(person2019_mainpurp_U$weight_person)
table(is.na(person2019_mainpurp_U$hzone))

#Build 2019 Phase panels (1km)
panel2019U_1ab <- person2019_mainpurp_U %>%
  left_join(treat2000_1ab %>% select(taz_id, trt_1km), by = c("hzone" = "taz_id")) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(phase = "1AB", year = 2019)

panel2019U_1a <- person2019_mainpurp_U %>%
  left_join(treat2000_1a %>% select(taz_id, trt_1km), by = c("hzone" = "taz_id")) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(phase = "1A", year = 2019)

panel2019U_1b <- person2019_mainpurp_U %>%
  left_join(treat2000_1b %>% select(taz_id, trt_1km), by = c("hzone" = "taz_id")) %>%
  rename(treat_1km = trt_1km) %>%
  mutate(phase = "1B", year = 2019)

#4) Quick summaries (unconditional)
summ_quick <- function(df, label){
  cat("\n---", label, "---\n")
  cat("N:", nrow(df), "\n")
  cat("Work share (unweighted):", mean(df$main_purp == 1, na.rm=TRUE), "\n")
  ok <- !is.na(df$main_purp) & !is.na(df$weight_person)
  cat("Work share (weighted):", sum((df$main_purp[ok]==1)*df$weight_person[ok]) / sum(df$weight_person[ok]), "\n")
  print(table(df$treat_1km, useNA="ifany"))
}

summ_quick(panel2000U_1b, "2000 1B unconditional")
summ_quick(panel2014U_1b, "2014 1B unconditional")
summ_quick(panel2019U_1b, "2019 1B unconditional")


#===========================================
#CROSS-SECTIONAL DESCRIPTIVES
#===========================================
library(dplyr)

compute_cross_sectional_mainpurp_uncond <- function(df, year_label, phase_label) {
  
  df %>%
    # drop missing treatment
    filter(!is.na(treat_1km)) %>%
    
    mutate(
      Year  = year_label,
      Phase = phase_label,
      TreatGroup = case_when(
        treat_1km == "Treated"     ~ "Treated",
        treat_1km == "Partial"     ~ "Partial",
        treat_1km == "Not Treated" ~ "Not treated",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(TreatGroup)) %>%
    
    group_by(Year, Phase, TreatGroup) %>%
    summarise(
      sample_n   = n(),
      weighted_N = sum(weight_person, na.rm = TRUE),
      
      work_share = {
        ok <- !is.na(main_purp) & !is.na(weight_person)
        if (!any(ok)) NA_real_
        else sum((main_purp[ok] == 1) * weight_person[ok]) / sum(weight_person[ok])
      },
      .groups = "drop"
    )
}

#Run for all years & phases
# 2000
cxU_2000_1A  <- compute_cross_sectional_mainpurp_uncond(panel2000U_1a,  2000, "1A")
cxU_2000_1B  <- compute_cross_sectional_mainpurp_uncond(panel2000U_1b,  2000, "1B")
cxU_2000_1AB <- compute_cross_sectional_mainpurp_uncond(panel2000U_1ab, 2000, "1AB")

# 2014
cxU_2014_1A  <- compute_cross_sectional_mainpurp_uncond(panel2014U_1a,  2014, "1A")
cxU_2014_1B  <- compute_cross_sectional_mainpurp_uncond(panel2014U_1b,  2014, "1B")
cxU_2014_1AB <- compute_cross_sectional_mainpurp_uncond(panel2014U_1ab, 2014, "1AB")

# 2019
cxU_2019_1A  <- compute_cross_sectional_mainpurp_uncond(panel2019U_1a,  2019, "1A")
cxU_2019_1B  <- compute_cross_sectional_mainpurp_uncond(panel2019U_1b,  2019, "1B")
cxU_2019_1AB <- compute_cross_sectional_mainpurp_uncond(panel2019U_1ab, 2019, "1AB")

cross_sectional_mainpurp_uncond <- bind_rows(
  cxU_2000_1A, cxU_2000_1B, cxU_2000_1AB,
  cxU_2014_1A, cxU_2014_1B, cxU_2014_1AB,
  cxU_2019_1A, cxU_2019_1B, cxU_2019_1AB
)

print(cross_sectional_mainpurp_uncond, n = Inf)


###############################################################################==============================
#Pre-post for TAZ
############################################################################### ==============================
#✅ UNCONDITIONAL MAIN_PURPu Pre–Post Table
library(dplyr)
library(tibble)

# weighted mean helper (robust)
w_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  if (sum(w[ok]) == 0) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# Ensure treat_group exists (derived from treat_1km)
ensure_treat_group <- function(df) {
  if (!("treat_group" %in% names(df))) {
    df <- df %>%
      mutate(
        treat_group = case_when(
          treat_1km == "Treated"     ~ "Treated",
          treat_1km == "Partial"     ~ "Partial",
          treat_1km == "Not Treated" ~ "Not treated",
          TRUE                       ~ NA_character_
        )
      )
  }
  df
}

make_prepost_phase_mainpurp <- function(df_2000, df_post, phase_label, post_year) {
  
  df_2000 <- ensure_treat_group(df_2000)
  df_post <- ensure_treat_group(df_post)
  
  pre <- df_2000 %>%
    mutate(Year = 2000L) %>%
    filter(!is.na(main_purp), !is.na(weight_person), !is.na(treat_group))
  
  post <- df_post %>%
    mutate(Year = post_year) %>%
    filter(!is.na(main_purp), !is.na(weight_person), !is.na(treat_group))
  
  # Treated = fully treated TAZs
  pre_T  <- pre  %>% filter(treat_group == "Treated")
  post_T <- post %>% filter(treat_group == "Treated")
  
  # Control = Not treated only (exclude Partial)
  pre_C  <- pre  %>% filter(treat_group == "Not treated")
  post_C <- post %>% filter(treat_group == "Not treated")
  
  Treated_Pre  <- w_mean(pre_T$main_purp,  pre_T$weight_person)
  Treated_Post <- w_mean(post_T$main_purp, post_T$weight_person)
  Control_Pre  <- w_mean(pre_C$main_purp,  pre_C$weight_person)
  Control_Post <- w_mean(post_C$main_purp, post_C$weight_person)
  
  Delta_T <- Treated_Post - Treated_Pre
  Delta_C <- Control_Post - Control_Pre
  DID     <- Delta_T - Delta_C
  
  tibble(
    Phase             = phase_label,
    Post_Year         = as.integer(post_year),
    Outcome           = "Main Trip Purpose: Work (Unconditional)",
    Standardised      = "MAIN_PURP (1 = any work trip; 0 otherwise)",
    Treated_Pre       = Treated_Pre,
    Treated_Post      = Treated_Post,
    Delta_T           = Delta_T,
    Control_Pre       = Control_Pre,
    Control_Post      = Control_Post,
    Delta_C           = Delta_C,
    DID_DeltaT_DeltaC = DID
  )
}

#✅ Build the full 6-row pre–post table (UNCONDITIONAL panels)
prepost_mainpurp_uncond <- bind_rows(
  # Phase 1A
  make_prepost_phase_mainpurp(panel2000U_1a,  panel2014U_1a,  "Phase 1A",  2014L),
  make_prepost_phase_mainpurp(panel2000U_1a,  panel2019U_1a,  "Phase 1A",  2019L),
  
  # Phase 1B
  make_prepost_phase_mainpurp(panel2000U_1b,  panel2014U_1b,  "Phase 1B",  2014L),
  make_prepost_phase_mainpurp(panel2000U_1b,  panel2019U_1b,  "Phase 1B",  2019L),
  
  # Phase 1AB
  make_prepost_phase_mainpurp(panel2000U_1ab, panel2014U_1ab, "Phase 1AB", 2014L),
  make_prepost_phase_mainpurp(panel2000U_1ab, panel2019U_1ab, "Phase 1AB", 2019L)
)

options(tibble.width = Inf)
print(prepost_mainpurp_uncond, n = Inf)




###############################################################################==============================
#Stack panels from all years to build 3 master panels (for each phase 1AB, 1A, 1B: AFTER pre–post)
############################################################################### ==============================
#Stack panels across years
library(dplyr)
library(janitor)

add_treat_group <- function(df, drop_na = TRUE) {
  out <- df %>%
    mutate(
      treat_group = case_when(
        treat_1km == "Treated"     ~ "Treated",
        treat_1km == "Partial"     ~ "Partial",
        treat_1km == "Not Treated" ~ "Not treated",
        TRUE                       ~ NA_character_
      )
    )
  if (drop_na) out <- out %>% filter(!is.na(treat_group))
  out
}

# Apply to unconditional panels
panel2000U_1a  <- add_treat_group(panel2000U_1a,  drop_na = TRUE)
panel2000U_1b  <- add_treat_group(panel2000U_1b,  drop_na = TRUE)
panel2000U_1ab <- add_treat_group(panel2000U_1ab, drop_na = TRUE)

panel2014U_1a  <- add_treat_group(panel2014U_1a,  drop_na = TRUE)
panel2014U_1b  <- add_treat_group(panel2014U_1b,  drop_na = TRUE)
panel2014U_1ab <- add_treat_group(panel2014U_1ab, drop_na = TRUE)

panel2019U_1a  <- add_treat_group(panel2019U_1a,  drop_na = TRUE)
panel2019U_1b  <- add_treat_group(panel2019U_1b,  drop_na = TRUE)
panel2019U_1ab <- add_treat_group(panel2019U_1ab, drop_na = TRUE)


#1) Stack panels across years (your harmonize-to-character method)
library(dplyr)
library(janitor)

harmonize_types <- function(df) {
  df %>% mutate(across(everything(), as.character))
}

#✅ Stack for Phase 1A (UNCONDITIONAL)
panel2000U_1a_c <- panel2000U_1a %>%
  mutate(Year = "2000", Phase = "1A") %>%
  harmonize_types()

panel2014U_1a_c <- panel2014U_1a %>%
  mutate(Year = "2014", Phase = "1A") %>%
  harmonize_types()

panel2019U_1a_c <- panel2019U_1a %>%
  mutate(Year = "2019", Phase = "1A") %>%
  harmonize_types()

panelU_mainpurp_1a <- bind_rows(
  panel2000U_1a_c,
  panel2014U_1a_c,
  panel2019U_1a_c
) %>%
  mutate(
    Year          = as.integer(Year),
    main_purp     = as.numeric(main_purp),
    weight_person = as.numeric(weight_person),
    treat_group   = factor(treat_group, levels = c("Not treated", "Partial", "Treated")),
    treated_main  = as.integer(treat_group == "Treated"),
    partial_main  = as.integer(treat_group == "Partial")
  )

#✅ Stack for Phase 1B (UNCONDITIONAL)
panel2000U_1b_c <- panel2000U_1b %>%
  mutate(Year = "2000", Phase = "1B") %>%
  harmonize_types()

panel2014U_1b_c <- panel2014U_1b %>%
  mutate(Year = "2014", Phase = "1B") %>%
  harmonize_types()

panel2019U_1b_c <- panel2019U_1b %>%
  mutate(Year = "2019", Phase = "1B") %>%
  harmonize_types()

panelU_mainpurp_1b <- bind_rows(
  panel2000U_1b_c,
  panel2014U_1b_c,
  panel2019U_1b_c
) %>%
  mutate(
    Year          = as.integer(Year),
    main_purp     = as.numeric(main_purp),
    weight_person = as.numeric(weight_person),
    treat_group   = factor(treat_group, levels = c("Not treated", "Partial", "Treated")),
    treated_main  = as.integer(treat_group == "Treated"),
    partial_main  = as.integer(treat_group == "Partial")
  )

#✅ Stack for Phase 1AB (UNCONDITIONAL)
panel2000U_1ab_c <- panel2000U_1ab %>%
  mutate(Year = "2000", Phase = "1AB") %>%
  harmonize_types()

panel2014U_1ab_c <- panel2014U_1ab %>%
  mutate(Year = "2014", Phase = "1AB") %>%
  harmonize_types()

panel2019U_1ab_c <- panel2019U_1ab %>%
  mutate(Year = "2019", Phase = "1AB") %>%
  harmonize_types()

panelU_mainpurp_1ab <- bind_rows(
  panel2000U_1ab_c,
  panel2014U_1ab_c,
  panel2019U_1ab_c
) %>%
  mutate(
    Year          = as.integer(Year),
    main_purp     = as.numeric(main_purp),
    weight_person = as.numeric(weight_person),
    treat_group   = factor(treat_group, levels = c("Not treated", "Partial", "Treated")),
    treated_main  = as.integer(treat_group == "Treated"),
    partial_main  = as.integer(treat_group == "Partial")
  )

#2) Quick checks (same as you do)
str(panelU_mainpurp_1ab$main_purp)
str(panelU_mainpurp_1ab$weight_person)
table(panelU_mainpurp_1ab$main_purp, useNA = "ifany")
table(panelU_mainpurp_1ab$Year, useNA="ifany")
table(panelU_mainpurp_1ab$treat_group, useNA="ifany")


###############################################################################==============================
#PARALLEL-TRENDS DESCRIPTIVES & PLOTS
############################################################################### ==============================
#3️⃣ Parallel Trends Check (Pre-Trend)
library(fixest)
library(dplyr)
library(ggplot2)

#1) Create the Pre-period Summary Table (2000 & 2014), Treated vs Control
library(dplyr)

pretrend_1b <- panelU_mainpurp_1b %>%
  filter(Year %in% c(2000, 2014)) %>%
  filter(treat_group %in% c("Treated", "Not treated")) %>%
  mutate(
    group = ifelse(treated_main == 1, "Treated", "Control")
  )

#Weighted means by group & year
pre_table_1b <- pretrend_1b %>%
  group_by(group, Year) %>%
  summarise(
    n_unweighted = n(),
    w_N          = sum(weight_person, na.rm = TRUE),
    w_mean       = weighted.mean(main_purp, weight_person, na.rm = TRUE),
    .groups = "drop"
  )

pre_table_1b

#2) Format into a nice 2000–2014 Δ table
library(tidyr)

pre_delta_1b <- pre_table_1b %>%
  select(group, Year, w_mean) %>%
  pivot_wider(
    names_from  = Year,
    values_from = w_mean,
    names_prefix = "y_"
  ) %>%
  mutate(delta = y_2014 - y_2000)

pre_delta_1b

#Compute the DiD-style pre-period difference (ATT_pretrend)
pre_DiD_1b <- pre_delta_1b %>%
  select(group, delta) %>%
  pivot_wider(names_from = group, values_from = delta) %>%
  mutate(ATT_pretrend = Treated - Control)

pre_DiD_1b


#3) Parallel Trends Regression (statistical test)
library(fixest)

pt_1b <- feols(
  main_purp ~ i(Year, treated_main, ref = 2000),
  data    = pretrend_1b,
  weights = ~ weight_person,
  cluster = ~ hzone
)

summary(pt_1b)

#4) Parallel Trends Plot (Dissertation-ready)
library(ggplot2)

plot_df <- pretrend_1b %>%
  group_by(Year, group) %>%
  summarise(
    w_mean = weighted.mean(main_purp, weight_person, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(plot_df, aes(x = Year, y = w_mean, color = group, group = group)) +
  geom_point(size = 3) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "black") +
  labs(
    title = "Parallel Trends Check – Phase 1B Only (2000–2014)",
    subtitle = "Any Work Trip Indicator (MAIN_PURP, Weighted Means, Unconditional)",
    x = "Survey Year (Pre-treatment only)",
    y = "Share with Any Work Trip (Weighted)",
    color = "Group"
  ) +
  theme_minimal(base_size = 8)


###############################################################################==============================
#DID Model
############################################################################### ==============================
library(fixest)

# Phase 1AB
didU_mainpurp_1AB <- feols(
  main_purp ~ 
    i(year, treated_main, ref = 2000) +
    i(year, partial_main, ref = 2000)
  | hzone + year,
  weights = ~ weight_person,
  cluster = ~ hzone,
  data = panelU_mainpurp_1ab
)

# Phase 1A
didU_mainpurp_1A <- feols(
  main_purp ~ 
    i(year, treated_main, ref = 2000) +
    i(year, partial_main, ref = 2000)
  | hzone + year,
  weights = ~ weight_person,
  cluster = ~ hzone,
  data = panelU_mainpurp_1a
)

# Phase 1B
didU_mainpurp_1B <- feols(
  main_purp ~ 
    i(year, treated_main, ref = 2000) +
    i(year, partial_main, ref = 2000)
  | hzone + year,
  weights = ~ weight_person,
  cluster = ~ hzone,
  data = panelU_mainpurp_1b
)

summary(didU_mainpurp_1AB)
summary(didU_mainpurp_1A)
summary(didU_mainpurp_1B)


#Compact Baseline DID Tables (same extractor logic)
library(broom)
library(stringr)

extract_did_uncond <- function(model, phase_label) {
  tidy(model) %>%
    filter(str_detect(term, "year::")) %>%
    mutate(
      Phase = phase_label,
      Effect = case_when(
        term == "year::2014:treated_main" ~ "Treated × 2014",
        term == "year::2019:treated_main" ~ "Treated × 2019",
        term == "year::2014:partial_main" ~ "Partial × 2014",
        term == "year::2019:partial_main" ~ "Partial × 2019",
        TRUE ~ term
      )
    ) %>%
    select(Phase, Effect, estimate, std.error, statistic, p.value)
}

didU_results <- bind_rows(
  extract_did_uncond(didU_mainpurp_1AB, "Phase 1AB"),
  extract_did_uncond(didU_mainpurp_1A,  "Phase 1A"),
  extract_did_uncond(didU_mainpurp_1B,  "Phase 1B")
)

didU_results

