# =========================
# Rea Vaya SDID — MAIN_PURP end-to-end
# Standardized name: MAIN_PURP
# Description: Trip purpose (3-category)
# Type: Integer (plus optional factor label)
# =========================

#To save panels on Mac: At the top of your MAIN_PURP script:
source("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Processed Data/Panels/panel_utils.R")  # adjust path as needed
outcome_name <- "MAIN_PURP"


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
TREATED_1B <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Processed Data/1b_TreatedAreas 2.xlsx")

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
encod








###############################################################################
# Save panels to MAC
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
