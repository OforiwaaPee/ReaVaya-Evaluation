# Packages
install.packages(c("readxl", "dplyr", "janitor", "stringr"))

library(readxl)
library(dplyr)
library(janitor)
library(stringr)

# libraries to load later:
# library(sf); library(terra); library(tidyverse); library(geodata)
# library(rnaturalearth); library(osmdata); library(leaflet)
# library(rayshader); library(gganimate)
# library(arrow)   # for parquet

# 1) Import and save 2000 data
#PERSON2000
library(readxl)
PERSON2000 <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Raw Data/HTS_surveys/2000/PERSON.xlsx")

#TRIP2000
library(readxl)
TRIP2000 <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Raw Data/HTS_surveys/2000/TRIP.xlsx")

#HOUSE2000
library(readxl)
HOUSE2000 <- read_excel("~/Documents/UMN PhD/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Raw Data/HTS_surveys/2000/HOUSE.xlsx")

save(PERSON2000, TRIP2000, HOUSE2000, file = "ReaVaya.RData")

# 2) Standardize & sentinel -> NA
to_na <- function(x) { x <- ifelse(x %in% c(-100, "-100"), NA, x); suppressWarnings(as.integer(x)) }
HOUSE2000 <- HOUSE2000 |>
  mutate(across(everything(), ~ifelse(.x == "-100", NA, .x))) |>
  mutate(HZONE = to_na(HZONE),
         weight_HOUSE2000 = suppressWarnings(as.numeric(weight)))
PERSON2000 <- PERSON2000 |>
  mutate(across(everything(), ~ifelse(.x == "-100", NA, .x))) |>
  mutate(HZONE = to_na(HZONE))
TRIP2000 <- TRIP2000 |>
  mutate(across(everything(), ~ifelse(.x == "-100", NA, .x))) |>
  mutate(ozone = to_na(ozone),
         dzone = to_na(dzone),
         origin = to_na(origin),
         destin = to_na(destin))

# 3) Authoritative (HOUSE)
hh_base <- house |> select(qno, hzone_house = hzone, weight_house)

# 4) Fallback 1: PERSON modal HZONE per household
person_modal <- person |>
  filter(!is.na(hzone)) |>
  count(qno, hzone, name = "n") |>
  arrange(qno, desc(n)) |>
  group_by(qno) |>
  slice_max(order_by = n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  rename(hzone_person_modal = hzone)

# 5) Fallback 2: TRIP home-origin/destination modal zones
trip_origin_modal <- trip |>
  filter(origin == 1L, !is.na(ozone)) |>
  count(qno, ozone, name = "n") |>
  arrange(qno, desc(n)) |>
  group_by(qno) |>
  slice_max(order_by = n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  rename(hzone_trip_origin_modal = ozone)

trip_dest_modal <- trip |>
  filter(destin == 1L, !is.na(dzone)) |>
  count(qno, dzone, name = "n") |>
  arrange(qno, desc(n)) |>
  group_by(qno) |>
  slice_max(order_by = n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  rename(hzone_trip_dest_modal = dzone)

# 6) Combine + choose final home_taz_2000 with provenance flags
hh_home <- hh_base |>
  left_join(person_modal, by = "qno") |>
  left_join(trip_origin_modal, by = "qno") |>
  left_join(trip_dest_modal, by = "qno") |>
  mutate(
    home_taz_2000 = coalesce(hzone_house, hzone_person_modal, hzone_trip_origin_modal, hzone_trip_dest_modal),
    home_taz_source = case_when(
      !is.na(hzone_house) ~ "HOUSE",
      is.na(hzone_house) & !is.na(hzone_person_modal) ~ "PERSON_MODAL",
      is.na(hzone_house) & is.na(hzone_person_modal) & !is.na(hzone_trip_origin_modal) ~ "TRIP_ORIGIN_MODAL",
      is.na(hzone_house) & is.na(hzone_person_modal) & is.na(hzone_trip_origin_modal) & !is.na(hzone_trip_dest_modal) ~ "TRIP_DEST_MODAL",
      TRUE ~ "MISSING"
    )
  )

# 7) Diagnostics
diag_house_vs_person <- hh_home |>
  filter(!is.na(hzone_house), !is.na(hzone_person_modal)) |>
  summarize(mismatch_rate = mean(hzone_house != hzone_person_modal), n = n())

diag_trip_origin_agree <- trip |>
  filter(origin == 1L, !is.na(ozone)) |>
  left_join(hh_home |> select(qno, home_taz_2000), by = "qno") |>
  summarize(share = mean(ozone == home_taz_2000, na.rm = TRUE), n = n())

diag_trip_dest_agree <- trip |>
  filter(destin == 1L, !is.na(dzone)) |>
  left_join(hh_home |> select(qno, home_taz_2000), by = "qno") |>
  summarize(share = mean(dzone == home_taz_2000, na.rm = TRUE), n = n())

# 8) Attach to person & trip
person_with_home <- person |> left_join(hh_home |> select(qno, home_taz_2000), by = "qno")
trip_with_home   <- trip   |> left_join(hh_home |> select(qno, home_taz_2000), by = "qno")

# 9) Save (paths are examples)
# readr::write_csv(hh_home, "/mnt/data/home_taz_2000_hh.csv")
# readr::write_csv(person_with_home, "/mnt/data/home_taz_2000_person.csv")
# readr::write_csv(trip_with_home, "/mnt/data/home_taz_2000_trip.csv")





###After exporting the lookup tables above from ArcGIS
library(readxl)
library(dplyr)

house2014  <- read_xlsx("HOUSE_2014.xlsx")
lookup2014 <- read_csv("2014_household_lookup.csv")

house2014 <- house2014 %>%
  left_join(lookup2014, by = "QNO")


