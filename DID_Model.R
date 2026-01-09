# =========================================================
#HTS 2000/2014/2019 — Harmonize + Outcomes + Staggered DiD
# Treatment scopes: "1A1B", "1A", "1B"
# =========================================================

# ---- 0) Setup: Folder & Packages ------------------------------------------
data_dir <- "/Users/Pee/Documents/UMN Phd/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Raw Data/HTS_surveys"

# single Excel that contains all three treatment sheets (1A1B, 1A, 1B)
treatment_file <- "/Users/Pee/Documents/UMN Phd/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Processed Data/Phases_TreatedAreas.xls"

# sheet names differ:
treatment_sheets <- c(`Combined Phase 1A1B` = "1A1B", `Phase 1A` = "1A", `Phase 1B` = "1B")

pkgs <- c("tidyverse","data.table","readxl","janitor",
          "stringr","broom","modelsummary","fixest")
invisible(lapply(setdiff(pkgs, rownames(installed.packages())), install.packages))
invisible(lapply(pkgs, library, character.only=TRUE))
options(scipen=999, dplyr.width=Inf)
set.seed(42)

odir <- file.path(data_dir,"outputs"); dir.create(odir, showWarnings = FALSE)


# ---------- 1) Autoload helper & file paths --------------------------------
# pick() searches ONLY within data_dir for HTS files.
pick <- function(patterns) {
  all <- list.files(data_dir, recursive=TRUE, full.names=TRUE)
  hit <- all[str_detect(tolower(all), str_c(patterns, collapse="|"))]
  if (length(hit)) hit[1] else NA_character_
}

paths <- list(
  `2000_house` = pick(c("2000.*house\\.(xls|xlsx)$","\\bhouse\\.xls$","\\bhouse\\.xlsx$")),
  `2000_person`= pick(c("2000.*person\\.(xls|xlsx)$","\\bperson\\.xls$","\\bperson\\.xlsx$")),
  `2000_trip`  = pick(c("2000.*trip\\.(xls|xlsx)$","\\btrip\\.xls$","\\btrip\\.xlsx$")),
  
  `2014_house` = pick(c("2014.*house\\.(xls|xlsx)$","cojhousefile\\.xls","coj_house.*\\.xlsx")),
  `2014_person`= pick(c("2014.*person\\.(xls|xlsx)$","cojpersonfile\\.xlsx","coj_person.*\\.xlsx")),
  `2014_trip`  = pick(c("2014.*trip\\.(xls|xlsx)$","cojtripfile\\.xlsx","coj_trip.*\\.xlsx")),
  `2014_att`   = pick(c("2014.*attitude.*\\.(xls|xlsx)$","attitudefile\\.xlsx")),
  
  `2019_house` = pick(c("2019.*house.*\\.(xls|xlsx)$","household_data\\.xlsx")),
  `2019_person`= pick(c("2019.*person.*\\.(xls|xlsx)$","person_data\\.xlsx")),
  `2019_imputed_persons` = pick(c("imputed_persons.*\\.(xls|xlsx)$"))
)

# sanity checks
print(paths)
if (!file.exists(treatment_file)) stop("Treatment Excel not found: ", treatment_file)

treatment_sheets <- list(
  "1A1B" = "Combined Phase 1A1B",
  "1A"   = "Phase 1A",
  "1B"   = "Phase 1B"
)

# Read treatment sheets (all from the same Excel)
treated_1a1b <- readxl::read_excel(treatment_file, sheet = treatment_sheets[["Combined Phase 1A1B"]]) |> janitor::clean_names()
treated_1a   <- readxl::read_excel(treatment_file, sheet = treatment_sheets[["Phase 1A"]]) |> janitor::clean_names()
treated_1b   <- readxl::read_excel(treatment_file, sheet = treatment_sheets[["Phase 1B"]]) |> janitor::clean_names()

cat("Loaded treatment sheets from:\n  ", treatment_file, "\n",
    "Sheets -> 1A1B: ", treatment_sheets[["1A1B"]],
    ", 1A: ", treatment_sheets[["1A"]],
    ", 1B: ", treatment_sheets[["1B"]], "\n", sep = "")


# ---------- 2A) Small helpers ---------------------------------------------
# Safe coercions (use these everywhere)
to_i <- function(x) suppressWarnings(as.integer(x))
to_n <- function(x) suppressWarnings(as.numeric(x))

# Read + clean names (IO helper)
read_any <- function(p) {
  if (is.na(p) || is.null(p)) return(NULL)
  readxl::read_excel(p) |> janitor::clean_names()
}

# Replace sentinel codes with NA
na_sentinels <- function(df) {
  if (is.null(df)) return(df)
  dplyr::mutate(df, across(everything(), \(x) {
    if (is.numeric(x)) replace(x, x %in% c(-100, -999, 888), NA) else x
  }))
}

get1 <- function(df, aliases) {
  # aliases: character vector of possible column names (already lower_snake case)
  nm <- names(df)
  cand <- aliases[aliases %in% nm]
  if (length(cand)==0) NULL else df[[cand[1]]]
}
coalesce_cols <- function(df, ...) {
  al <- list(...)
  cols <- lapply(al, \(a) get1(df, a))
  reduce(cols, ~dplyr::coalesce(.x, .y))
}

# ---------- 2B) Reversible pipeline scaffolding ----------------------------
# In-memory vault for raw snapshots
RAW <- new.env(parent = emptyenv())

# Save a clean snapshot immediately after reading
snapshot_raw <- function(name, df) {
  RAW[[name]] <- df
  df
}

# Restore a full table from the snapshot
restore_table <- function(name) {
  stopifnot(!is.null(RAW[[name]]))
  RAW[[name]]
}

# Restore specific columns from snapshot into a working table
restore_cols <- function(current_df, name, cols) {
  stopifnot(!is.null(RAW[[name]]))
  orig <- RAW[[name]]
  missing <- setdiff(cols, names(orig))
  if (length(missing)) stop("These columns were not in the original snapshot: ", paste(missing, collapse=", "))
  for (c in cols) current_df[[c]] <- orig[[c]]
  current_df
}

# ---------- 3) Load raw Excel ----------------------------------------------
# Helper: read file if path exists, else return NULL
# Replace your read_any() and na_sentinels() with these

read_any <- function(p) {
  if (is.na(p)) return(NULL)
  df <- readxl::read_excel(
    p,
    col_types = "text",         # <-- force everything to character
    guess_max = 100000,         # scan deep sheets without guessing types
    .name_repair = "unique"
  ) |>
    janitor::clean_names() |>
    na_sentinels()              # standardize missing codes while still text
  cat(basename(p), ":", nrow(df), "rows,", ncol(df), "columns\n")
  df
}

# Standardize common sentinel/missing codes both for text and numeric
na_sentinels <- function(df) {
  if (is.null(df)) return(df)
  dplyr::mutate(df, dplyr::across(everything(), \(x) {
    if (is.character(x)) {
      z <- trimws(x)
      z[z %in% c("-100","-999","888","","NA","N/A","n/a","NaN",".")] <- NA_character_
      z
    } else if (is.numeric(x)) {
      replace(x, x %in% c(-100,-999,888), NA)
    } else x
  }))
}

# Load 2000
H00 <- read_any(paths$`2000_house`)
P00 <- read_any(paths$`2000_person`)
T00 <- read_any(paths$`2000_trip`)

# Load 2014
H14 <- read_any(paths$`2014_house`)
P14 <- read_any(paths$`2014_person`)
T14 <- read_any(paths$`2014_trip`)
A14 <- read_any(paths$`2014_att`)

# Load 2019
H19 <- read_any(paths$`2019_house`)
P19_raw <- read_any(paths$`2019_person`)
Imp19 <- read_any(paths$`2019_imputed_persons`)

# Prefer imputed persons if available
P19 <- if (!is.null(Imp19)) {
  cat("Using imputed persons file for 2019\n")
  Imp19
} else {
  P19_raw
}

# ---------- 4) Variable-driven extractor (per year) ------------------------------------------
# ================================
# Variable-driven extractor:GENDER
# ================================
# ---- helpers (self-contained) ----
to_int <- function(x) suppressWarnings(as.integer(x))

# ----------------- helpers (safe to paste once) -----------------
library(dplyr); library(janitor); library(stringr); library(purrr); library(tibble)

to_int <- function(x) suppressWarnings(as.integer(x))

# Coalesce across a list of candidate column names in df (returns a vector)
coalesce_cols <- function(df, candidates) {
  if (is.null(df)) return(NULL)
  df <- janitor::clean_names(df)
  hits <- candidates[candidates %in% names(df)]
  if (!length(hits)) return(rep(NA, nrow(df)))
  out <- df[[hits[1]]]
  if (length(hits) > 1) for (v in hits[-1]) out <- dplyr::coalesce(out, df[[v]])
  out
}

# Score columns by how much their values look like gender tokens
score_gender_column <- function(x) {
  v <- tolower(trimws(as.character(x)))
  v[v %in% c("", "na", "n/a", "nan", ".", "-100", "-1")] <- NA
  mean(v %in% c("1","2","m","f","male","female","man","woman","boy","girl"), na.rm=TRUE)
}

# Find candidate gender columns by NAME and by VALUE pattern
find_gender_cols <- function(df) {
  df <- janitor::clean_names(df)
  nms <- names(df)
  
  # Name-based regex (extend if you discover more)
  name_hits <- nms[str_detect(nms,
                              "(^|_)sex($|_)|(^|_)gender($|_)|^q3_?2$|^p2_?gender$|respondent.*sex|person.*sex|sex_code"
  )]
  
  # Value-based hits (≥5% look like gender tokens)
  scores <- sapply(df, score_gender_column)
  val_hits <- names(df)[scores > 0.05]
  
  # Order: highest score first
  cand <- union(name_hits, val_hits)
  cand[order(match(cand, names(sort(scores, decreasing = TRUE))))]
}

# ----------------- year-agnostic GENDER extractor -----------------
extract_gender_auto <- function(year, P_person) {
  stopifnot(!is.null(P_person))
  P <- janitor::clean_names(P_person)
  
  # IDs via robust coalescing of common aliases
  HH_ID <- to_int(coalesce_cols(P, c("hh_id","q_id","qno","id")))
  P_ID  <- to_int(coalesce_cols(P, c("p_id","pindex","pnumber","pid")))
  
  # Find gender columns and coalesce + recode
  cand <- find_gender_cols(P)
  if (!length(cand)) {
    message(sprintf("Year %s: no plausible gender columns found by name/value scan.", year))
    g_num <- rep(NA_integer_, nrow(P))
  } else {
    # coalesce in the best order
    g_raw <- NULL
    for (c in cand) g_raw <- if (is.null(g_raw)) P[[c]] else dplyr::coalesce(g_raw, P[[c]])
    # normalize to 1/2/NA
    g_chr <- tolower(trimws(as.character(g_raw)))
    g_chr[g_chr %in% c("", "na", "n/a", "nan", ".", "-100", "-1")] <- NA_character_
    g_chr[g_chr %in% c("m","male","man","boy")]   <- "1"
    g_chr[g_chr %in% c("f","female","woman","girl")] <- "2"
    g_num <- to_int(g_chr)
    g_num[!(g_num %in% c(1L,2L))] <- NA_integer_
    message(sprintf("Year %s: gender candidates tried (in order): %s", year, paste(cand, collapse = ", ")))
  }
  
  tibble(
    SURVEY_YEAR = as.integer(year),
    HH_ID = HH_ID,
    P_ID  = P_ID,
    GENDER = g_num
  )
}

gender_2000 <- extract_gender_auto(2000, P00)
gender_2014 <- extract_gender_auto(2014, P14)
gender_2019 <- extract_gender_auto(2019, P19)
gender_all  <- dplyr::bind_rows(gender_2000, gender_2014, gender_2019)

##GENDER DESCRIPTIVES & plot
library(dplyr); library(ggplot2)

gender_summary <- gender_all %>%
  group_by(SURVEY_YEAR, GENDER) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(100 * n / sum(n, na.rm = TRUE), 1))

missing_summary <- gender_all %>%
  group_by(SURVEY_YEAR) %>%
  summarise(total = n(),
            missing_gender = sum(is.na(GENDER)),
            missing_percent = round(100 * missing_gender / total, 1),
            .groups="drop")

print(gender_summary)
print(missing_summary)

ggplot(gender_all, aes(x = factor(SURVEY_YEAR), fill = factor(GENDER))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Gender Distribution by Survey Year",
    x = "Survey Year",
    y = "Percentage",
    fill = "Gender (1=Male, 2=Female, NA)"
  ) +
  theme_minimal(base_size = 14)

# ================================
# Variable-driven extractor:AGE and AGE_GROUP
# ================================
# ---------- helpers (safe to paste once) ----------
library(dplyr); library(janitor); library(stringr); library(tibble)

to_int <- function(x) suppressWarnings(as.integer(x))
to_num <- function(x) suppressWarnings(as.numeric(x))

coalesce_cols <- function(df, candidates) {
  df <- janitor::clean_names(df)
  hits <- candidates[candidates %in% names(df)]
  if (!length(hits)) return(rep(NA, nrow(df)))
  out <- df[[hits[1]]]
  if (length(hits) > 1) for (v in hits[-1]) out <- dplyr::coalesce(out, df[[v]])
  out
}

age_group <- function(age) dplyr::case_when(
  !is.na(age) & age <= 17 ~ "0-17",
  !is.na(age) & age <= 34 ~ "18-34",
  !is.na(age) & age <= 64 ~ "35-64",
  !is.na(age) & age >= 65 ~ "65+",
  TRUE ~ NA_character_
)

parse_date_loose <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", ".", "nan")] <- NA
  fmts <- c("%Y-%m-%d","%d-%m-%Y","%m/%d/%Y","%d/%m/%Y","%Y/%m/%d","%d %b %Y","%d %B %Y")
  out <- as.Date(NA)
  suppressWarnings({
    for (f in fmts) {
      y <- as.Date(x, format = f)
      out <- ifelse(is.na(out), y, out)
    }
  })
  as.Date(out)
}

# ---------- PATCHED extractor (person-only) ----------
extract_age <- function(year, P_person) {
  stopifnot(!is.null(P_person))
  P <- janitor::clean_names(P_person)
  
  # IDs
  HH_ID <- to_int(coalesce_cols(P, c("hh_id","q_id","qno","id")))
  P_ID  <- to_int(coalesce_cols(P, c("p_id","pindex","pnumber","pid")))
  
  # Direct age aliases by year (extend if needed)
  age_alias <- list(
    `2000` = c("age","age_years","p_age","q3_1_age"),
    `2014` = c("age","age_years","q3_1_age","p_age"),
    `2019` = c("age","age_years","q3_1_age","p_age")
  )
  # DOB / YOB / interview date
  dob_alias   <- c("dob","date_of_birth","birth_date","q3_1_dob")
  byear_alias <- c("birth_year","year_of_birth","yob")
  intd_alias  <- c("interview_date","int_date","date_interviewed","survey_date")
  
  # 1) Direct age
  age_direct <- to_num(coalesce_cols(P, age_alias[[as.character(year)]]))
  age_direct[age_direct < 0 | age_direct > 120] <- NA
  need_calc <- is.na(age_direct)
  
  # 2) DOB → age (with safe Date arithmetic)
  dob  <- as.Date(parse_date_loose(coalesce_cols(P, dob_alias)))
  intd <- as.Date(parse_date_loose(coalesce_cols(P, intd_alias)))
  ref_date <- as.Date(sprintf("%s-06-30", year))
  ref_vec  <- as.Date(ifelse(!is.na(intd), intd, ref_date))
  age_from_dob <- suppressWarnings(
    floor(as.numeric(difftime(ref_vec, dob, units = "days")) / 365.25)
  )
  age_from_dob[age_from_dob < 0 | age_from_dob > 120] <- NA
  
  # 3) Birth-year → age
  byear <- to_int(coalesce_cols(P, byear_alias))
  age_from_yob <- ifelse(!is.na(byear), as.integer(year) - byear, NA_integer_)
  age_from_yob[age_from_yob < 0 | age_from_yob > 120] <- NA
  
  # Final AGE: prefer direct > DOB > YOB
  AGE <- age_direct
  idx <- which(need_calc & !is.na(age_from_dob)); if (length(idx)) AGE[idx] <- age_from_dob[idx]
  idx <- which(need_calc & is.na(age_from_dob) & !is.na(age_from_yob)); if (length(idx)) AGE[idx] <- age_from_yob[idx]
  AGE <- to_int(AGE)
  
  tibble(
    SURVEY_YEAR = as.integer(year),
    HH_ID = HH_ID,
    P_ID  = P_ID,
    AGE = AGE,
    AGE_GROUP = age_group(AGE)
  )
}

age_2000 <- extract_age(2000, P00)
age_2014 <- extract_age(2014, P14)
age_2019 <- extract_age(2019, P19)
age_all  <- dplyr::bind_rows(age_2000, age_2014, age_2019)

#Quick descriptives
age_stats <- age_all %>%
  group_by(SURVEY_YEAR) %>%
  summarise(
    n = n(),
    n_missing = sum(is.na(AGE)),
    missing_pct = round(100*n_missing/n,1),
    mean = round(mean(AGE, na.rm=TRUE),1),
    sd   = round(sd(AGE, na.rm=TRUE),1),
    p25  = quantile(AGE, .25, na.rm=TRUE, type=2),
    p50  = quantile(AGE, .50, na.rm=TRUE, type=2),
    p75  = quantile(AGE, .75, na.rm=TRUE, type=2),
    .groups="drop"
  )
print(age_stats)

# Make sure age_all exists (from the extract_age() step)
# Build age_groups (counts + % by year, incl. Missing)
age_groups <- age_all %>%
  mutate(
    AGE_GROUP = ifelse(is.na(AGE_GROUP), "Missing", AGE_GROUP),
    AGE_GROUP = factor(AGE_GROUP, levels = c("0-17","18-34","35-64","65+","Missing"))
  ) %>%
  group_by(SURVEY_YEAR, AGE_GROUP) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  group_by(SURVEY_YEAR) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

# View the table
print(age_groups)

#Quick plot
library(ggplot2)
# Histogram by year
ggplot(age_all, aes(x = AGE)) +
  geom_histogram(binwidth = 5, boundary = 0, closed = "left") +
  facet_wrap(~ SURVEY_YEAR, ncol = 1, scales = "free_y") +
  labs(title = "AGE distribution by survey year",
       x = "Age (years)", y = "Count") +
  theme_minimal(base_size = 13)

# Age-group shares by year
ggplot(age_groups, aes(x = factor(SURVEY_YEAR), y = pct, fill = AGE_GROUP)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Age-group composition by survey year",
       x = "Survey year", y = "Share", fill = "Age group") +
  theme_minimal(base_size = 13)
# Optional: stacked % bar chart
library(ggplot2)
ggplot(age_groups, aes(x = factor(SURVEY_YEAR), y = pct, fill = AGE_GROUP)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Age-group composition by survey year",
    x = "Survey year", y = "Share"
  ) +
  theme_minimal(base_size = 13)

# Build age_groups EXCLUDING missing
age_groups <- age_all %>%
  filter(!is.na(AGE_GROUP)) %>%
  mutate(
    AGE_GROUP = factor(AGE_GROUP, levels = c("0-17","18-34","35-64","65+"))
  ) %>%
  group_by(SURVEY_YEAR, AGE_GROUP) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  group_by(SURVEY_YEAR) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

# View table
print(age_groups)
library(flextable)
ft <- flextable(age_groups)
save_as_image(ft, path = "age_groups.png")

# Stacked % bar chart without Missing
library(ggplot2)
ggplot(age_groups, aes(x = factor(SURVEY_YEAR), y = pct, fill = AGE_GROUP)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Age-group composition by survey year (excluding missing)",
    x = "Survey year", y = "Share"
  ) +
  theme_minimal(base_size = 13)


# ================================
# Variable-driven extractor:RACE
# ================================
library(dplyr); library(janitor); library(stringr); library(purrr); library(tibble); library(ggplot2)

to_int <- function(x) suppressWarnings(as.integer(x))

# Coalesce across candidate column names
coalesce_cols <- function(df, candidates) {
  df <- janitor::clean_names(df)
  hits <- candidates[candidates %in% names(df)]
  if (!length(hits)) return(rep(NA, nrow(df)))
  out <- df[[hits[1]]]
  if (length(hits) > 1) for (v in hits[-1]) out <- dplyr::coalesce(out, df[[v]])
  out
}

library(dplyr); library(janitor); library(stringr); library(purrr); library(tibble); library(ggplot2)

to_int <- function(x) suppressWarnings(as.integer(x))

# Coalesce across candidate column names
coalesce_cols <- function(df, candidates) {
  df <- janitor::clean_names(df)
  hits <- candidates[candidates %in% names(df)]
  if (!length(hits)) return(rep(NA, nrow(df)))
  out <- df[[hits[1]]]
  if (length(hits) > 1) for (v in hits[-1]) out <- dplyr::coalesce(out, df[[v]])
  out
}

# Auto-detect + recode for RACE
# score how "race-like" a column's values are (strings or codes)
score_race_column <- function(x) {
  v <- tolower(trimws(as.character(x)))
  v[v %in% c("", "na", "n/a", "nan", ".", "-100", "-1")] <- NA
  # tokens commonly seen for SA population group
  tokens <- c("1","2","3","4","black","black african","african",
              "white","coloured","colored","indian","asian","indian/asian")
  mean(v %in% tokens, na.rm = TRUE)
}

# candidates by name & value patterns
find_race_cols <- function(df) {
  df <- janitor::clean_names(df)
  nms <- names(df)
  # common header patterns (extend if you see new ones)
  name_hits <- nms[str_detect(nms,
                              "(^|_)race($|_)|population.?group|pop.?group|populationgroup|^q3_?3$|^p2_?race$|^race_grp$"
  )]
  scores <- sapply(df, score_race_column)
  val_hits <- names(df)[scores > 0.05]
  cand <- union(name_hits, val_hits)
  cand[order(match(cand, names(sort(scores, decreasing = TRUE))))]
}

# map raw values to 1..4 codes
recode_race_to_1_4 <- function(x) {
  z <- tolower(trimws(as.character(x)))
  z[z %in% c("", "na", "n/a", "nan", ".", "-100", "-1")] <- NA
  # allow numeric codes already present
  z[z %in% c("black","black african","african","1")]    <- "1"
  z[z %in% c("white","2")]                              <- "2"
  z[z %in% c("coloured","colored","3")]                 <- "3"
  z[z %in% c("indian","asian","indian/asian","4")]      <- "4"
  # anything else (e.g., "other", "mixed", "refused") → NA
  out <- to_int(z)
  out[!(out %in% c(1L,2L,3L,4L))] <- NA_integer_
  out
}

# main extractor (PERSON table only)
extract_race_auto <- function(year, P_person) {
  stopifnot(!is.null(P_person))
  P <- janitor::clean_names(P_person)
  
  # IDs
  HH_ID <- to_int(coalesce_cols(P, c("hh_id","q_id","qno","id")))
  P_ID  <- to_int(coalesce_cols(P, c("p_id","pindex","pnumber","pid")))
  
  # find candidate race columns and coalesce
  cand <- find_race_cols(P)
  if (!length(cand)) {
    message(sprintf("Year %s: no plausible race columns found by name/value scan.", year))
    race_raw <- rep(NA, nrow(P))
  } else {
    race_raw <- NULL
    for (c in cand) race_raw <- if (is.null(race_raw)) P[[c]] else dplyr::coalesce(race_raw, P[[c]])
    message(sprintf("Year %s: race candidates tried (in order): %s", year, paste(cand, collapse=", ")))
  }
  
  RACE <- recode_race_to_1_4(race_raw)
  
  tibble(
    SURVEY_YEAR = as.integer(year),
    HH_ID = HH_ID,
    P_ID  = P_ID,
    RACE  = RACE
  )
}

race_2000 <- extract_race_auto(2000, P00)
race_2014 <- extract_race_auto(2014, P14)
race_2019 <- extract_race_auto(2019, P19)

race_all <- bind_rows(race_2000, race_2014, race_2019)
dplyr::glimpse(race_all)

#Descriptives (counts/percents) and chart excluding missing
# table: counts & shares (exclude NA)
race_summary <- race_all %>%
  filter(!is.na(RACE)) %>%
  group_by(SURVEY_YEAR, RACE) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  ungroup()

# missing by year (FYI)
race_missing <- race_all %>%
  group_by(SURVEY_YEAR) %>%
  summarise(total = n(),
            missing_race = sum(is.na(RACE)),
            missing_percent = round(100 * missing_race / total, 1),
            .groups = "drop")

print(race_summary)
print(race_missing)

# View table
print(race_summary)
library(flextable)
ft <- flextable(race_summary)
save_as_image(ft, path = "race_summary.png")


# plot: stacked shares without missing
ggplot(race_all %>% filter(!is.na(RACE)),
       aes(x = factor(SURVEY_YEAR), fill = factor(RACE))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Race distribution by survey year (excluding missing)",
    x = "Survey Year", y = "Share",
    fill = "RACE\n1=Black, 2=White, 3=Coloured, 4=Indian/Asian"
  ) +
  theme_minimal(base_size = 13)


# ================================
# Variable-driven extractor:EDU_LEVEL
# ================================
library(dplyr); library(janitor); library(stringr); library(purrr); library(tibble); library(ggplot2)

to_int <- function(x) suppressWarnings(as.integer(x))

coalesce_cols <- function(df, candidates) {
  df <- janitor::clean_names(df)
  hits <- candidates[candidates %in% names(df)]
  if (!length(hits)) return(rep(NA, nrow(df)))
  out <- df[[hits[1]]]
  if (length(hits) > 1) for (v in hits[-1]) out <- dplyr::coalesce(out, df[[v]])
  out
}

#Auto-detect + recode for education
# How "education-like" are a column's values?
score_edu_column <- function(x) {
  v <- tolower(trimws(as.character(x)))
  v[v %in% c("", "na", "n/a", "nan", ".", "-100", "-1")] <- NA
  tokens <- c(
    # numeric codes
    "1","2","3","4","5","6","7",
    # text tokens (SA-style)
    "none","no schooling","no education","primary","some primary",
    "some secondary","incomplete secondary","grade 8","grade 9","grade 10","grade 11",
    "matric","grade 12","completed secondary","secondary completed",
    "certificate","technical","artisan","college",
    "diploma","degree","bachelors","bachelor","undergraduate",
    "honours","honors","masters","master","phd","doctorate","postgraduate","post grad","post-grad"
  )
  mean(v %in% tokens, na.rm = TRUE)
}

# Find candidate columns by name patterns + value patterns
find_edu_cols <- function(df) {
  df <- janitor::clean_names(df)
  nms <- names(df)
  name_hits <- nms[str_detect(
    nms,
    "(^|_)(edu|educ|education|school|highest_edu|highest_education|q3_?4)(_|$)"
  )]
  scores <- sapply(df, score_edu_column)
  val_hits <- names(df)[scores > 0.05]
  cand <- union(name_hits, val_hits)
  cand[order(match(cand, names(sort(scores, decreasing = TRUE))))]
}

# Recode to 1..7 using robust string matching
recode_edu_to_1_7 <- function(x) {
  z <- tolower(trimws(as.character(x)))
  z[z %in% c("", "na", "n/a", "nan", ".", "-100", "-1")] <- NA
  
  # numeric codes pass through if 1..7
  # text → map:
  # 1: none
  z[str_detect(z, "no school|no educ|none")] <- "1"
  # 2: primary
  z[str_detect(z, "primary")] <- "2"
  # 4 first: explicit matric/grade12 (avoid catching by 'secondary' before)
  z[str_detect(z, "matric|grade\\s*12|completed secondary|secondary completed")] <- "4"
  # 3: some/incomplete secondary (not matric)
  z[str_detect(z, "some secondary|incomplete secondary|grade\\s*(8|9|10|11)")] <- "3"
  # 5: certificate/technical/college (non-degree)
  z[str_detect(z, "certificate|technical|artisan|college")] <- "5"
  # 7: postgraduate (masters, phd, honours)
  z[str_detect(z, "honours|honors|masters|master|phd|doctorate|post\\s*grad")] <- "7"
  # 6: degree/bachelor (undergrad)
  z[str_detect(z, "degree|bachelor|undergraduate|diploma")] <- ifelse(
    str_detect(z, "diploma"), "6", "6"
  )
  
  out <- to_int(z)
  out[!(out %in% 1:7)] <- NA_integer_
  out
}

# Main extractor (PERSON table only)
extract_edu_level_auto <- function(year, P_person) {
  stopifnot(!is.null(P_person))
  P <- janitor::clean_names(P_person)
  
  # IDs
  HH_ID <- to_int(coalesce_cols(P, c("hh_id","q_id","qno","id")))
  P_ID  <- to_int(coalesce_cols(P, c("p_id","pindex","pnumber","pid")))
  
  # Try year-specific aliases first (edit these if you know exact headers)
  edu_alias <- list(
    `2000` = c("education","edu_level","highest_education","q3_4"),
    `2014` = c("education","edu_level","highest_education","q3_4"),
    `2019` = c("education","edu_level","highest_education","q3_4")
  )
  edu_raw <- coalesce_cols(P, edu_alias[[as.character(year)]])
  
  # If still empty, auto-find and coalesce across candidates
  if (all(is.na(edu_raw))) {
    cand <- find_edu_cols(P)
    if (length(cand)) {
      message(sprintf("Year %s: EDU candidates tried (in order): %s", year, paste(cand, collapse=", ")))
      for (c in cand) edu_raw <- if (is.null(edu_raw)) P[[c]] else dplyr::coalesce(edu_raw, P[[c]])
    } else {
      message(sprintf("Year %s: no plausible EDU columns found.", year))
      edu_raw <- rep(NA, nrow(P))
    }
  }
  
  EDU_LEVEL <- recode_edu_to_1_7(edu_raw)
  
  tibble(
    SURVEY_YEAR = as.integer(year),
    HH_ID = HH_ID,
    P_ID  = P_ID,
    EDU_LEVEL = EDU_LEVEL
  )
}

edu_2000 <- extract_edu_level_auto(2000, P00)
edu_2014 <- extract_edu_level_auto(2014, P14)
edu_2019 <- extract_edu_level_auto(2019, P19)

edu_all <- bind_rows(edu_2000, edu_2014, edu_2019)
dplyr::glimpse(edu_all)

#Descriptives (counts/percents) and chart excluding missing
# counts & shares (ignore missing)
edu_summary <- edu_all %>%
  filter(!is.na(EDU_LEVEL)) %>%
  group_by(SURVEY_YEAR, EDU_LEVEL) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  ungroup()

# missing by year (FYI)
edu_missing <- edu_all %>%
  group_by(SURVEY_YEAR) %>%
  summarise(
    total = n(),
    missing_edu = sum(is.na(EDU_LEVEL)),
    missing_percent = round(100 * missing_edu / total, 1),
    .groups = "drop"
  )

print(edu_summary)
print(edu_missing)

# stacked shares (exclude missing)
ggplot(edu_all %>% filter(!is.na(EDU_LEVEL)),
       aes(x = factor(SURVEY_YEAR), fill = factor(EDU_LEVEL))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Education distribution by survey year (excluding missing)",
    x = "Survey Year", y = "Share",
    fill = "EDU_LEVEL\n1=None, 2=Primary, 3=Some Sec,\n4=Matric, 5=Certificate, 6=Diploma/Degree, 7=Postgrad"
  ) +
  theme_minimal(base_size = 13)


# ================================
# Variable-driven extractor:EMPLOY_STATUS + UNEMPLOYED_LF
# ================================
# --- Helper: Convert text/codes to unified EMPLOY_STATUS
recode_employment_status <- function(x) {
  z <- tolower(trimws(as.character(x)))
  z[z %in% c("", "na", "n/a", "nan", ".", "-100", "-1")] <- NA
  
  # Map text responses to numeric codes
  z[str_detect(z, "employed|self")] <- "1"
  z[str_detect(z, "unemployed|no job|looking for work")] <- "2"
  z[str_detect(z, "retired|pension")] <- "3"
  z[str_detect(z, "housewife|home duties")] <- "4"
  z[str_detect(z, "child at home")] <- "5"
  z[str_detect(z, "scholar|student")] <- "6"
  z[str_detect(z, "pre[- ]?school")] <- "7"
  z[str_detect(z, "other")] <- "8"
  
  out <- as.integer(z)
  out[!(out %in% 1:8)] <- NA_integer_
  out
}

# --- Helper: Create UNEMPLOYED_LF from EMPLOY_STATUS
create_unemployed_lf <- function(emp_status) {
  ifelse(emp_status %in% c(1, 2),
         ifelse(emp_status == 2, 1, 0),
         NA_integer_)
}

# --- Main extraction function
extract_employment_auto <- function(year, P_person) {
  stopifnot(!is.null(P_person))
  P <- janitor::clean_names(P_person)
  
  # IDs
  HH_ID <- coalesce_cols(P, c("hh_id","q_id","qno","id"))
  P_ID  <- coalesce_cols(P, c("p_id","pindex","pnumber","pid"))
  
  # Likely employment status columns by year
  emp_alias <- list(
    `2000` = c("empstat","wrkstat","employment_status","q4_1"),
    `2014` = c("empstat","wrkstat","employment_status","q4_1"),
    `2019` = c("empstat","wrkstat","employment_status","q4_1")
  )
  
  emp_raw <- coalesce_cols(P, emp_alias[[as.character(year)]])
  
  # If not found, try auto-search
  if (all(is.na(emp_raw))) {
    cand <- names(P)[str_detect(names(P), "(emp|wrk|work|job|labour|labor|employment)")]
    if (length(cand)) {
      message(sprintf("Year %s: EMP candidates tried (in order): %s", year, paste(cand, collapse=", ")))
      for (c in cand) emp_raw <- coalesce(emp_raw, P[[c]])
    } else {
      message(sprintf("Year %s: no plausible EMP columns found.", year))
      emp_raw <- rep(NA, nrow(P))
    }
  }
  
  EMPLOY_STATUS <- recode_employment_status(emp_raw)
  UNEMPLOYED_LF <- create_unemployed_lf(EMPLOY_STATUS)
  
  tibble(
    SURVEY_YEAR = as.integer(year),
    HH_ID = as.integer(HH_ID),
    P_ID  = as.integer(P_ID),
    EMPLOY_STATUS = EMPLOY_STATUS,
    UNEMPLOYED_LF = UNEMPLOYED_LF
  )
}

emp_2000 <- extract_employment_auto(2000, P00)
emp_2014 <- extract_employment_auto(2014, P14)
emp_2019 <- extract_employment_auto(2019, P19)
emp_all  <- bind_rows(emp_2000, emp_2014, emp_2019)

# EMPLOY_STATUS distribution
emp_summary <- emp_all %>%
  filter(!is.na(EMPLOY_STATUS)) %>%
  group_by(SURVEY_YEAR, EMPLOY_STATUS) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  ungroup()

# Missingness
emp_missing <- emp_all %>%
  group_by(SURVEY_YEAR) %>%
  summarise(
    total = n(),
    missing_emp = sum(is.na(EMPLOY_STATUS)),
    missing_percent = round(100 * missing_emp / total, 1),
    .groups = "drop"
  )

# UNEMPLOYED_LF distribution
unemp_summary <- emp_all %>%
  filter(!is.na(UNEMPLOYED_LF)) %>%
  group_by(SURVEY_YEAR, UNEMPLOYED_LF) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  ungroup()

#Optional charts
ggplot(emp_all %>% filter(!is.na(EMPLOY_STATUS)),
       aes(x = factor(SURVEY_YEAR), fill = factor(EMPLOY_STATUS))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Employment status by year",
       x = "Survey Year", y = "Share",
       fill = "EMPLOY_STATUS") +
  theme_minimal()

ggplot(emp_all %>% filter(!is.na(UNEMPLOYED_LF)),
       aes(x = factor(SURVEY_YEAR), fill = factor(UNEMPLOYED_LF))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Unemployed in labour force by year",
       x = "Survey Year", y = "Share",
       fill = "UNEMPLOYED_LF") +
  theme_minimal()

###LATER SORT OUT then chain this with GENDER, AGE, and RACE so you start building a harmonized person-level dataset for the panel while keeping EMPLOY_STATUS and UNEMPLOYED_LF definitions strict.


# ================================
# Variable-driven extractor: ANY CAR ACCESS
# ================================
library(dplyr)
library(janitor)

to_int <- function(x) suppressWarnings(as.integer(x))
to_num <- function(x) suppressWarnings(as.numeric(x))

# Coalesce across candidate column names (after clean_names)
coalesce_cols <- function(df, candidates) {
  df <- janitor::clean_names(df)
  hits <- candidates[candidates %in% names(df)]
  if (!length(hits)) return(rep(NA, nrow(df)))
  out <- df[[hits[1]]]
  if (length(hits) > 1) for (v in hits[-1]) out <- dplyr::coalesce(out, df[[v]])
  out
}

# Get a numeric column from any of a set of candidates; always length nrow(df)
get_num_col <- function(df, candidates) {
  df <- janitor::clean_names(df)
  cand_low <- tolower(candidates)
  hits <- intersect(cand_low, names(df))
  if (!length(hits)) return(rep(NA_real_, nrow(df)))
  out <- NULL
  for (h in hits) {
    v <- to_num(df[[h]])
    out <- if (is.null(out)) v else dplyr::coalesce(out, v)
  }
  out
}

extract_car_access_explicit <- function(year, H, cars_cols, cocar_cols) {
  stopifnot(!is.null(H))
  Hc <- janitor::clean_names(H)
  
  # IDs (robust aliases)
  HH_ID <- to_int(coalesce_cols(Hc, c("hh_id","q_id","qno","id","household_id")))
  HZONE <- to_int(coalesce_cols(Hc, c("hzone","tz","zoneid","zone_id")))
  
  # numeric car counts from candidate names (case-insensitive, coalesced)
  cars   <- get_num_col(Hc, cars_cols)
  cocars <- get_num_col(Hc, cocar_cols)
  
  # combine: 1 if sum>0, 0 if sum==0 and not both NA, NA if both NA
  sum2  <- rowSums(cbind(replace_na(cars, 0), replace_na(cocars, 0)))
  both_na <- is.na(cars) & is.na(cocars)
  any_car <- ifelse(both_na, NA_integer_, ifelse(sum2 > 0, 1L, 0L))
  
  tibble::tibble(
    SURVEY_YEAR     = as.integer(year),
    HH_ID           = HH_ID,
    HZONE           = HZONE,
    ANY_CAR_ACCESS  = any_car
  )
}

# 2000: CARS, COCAR
car_2000 <- extract_car_access_explicit(
  2000, H00,
  cars_cols  = c("CARS","cars"),
  cocar_cols = c("COCAR","cocar","companycars")  # include lowercase just in case
)

# 2014: cars, companycars
car_2014 <- extract_car_access_explicit(
  2014, H14,
  cars_cols  = c("cars","CARS"),
  cocar_cols = c("companycars","COCAR","cocar")
)

# 2019: CARS, COCAR
car_2019 <- extract_car_access_explicit(
  2019, H19,
  cars_cols  = c("CARS","cars"),
  cocar_cols = c("COCAR","cocar","companycars")
)

car_all <- dplyr::bind_rows(car_2000, car_2014, car_2019)

# Quick QA
car_summary <- car_all %>%
  group_by(SURVEY_YEAR, ANY_CAR_ACCESS) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(100 * n / sum(n), 1))
print(car_summary)





#####BASELINE ANALYSIS
library(dplyr); library(janitor); library(stringr); library(tibble); library(purrr)

to_int <- function(x) suppressWarnings(as.integer(x))
to_num <- function(x) suppressWarnings(as.numeric(x))

# Coalesce across candidate column names in a data.frame (after clean_names)
coalesce_cols <- function(df, candidates) {
  df <- janitor::clean_names(df)
  hits <- candidates[candidates %in% names(df)]
  if (!length(hits)) return(rep(NA, nrow(df)))
  out <- df[[hits[1]]]
  if (length(hits) > 1) for (v in hits[-1]) out <- dplyr::coalesce(out, df[[v]])
  out
}

# Build IDs from common aliases in any table
ids_from <- function(df) {
  d <- janitor::clean_names(df)
  tibble(
    HH_ID = to_int(coalesce_cols(d, c("hh_id","q_id","qno","id","household_id"))),
    P_ID  = to_int(coalesce_cols(d, c("p_id","pindex","pnumber","pid"))),
    HZONE = to_int(coalesce_cols(d, c("hzone","tz","zoneid","zone_id")))
  )
}

# Returns PERSON-level rows: SURVEY_YEAR, HH_ID, P_ID, HZONE, TRIPS_PER_DAY
extract_trips_per_day <- function(year, P = NULL, T = NULL) {
  y <- as.integer(year)
  
  # ---------- Preferred: compute from TRIP table (2000/2014) ----------
  from_trips <- function(Ttrip) {
    if (is.null(Ttrip)) return(NULL)
    TT <- janitor::clean_names(Ttrip)
    keys <- ids_from(TT)
    
    # Try to identify a trip identifier (to avoid accidental duplicates)
    trip_id <- coalesce_cols(TT, c("trip_id","trindex","trip_num"))
    has_trid <- !all(is.na(trip_id))
    
    # Count trips per person
    base <- bind_cols(keys, tibble(TRIP_ID = if (has_trid) trip_id else NA_integer_)) %>%
      filter(!is.na(HH_ID), !is.na(P_ID)) %>%
      {
        if (has_trid) group_by(., HH_ID, P_ID, HZONE) %>%
          summarise(TRIPS_PER_DAY = n_distinct(TRIP_ID, na.rm = TRUE), .groups = "drop")
        else group_by(., HH_ID, P_ID, HZONE) %>%
          summarise(TRIPS_PER_DAY = dplyr::n(), .groups = "drop")
      }
    
    mutate(base, SURVEY_YEAR = y, .before = 1)
  }
  
  # ---------- Fallback / 2019: from PERSON table ----------
  from_person <- function(Ppers) {
    if (is.null(Ppers)) return(NULL)
    PP <- janitor::clean_names(Ppers)
    keys <- ids_from(PP)
    # Common headers people use for total trips in person file (esp. 2019)
    tp <- coalesce_cols(PP, c("trips_per_day","q5_totaltrips","total_trips","num_trips"))
    tp <- to_int(tp)
    
    tibble(
      SURVEY_YEAR   = y,
      HH_ID         = keys$HH_ID,
      P_ID          = keys$P_ID %>% ifelse(is.na(.), 1L, .),  # fallback index if not provided
      HZONE         = keys$HZONE,
      TRIPS_PER_DAY = tp
    )
  }
  
  out <- NULL
  # Prefer trip-based computation for 2000/2014 if T is provided
  if (y %in% c(2000, 2014) && !is.null(T)) out <- from_trips(T)
  
  # If no trip table (or all NA), try person-based
  if (is.null(out) || (all(is.na(out$TRIPS_PER_DAY)) && !is.null(P))) {
    alt <- from_person(P)
    if (!is.null(alt)) {
      # If we also had trip-based rows, prefer trip counts where available
      if (is.null(out)) out <- alt else {
        out <- out %>%
          full_join(alt, by = c("SURVEY_YEAR","HH_ID","P_ID","HZONE"),
                    suffix = c("_trip","_pers")) %>%
          mutate(TRIPS_PER_DAY = coalesce(TRIPS_PER_DAY_trip, TRIPS_PER_DAY_pers)) %>%
          select(SURVEY_YEAR, HH_ID, P_ID, HZONE, TRIPS_PER_DAY)
      }
    }
  }
  
  if (is.null(out)) {
    # Nothing to compute—return an empty tibble with correct columns
    return(tibble(SURVEY_YEAR = integer(), HH_ID = integer(), P_ID = integer(),
                  HZONE = integer(), TRIPS_PER_DAY = integer()))
  }
  
  # Clean unrealistic values
  out$TRIPS_PER_DAY <- ifelse(out$TRIPS_PER_DAY < 0 | out$TRIPS_PER_DAY > 40, NA_integer_, out$TRIPS_PER_DAY)
  out
}

#Use it with your loaded data frames
# Assuming you already loaded/created:
# P00, T00, P14, T14, P19  (P19 can be the imputed persons file if you prefer)

trip_2000 <- extract_trips_per_day(2000, P = P00, T = T00)
trip_2014 <- extract_trips_per_day(2014, P = P14, T = T14)
trip_2019 <- extract_trips_per_day(2019, P = P19, T = NULL)  # 2019 has no trip file

microdata <- bind_rows(trip_2000, trip_2014, trip_2019)

# Quick QA
micro_qc <- microdata %>%
  group_by(SURVEY_YEAR) %>%
  summarise(
    n_person = n(),
    missing_tp = sum(is.na(TRIPS_PER_DAY)),
    missing_pct = round(100*missing_tp/n_person, 1),
    mean_tp = mean(TRIPS_PER_DAY, na.rm = TRUE),
    p50 = quantile(TRIPS_PER_DAY, .5, na.rm = TRUE, type = 2),
    p90 = quantile(TRIPS_PER_DAY, .9, na.rm = TRUE, type = 2),
    .groups = "drop"
  )
print(micro_qc)

#Hook into the baseline DiD (TAZ × Year)
library(readxl); library(fixest)

# Load treatment sheets 
treat_1a1b <- readxl::read_excel("/mnt/data/Phases_TreatedAreas.xls", sheet = "Combined Phase 1A1B") %>%
  janitor::clean_names() %>% mutate(treatment_phase = "Phase_1A1B")
treat_1a   <- readxl::read_excel("/mnt/data/Phases_TreatedAreas.xls", sheet = "Phase 1A") %>%
  janitor::clean_names() %>% mutate(treatment_phase = "Phase_1A")
treat_1b   <- readxl::read_excel("/mnt/data/Phases_TreatedAreas.xls", sheet = "Phase 1B") %>%
  janitor::clean_names() %>% mutate(treatment_phase = "Phase_1B")


# Read treatment sheets (all from the same Excel)
treated_1a1b <- readxl::read_excel(treatment_file, sheet = treatment_sheets[["Combined Phase 1A1B"]]) |> janitor::clean_names() () %>% mutate(treatment_phase = "Phase_1A1B")
treated_1a   <- readxl::read_excel(treatment_file, sheet = treatment_sheets[["Phase 1A"]]) |> janitor::clean_names() %>% mutate(treatment_phase = "Phase_1A")
treated_1b   <- readxl::read_excel(treatment_file, sheet = treatment_sheets[["Phase 1B"]]) |> janitor::clean_names() %>% mutate(treatment_phase = "Phase_1B")

treatment <- bind_rows(treated_1a1b, treated_1a, treated_1b) %>%
  rename(HZONE = zoneid) %>%
  mutate(
    treatment_year = dplyr::case_when(
      treatment_phase == "Phase_1A" ~ 2009L,
      treatment_phase == "Phase_1B" ~ 2013L,
      TRUE ~ NA_integer_
    ),
    treatment = ifelse(!is.na(treatment_year), 1L, 0L)
  )

# Build TAZ×Year panel (avg trips per person)
panel <- microdata %>%
  group_by(HZONE, SURVEY_YEAR) %>%
  summarise(TRIPS_PER_DAY = mean(TRIPS_PER_DAY, na.rm = TRUE), .groups = "drop") %>%
  left_join(treatment, by = "HZONE") %>%
  mutate(post_treatment = as.integer(!is.na(treatment_year) & SURVEY_YEAR >= treatment_year))

# Baseline DiD (TAZ & Year FE; cluster by TAZ)
m_base <- feols(
  TRIPS_PER_DAY ~ post_treatment * treatment | HZONE + SURVEY_YEAR,
  data = panel, cluster = ~ HZONE
)
summary(m_base)

















  
  
  
  # TRIPS_PER_DAY & FREQ_PER_PERSON
  if (!is.null(T) && year %in% c(2000,2014)) {
    trips_by_person <- T |>
      group_by(HH_ID = hh_id, P_ID = p_id, HZONE = hzone) |>
      summarize(TRIPS_PER_DAY = n(), FREQ_PER_PERSON = n(), .groups="drop")
    base <- base |> left_join(trips_by_person, by=c("HH_ID","P_ID","HZONE"))
  } else if (!is.null(P) && year==2019) {
    base <- base |>
      mutate(TRIPS_PER_DAY = to_num(coalesce(!!!rlang::syms(c("trips_per_day","q5_totaltrips")))),
             FREQ_PER_PERSON = TRIPS_PER_DAY)
  }
  
  # Time variables, distance, mode share, work share
  if (!is.null(T) && year %in% c(2000,2014)) {
    dep  <- coalesce(T$dep_time, T$q5_6)
    arr  <- coalesce(T$arr_time, T$q5_9)
    tmin <- as_minutes(arr) - as_minutes(dep)
    dist <- coalesce(T$trip_distance_km, T$distance_km, T$tripdist_km)
    mode <- coalesce(T$mode, T$MODE1, T$q5_10_mode1)
    purp <- coalesce(T$purpose, T$MAIN_PURP, T$q5_11)
    
    Tcalc <- T |>
      transmute(HH_ID = hh_id, P_ID = p_id, HZONE = hzone,
                time_min = tmin, dist_km = to_num(dist),
                pt_main = is_pt_mode(mode),
                work = as.integer(to_int(purp)==2))
    
    agg <- Tcalc |>
      group_by(HH_ID,P_ID,HZONE) |>
      summarize(
        AVG_TRAVEL_TIME = mean(time_min, na.rm=TRUE),
        TRIPDIST_TOT = sum(dist_km, na.rm=TRUE),
        TRAVEL_TIME_PER_MILE = ifelse(sum(dist_km, na.rm=TRUE)>0,
                                      sum(time_min, na.rm=TRUE)/sum(dist_km, na.rm=TRUE), NA_real_),
        PT_MAIN_SHARE = mean(pt_main, na.rm=TRUE),
        AVG_WORK_TIME = mean(ifelse(work==1, time_min, NA), na.rm=TRUE),
        MAIN_PURP_WORK_SHARE = mean(work, na.rm=TRUE),
        .groups="drop"
      )
    base <- base |> left_join(agg, by=c("HH_ID","P_ID","HZONE"))
  } else if (!is.null(P) && year==2019) {
    base <- base |>
      mutate(
        AVG_TRAVEL_TIME = to_num(coalesce(!!!rlang::syms(c("avg_travel_time")))),
        TRIPDIST_TOT = to_num(coalesce(!!!rlang::syms(c("tripdist_tot","total_km")))),
        TRAVEL_TIME_PER_MILE = to_num(coalesce(!!!rlang::syms(c("travel_time_per_mile")))),
        PT_MAIN_SHARE = to_num(coalesce(!!!rlang::syms(c("pt_main_share")))),
        AVG_WORK_TIME = to_num(coalesce(!!!rlang::syms(c("avg_work_time")))),
        MAIN_PURP_WORK_SHARE = to_num(coalesce(!!!rlang::syms(c("work_trip_share"))))
      )
  }
  
  # UNEMPLOYED_LF (from person)
  if (!is.null(P)) {
    base <- base |>
      mutate(UNEMPLOYED_LF = as.integer(to_int(coalesce(!!!rlang::syms(c("wrkstat","employment_status","q3_1"))))==2))
  }
  
  base
}

# Build the PANEL (stacked person–year)
Y2000 <- extract_year(2000)
Y2014 <- extract_year(2014)
Y2019 <- extract_year(2019)
Panel  <- bind_rows(Y2000, Y2014, Y2019)

# Save the raw panel snapshot
readr::write_csv(Panel, file.path(odir, "panel_variable_driven_raw.csv"))

# ---- 5) Treatment from Excel (scopes: 1A1B / 1A / 1B) --------------------
buffer_choice <- "1km"   # "500m","1km","2km"
scope_choice  <- "1A1B"  # "1A1B","1A","1B"

pick_sheet <- function(path, hint) {
  sh <- tryCatch(readxl::excel_sheets(path), error=function(e) character(0))
  if (!length(sh)) return(NULL)
  sh[str_detect(tolower(sh), tolower(hint))][1]
}
read_treat_one <- function(xls_path, buffer, sheet=NULL) {
  stopifnot(!is.na(xls_path))
  df <- readxl::read_excel(xls_path, sheet = sheet) |> janitor::clean_names()
  zcol <- names(df)[str_detect(names(df), "^zoneid$|^id$")][1]
  tag  <- switch(buffer, "500m"="500m","1km"="1km","2km"="2km")
  tcol <- names(df)[ str_detect(names(df), paste0("trt.*sts.*",tag)) |
                       str_detect(names(df), paste0("treatment.*status.*",tag)) ][1]
  if (is.na(tcol)) {
    pcol <- names(df)[str_detect(names(df), paste0("pct.*svd.*",tag))][1]
    if (is.na(pcol)) stop("Treatment or % served column for buffer not found.")
    tibble(HZONE = to_int(df[[zcol]]), treat = as.integer(to_num(df[[pcol]]) >= 50))
  } else {
    tibble(HZONE = to_int(df[[zcol]]), treat = to_int(df[[tcol]]))
  }
}

if (scope_choice == "1A1B") {
  t1a <- read_treat_one(paths$treated_1a, buffer_choice, sheet = pick_sheet(paths$treated_1a,"1a")) |>
    mutate(treat_year = ifelse(treat==1, 2009L, NA_integer_))
  t1b <- read_treat_one(paths$treated_1b, buffer_choice, sheet = pick_sheet(paths$treated_1b,"1b")) |>
    mutate(treat_year = ifelse(treat==1, 2013L, NA_integer_))
  treat_df <- full_join(t1a, t1b, by="HZONE", suffix=c("_1a","_1b")) |>
    transmute(HZONE,
              treat = as.integer(pmax(coalesce(treat_1a,0L), coalesce(treat_1b,0L))),
              treat_year = case_when(coalesce(treat_1a,0L)==1L ~ 2009L,
                                     coalesce(treat_1b,0L)==1L ~ 2013L,
                                     TRUE ~ NA_integer_))
} else if (scope_choice == "1A") {
  treat_df <- read_treat_one(paths$treated_1a, buffer_choice, sheet = pick_sheet(paths$treated_1a,"1a")) |>
    mutate(treat_year = ifelse(treat==1, 2009L, NA_integer_))
} else if (scope_choice == "1B") {
  treat_df <- read_treat_one(paths$treated_1b, buffer_choice, sheet = pick_sheet(paths$treated_1b,"1b")) |>
    mutate(treat_year = ifelse(treat==1, 2013L, NA_integer_))
} else stop("scope_choice must be one of: '1A1B','1A','1B'")

# Attach treatment & post; keep treated/control only
Panel <- Panel |>
  mutate(HZONE = to_int(HZONE)) |>
  left_join(treat_df, by="HZONE") |>
  mutate(post = as.integer(!is.na(treat_year) & SURVEY_YEAR >= treat_year)) |>
  filter(treat %in% c(0,1))

readr::write_csv(Panel, file.path(odir, sprintf("panel_after_treatment_%s_%s.csv", scope_choice, buffer_choice)))

# ---- 6) Outcomes list (for modeling & tables) -----------------------------
# Required by you:
# TRIPS_PER_DAY, PT_MAIN_SHARE (proxy for Primary Trip Mode), TRIPDIST_TOT,
# AVG_TRAVEL_TIME, TRAVEL_TIME_PER_MILE, AVG_WORK_TIME,
# MAIN_PURP_WORK_SHARE (proxy for Main Trip Purpose), FREQ_PER_PERSON, UNEMPLOYED_LF
outcomes <- c("TRIPS_PER_DAY","PT_MAIN_SHARE","TRIPDIST_TOT","AVG_TRAVEL_TIME",
              "TRAVEL_TIME_PER_MILE","AVG_WORK_TIME","MAIN_PURP_WORK_SHARE",
              "FREQ_PER_PERSON","UNEMPLOYED_LF")

Analysis_df <- Panel |> select(SURVEY_YEAR, HZONE, treat, treat_year, post, all_of(outcomes))

# ===== Load & attach 2019 weights (just before build Panel) ================
library(readxl); library(janitor); library(stringr); library(dplyr)

# A) Point to the single Excel that has both 2019 PERSON and HOUSE weights
#    (It can be anywhere; it does NOT need to be in data_dir.)
weights_2019_file <- "/absolute/path/to/2019_weights.xlsx"   # <-- EDIT THIS

stopifnot(file.exists(weights_2019_file))

# B) Auto-detect sheets: look for "person" and "house" in sheet names
sheets_all <- readxl::excel_sheets(weights_2019_file)
sheet_person <- sheets_all[str_detect(tolower(sheets_all), "person|people|p_|ind")][1]
sheet_house  <- sheets_all[str_detect(tolower(sheets_all), "house|hh|household")][1]
if (is.na(sheet_person) || is.na(sheet_house)) {
  stop("Could not auto-detect person/house sheets in: ", weights_2019_file,
       "\nSheets found: ", paste(sheets_all, collapse=", "))
}

# C) Read the two sheets
w19_person_raw <- readxl::read_excel(weights_2019_file, sheet = sheet_person) |> janitor::clean_names()
w19_house_raw  <- readxl::read_excel(weights_2019_file, sheet = sheet_house)  |> janitor::clean_names()

# D) Standardize keys + weight columns (robust to different names)
#    We try common variants and fall back sensibly.

# --- PERSON weights ---
p_id_col <- names(w19_person_raw)[str_detect(names(w19_person_raw), "^p_id$|person_id|pindex|pnumber")]
p_wt_col <- names(w19_person_raw)[str_detect(names(w19_person_raw), "weight|wgt|final_wt|expan")]
if (!length(p_id_col) || !length(p_wt_col)) {
  stop("2019 PERSON weights: couldn't find P_ID and/or weight columns.\nCols: ",
       paste(names(w19_person_raw), collapse=", "))
}
w19_person <- w19_person_raw |>
  transmute(
    P_ID = as.integer(.data[[p_id_col[1]]]),
    WT_PERSON_2019 = as.numeric(.data[[p_wt_col[1]]])
  )

# --- HOUSE weights ---
hh_id_col <- names(w19_house_raw)[str_detect(names(w19_house_raw), "^hh_id$|household_id|q_id|qno")]
hh_wt_col <- names(w19_house_raw)[str_detect(names(w19_house_raw), "weight|wgt|final_wt|expan")]
if (!length(hh_id_col) || !length(hh_wt_col)) {
  stop("2019 HOUSE weights: couldn't find HH_ID and/or weight columns.\nCols: ",
       paste(names(w19_house_raw), collapse=", "))
}
w19_house <- w19_house_raw |>
  transmute(
    HH_ID = as.integer(.data[[hh_id_col[1]]]),
    WT_HH_2019 = as.numeric(.data[[hh_wt_col[1]]])
  )

# E) Attach weights to the CLEANED 2019 data (P19, H19 must already be harmonized)
#    (These joins only affect 2019 rows; earlier years remain unchanged.)
P19 <- P19 |>
  mutate(P_ID = as.integer(P_ID)) |>
  left_join(w19_person, by = "P_ID")

H19 <- H19 |>
  mutate(HH_ID = as.integer(HH_ID)) |>
  left_join(w19_house, by = "HH_ID")

# F) (Optional) sanity prints
cat("2019 weights attached: ",
    sum(!is.na(P19$WT_PERSON_2019)), "person weights; ",
    sum(!is.na(H19$WT_HH_2019)), "house weights.\n", sep="")

# G) When you build Person_all / House_all, these weight cols will flow into Panel.
#    If your code already created Person_all / House_all above, re-create them now
#    so they include the weight columns. Otherwise, if you create them below, you're set.
#    Example (adjust if your script names differ):

# Person_all <- bind_rows(P00, P14, P19)  # <- keep your existing line
# House_all  <- bind_rows(H00, H14, H19)  # <- keep your existing line

# H) Later, after you merge into Panel, define unified weight columns for modeling:
#    (Do this once, right after Panel is created.)
# Panel <- Panel |>
#   mutate(
#     WT_PERSON = dplyr::case_when(
#       SURVEY_YEAR == 2019 ~ WT_PERSON_2019,
#       TRUE ~ NA_real_    # (If you have weights for 2000/2014, plug them here)
#     ),
#     WT_HH = dplyr::case_when(
#       SURVEY_YEAR == 2019 ~ WT_HH_2019,
#       TRUE ~ NA_real_
#     )
#   )
#
# In your fixest models, you can then use: weights = ~ WT_PERSON (or WT_HH)

# ---- 7) Summary tables ----------------------------------------------------
desc <- Analysis_df |>
  group_by(SURVEY_YEAR, treat) |>
  summarize(
    n = n(),
    mean_trips = mean(TRIPS_PER_DAY, na.rm=TRUE),
    pt_main_share = mean(PT_MAIN_SHARE, na.rm=TRUE),
    mean_travel_time = mean(AVG_TRAVEL_TIME, na.rm=TRUE),
    mean_work_time = mean(AVG_WORK_TIME, na.rm=TRUE),
    mean_dist = mean(TRIPDIST_TOT, na.rm=TRUE),
    share_unemp_lf = mean(UNEMPLOYED_LF, na.rm=TRUE),
    .groups="drop"
  )
readr::write_csv(desc, file.path(odir, sprintf("summary_by_year_treat_%s_%s.csv", scope_choice, buffer_choice)))


# ---- 8) Staggered DiD (Sun & Abraham) ------------------------------------
Panel_fit <- Analysis_df |>
  filter(!is.na(treat_year)) |>
  mutate(year = factor(SURVEY_YEAR), HZONE = factor(HZONE))

run_es <- function(y) {
  frm <- as.formula(paste0(y, " ~ sunab(treat_year, as.integer(as.character(year))) | HZONE + year"))
  feols(frm, data=Panel_fit, cluster=~HZONE)
}
mods <- lapply(outcomes, run_es); names(mods) <- outcomes

modelsummary::msummary(
  mods, stars=TRUE, gof_omit='IC|Log|Adj|Within|Pseudo',
  output = file.path(odir, sprintf("staggered_DiD_%s_%s.html", scope_choice, buffer_choice))
)

# Optional: one event-study plot (Avg Travel Time)
png(file.path(odir, sprintf("event_study_%s_%s_AVG_TRAVEL_TIME.png", scope_choice, buffer_choice)), width=900, height=520, res=120)
iplot(mods[["AVG_TRAVEL_TIME"]], ref.line=0, xlab="Years since treatment", ylab="Effect on Avg Travel Time (mins)")
dev.off()

cat("\n✅ Done. Outputs in:", odir, "\n")

