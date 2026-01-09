# =========================================================
# GENDER VARIABLE
#HTS 2000/2014/2019
# =========================================================

# install.packages(c("readxl","writexl","dplyr","stringr","forcats","ggplot2","janitor","e1071"))
library(readxl); library(writexl); library(dplyr); library(stringr)
library(forcats); library(ggplot2); library(janitor); library(e1071)

# directory that holds the files
data_dir <- "/Users/Pee/Documents/UMN Phd/Dissertation/Thesis_Data_Analysis/Analysis/Quant Data Analysis/Raw Data/HTS_surveys"

find_first <- function(patterns, root = ".") {
  pat <- paste0("(", paste(patterns, collapse = "|"), ")")
  hits <- list.files(root, pattern = pat, recursive = TRUE, full.names = TRUE)
  if (length(hits)) hits[1] else NA_character_
}

# ---------- paths  ----------
paths <- list(
  `2000` = list(
    house  = find_first_year(c("2000.*house\\.(xls|xlsx)$","\\bhouse\\.xls$","\\bhouse\\.xlsx$"), root = data_dir),
    person = find_first_year(c("2000.*person\\.(xls|xlsx)$","\\bperson\\.xls$","\\bperson\\.xlsx$"), root = data_dir),
    trip   = find_first_year(c("2000.*trip\\.(xls|xlsx)$","\\btrip\\.xls$","\\btrip\\.xlsx$"), root = data_dir)
  ),
  `2014` = list(
    house  = find_first_year(c("2014.*house\\.(xls|xlsx)$","cojhousefile\\.xls","coj_house.*\\.xlsx"), root = data_dir),
    person = find_first_year(c("2014.*person\\.(xls|xlsx)$","cojpersonfile\\.xlsx","coj_person.*\\.xlsx"), root = data_dir),
    trip   = find_first_year(c("2014.*trip\\.(xls|xlsx)$","cojtripfile\\.xlsx","coj_trip.*\\.xlsx"), root = data_dir),
    att    = find_first_year(c("2014.*attitude.*\\.(xls|xlsx)$","attitudefile\\.xlsx"), root = data_dir)
  ),
  `2019` = list(
    house  = find_first(c("2019.*house.*\\.(xls|xlsx)$","household_data\\.xlsx"), root = data_dir),
    person = find_first(c("2019.*person.*\\.(xls|xlsx)$","person_data\\.xlsx"), root = data_dir),
    imputed_persons = find_first(c("imputed_persons.*\\.(xls|xlsx)$"), root = data_dir)
  )
)

for (yr in names(paths)) {
  message("Processing ", yr, " …")
  f_person <- paths[[yr]]$person
  
  if (is.na(f_person) || !file.exists(f_person)) {
    stop("No PERSON file found for year ", yr, " (got: ", f_person, ").")
  }
}

# ---------- helpers ----------
possible_gender_cols <- function(df) {
  # search headers likely to contain gender info
  nms <- names(df)
  cand <- nms[ str_detect(tolower(nms), "\\bsex\\b|\\bgender\\b|^p_sex$|^psex$|^sex_cd$") ]
  # occasionally labels are swapped in specs; keep age if it only has 1/2/NA
  if ("age" %in% nms) cand <- unique(c(cand, "age"))
  if ("GENDER" %in% nms) cand <- unique(c(cand, "GENDER"))
  cand
}

to_gender_code <- function(x) {
  # Coerce to character for normalization
  y <- as.character(x)
  
  # Trim and lower
  y <- str_squish(str_to_lower(y))
  
  # Map common text
  y <- case_when(
    y %in% c("1","m","male","man","boy") ~ "1",
    y %in% c("2","f","female","woman","girl") ~ "2",
    TRUE ~ y
  )
  
  # Treat special missing / invalids as NA
  y[ y %in% c("-1","-100","-99","-9","888","999","na","n/a","", "refused","dont know","don't know","dk") ] <- NA
  
  # If some sources used 0/1: try to detect binary 0/1 with many zeros/ones + no 2's
  if (all(na.omit(unique(y)) %in% c("0","1"))) {
    # Heuristic: assume 1=Male, 0=Female (flip if female share implausible)
    y <- ifelse(y=="1","1", ifelse(y=="0","2", NA))
  }
  
  # Final numeric
  z <- suppressWarnings(as.integer(y))
  # Drop anything not 1 or 2 to NA
  z[ !(z %in% c(1,2)) ] <- NA_integer_
  z
}

first_good <- function(df, cols) {
  # return the first column among 'cols' whose values look like gender after recode
  for (c in cols) {
    if (!c %in% names(df)) next
    z <- to_gender_code(df[[c]])
    # accept if there is variation and a decent non-missing share
    if (sum(!is.na(z)) > 0 && length(unique(na.omit(z))) <= 2) return(c)
  }
  NA_character_
}

# Weighted stats (works even if weights missing)
w_mean <- function(x, w=NULL) {
  if (is.null(w)) return(mean(x, na.rm=TRUE))
  sum(w * x, na.rm=TRUE) / sum(w[!is.na(x)], na.rm=TRUE)
}

# ---------- main per-year processing ----------
all_summaries <- list()
all_year_props <- list()

for (yr in names(paths)) {
  message("Processing ", yr, " …")
  f_person <- paths[[yr]]$person
  stopifnot(file.exists(f_person))
  
  # read (first sheet by default)
  df <- readxl::read_excel(f_person)
  df <- janitor::clean_names(df)
  
  # candidate weight columns (year-specific)
  weight_col <- c("pp_weight","person_weight","person_wt","pweight","weight")[
    c("pp_weight","person_weight","person_wt","pweight","weight") %in% names(df)
  ]
  wt <- if (length(weight_col)) df[[weight_col[1]]] else NULL
  
  # choose raw gender column by name+content
  cand <- possible_gender_cols(df)
  raw_col <- first_good(df, cand)
  if (is.na(raw_col)) {
    stop(paste0("No suitable gender column found in ", f_person,
                ". Checked: ", paste(cand, collapse=", ")))
  }
  
  # harmonise
  df <- df %>%
    mutate(
      GENDER = to_gender_code(.data[[raw_col]]),
      GENDER_label = factor(GENDER, levels=c(1,2), labels=c("Male","Female"))
    )
  
  # keep keys if present
  key_cols <- intersect(c("q_id","pnumber","person_id","pid","household_id","hhid","tz","tznum"), names(df))
  
  # ---------- descriptives ----------
  # counts & proportions
  tab <- df %>%
    count(GENDER_label, wt = if (is.null(wt)) NULL else wt, name = "weighted_n") %>%
    mutate(prop = weighted_n / sum(weighted_n, na.rm=TRUE),
           year = as.integer(yr))
  
  all_year_props[[yr]] <- tab
  
  # missingness
  miss_rate <- mean(is.na(df$GENDER))
  
  # skewness (note: not meaningful for nominal; here computed on {1,2} for completeness)
  sk <- tryCatch(e1071::skewness(df$GENDER, na.rm=TRUE, type=2), error=function(e) NA_real_)
  
  # treated/control split if TREATED is available (0/1 or FALSE/TRUE)
  by_tc <- NULL
  if ("treated" %in% names(df) || "TREATED" %in% names(df)) {
    tcol <- if ("treated" %in% names(df)) "treated" else "TREATED"
    by_tc <- df %>%
      mutate(TREAT = as.integer(!!sym(tcol) %in% c(1,TRUE))) %>%
      group_by(TREAT, GENDER_label) %>%
      summarise(weighted_n = if (is.null(wt)) n() else sum(wt, na.rm=TRUE), .groups="drop") %>%
      group_by(TREAT) %>%
      mutate(prop = weighted_n / sum(weighted_n, na.rm=TRUE), year = as.integer(yr)) %>%
      ungroup()
  }
  
  # chi-square test treated vs control (if present and both groups exist)
  p_tc <- NA_real_
  if (!is.null(by_tc)) {
    wide <- by_tc %>% select(TREAT, GENDER_label, weighted_n) %>% tidyr::pivot_wider(names_from=GENDER_label, values_from=weighted_n, values_fill=0)
    if (nrow(wide) == 2) {
      p_tc <- suppressWarnings(chisq.test(as.matrix(wide[, -1]))$p.value)
    }
  }
  
  # save cleaned per-year person with harmonised GENDER
  out_person <- file.path("02_clean", paste0(yr, "_person_gender.csv"))
  write.csv(df %>% select(all_of(key_cols), GENDER, GENDER_label, all_of(weight_col)), out_person, row.names = FALSE)
  
  # collect summary row
  all_summaries[[yr]] <- tibble::tibble(
    year = as.integer(yr),
    file = f_person,
    raw_gender_col = raw_col,
    n = nrow(df),
    missing_rate = miss_rate,
    skewness_on_codes = sk,
    p_treated_vs_control = p_tc
  )
  
  # ---------- plots ----------
  # by-year proportions
  p1 <- ggplot(tab, aes(x = factor(year), y = prop, fill = GENDER_label)) +
    geom_col(position = "fill") +
    geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
              position = position_fill(vjust=0.5), size = 3) +
    labs(x = "Year", y = "Share", fill = "Gender",
         title = "Gender composition by year",
         subtitle = "Harmonised: 1=Male, 2=Female; NA=missing") +
    theme_minimal()
  ggsave(filename = file.path("06_checks", paste0("gender_by_year_", yr, ".png")),
         plot = p1, width = 7, height = 5, dpi = 150)
  
  # treated/control if available
  if (!is.null(by_tc)) {
    p2 <- ggplot(by_tc, aes(x = factor(TREAT, labels=c("Control","Treated")), y = prop, fill = GENDER_label)) +
      geom_col(position = "fill") +
      facet_wrap(~year) +
      geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
                position = position_fill(vjust=0.5), size = 3) +
      labs(x = NULL, y = "Share", fill = "Gender",
           title = "Gender by treated/control (if available)") +
      theme_minimal()
    ggsave(filename = file.path("06_checks", paste0("gender_treated_control_", yr, ".png")),
           plot = p2, width = 7, height = 5, dpi = 150)
  }
}

# ---------- consolidate & export summaries ----------
gender_summary <- bind_rows(all_summaries)
write.csv(gender_summary, file.path("06_checks","gender_summary_overview.csv"), row.names = FALSE)

gender_props <- bind_rows(all_year_props)
write.csv(gender_props, file.path("06_checks","gender_props_by_year.csv"), row.names = FALSE)

# ---------- year-over-year % change table ----------
if (length(unique(gender_props$year)) > 1) {
  yoy <- gender_props %>%
    arrange(GENDER_label, year) %>%
    group_by(GENDER_label) %>%
    mutate(prop_yoy_change_pct = 100 * (prop - dplyr::lag(prop)) / dplyr::lag(prop)) %>%
    ungroup()
  write.csv(yoy, file.path("06_checks","gender_yoy_changes.csv"), row.names = FALSE)
}