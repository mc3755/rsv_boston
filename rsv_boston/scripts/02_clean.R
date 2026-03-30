# =============================================================================
# Title:       02_clean.R
# Author:      Tahir Arif, MPH
# Date:        March 2026
# Description: Clean raw NSSP RSV ED visit data. Standardize columns,
#              parse dates, derive MMWR week and season variables, and
#              classify pediatric/adult age groups (<5, 5-17, 18-64, 65+).
# =============================================================================

# ---- Libraries ---------------------------------------------------------------
library(tidyverse)   # dplyr, tidyr, readr, stringr
library(janitor)     # clean_names(), remove_empty()
library(lubridate)   # Date parsing
library(MMWRweek)    # MMWR epidemiologic week
library(here)        # Relative paths

# ---- Paths -------------------------------------------------------------------
RAW_PATH   <- here("data", "raw_rsv_nssp.csv")
CLEAN_PATH <- here("data", "clean_rsv.csv")

# ---- Load raw data -----------------------------------------------------------
message("Loading raw RSV data from: ", RAW_PATH)
rsv_raw <- read_csv(RAW_PATH, show_col_types = FALSE)
message("Raw dimensions: ", nrow(rsv_raw), " rows × ", ncol(rsv_raw), " cols")

# ---- Step 1: Standardize column names ----------------------------------------
rsv <- rsv_raw |>
  clean_names() |>
  remove_empty(which = c("rows", "cols"))

message("Standardized column names:")
print(names(rsv))

# ---- Step 2: Parse dates -----------------------------------------------------
rsv <- rsv |>
  mutate(
    week_end_date   = as_date(parse_date_time(week_end_date,
                               orders = c("ymdHMS", "ymd", "mdy"))),
    week_start_date = week_end_date - days(6),
    year            = year(week_end_date),
    month_num       = month(week_end_date),
    month           = month(week_end_date, label = TRUE, abbr = TRUE)
  )

# ---- Step 3: MMWR epidemiologic week -----------------------------------------
mmwr_vars <- MMWRweek(rsv$week_end_date)
rsv <- rsv |>
  mutate(
    mmwr_week = mmwr_vars$MMWRweek,
    mmwr_year = mmwr_vars$MMWRyear
  )

# ---- Step 4: Classify RSV respiratory seasons --------------------------------
# RSV season: MMWR weeks 40–20 (approximately Oct–May).
# RSV peaks earlier than influenza: typically weeks 45–52 (Nov–Dec).

classify_rsv_season <- function(mmwr_week, mmwr_year) {
  case_when(
    mmwr_week >= 40 ~ paste0(mmwr_year,     "-", substr(mmwr_year + 1, 3, 4)),
    mmwr_week <= 20 ~ paste0(mmwr_year - 1, "-", substr(mmwr_year, 3, 4)),
    TRUE            ~ "Off-season"
  )
}

rsv <- rsv |>
  mutate(
    season      = classify_rsv_season(mmwr_week, mmwr_year),
    peak_season = season != "Off-season",
    # RSV "peak window": historically weeks 44-52 and 1-8 (Nov-Feb)
    peak_window = mmwr_week >= 44 | mmwr_week <= 8
  )

message("Seasons identified: ", paste(sort(unique(rsv$season)), collapse = ", "))

# ---- Step 5: Coerce numeric columns ------------------------------------------
# Identify % and count columns
num_cols <- names(rsv)[str_detect(names(rsv), "percent|pct|rate|count|visits|num")]

rsv <- rsv |>
  mutate(across(all_of(num_cols), ~ suppressWarnings(as.numeric(.x))))

# Identify primary percentage column
pct_col <- names(rsv)[str_detect(names(rsv), "pct_ed_visits|percent_visits|percent|visits_pct")][1]
message("Primary percentage column: ", pct_col)

# ---- Step 6: Classify pediatric/adult age groups -----------------------------
# RSV burden varies dramatically by age. We map NSSP age labels to 4 categories:
#   <5 years      (highest RSV hospitalization risk — bronchiolitis)
#   5–17 years    (school-age, community vector)
#   18–64 years   (working-age adults, mild illness)
#   65+ years     (high-risk adults, RSV vaccine target group)

classify_age_group <- function(age_label) {
  label_lower <- tolower(age_label)
  case_when(
    # Under 5
    str_detect(label_lower, "<5|under 5|0.?4|0 to 4|0-4|infant|toddler|birth.?4")     ~ "<5 years",
    # School age
    str_detect(label_lower, "5.?17|5 to 17|school|child")                               ~ "5–17 years",
    # Working age adult
    str_detect(label_lower, "18.?64|18 to 64|adult|working")                            ~ "18–64 years",
    # Older adult
    str_detect(label_lower, "65|older|senior|elder|\\+")                                ~ "65+ years",
    # All ages / total
    str_detect(label_lower, "all|overall|total")                                         ~ "All Ages",
    TRUE                                                                                  ~ "Other / Unknown"
  )
}

if ("age_group" %in% names(rsv)) {
  rsv <- rsv |>
    mutate(
      age_group_std = classify_age_group(age_group),
      # Pediatric flag: <5 or 5-17
      pediatric     = age_group_std %in% c("<5 years", "5–17 years"),
      # High-risk flag: <5 or 65+
      high_risk_age = age_group_std %in% c("<5 years", "65+ years")
    )
  
  message("Age group classification summary:")
  print(table(rsv$age_group_std, useNA = "ifany"))
  message("Pediatric records: ", sum(rsv$pediatric, na.rm = TRUE))
  message("High-risk age records: ", sum(rsv$high_risk_age, na.rm = TRUE))
  
} else {
  message("No age_group column found — adding NA placeholder columns")
  rsv <- rsv |>
    mutate(
      age_group_std = NA_character_,
      pediatric     = NA,
      high_risk_age = NA
    )
}

# ---- Step 7: Handle missing values -------------------------------------------
missing_report <- rsv |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "col", values_to = "n_missing") |>
  filter(n_missing > 0) |>
  arrange(desc(n_missing))

message("\nMissingness:")
print(missing_report)

# Drop rows with no date (cannot be placed in time)
rsv <- rsv |> filter(!is.na(week_end_date))

# ---- Step 8: Reorder columns for readability ---------------------------------
priority_cols <- c(
  "week_end_date", "week_start_date", "year", "month", "month_num",
  "mmwr_week", "mmwr_year", "season", "peak_season", "peak_window",
  "geography", "pathogen",
  "age_group_std", "pediatric", "high_risk_age"
)

# Keep only columns that exist
priority_exists <- intersect(priority_cols, names(rsv))
other_cols      <- setdiff(names(rsv), priority_exists)

rsv <- rsv |> select(all_of(priority_exists), all_of(other_cols))

# ---- Save -------------------------------------------------------------------
write_csv(rsv, CLEAN_PATH)
message("\nClean RSV data saved to: ", CLEAN_PATH)
message("Rows: ", nrow(rsv), "  Cols: ", ncol(rsv))
