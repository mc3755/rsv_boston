# =============================================================================
# Title:       03_analysis.R
# Author:      Tahir Arif, MPH
# Date:        March 2026
# Description: Epidemiologic analysis of RSV ED visits in Massachusetts.
#              Season-over-season comparisons, age-specific ED visit rates,
#              peak timing analysis (overall and by age group), and
#              pediatric burden estimation.
# =============================================================================

# ---- Libraries ---------------------------------------------------------------
library(tidyverse)   # dplyr, tidyr, readr, stringr, purrr
library(janitor)     # tabyl(), adorn_*
library(lubridate)   # Date arithmetic
library(scales)      # Number formatting
library(here)        # Relative paths

# ---- Paths -------------------------------------------------------------------
CLEAN_PATH        <- here("data", "clean_rsv.csv")
OUT_WEEKLY        <- here("output", "table_rsv_weekly.csv")
OUT_SEASON        <- here("output", "table_rsv_season_comparison.csv")
OUT_AGE_SEASON    <- here("output", "table_rsv_age_season.csv")
OUT_PEAK_TIMING   <- here("output", "table_rsv_peak_timing.csv")
OUT_PEDIATRIC     <- here("output", "table_rsv_pediatric_burden.csv")

# ---- Load data ---------------------------------------------------------------
message("Loading cleaned RSV data from: ", CLEAN_PATH)
rsv <- read_csv(CLEAN_PATH, show_col_types = FALSE)

# Identify primary percentage column
pct_col <- names(rsv)[str_detect(names(rsv), "pct_ed_visits|percent_visits|percent|visits_pct")][1]
message("Primary column: ", pct_col)

# Check for age data
has_age <- "age_group_std" %in% names(rsv) && !all(is.na(rsv$age_group_std))
message("Age-stratified data available: ", has_age)

# ---- Analysis 1: Weekly all-ages summary -------------------------------------
# Use all-ages rows where possible; otherwise aggregate across all age groups

weekly_all <- rsv |>
  {
    if (has_age) {
      filter(., age_group_std == "All Ages")
    } else {
      .
    }
  } |>
  group_by(week_end_date, mmwr_week, mmwr_year, season, peak_season, peak_window) |>
  summarise(
    pct_ed_visits = mean(.data[[pct_col]], na.rm = TRUE),
    n_records     = n(),
    .groups       = "drop"
  ) |>
  arrange(week_end_date)

write_csv(weekly_all, OUT_WEEKLY)
message("Weekly summary saved to: ", OUT_WEEKLY, " (", nrow(weekly_all), " rows)")

# ---- Analysis 2: Season-over-season comparison --------------------------------
# For each RSV season, characterize peak, duration, and burden magnitude

season_comparison <- weekly_all |>
  filter(peak_season) |>
  group_by(season) |>
  summarise(
    n_weeks              = n(),
    season_start         = min(week_end_date),
    season_end           = max(week_end_date),
    mean_pct             = round(mean(pct_ed_visits, na.rm = TRUE), 3),
    sd_pct               = round(sd(pct_ed_visits, na.rm = TRUE), 3),
    max_pct              = round(max(pct_ed_visits, na.rm = TRUE), 3),
    # Peak: MMWR week of highest activity
    peak_mmwr_week       = mmwr_week[which.max(pct_ed_visits)],
    peak_week_date       = as.character(week_end_date[which.max(pct_ed_visits)]),
    # Peak timing in weeks since season start (week 40 = week 1)
    peak_season_week     = if_else(
      peak_mmwr_week >= 40,
      peak_mmwr_week - 39L,
      peak_mmwr_week + 13L
    ),
    # Duration of elevated activity (>0.5% of ED visits)
    weeks_elevated_05    = sum(pct_ed_visits >= 0.5, na.rm = TRUE),
    weeks_elevated_10    = sum(pct_ed_visits >= 1.0, na.rm = TRUE),
    .groups              = "drop"
  ) |>
  arrange(season)

message("\nSeason comparison:")
print(season_comparison)

write_csv(season_comparison, OUT_SEASON)
message("Season comparison saved to: ", OUT_SEASON)

# ---- Analysis 3: Age-specific rates by season --------------------------------
if (has_age) {
  
  # Filter out "Other / Unknown" and "All Ages" for age-specific analysis
  age_levels <- c("<5 years", "5–17 years", "18–64 years", "65+ years")
  
  age_season_summary <- rsv |>
    filter(
      peak_season,
      age_group_std %in% age_levels
    ) |>
    group_by(age_group_std, season) |>
    summarise(
      n_weeks       = n_distinct(week_end_date),
      mean_pct      = round(mean(.data[[pct_col]], na.rm = TRUE), 3),
      sd_pct        = round(sd(.data[[pct_col]], na.rm = TRUE), 3),
      max_pct       = round(max(.data[[pct_col]], na.rm = TRUE), 3),
      peak_mmwr_wk  = mmwr_week[which.max(.data[[pct_col]])],
      .groups       = "drop"
    ) |>
    # Order age groups clinically (youngest to oldest)
    mutate(age_group_std = factor(age_group_std, levels = age_levels)) |>
    arrange(age_group_std, season)
  
  message("\nAge-specific season summary:")
  print(age_season_summary)
  
  write_csv(age_season_summary, OUT_AGE_SEASON)
  message("Age-season summary saved to: ", OUT_AGE_SEASON)
  
} else {
  message("Skipping age-specific analysis — no age_group_std column.")
  tibble(note = "Age-stratified data requires age_group variable in NSSP pull.") |>
    write_csv(OUT_AGE_SEASON)
}

# ---- Analysis 4: Peak timing analysis ----------------------------------------
# For each season and age group, identify the MMWR week of peak RSV activity
# and convert to calendar date. This reveals whether peak timing differs
# between pediatric and adult populations.

peak_timing <- rsv |>
  filter(peak_season) |>
  {
    if (has_age && "age_group_std" %in% names(.)) {
      group_by(., season, age_group_std)
    } else {
      group_by(., season)
    }
  } |>
  summarise(
    peak_pct          = round(max(.data[[pct_col]], na.rm = TRUE), 3),
    peak_week_end     = week_end_date[which.max(.data[[pct_col]])],
    peak_mmwr_week    = mmwr_week[which.max(.data[[pct_col]])],
    .groups           = "drop"
  ) |>
  mutate(
    peak_month   = month(peak_week_end, label = TRUE, abbr = FALSE),
    peak_month_n = month(peak_week_end)
  ) |>
  arrange(season)

message("\nPeak timing by season (and age group if available):")
print(peak_timing)

write_csv(peak_timing, OUT_PEAK_TIMING)
message("Peak timing saved to: ", OUT_PEAK_TIMING)

# ---- Analysis 5: Pediatric burden estimation ---------------------------------
if (has_age) {
  
  pediatric_df <- rsv |>
    filter(pediatric == TRUE, peak_season)
  
  pediatric_burden <- pediatric_df |>
    group_by(season, age_group_std) |>
    summarise(
      n_weeks        = n_distinct(week_end_date),
      mean_pct       = round(mean(.data[[pct_col]], na.rm = TRUE), 3),
      max_pct        = round(max(.data[[pct_col]], na.rm = TRUE), 3),
      # Cumulative proxy: sum of weekly percentages (higher = longer/more intense season)
      cumulative_pct = round(sum(.data[[pct_col]], na.rm = TRUE), 2),
      .groups        = "drop"
    )
  
  message("\nPediatric RSV burden by season:")
  print(pediatric_burden)
  
  write_csv(pediatric_burden, OUT_PEDIATRIC)
  message("Pediatric burden saved to: ", OUT_PEDIATRIC)
  
} else {
  message("Skipping pediatric burden analysis — age data not available.")
  tibble(note = "Pediatric burden analysis requires age_group variable.") |>
    write_csv(OUT_PEDIATRIC)
}

# ---- Print key findings ------------------------------------------------------
message("\n=== KEY FINDINGS ===")
message("Seasons analyzed: ", paste(unique(season_comparison$season), collapse = ", "))
message("Overall peak season: ",
        season_comparison$season[which.max(season_comparison$max_pct)],
        " (peak: ", max(season_comparison$max_pct), "%)")
message("Typical peak MMWR week (median): ",
        median(season_comparison$peak_mmwr_week))
