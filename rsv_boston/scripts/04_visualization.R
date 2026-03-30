# =============================================================================
# Title:       04_visualization.R
# Author:      Tahir Arif, MPH
# Date:        March 2026
# Description: ggplot2 visualizations of RSV ED visits in Massachusetts,
#              with an emphasis on pediatric and age-stratified patterns.
#              Produces: (1) age-stratified time series, (2) seasonal overlay
#              by age group, (3) pediatric-focused heatmap.
# =============================================================================

# ---- Libraries ---------------------------------------------------------------
library(tidyverse)   # ggplot2, dplyr, tidyr, readr, forcats
library(lubridate)   # Date arithmetic
library(viridis)     # Color-blind-safe palettes
library(scales)      # Axis label formatting
library(here)        # Relative paths

# ---- Shared plot theme -------------------------------------------------------
theme_epi <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 2, hjust = 0),
      plot.subtitle    = element_text(size = base_size - 1, color = "grey40", hjust = 0),
      plot.caption     = element_text(size = base_size - 3, color = "grey55", hjust = 0),
      axis.title       = element_text(size = base_size - 1, color = "grey30"),
      axis.text        = element_text(size = base_size - 2, color = "grey30"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = base_size - 2),
      legend.text      = element_text(size = base_size - 2),
      strip.text       = element_text(face = "bold", size = base_size - 1),
      plot.margin      = margin(12, 16, 8, 12)
    )
}
theme_set(theme_epi())

# ---- Paths -------------------------------------------------------------------
CLEAN_PATH     <- here("data", "clean_rsv.csv")
WEEKLY_PATH    <- here("output", "table_rsv_weekly.csv")
SEASON_PATH    <- here("output", "table_rsv_season_comparison.csv")
AGE_PATH       <- here("output", "table_rsv_age_season.csv")
PEAK_PATH      <- here("output", "table_rsv_peak_timing.csv")
PEDIATRIC_PATH <- here("output", "table_rsv_pediatric_burden.csv")

OUT_TS         <- here("output", "fig_rsv_age_timeseries.png")
OUT_OVERLAY    <- here("output", "fig_rsv_season_overlay.png")
OUT_HEATMAP    <- here("output", "fig_rsv_pediatric_heatmap.png")

# ---- Load data ---------------------------------------------------------------
message("Loading data...")
rsv          <- read_csv(CLEAN_PATH,     show_col_types = FALSE)
weekly_df    <- read_csv(WEEKLY_PATH,    show_col_types = FALSE)
season_df    <- read_csv(SEASON_PATH,    show_col_types = FALSE)

age_raw      <- read_csv(AGE_PATH,       show_col_types = FALSE)
has_age_data <- !"note" %in% names(age_raw)

pediatric_raw <- read_csv(PEDIATRIC_PATH, show_col_types = FALSE)
has_ped_data  <- !"note" %in% names(pediatric_raw)

# Identify primary percentage column in full dataset
pct_col <- names(rsv)[str_detect(names(rsv), "pct_ed_visits|percent_visits|percent|visits_pct")][1]

# ---- Figure 1: Age-stratified time series ------------------------------------
# Shows four age group lines (or single overall line if age data unavailable)
# Color palette: viridis for accessibility

age_levels <- c("<5 years", "5–17 years", "18–64 years", "65+ years")
age_colors <- c(
  "<5 years"   = "#440154",  # Dark purple  (youngest, highest severity)
  "5–17 years" = "#31688E",  # Blue
  "18–64 years"= "#35B779",  # Green
  "65+ years"  = "#FDE725"   # Yellow       (older adults, also high risk)
)

if (has_age_data && "age_group_std" %in% names(rsv)) {
  
  age_ts_df <- rsv |>
    filter(age_group_std %in% age_levels) |>
    group_by(week_end_date, age_group_std) |>
    summarise(
      pct_ed_visits = mean(.data[[pct_col]], na.rm = TRUE),
      .groups       = "drop"
    ) |>
    mutate(age_group_std = factor(age_group_std, levels = age_levels))
  
  fig1 <- ggplot(age_ts_df, aes(x = week_end_date, y = pct_ed_visits,
                                  color = age_group_std, group = age_group_std)) +
    geom_line(linewidth = 0.9, alpha = 0.85) +
    scale_color_manual(values = age_colors, name = "Age Group") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
    scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, NA)) +
    labs(
      title    = "RSV Disproportionately Burdens the Very Young and Older Adults in Massachusetts",
      subtitle = "Age-stratified RSV-related ED visit %, by week",
      x        = "Week Ending Date",
      y        = "% of ED Visits",
      caption  = "Source: CDC NSSP BioSense Platform | Analysis: Tahir Arif, MPH"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
} else {
  # Single-line fallback with no age stratification
  fig1 <- ggplot(weekly_df, aes(x = week_end_date, y = pct_ed_visits)) +
    geom_line(color = "#440154", linewidth = 0.9) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
    scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, NA)) +
    labs(
      title    = "RSV-Related ED Visits in Massachusetts: Overall Trend",
      subtitle = "Age-stratified data not available — showing all-ages trend",
      x        = "Week Ending Date",
      y        = "% of ED Visits",
      caption  = "Source: CDC NSSP BioSense Platform | Analysis: Tahir Arif, MPH"
    )
}

ggsave(OUT_TS, plot = fig1, width = 12, height = 6, dpi = 300, bg = "white")
message("Saved: ", OUT_TS)

# ---- Figure 2: Seasonal overlay — MMWR season week on x-axis ----------------
# Align seasons to compare year-over-year timing and peak shape
# Use weekly_df (all-ages) for cross-season comparison

weekly_peak <- weekly_df |>
  filter(peak_season) |>
  mutate(
    # Season week 1 = MMWR week 40 (early October)
    season_week  = if_else(mmwr_week >= 40, mmwr_week - 39L, mmwr_week + 13L),
    season_label = factor(season, levels = sort(unique(season)))
  )

n_seasons   <- n_distinct(weekly_peak$season)
pal_overlay <- viridis(n_seasons, option = "C", begin = 0.05, end = 0.95)

fig2 <- ggplot(weekly_peak, aes(x = season_week, y = pct_ed_visits,
                                  color = season_label, group = season_label)) +
  geom_line(linewidth = 1.0, alpha = 0.85) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "grey60", linewidth = 0.5) +  # ~Dec 1
  annotate("text", x = 10.5, y = max(weekly_peak$pct_ed_visits, na.rm = TRUE) * 0.95,
           label = "Dec 1", size = 3.2, color = "grey50", hjust = 0) +
  scale_color_manual(values = pal_overlay, name = "Respiratory Season") +
  scale_x_continuous(
    breaks = c(1, 5, 10, 14, 19, 24, 29),
    labels = c("Wk 40\n(Oct)", "Wk 44\n(Oct)", "Wk 49\n(Dec)",
               "Wk 1\n(Jan)", "Wk 7\n(Feb)", "Wk 12\n(Mar)", "Wk 17\n(Apr)")
  ) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, NA)) +
  labs(
    title    = "RSV Seasonal Peaks Typically Occur in November–December in Massachusetts",
    subtitle = "Season overlay — each line represents one RSV respiratory season (Wk 40–20)",
    x        = "Respiratory Season Week",
    y        = "% of ED Visits",
    caption  = "Source: CDC NSSP BioSense Platform | Analysis: Tahir Arif, MPH"
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave(OUT_OVERLAY, plot = fig2, width = 12, height = 6, dpi = 300, bg = "white")
message("Saved: ", OUT_OVERLAY)

# ---- Figure 3: Pediatric heatmap — season × week × age ---------------------
# Heatmap of RSV % for <5 and 5-17 age groups across seasons
# X-axis: MMWR season week | Y-axis: season | Fill: % ED visits

if (has_age_data && "age_group_std" %in% names(rsv)) {
  
  ped_heatmap_df <- rsv |>
    filter(
      age_group_std %in% c("<5 years", "5–17 years"),
      peak_season
    ) |>
    mutate(
      season_week = if_else(mmwr_week >= 40, mmwr_week - 39L, mmwr_week + 13L),
      age_group_std = factor(age_group_std, levels = c("<5 years", "5–17 years"))
    ) |>
    group_by(season, season_week, age_group_std) |>
    summarise(
      pct_ed_visits = mean(.data[[pct_col]], na.rm = TRUE),
      .groups       = "drop"
    )
  
  fig3 <- ggplot(ped_heatmap_df,
                 aes(x = season_week, y = fct_rev(season), fill = pct_ed_visits)) +
    geom_tile(color = "white", linewidth = 0.25) +
    facet_wrap(~ age_group_std, ncol = 1) +
    scale_fill_viridis_c(
      option   = "B",           # Magma: intuitive warm = high
      name     = "% ED Visits",
      labels   = label_percent(scale = 1),
      na.value = "grey95"
    ) +
    scale_x_continuous(
      breaks = c(1, 5, 10, 14, 19, 24),
      labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar/Apr")
    ) +
    labs(
      title    = "RSV Hits Children Under 5 Earlier and More Intensely Than Older Children",
      subtitle = "Heatmap of RSV-related ED visit % for pediatric age groups, by season and month",
      x        = "Month (Approximate)",
      y        = "Season",
      caption  = "Source: CDC NSSP BioSense Platform | Analysis: Tahir Arif, MPH"
    ) +
    theme(
      axis.text.y      = element_text(size = 8),
      legend.key.width = unit(1.5, "cm")
    )
  
} else {
  # Fallback: single heatmap with overall data
  fallback_df <- weekly_peak |>
    select(season, season_week, pct_ed_visits) |>
    filter(!is.na(pct_ed_visits))
  
  fig3 <- ggplot(fallback_df,
                 aes(x = season_week, y = fct_rev(season), fill = pct_ed_visits)) +
    geom_tile(color = "white", linewidth = 0.25) +
    scale_fill_viridis_c(
      option   = "B",
      name     = "% ED Visits",
      labels   = label_percent(scale = 1),
      na.value = "grey95"
    ) +
    scale_x_continuous(
      breaks = c(1, 5, 10, 14, 19, 24),
      labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar/Apr")
    ) +
    labs(
      title    = "RSV Activity in Massachusetts: Season-by-Week Heatmap",
      subtitle = "All ages (age-stratified data not available in this pull)",
      x        = "Month (Approximate)",
      y        = "Season",
      caption  = "Source: CDC NSSP BioSense Platform | Analysis: Tahir Arif, MPH"
    )
}

ggsave(OUT_HEATMAP, plot = fig3, width = 11, height = 8, dpi = 300, bg = "white")
message("Saved: ", OUT_HEATMAP)

message("\nAll RSV figures saved. Done.")
