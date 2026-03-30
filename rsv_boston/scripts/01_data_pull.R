# =============================================================================
# Title:       01_data_pull.R
# Author:      Tahir Arif, MPH
# Date:        March 2026
# Description: Query CDC NSSP BioSense ED visit data for RSV in
#              Massachusetts. Filters by pathogen = "RSV" and
#              geography = "Massachusetts". Saves raw CSV for cleaning.
# =============================================================================

# ---- Libraries ---------------------------------------------------------------
library(httr)        # HTTP GET
library(jsonlite)    # JSON → data frame
library(readr)       # write_csv
library(dplyr)       # Data wrangling
library(here)        # Relative paths

# ---- Configuration -----------------------------------------------------------

# NSSP ED Visit endpoint (Socrata, no API key required for small queries)
NSSP_ENDPOINT <- "https://data.cdc.gov/resource/7xva-uux8.json"

# RSV filter — NSSP may use "RSV" or "Respiratory Syncytial Virus"
# We query for both and combine to be safe
RSV_PARAMS_1 <- list(
  geography = "Massachusetts",
  pathogen  = "RSV",
  "$limit"  = 5000,
  "$order"  = "week_end_date DESC"
)

RSV_PARAMS_2 <- list(
  geography = "Massachusetts",
  pathogen  = "Respiratory Syncytial Virus",
  "$limit"  = 5000,
  "$order"  = "week_end_date DESC"
)

# Output path
RAW_OUTPUT <- here("data", "raw_rsv_nssp.csv")

# ---- Helper: Socrata GET call ------------------------------------------------

#' Query Socrata REST API and return a data frame
#'
#' @param endpoint  Character. API URL (Socrata dataset endpoint).
#' @param params    Named list. Query parameters including filters and $limit.
#' @return Data frame of results, or empty data frame on failure.
pull_socrata <- function(endpoint, params) {
  message("Querying: ", endpoint)
  message("  Params: ", paste(names(params), unlist(params), sep = "=", collapse = " | "))
  
  response <- GET(
    url     = endpoint,
    query   = params,
    add_headers(
      "Accept"      = "application/json",
      "X-App-Token" = Sys.getenv("SOCRATA_TOKEN")   # Empty OK; improves rate limits if set
    ),
    timeout(60)
  )
  
  if (http_error(response)) {
    warning(
      "HTTP error ", status_code(response), " for params: ",
      paste(names(params), unlist(params), sep = "=", collapse = ", ")
    )
    return(data.frame())
  }
  
  raw_text <- content(response, as = "text", encoding = "UTF-8")
  df       <- fromJSON(raw_text, flatten = TRUE)
  message("  Records returned: ", nrow(df))
  return(df)
}

# ---- Pull RSV data (both pathogen label variants) ----------------------------
message("=== Pulling RSV ED visit data ===")

rsv_1 <- pull_socrata(NSSP_ENDPOINT, RSV_PARAMS_1)
rsv_2 <- pull_socrata(NSSP_ENDPOINT, RSV_PARAMS_2)

# Combine results; remove exact duplicates
rsv_raw <- bind_rows(rsv_1, rsv_2) |>
  distinct()

message("\nTotal unique records retrieved: ", nrow(rsv_raw))

# ---- Validate ----------------------------------------------------------------
if (nrow(rsv_raw) == 0) {
  stop(
    "No RSV records found. Possible causes:\n",
    "  1. NSSP uses a different pathogen label (check the data dictionary).\n",
    "  2. The API endpoint has changed.\n",
    "  3. RSV data is only available in certain dataset versions.\n",
    "Action: Visit https://data.cdc.gov/resource/7xva-uux8.json to inspect available values."
  )
}

message("\nColumn names in raw RSV data:")
print(names(rsv_raw))

# Show unique pathogen values retrieved
if ("pathogen" %in% names(rsv_raw)) {
  message("Pathogen values in data: ", paste(unique(rsv_raw$pathogen), collapse = ", "))
}

# Show date range
if ("week_end_date" %in% names(rsv_raw)) {
  message("Date range: ",
          min(rsv_raw$week_end_date, na.rm = TRUE), " to ",
          max(rsv_raw$week_end_date, na.rm = TRUE))
}

# Show age groups if present
if ("age_group" %in% names(rsv_raw)) {
  message("Age groups in data:")
  print(sort(unique(rsv_raw$age_group)))
}

# ---- Save raw data -----------------------------------------------------------
write_csv(rsv_raw, RAW_OUTPUT)
message("\nRaw RSV data saved to: ", RAW_OUTPUT)
message("Rows: ", nrow(rsv_raw), "  Cols: ", ncol(rsv_raw))
message("\nProceed with 02_clean.R")
