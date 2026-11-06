
# EPA PM2.5 Annual Design Values: Monitor Data ----
#
# Source: U.S. Environmental Protection Agency
# Dataset: 2023-2025 PM2.5 Design Values
# Updated: September 16, 2026
#
# Purpose:
# Prepare CONUS monitoring sites with valid 2023-2025
# annual PM2.5 design values for interactive mapping.
#
# Annual PM2.5 NAAQS: 9.0 ug/m3
# Monitoring period: 2023-2025


# 1. Libraries ----

library(readxl)
library(dplyr)
library(stringr)
library(readr)


# 2. Input files ----

input_file <- paste0(
  "data/raw/",
  "2023-2025-pm2.5-design-values-updated-9.16.2026.xlsx"
)


# 3. Read EPA design values ----

dv_raw <- read_excel(
  input_file,
  col_types = "text"
)


# 4. Define CONUS states ----

# State FIPS codes for the contiguous 48 states and DC.
# Excludes Alaska, Hawaii, and U.S. territories.

conus_fips <- c(
  "01", "04", "05", "06", "08", "09", "10", "11",
  "12", "13", "16", "17", "18", "19", "20", "21",
  "22", "23", "24", "25", "26", "27", "28", "29",
  "30", "31", "32", "33", "34", "35", "36", "37",
  "38", "39", "40", "41", "42", "44", "45", "46",
  "47", "48", "49", "50", "51", "53", "54", "55",
  "56"
)


# 5. Prepare monitoring data ----

## Standardize site IDs and values ----

dv_monitors <- dv_raw %>%
  mutate(
    site_id = str_pad(
      str_remove_all(`AQS Site ID`, "[^0-9]"),
      width = 9,
      pad = "0"
    ),
    state_fips = str_sub(site_id, 1, 2),
    latitude = as.numeric(`Site Latitude`),
    longitude = as.numeric(`Site Longitude`),
    dv_2023_2025 = as.numeric(
      `Valid 2023-2025 DV (µg/m3)`
    )
  ) %>%
  
  ## Filter to CONUS and valid design values ----

filter(
  state_fips %in% conus_fips,
  !is.na(dv_2023_2025),
  !is.na(latitude),
  !is.na(longitude)
) %>%
  
  ## Select mapping attributes ----

transmute(
  site_id,
  state = `State Name`,
  county = `County Name`,
  site_name = `Local Site Name`,
  latitude,
  longitude,
  dv_2023_2025,
  exceeds_standard = dv_2023_2025 > 9.0,
  designated_area_2012 =
    `2012 Annual PM2.5 NAAQS Designated Area`
)


# 6. Validate results ----

## Monitor counts ----

cat("CONUS monitors:", nrow(dv_monitors), "\n")

cat(
  "Above 9.0:",
  sum(dv_monitors$exceeds_standard),
  "\n"
)

## Duplicate site IDs ----

cat(
  "Duplicate IDs:",
  anyDuplicated(dv_monitors$site_id),
  "\n"
)

## Missing values ----

cat(
  "Missing coordinates:",
  sum(
    is.na(dv_monitors$latitude) |
      is.na(dv_monitors$longitude)
  ),
  "\n"
)

## Design value summary ----

summary(dv_monitors$dv_2023_2025)


# 7. Export processed data ----

write_csv(
  dv_monitors,
  "data/processed/pm25_monitor_dv_2023_2025.csv"
)
