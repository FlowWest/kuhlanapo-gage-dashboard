# scripts/fetch_hydrovu.R
library(hydrovur)
library(dplyr)
library(lubridate)

source(here::here("global.R"))

## CONFIG =====================================================================

# Credentials are read from HYDROVU_CLIENT_ID / HYDROVU_CLIENT_SECRET
stopifnot(nzchar(Sys.getenv("HYDROVU_CLIENT_ID")),
          nzchar(Sys.getenv("HYDROVU_CLIENT_SECRET")))

DATA_FILE <- "data/gage_data.rds"

# Safety overlap when appending (prevents boundary misses)
OVERLAP_DAYS <- 2

# Lookback for locations with no existing data
FRESH_START_DAYS <- 14

START_FRESH <- FALSE

UPDATE_PIEZOMETERS <- FALSE

## LOAD EXISTING DATA ==========================================================

existing_data <- if (file.exists(DATA_FILE) && !START_FRESH) {
  message("Loading existing dataset.")
  readRDS(DATA_FILE)
} else {
  message("No existing dataset loaded.")
  NULL
}

## FETCH NEW DATA ==============================================================

con <- hv_connect()

locs <- hv_locations(con) |>
  filter(name %in% if (UPDATE_PIEZOMETERS) sensors$name else gages$name) |>
  select(location_id, name)

windows <- hv_compute_windows(existing_data, locs,
                              overlap_days     = OVERLAP_DAYS,
                              fresh_start_days = FRESH_START_DAYS,
                              start_fresh      = is.null(existing_data))

new_data <- hv_readings(con,
                        location_ids = windows$location_id,
                        start        = windows$start_ts,
                        end          = windows$end_ts) |>
  inner_join(locs, by = join_by(location_id)) |>
  inner_join(sensors |> select(name, code, site, type, category), by = join_by(name)) |>
  relocate(name, code, site, type, category, .after = location_id)

message("Loaded ", nrow(new_data), " new records.")

## APPEND + SAVE ===============================================================

combined <- hv_append(existing_data, new_data) |>
  arrange(name, timestamp)

dir.create("data", showWarnings = FALSE)
saveRDS(combined, DATA_FILE)

message("Saved ", nrow(combined), " total records.")
