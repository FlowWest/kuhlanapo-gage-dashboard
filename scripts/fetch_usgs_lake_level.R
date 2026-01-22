library(httr)
library(dplyr)
library(lubridate)
library(stringr)
library(inldata)

## CONFIG =====================================================================

SITE_ID      <- "11450000"
PARAMETER_CD <- "62615"   # lake level
BASE_URL     <- "https://nwis.waterservices.usgs.gov/nwis/iv/"

DATA_TZ <- "America/Los_Angeles"

MIN_START_DATE <- as.POSIXct(
  "2025-12-05 00:00:00",
  tz = DATA_TZ
)

DATA_FILE <- "data/usgs_lake_level_11450000.rds"

OVERLAP_DAYS <- 2

## URL BUILDER ================================================================

# install.packages("inldata", dependencies = TRUE)
read_usgs_rdb <- function(file, tz = "UTC") {
  raw_df <- inldata::read_rdb(file)
  col_types <- attributes(raw_df)$column_definitions |> str_sub(-1,-1) 
  raw_df |>
    mutate(across(which(col_types == "n"), as.numeric)) |>
    mutate(across(
      which(col_types == "d"),
      \(x) force_tz(as.POSIXct(x), tz)
    ))
}

build_usgs_url <- function(start_dt, end_dt) {
  modify_url(
    BASE_URL,
    query = list(
      sites       = SITE_ID,
      agencyCd    = "USGS",
      startDT     = format(start_dt, "%Y-%m-%dT%H:%M:%OS%z"),
      endDT       = format(end_dt,   "%Y-%m-%dT%H:%M:%OS%z"),
      parameterCd = PARAMETER_CD,
      format      = "rdb"
    )
  )
}

## LOAD EXISTING CACHE =========================================================

existing_data <- if (file.exists(DATA_FILE)) {
  message("Loading existing lake level cache")
  readRDS(DATA_FILE)
} else {
  message("No existing cache found")
  NULL
}

# Normalize cached datetimes to canonical TZ
if (!is.null(existing_data) && "datetime" %in% names(existing_data)) {
  existing_data <- existing_data |>
    mutate(datetime = with_tz(datetime, DATA_TZ))
}

## DETERMINE FETCH WINDOW ======================================================

end_ts <- with_tz(Sys.time(), DATA_TZ)

start_ts <- if (!is.null(existing_data) && nrow(existing_data) > 0) {
  candidate <- max(existing_data$timestamp, na.rm = TRUE) - days(OVERLAP_DAYS)
  max(candidate, MIN_START_DATE)
} else {
  MIN_START_DATE
}

message("Fetching USGS lake level from ", start_ts, " to ", end_ts)

## FETCH ======================================================================

url <- build_usgs_url(start_ts, end_ts)

tmp <- tempfile(fileext = ".rdb")
res <- GET(url, write_disk(tmp, overwrite = TRUE), timeout(60))
stop_for_status(res)

new_data <- read_usgs_rdb(tmp, tz = DATA_TZ)
unlink(tmp)

if (nrow(new_data) == 0) {
  message("No new data returned from USGS")
  quit(status = 0)
}

# Normalize fetched datetimes to canonical TZ
if ("datetime" %in% names(new_data)) {
  new_data <- new_data |>
    mutate(datetime = with_tz(datetime, DATA_TZ))
}

# normalize 
new_data <- new_data |> 
  as_tibble() |>
  transmute(name = "USGS 11450000",
            timestamp = datetime,
            parm_name = "Lake Level",
            value = `346353_62615`)

## OPTIONAL SANITY CHECK =======================================================

if (min(new_data$timestamp, na.rm = TRUE) > start_ts + hours(1)) {
  warning("USGS response starts later than requested startDT")
}

## MERGE + DEDUPLICATE =========================================================

combined <- bind_rows(existing_data, new_data) |>
  distinct(name, timestamp, parm_name, .keep_all = TRUE) |>
  arrange(timestamp)

## SAVE =======================================================================

dir.create("data", showWarnings = FALSE)
saveRDS(combined, DATA_FILE)

message("Saved ", nrow(combined), " total lake level records")
