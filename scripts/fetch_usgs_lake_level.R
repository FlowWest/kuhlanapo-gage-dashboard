# scripts/fetch_usgs_lake_level.R
#
# Pulls continuous (15-min) lake level for USGS 11450000 Clear Lake at Lakeport
# from the USGS Water Data OGC API:
# https://api.waterdata.usgs.gov/ogcapi/v1/collections/continuous
library(httr2)
library(jsonlite)
library(dplyr)
library(lubridate)

## CONFIG =====================================================================

SITE_ID      <- "11450000"
PARAMETER_CD <- "62615"   # lake level, ft NAVD88
BASE_URL     <- "https://api.waterdata.usgs.gov/ogcapi/v1/collections/continuous/items"

# Optional; raises the API rate limit (https://api.waterdata.usgs.gov/signup/)
API_KEY <- Sys.getenv("API_USGS_PAT")

DATA_TZ <- "America/Los_Angeles"

MIN_START_DATE <- as.POSIXct(
  "2025-12-05 00:00:00",
  tz = DATA_TZ
)

DATA_FILE <- "data/usgs_lake_level_11450000.rds"

OVERLAP_DAYS <- 2

PAGE_LIMIT <- 10000

## API HELPERS ================================================================

usgs_request <- function(url) {
  req <- request(url) |>
    req_retry(max_tries = 3) |>
    req_timeout(60)
  if (nzchar(API_KEY)) req <- req |> req_headers(`X-Api-Key` = API_KEY)
  req
}

# Follows the cursor-based `next` links until the result set is exhausted
fetch_continuous <- function(start_ts, end_ts) {
  fmt <- function(x) format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  req <- usgs_request(BASE_URL) |>
    req_url_query(
      f                      = "json",
      monitoring_location_id = paste0("USGS-", SITE_ID),
      parameter_code         = PARAMETER_CD,
      time                   = paste0(fmt(start_ts), "/", fmt(end_ts)),
      properties             = "parameter_code,time,value,approval_status",
      skipGeometry           = "true",
      limit                  = PAGE_LIMIT
    )

  pages <- list()
  repeat {
    body <- req |> req_perform() |> resp_body_string() |> fromJSON()
    if (length(body$features) > 0) {
      pages[[length(pages) + 1]] <- as_tibble(body$features$properties)
    }
    next_url <- body$links$href[body$links$rel == "next"]
    if (length(next_url) == 0 || length(body$features) == 0) break
    req <- usgs_request(next_url)
  }

  bind_rows(pages)
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
if (!is.null(existing_data) && nrow(existing_data) > 0) {
  existing_data <- existing_data |>
    mutate(timestamp = with_tz(timestamp, DATA_TZ))
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

raw <- fetch_continuous(start_ts, end_ts)

if (nrow(raw) == 0) {
  message("No new data returned from USGS")
  quit(status = 0)
}

new_data <- raw |>
  transmute(name      = paste("USGS", SITE_ID),
            timestamp = with_tz(ymd_hms(time), DATA_TZ),
            parm_name = "Lake Level",
            value     = as.numeric(value))

message("Fetched ", nrow(new_data), " records")

## OPTIONAL SANITY CHECK =======================================================

if (min(new_data$timestamp, na.rm = TRUE) > start_ts + hours(1)) {
  warning("USGS response starts later than requested start time")
}

## MERGE + DEDUPLICATE =========================================================

# new rows win, so provisional values get replaced as USGS revises/approves them
combined <- bind_rows(new_data, existing_data) |>
  distinct(name, timestamp, parm_name, .keep_all = TRUE) |>
  arrange(timestamp)

## SAVE =======================================================================

dir.create("data", showWarnings = FALSE)
saveRDS(combined, DATA_FILE)

message("Saved ", nrow(combined), " total lake level records")
