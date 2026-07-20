library(tidyverse)
library(lubridate)
library(httr)
library(furrr)

## CONFIG =====================================================================

rds_path <- here::here("data/precip_ts.rds")

precip_sites <- tibble::tribble(
  ~id, ~site,                                ~lat,      ~lon,
  "KPD", "Kuhlanapo at Manning Creek Delta", 39.024924, -122.906493,
  "UMC", "Upper Manning Creek at Hwy 175",   38.995516, -122.934703
)

MIN_START_DATE <- as.POSIXct("2025-12-01 00:00:00", tz = "UTC")
OVERLAP_HOURS  <- 48   # safety overlap when resuming from existing data
INGEST_LAG     <- days(6)  # NLDAS near-real-time files run ~5 days behind Sys.time()

BASE_URL <- "https://hydro1.gesdisc.eosdis.nasa.gov/opendap/NLDAS/NLDAS_FORA0125_H.2.0"

# NLDAS_FORA0125 native grid (see batch_download_nldas.R in interoperable-flows)
GRID_LONS <- seq(-124.9375, -67.0625, 0.125)
GRID_LATS <- seq(25.0625, 52.9375, 0.125)

NETRC_PATH  <- path.expand("~/.netrc")
COOKIE_PATH <- tempfile(fileext = ".cookies")  # shared session cookie, established once

# NASA Earthdata caps concurrent connections per account at 5
MAX_WORKERS <- 5

## AUTH =======================================================================
# NLDAS OPeNDAP requires the standard EDL netrc+cookie redirect flow - a plain
# Authorization: Bearer <token> header (as the old Giovanni proxy script used)
# gets a 401 here, so this needs EARTHDATA_USER / EARTHDATA_PASSWORD instead.

earthdata_user <- Sys.getenv("EARTHDATA_USER")
earthdata_password <- Sys.getenv("EARTHDATA_PASSWORD")

if (nzchar(earthdata_user) && nzchar(earthdata_password)) {
  writeLines(
    c(
      "machine urs.earthdata.nasa.gov",
      paste("login", earthdata_user),
      paste("password", earthdata_password)
    ),
    NETRC_PATH
  )
} else if (!file.exists(NETRC_PATH)) {
  stop("No EARTHDATA_USER/EARTHDATA_PASSWORD in environment, and no ~/.netrc found")
}

# Do the netrc+redirect handshake once up front and keep the resulting session
# cookie, rather than repeating the full URS login/redirect on every request -
# subsequent requests just present this cookie directly.
establish_session <- function() {
  probe_url <- build_nldas_url(
    floor_date(with_tz(Sys.time(), "UTC"), "hour") - INGEST_LAG - hours(1),
    0, 0
  )
  res <- GET(
    probe_url,
    config(netrc = TRUE, netrc_file = NETRC_PATH, followlocation = TRUE,
           cookiefile = COOKIE_PATH, cookiejar = COOKIE_PATH),
    timeout(60)
  )
  if (status_code(res) != 200) {
    stop("Failed to establish EDL session (HTTP ", status_code(res), ") - check EARTHDATA_USER/EARTHDATA_PASSWORD")
  }
  invisible(NULL)
}

## GRID HELPERS ================================================================

nearest_idx <- function(lon, lat) {
  list(
    x = which.min(abs(GRID_LONS - lon)) - 1L,  # OPeNDAP indices are 0-based
    y = which.min(abs(GRID_LATS - lat)) - 1L
  )
}

build_nldas_url <- function(dt, x_idx, y_idx) {
  yyyy     <- format(dt, "%Y")
  doy      <- sprintf("%03d", as.integer(format(dt, "%j")))
  yyyymmdd <- format(dt, "%Y%m%d")
  hhhh     <- sprintf("%02d00", as.integer(format(dt, "%H")))
  sprintf(
    "%s/%s/%s/NLDAS_FORA0125_H.A%s.%s.020.nc.ascii?Rainf[0:1:0][%d][%d]",
    BASE_URL, yyyy, doy, yyyymmdd, hhhh, y_idx, x_idx
  )
}

parse_rainf_ascii <- function(txt) {
  last_line <- tail(str_split(str_trim(txt), "\n")[[1]], 1)
  val <- str_extract(last_line, "-?[0-9.eE+-]+$")
  as.numeric(val)
}

fetch_one_hour <- function(dt, x_idx, y_idx) {
  url <- build_nldas_url(dt, x_idx, y_idx)
  # read the already-established shared session cookie, but write any
  # cookie updates to a throwaway per-call file so parallel workers never
  # write-race the shared one
  scratch_jar <- tempfile(fileext = ".cookies")
  on.exit(unlink(scratch_jar), add = TRUE)
  res <- GET(
    url,
    config(netrc = TRUE, netrc_file = NETRC_PATH, followlocation = TRUE,
           cookiefile = COOKIE_PATH, cookiejar = scratch_jar),
    timeout(60)
  )
  if (status_code(res) != 200) {
    return(NA_real_)
  }
  parse_rainf_ascii(content(res, "text", encoding = "UTF-8"))
}

safe_fetch_one_hour <- possibly(fetch_one_hour, otherwise = NA_real_, quiet = TRUE)

## DETERMINE FETCH WINDOW ======================================================

existing <- if (file.exists(rds_path)) readRDS(rds_path) else NULL

start_ts <- if (!is.null(existing) && nrow(existing) > 0) {
  max(existing$timestamp_utc, na.rm = TRUE) - hours(OVERLAP_HOURS)
} else {
  MIN_START_DATE
}
start_ts <- max(start_ts, MIN_START_DATE)
end_ts <- floor_date(with_tz(Sys.time(), "UTC"), "hour") - INGEST_LAG

if (start_ts >= end_ts) {
  message("Nothing new to fetch")
  quit(status = 0)
}

hours_seq <- seq(start_ts, end_ts, by = "1 hour")
message(
  "Fetching ", length(hours_seq), " hours x ", nrow(precip_sites),
  " sites from ", start_ts, " to ", end_ts
)

## FETCH (parallel) =============================================================

establish_session()

plan(multisession, workers = min(MAX_WORKERS, future::availableCores()))

new_data <- precip_sites |>
  mutate(idx = map2(lon, lat, nearest_idx)) |>
  select(id, idx) |>
  pmap(function(id, idx) {
    tibble(
      site = id,
      timestamp_utc = hours_seq,
      precip_mm = future_map_dbl(
        hours_seq,
        ~ safe_fetch_one_hour(.x, idx$x, idx$y),
        .options = furrr_options(seed = TRUE)
      )
    )
  }) |>
  bind_rows() |>
  filter(!is.na(precip_mm)) |>
  mutate(
    precip_in = precip_mm / 25.4,
    timestamp = with_tz(timestamp_utc, "America/Los_Angeles")
  )

plan(sequential)

if (nrow(new_data) == 0) {
  message("No new data returned")
  quit(status = 0)
}

## MERGE + DEDUPLICATE + SAVE ===================================================

combined <- if (!is.null(existing)) {
  bind_rows(existing, new_data) |>
    distinct(site, timestamp_utc, .keep_all = TRUE) |>
    arrange(site, timestamp_utc)
} else {
  new_data |> arrange(site, timestamp_utc)
}

saveRDS(combined, rds_path)
message("Saved ", nrow(combined), " total precip records")
