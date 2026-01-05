# scripts/fetch_hydrovu.R
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(tibble)

## CONFIG =====================================================================

CLIENT_ID     <- Sys.getenv("HYDROVU_CLIENT_ID")
CLIENT_SECRET <- Sys.getenv("HYDROVU_CLIENT_SECRET")
stopifnot(nzchar(CLIENT_ID), nzchar(CLIENT_SECRET))

TOKEN_URL <- "https://hydrovu.com/public-api/oauth/token"
BASE_API  <- "https://hydrovu.com/public-api/v1"

DATA_FILE <- "data/gage_data.rds"

# Safety overlap when appending (prevents boundary misses)
OVERLAP_DAYS <- 2

gages <- tribble(
  ~code,   ~site,                 ~name,           ~type,
  "MC-01","Lower Manning Creek", "2025SGMC01",    "troll",
  "MC-01","Lower Manning Creek", "2025SGMC01_VL", "vulink",
  "MC-03","Upper Manning Creek", "2025SGMC03",    "troll",
  "MC-03","Upper Manning Creek", "2025SGMC03_VL", "vulink",
  "MC-02","Secondary Channel",   "2025SGMC02",    "troll",
  "MC-02","Secondary Channel",   "2025SGMC02_VL", "vulink"
) |> mutate(across(everything(), as.character))

## AUTH =======================================================================

get_access_token <- function() {
  res <- POST(
    TOKEN_URL,
    authenticate(CLIENT_ID, CLIENT_SECRET),
    body = list(grant_type = "client_credentials"),
    encode = "form"
  )
  stop_for_status(res)
  content(res, "parsed", type = "application/json")$access_token
}

## API HELPERS ================================================================

get_locations <- function(token) {
  GET(paste0(BASE_API, "/locations/list"),
      add_headers(Authorization = paste("Bearer", token))) |>
    content("text", encoding = "UTF-8") |>
    fromJSON(flatten = TRUE) |>
    as_tibble()
}

get_parameter_names <- function(token) {
  GET(paste0(BASE_API, "/sispec/friendlynames"),
      add_headers(Authorization = paste("Bearer", token))) |>
    content("parsed", type = "application/json") |>
    (\(x) x$parameters)() |>
    enframe(name = "parm_id", value = "parm_name") |>
    unnest(parm_name)
}

get_readings_paginated <- function(token, location_id, parameter_df,
                                   start_ts, end_ts) {
  
  start_str <- format(start_ts, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  end_str   <- format(end_ts,   "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  pages <- list()
  next_page <- NULL
  
  repeat {
    hdrs <- c(Authorization = paste("Bearer", token))
    if (!is.null(next_page)) hdrs["X-ISI-Start-Page"] <- next_page
    
    res <- GET(
      paste0(BASE_API, "/locations/", location_id, "/data"),
      add_headers(.headers = hdrs),
      query = list(startDate = start_str, endDate = end_str)
    )
    stop_for_status(res)
    
    parms <- content(res, "parsed", type = "application/json")$parameters
    
    if (length(parms) > 0) {
      df <- parms |>
        enframe() |>
        unnest_wider(value) |>
        select(parameterId, unitId, readings) |>
        inner_join(parameter_df, by = join_by(parameterId == parm_id)) |>
        unnest(readings) |>
        unnest_wider(readings) |>
        mutate(timestamp = as_datetime(as.numeric(timestamp), tz = "UTC"))
      
      pages[[length(pages) + 1]] <- df
    }
    
    next_page <- headers(res)[["x-isi-next-page"]]
    if (is.null(next_page)) break
  }
  
  bind_rows(pages)
}

## LOAD EXISTING DATA ==========================================================

existing_data <- if (file.exists(DATA_FILE)) {
  message("Loading existing dataset.")
  readRDS(DATA_FILE)
} else {
  message("No existing dataset found.")
  NULL
}

## DETERMINE FETCH WINDOW ======================================================

end_ts <- Sys.time()

start_ts <- if (!is.null(existing_data) && nrow(existing_data) > 0) {
  max(existing_data$timestamp, na.rm = TRUE) - days(OVERLAP_DAYS)
} else {
  end_ts - days(14)
}

message("Fetching data from ", start_ts, " to ", end_ts)

## FETCH NEW DATA ==============================================================

token <- get_access_token()
locs  <- get_locations(token)
parms <- get_parameter_names(token)

new_data <- locs |>
  inner_join(gages, by = join_by(name == name)) |>
  mutate(result = map(
    id,
    ~ get_readings_paginated(token, .x, parms, start_ts, end_ts)
  )) |>
  unnest(result) |>
  mutate(
    timestamp = as_datetime(timestamp, tz = "UTC")
  )

## APPEND + DEDUPLICATE =========================================================

combined <- bind_rows(existing_data, new_data) |>
  distinct(id, parameterId, timestamp, .keep_all = TRUE) |>
  arrange(name, timestamp)

## SAVE ========================================================================

dir.create("data", showWarnings = FALSE)
saveRDS(combined, DATA_FILE)

message("Saved ", nrow(combined), " total records.")
