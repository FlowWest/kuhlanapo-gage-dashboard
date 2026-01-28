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

isi_paginate <- function(token, url, query = NULL, parse_as="text", body_parse = function(x) x) {
  out <- list()
  next_page <- NULL
  
  repeat {
    hdrs <- c(Authorization = paste("Bearer", token))
    if (!is.null(next_page)) hdrs["X-ISI-Start-Page"] <- next_page
    
    # Read as text
    res <- GET(url, add_headers(.headers = hdrs), query = query)
    stop_for_status(res)
    parsed_body <- content(res, parse_as, encoding = "UTF-8")
    
    # Parse via jsonlite
    out[[length(out) + 1]] <- body_parse(parsed_body)
    
    next_page <- headers(res)[["x-isi-next-page"]]
    if (is.null(next_page) || next_page == "") break
  }
  
  bind_rows(out)
}

get_locations <- function(token) {
  isi_paginate(
    token,
    paste0(BASE_API, "/locations/list"),
    parse_as = "text",
    body_parse = function(body) fromJSON(body, flatten = TRUE) |> as_tibble()
  )
}

get_parameter_names <- function(token) {
  res <- GET(paste0(BASE_API, "/sispec/friendlynames"),
             add_headers(Authorization = paste("Bearer", token)))
  stop_for_status(res)
  
  txt <- content(res, "text", encoding = "UTF-8")
  parsed <- fromJSON(txt, flatten = TRUE)
  
  # Convert parameters list to tibble
  parsed$parameters |>
    enframe(name = "parm_id", value = "parm_name") |>
    unnest(parm_name)
}

get_readings <- function(token, location_id, parameter_df,
                         start_ts, end_ts) {
  start_str <- format(start_ts, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  end_str   <- format(end_ts,   "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  query <- list(startDate = start_str, endDate = end_str)
  url <- paste0(BASE_API, "/locations/", location_id, "/data")
  
  isi_paginate(
    token,
    url,
    query = query,
    parse_as = "parsed",
    body_parse = function(body) {
      parms <- body$parameters
      if (length(parms) == 0) return(NULL)
      parms |>
        enframe() |>
        unnest_wider(value) |>
        select(parameterId, unitId, readings) |>
        inner_join(parameter_df, by = join_by(parameterId == parm_id)) |>
        unnest(readings) |>
        unnest_wider(readings) |>
        mutate(timestamp = as_datetime(as.numeric(timestamp), tz = "UTC"))
    }
  )
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
locs  <- get_locations(token) |>
  filter(name %in% gages$name)
parms <- get_parameter_names(token)

################################################################################
# # temporary patch for missing locations
# existing_locs <- existing_data |> 
#   group_by(id, name, description) |> 
#   summarize(.groups = "drop")
# 
# locs <- locs |> 
#   full_join(existing_locs, by = join_by(id, name, description))
################################################################################

new_data <- locs |>
  inner_join(gages, by = join_by(name == name)) |>
  mutate(result = map(
    id,
    ~ get_readings(token, .x, parms, start_ts, end_ts)
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
