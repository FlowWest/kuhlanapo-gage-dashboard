# app.R - HydroVu Shiny app with server-side stale-while-revalidate cache
library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(purrr)
library(tibble)
library(future)
library(promises)

plan(multisession)  # use multicore on Linux if preferred

## CONFIG ====================================================================== 

# Ensure client ID and secret are specified in .Renviron file
CLIENT_ID     <- Sys.getenv("HYDROVU_CLIENT_ID")
CLIENT_SECRET <- Sys.getenv("HYDROVU_CLIENT_SECRET")

stopifnot(
  nzchar(CLIENT_ID),
  nzchar(CLIENT_SECRET)
)

TOKEN_URL <- "https://hydrovu.com/public-api/oauth/token"
BASE_API  <- "https://hydrovu.com/public-api/v1"

# TTL for cache (seconds) - 24 hours default
CACHE_TTL <- 24 * 60 * 60

# Cache directory - override with env var HYDROVU_CACHE_DIR if desired
cache_dir <- Sys.getenv("HYDROVU_CACHE_DIR", unset = NA)
if (is.na(cache_dir) || cache_dir == "") {
  cache_dir <- file.path(tempdir(), "hydrovu_cache")
}
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

cache_data_file <- file.path(cache_dir, "gage_data.rds")
cache_meta_file <- file.path(cache_dir, "last_refresh.txt")
cache_lock_file <- file.path(cache_dir, "refresh.lock")

# gages (your provided table)
gages <- tribble(
  ~code,   ~site,                 ~name,           ~type,
  "MC-01","Lower Manning Creek", "2025SGMC01",    "troll",
  "MC-01","Lower Manning Creek", "2025SGMC01_VL", "vulink",
  "MC-03","Upper Manning Creek", "2025SGMC03",    "troll",
  "MC-03","Upper Manning Creek", "2025SGMC03_VL", "vulink",
  "MC-02","Secondary Channel",   "2025SGMC02",    "troll",
  "MC-02","Secondary Channel",   "2025SGMC02_VL", "vulink"
) |> mutate(across(c(code, site, name, type), as.character))

## HELPERS =====================================================================

### AUTH & API helpers ---------------------------------------------------------

get_access_token <- function(client_id, client_secret) {
  res <- POST(
    TOKEN_URL,
    authenticate(client_id, client_secret),
    body = list(grant_type = "client_credentials"),
    encode = "form"
  )
  stop_for_status(res)
  content(res, as = "parsed", type = "application/json")$access_token
}

get_locations <- function(token) {
  res <- GET(paste0(BASE_API, "/locations/list"),
             add_headers(Authorization = paste("Bearer", token)))
  stop_for_status(res)
  content(res, "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE) |> as_tibble()
}

get_parameter_names <- function(token) {
  res <- GET(paste0(BASE_API, "/sispec/friendlynames"),
             add_headers(Authorization = paste("Bearer", token)))
  stop_for_status(res)
  content(res, "parsed", type = "application/json")$parameters |>
    enframe(name = "parm_id", value = "parm_name") |> unnest(parm_name)
}

# fetch one page (no pagination handling) - kept for completeness
get_raw_location_data <- function(token, location_id, start_ts = NULL, end_ts = NULL) {
  if (is.null(end_ts))   end_ts   <- Sys.time()
  if (is.null(start_ts)) start_ts <- end_ts - days(30)
  start_str <- format(start_ts, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  end_str   <- format(end_ts,   "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  res <- GET(
    paste0(BASE_API, "/locations/", location_id, "/data"),
    add_headers(Authorization = paste("Bearer", token)),
    query = list(startDate = start_str, endDate = end_str)
  )
  stop_for_status(res)
  content(res, "parsed", type = "application/json")$parameters
}

# robust paginated fetch - returns flattened tibble (timestamp converted to POSIXct UTC)
get_readings_paginated <- function(token, location_id, parameter_df, start_ts = NULL, end_ts = NULL) {
  if (is.null(end_ts))   end_ts   <- Sys.time()
  if (is.null(start_ts)) start_ts <- end_ts - lubridate::days(14)
  start_str <- format(start_ts, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  end_str   <- format(end_ts,   "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  all_pages <- list()
  next_page <- NULL
  
  repeat {
    req_headers <- c(Authorization = paste("Bearer", token))
    if (!is.null(next_page)) req_headers["X-ISI-Start-Page"] <- next_page
    
    res <- GET(
      paste0(BASE_API, "/locations/", location_id, "/data"),
      add_headers(.headers = req_headers),
      query = list(startDate = start_str, endDate = end_str)
    )
    stop_for_status(res)
    
    page_parms <- content(res, as = "parsed", type = "application/json")$parameters
    # flatten page into tibble; if no parameters, return empty tibble
    if (length(page_parms) > 0) {
      page_result <- page_parms |>
        enframe() |>
        unnest_wider(value) |>
        select(parameterId, unitId, readings) |>
        inner_join(parameter_df, by = join_by(parameterId == parm_id)) |>
        unnest(readings) |>
        unnest_wider(readings) |>
        mutate(timestamp = as_datetime(as.numeric(timestamp), tz = "UTC"))
    } else {
      page_result <- tibble(parameterId = character(), unitId = character(), timestamp = as_datetime(numeric()), value = numeric(), parm_name = character())
    }
    all_pages <- append(all_pages, list(page_result))
    
    # get opaque cursor from response headers (use httr::headers)
    next_page <- httr::headers(res)[["x-isi-next-page"]]
    if (is.null(next_page)) break
  }
  
  dplyr::bind_rows(all_pages)
}

### Caching helpers (disk) -----------------------------------------------------

cache_is_stale <- function() {
  if (!file.exists(cache_meta_file)) return(TRUE)
  last <- as.numeric(readLines(cache_meta_file, warn = FALSE))
  if (length(last) == 0) return(TRUE)
  difftime(Sys.time(), as.POSIXct(last, origin = "1970-01-01"), units = "secs") > CACHE_TTL
}

read_cached_data <- function() {
  if (file.exists(cache_data_file)) {
    tryCatch(readRDS(cache_data_file), error = function(e) NULL)
  } else {
    NULL
  }
}

write_cache <- function(data) {
  tmp <- paste0(cache_data_file, ".tmp")
  saveRDS(data, tmp)
  file.rename(tmp, cache_data_file)
  writeLines(as.character(as.numeric(Sys.time())), cache_meta_file)
}

# background refresh with lock
refresh_cache_async <- function() {
  # don't start another refresh if lock exists
  if (file.exists(cache_lock_file)) {
    message("Cache refresh already in progress; skipping.")
    return(invisible(FALSE))
  }
  # create lock
  writeLines(as.character(Sys.getpid()), cache_lock_file)
  
  future({
    on.exit({
      tryCatch(unlink(cache_lock_file), error = function(e) NULL)
    }, add = TRUE)
    
    message("Background cache refresh started.")
    
    # Get token and fetch fresh data
    token <- get_access_token(CLIENT_ID, CLIENT_SECRET)
    
    # fetch all gage data
    fresh <- fetch_all_gage_data(token)
    
    # write cache
    write_cache(fresh)
    
    message("Background cache refresh finished.")
    TRUE
  }) %...!% (function(err) {
    warning("Background cache refresh failed: ", conditionMessage(err))
    if (file.exists(cache_lock_file)) unlink(cache_lock_file)
    NULL
  })
}

### Fetch-all function ---------------------------------------------------------
# (used for both foreground and background fetch)

fetch_all_gage_data <- function(token, start_ts = NULL, end_ts = NULL) {
  locs <- get_locations(token)
  parms <- get_parameter_names(token)
  
  locs_joined <- locs |> inner_join(gages, by = join_by(name == name))
  
  # For each location id, fetch paginated readings
  res_list <- locs_joined |> mutate(
    result = map(id, ~ get_readings_paginated(token, .x, parms, start_ts = start_ts, end_ts = end_ts))
  )
  
  # Unnest results and attach gage metadata
  df <- res_list |>
    select(id, name, site, type, code, result) |>
    unnest(result) |>
    # ensure expected columns exist
    mutate(
      timestamp = as_datetime(timestamp, tz = "UTC")
    ) |>
    arrange(name, timestamp)
  
  # If timestamp is numeric or character sometimes, coerce
  if ("timestamp" %in% names(df) && !inherits(df$timestamp, "POSIXct")) {
    df <- df |> mutate(timestamp = as_datetime(as.numeric(timestamp), tz = "UTC"))
  }
  
  df
}

## UI ==========================================================================

ui <- fluidPage(
  tags$head(tags$style("body { margin: 0; } .container-fluid { padding: 0 15px; }")),
  fluidRow(column(12, plotOutput("depth_plot", height = "400px"))),
  fluidRow(column(12, plotOutput("temp_plot", height = "400px"))),
  fluidRow(column(12, verbatimTextOutput("cache_info")))
)

## SERVER ======================================================================

server <- function(input, output, session) {
  
  # token reactive (will be created once per session)
  token_val <- reactiveVal(get_access_token(CLIENT_ID, CLIENT_SECRET))
  
  # ts_data reactive returns immediate cached data if available.
  ts_data <- reactive({
    cached <- read_cached_data()
    if (!is.null(cached)) {
      # If cache is stale, trigger background refresh (non-blocking)
      if (cache_is_stale()) {
        message("Cache is stale: scheduling background refresh.")
        refresh_cache_async()
      }
      return(cached)
    }
    
    # No cache: cold start - fetch synchronously (first-ever run)
    message("No cache found; fetching data synchronously (cold start).")
    token <- token_val()
    fresh <- fetch_all_gage_data(token)
    write_cache(fresh)
    fresh
  })
  
  # expose some cache info for debugging
  output$cache_info <- renderPrint({
    list(
      cache_file_exists = file.exists(cache_data_file),
      cache_age_secs = if (file.exists(cache_meta_file)) as.numeric(Sys.time() - as.POSIXct(as.numeric(readLines(cache_meta_file)), origin = "1970-01-01")) else NA,
      cache_path = cache_data_file,
      cache_dir = cache_dir
    )
  })
  
  # Depth plot (ggplot)
  output$depth_plot <- renderPlot({
    df <- ts_data() |> 
      filter(tolower(parm_name) == "depth") |> 
      arrange(name, timestamp) |>
      mutate(value = if_else(value > 0, value / 0.3048, 0))
    
    if (is.null(df) || nrow(df) == 0) {
      plot.new(); title("No Depth data available")
      return()
    }
    
    ggplot(df, aes(x = timestamp, y = value, color = site, group = name)) +
      geom_line() +
      theme_minimal() +
      labs(y = "Depth (ft)", x = NULL, color = "Gage") +
      scale_color_brewer(palette = "Paired")
  })
  
  # Temperature plot (ggplot)
  output$temp_plot <- renderPlot({
    df <- ts_data() |> 
      filter(tolower(parm_name) == "temperature") |> 
      arrange(name, timestamp) |>
      mutate(parm_name_modified = case_when(
        parm_name == "Temperature" ~
          if_else(type == "vulink", "Air Temperature", "Water Temperature"),
        TRUE ~ parm_name
      )) |>
      mutate(value = value * (9/5) + 32)
    
    if (is.null(df) || nrow(df) == 0) {
      plot.new(); title("No Temperature data available")
      return()
    }
    
    ggplot(df, aes(x = timestamp, y = value, color = site, group = name, linetype = parm_name_modified)) +
      geom_line() +
      theme_minimal() +
      labs(y = "Temperature (F)", x = NULL, color = "Gage") +
      scale_color_brewer(palette = "Paired") +
      scale_linetype_manual(name = "Parameter",
                            values = c("Air Temperature" = "dashed",
                                       "Water Temperature" = "solid"))
  })
}

shinyApp(ui, server)
