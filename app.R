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

plan(multisession)

## CONFIG ======================================================================

CLIENT_ID     <- Sys.getenv("HYDROVU_CLIENT_ID")
CLIENT_SECRET <- Sys.getenv("HYDROVU_CLIENT_SECRET")
stopifnot(nzchar(CLIENT_ID), nzchar(CLIENT_SECRET))

TOKEN_URL <- "https://hydrovu.com/public-api/oauth/token"
BASE_API  <- "https://hydrovu.com/public-api/v1"

CACHE_TTL <- 24 * 60 * 60
DAILY_REFRESH_HOUR <- 7
DAILY_REFRESH_MIN  <- 15
REFRESH_TZ <- "America/Los_Angeles"

# cache_dir <- Sys.getenv("HYDROVU_CACHE_DIR", unset = NA)
# if (is.na(cache_dir) || cache_dir == "") {
#   cache_dir <- file.path(tempdir(), "hydrovu_cache")
# }
# dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

get_cache_dir <- function() {
  
  env_dir <- Sys.getenv("HYDROVU_CACHE_DIR", unset = "")
  if (nzchar(env_dir)) {
    dir.create(env_dir, recursive = TRUE, showWarnings = FALSE)
    return(normalizePath(env_dir, winslash = "/", mustWork = FALSE))
  }
  
  if (nzchar(Sys.getenv("R_SHINY_SERVER_VERSION", "")) ||
      nzchar(Sys.getenv("POSIT_CONNECT", ""))) {
    
    base <- Sys.getenv("R_USER_CACHE_DIR", unset = "~/.cache")
    dir  <- file.path(base, "hydrovu")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    return(normalizePath(dir, winslash = "/", mustWork = FALSE))
  }
  
  dir <- file.path(getwd(), ".cache", "hydrovu")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(dir, winslash = "/", mustWork = FALSE)
}

cache_dir <- get_cache_dir()

cache_data_file <- file.path(cache_dir, "gage_data.rds")
cache_meta_file <- file.path(cache_dir, "last_refresh.txt")
cache_lock_file <- file.path(cache_dir, "refresh.lock")

gages <- tribble(
  ~code,   ~site,                 ~name,           ~type,
  "MC-01","Lower Manning Creek", "2025SGMC01",    "troll",
  "MC-01","Lower Manning Creek", "2025SGMC01_VL", "vulink",
  "MC-03","Upper Manning Creek", "2025SGMC03",    "troll",
  "MC-03","Upper Manning Creek", "2025SGMC03_VL", "vulink",
  "MC-02","Secondary Channel",   "2025SGMC02",    "troll",
  "MC-02","Secondary Channel",   "2025SGMC02_VL", "vulink"
) |> mutate(across(everything(), as.character))

## SCHEMA ======================================================================

empty_ts_schema <- tibble(
  code = character(),
  site = character(),
  name = character(),
  type = character(),
  parameterId = character(),
  unitId = character(),
  parm_name = character(),
  timestamp = as.POSIXct(character(), tz = "UTC"),
  value = numeric(),
  parm_name_modified = character()
)

## HELPERS =====================================================================

get_access_token <- function(client_id, client_secret) {
  res <- POST(
    TOKEN_URL,
    authenticate(client_id, client_secret),
    body = list(grant_type = "client_credentials"),
    encode = "form"
  )
  stop_for_status(res)
  content(res, "parsed", type = "application/json")$access_token
}

get_locations <- function(token) {
  res <- GET(
    paste0(BASE_API, "/locations/list"),
    add_headers(Authorization = paste("Bearer", token))
  )
  stop_for_status(res)
  content(res, "text", encoding = "UTF-8") |>
    fromJSON(flatten = TRUE) |>
    as_tibble()
}

get_parameter_names <- function(token) {
  res <- GET(
    paste0(BASE_API, "/sispec/friendlynames"),
    add_headers(Authorization = paste("Bearer", token))
  )
  stop_for_status(res)
  content(res, "parsed", type = "application/json")$parameters |>
    enframe(name = "parm_id", value = "parm_name") |>
    unnest(parm_name)
}

get_readings_paginated <- function(token, location_id, parameter_df,
                                   start_ts = NULL, end_ts = NULL) {
  
  if (is.null(end_ts))   end_ts   <- Sys.time()
  if (is.null(start_ts)) start_ts <- end_ts - days(14)
  
  start_str <- format(with_tz(start_ts, "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  end_str   <- format(with_tz(end_ts,   "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  
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
      page_df <- parms |>
        enframe() |>
        unnest_wider(value) |>
        select(parameterId, unitId, readings) |>
        inner_join(parameter_df, by = join_by(parameterId == parm_id)) |>
        unnest(readings) |>
        unnest_wider(readings) |>
        transmute(
          parameterId,
          unitId,
          parm_name,
          timestamp = as_datetime(as.numeric(timestamp), tz = "UTC"),
          value = as.numeric(value)
        )
    } else {
      page_df <- empty_ts_schema[0, c("parameterId", "unitId", "parm_name", "timestamp", "value")]
    }
    
    pages[[length(pages) + 1]] <- page_df
    next_page <- headers(res)[["x-isi-next-page"]]
    if (is.null(next_page)) break
  }
  
  bind_rows(pages)
}

## CACHE =======================================================================

last_refresh_time <- function() {
  if (!file.exists(cache_meta_file)) return(NA_real_)
  as.numeric(readLines(cache_meta_file, warn = FALSE))
}

needs_daily_refresh <- function() {
  now_pt <- with_tz(Sys.time(), REFRESH_TZ)
  trigger <- update(floor_date(now_pt, "day"),
                    hours = DAILY_REFRESH_HOUR,
                    minutes = DAILY_REFRESH_MIN)
  if (now_pt < trigger) return(FALSE)
  last <- last_refresh_time()
  if (is.na(last)) return(TRUE)
  with_tz(as.POSIXct(last, origin = "1970-01-01"), REFRESH_TZ) < trigger
}

read_cached_data <- function() {
  if (!file.exists(cache_data_file)) return(NULL)
  tryCatch(readRDS(cache_data_file), error = function(e) NULL)
}

write_cache <- function(data) {
  tmp <- paste0(cache_data_file, ".tmp")
  saveRDS(data, tmp)
  file.rename(tmp, cache_data_file)
  writeLines(as.character(as.numeric(Sys.time())), cache_meta_file)
}

fetch_all_gage_data <- function(token) {
  locs  <- get_locations(token)
  parms <- get_parameter_names(token)
  
  out <- locs |>
    inner_join(gages, by = join_by(name == name)) |>
    mutate(result = map(id, ~ get_readings_paginated(token, .x, parms))) |>
    unnest(result) |>
    mutate(
      parm_name_modified = case_when(
        parm_name == "Temperature" & type == "vulink" ~ "Air Temperature",
        parm_name == "Temperature"                   ~ "Water Temperature",
        TRUE                                         ~ parm_name
      )
    ) |>
    arrange(name, timestamp)
  
  if (nrow(out) == 0) empty_ts_schema else out
}

refresh_cache_async <- function() {
  if (file.exists(cache_lock_file)) return(invisible(FALSE))
  writeLines(as.character(Sys.getpid()), cache_lock_file)
  
  future({
    on.exit(unlink(cache_lock_file), add = TRUE)
    token <- get_access_token(CLIENT_ID, CLIENT_SECRET)
    write_cache(fetch_all_gage_data(token))
    TRUE
  }) %...!% (\(e) warning("Cache refresh failed: ", conditionMessage(e)))
}

## UI ==========================================================================

ui <- fluidPage(
  fluidRow(column(12, plotOutput("depth_plot", height = 400))),
  fluidRow(column(12, plotOutput("temp_plot",  height = 400)))
  # fluidRow(column(12, verbatimTextOutput("cache_info")))
)

## SERVER ======================================================================

server <- function(input, output, session) {
  
  ts_data <- reactive({
    readRDS("data/gage_data.rds")
  })
  
#  ts_data <- reactive({
#    cached <- read_cached_data()
#    if (!is.null(cached)) {
#      if (needs_daily_refresh()) refresh_cache_async()
#      return(cached)
#    }
#    token <- get_access_token(CLIENT_ID, CLIENT_SECRET)
#    fresh <- fetch_all_gage_data(token)
#    write_cache(fresh)
#    fresh
#  })
  
  # output$cache_info <- renderPrint({
  #   list(
  #     cache_dir = cache_dir,
  #     last_refresh_epoch = last_refresh_time(),
  #     needs_refresh = needs_daily_refresh()
  #   )
  # })
  
  message("cache_dir = ", cache_dir)
  message("last_refresh_epoch = ", last_refresh_time())
  message("needs_refresh = ", needs_daily_refresh())
  
  output$depth_plot <- renderPlot({
    df <- ts_data() |>
      filter(tolower(parm_name) == "depth") |>
      mutate(value = if_else(value > 0, value / 0.3048, 0))
    
    req(nrow(df) > 0)
    
    ggplot(df, aes(timestamp, value, color = site, group = name)) +
      geom_line() +
      theme_minimal() +
      labs(y = "Depth (ft)", x = NULL, color = "Gage") + 
      scale_color_brewer(palette = "Paired")
  })
  
  output$temp_plot <- renderPlot({
    df <- ts_data() |>
      filter(tolower(parm_name) == "temperature") |>
      mutate(value = value * 9/5 + 32)
    
    req(nrow(df) > 0)
    
    ggplot(df, aes(timestamp, value, color = site, linetype = parm_name_modified)) +
      geom_line() +
      theme_minimal() +
      labs(y = "Temperature (°F)", x = NULL, color = "Gage") + 
      scale_color_brewer(palette = "Paired") +
      scale_linetype_manual(name = "Parameter",
                            values = c("Water Temperature" = "solid",
                                       "Air Temperature" = "dashed"))
  })
}

shinyApp(ui, server)
