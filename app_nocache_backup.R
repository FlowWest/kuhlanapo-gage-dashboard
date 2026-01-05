library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)

# -------------------------------------------------
# CONFIG
# -------------------------------------------------

CLIENT_ID     <- Sys.getenv("HYDROVU_CLIENT_ID")
CLIENT_SECRET <- Sys.getenv("HYDROVU_CLIENT_SECRET")

TOKEN_URL <- "https://hydrovu.com/public-api/oauth/token"
BASE_API  <- "https://hydrovu.com/public-api/v1"

selected_parameters <- c("Depth" = 3, 
                         "Temperature" = 1) # codes from get_parameter_names()

gages <- tribble(  ~code,   ~site,                 ~name,           ~type
                   , "MC-01", "Lower Manning Creek", "2025SGMC01",    "troll"
                   , "MC-01", "Lower Manning Creek", "2025SGMC01_VL", "vulink"
                   , "MC-03", "Upper Manning Creek", "2025SGMC03",    "troll"
                   , "MC-03", "Upper Manning Creek", "2025SGMC03_VL", "vulink"
                   , "MC-02", "Secondary Channel",   "2025SGMC02",    "troll"
                   , "MC-02", "Secondary Channel",   "2025SGMC02_VL", "vulink"
) |>
  mutate(across(c(code, site, name, type), as_factor))

# -------------------------------------------------
# AUTH
# -------------------------------------------------

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

# ---------------------------------------------------------
# DATA ACCESS
# ---------------------------------------------------------

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
    enframe(name = "parm_id",
            value = "parm_name") |>
    unnest(parm_name)
}

get_raw_location_data <- function(token, location_id, start_ts = NULL, end_ts = NULL) {
  
  # Set defaults if NULL
  if (is.null(end_ts))   end_ts   <- Sys.time()
  if (is.null(start_ts)) start_ts <- end_ts - lubridate::days(30)
  
  # Convert to ISO 8601 format expected by API
  start_str <- format(start_ts, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  end_str   <- format(end_ts,   "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  res <- GET(
    paste0(BASE_API, "/locations/", location_id, "/data"),
    add_headers(Authorization = paste("Bearer", token)),
    query = list(
      startDate = start_str,
      endDate   = end_str
    )
  )
  
  stop_for_status(res)
  
  content(res, "parsed", type = "application/json")$parameters
}



get_readings_as_data_frame <- function(token, location_id, parameter_df,
                                              start_ts = NULL, end_ts = NULL) {
  raw <- get_raw_location_data(token, location_id, start_ts, end_ts)
  
  raw |> 
    enframe() |> 
    unnest_wider(value) |> 
    select(parameterId, unitId, readings) |>
    inner_join(parameter_df, by = join_by(parameterId == parm_id)) |>
    unnest(readings) |> 
    unnest_wider(readings) |> 
    mutate(
      timestamp = as_datetime(as.numeric(timestamp), tz = "UTC")
    )
}

get_readings_paginated <- function(token, location_id, parameter_df, start_ts = NULL, end_ts = NULL) {
  
  # Defaults: last 30 days
  if (is.null(end_ts))   end_ts   <- Sys.time()
  if (is.null(start_ts)) start_ts <- end_ts - lubridate::days(14)
  
  # Convert to ISO 8601 for API
  start_str <- format(start_ts, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  end_str   <- format(end_ts,   "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  all_pages <- list()
  next_page <- NULL
  
  repeat {
    message("parsing page...")
    message(next_page)
    headers <- c(
      Authorization = paste("Bearer", token)
    )
    
    if (!is.null(next_page)) {
      headers["X-ISI-Start-Page"] <- next_page
    }
    
    res <- GET(
      paste0(BASE_API, "/locations/", location_id, "/data"),
      add_headers(.headers = headers),
      query = list(
        startDate = start_str,
        endDate   = end_str
      )
    )

    stop_for_status(res)
    
    raw <- content(res, "parsed", type = "application/json")$parameters
    
    page_result <- raw |> 
      enframe() |> 
      unnest_wider(value) |> 
      select(parameterId, unitId, readings) |>
      inner_join(parameter_df, by = join_by(parameterId == parm_id)) |>
      unnest(readings) |> 
      unnest_wider(readings) |> 
      mutate(
        timestamp = as_datetime(as.numeric(timestamp), tz = "UTC")
      )
    
    all_pages <- append(all_pages, list(page_result))
    
    # Pagination header
    next_page <- headers(res)[["x-isi-next-page"]]
    if (is.null(next_page)) break
  }
  
  # Combine all pages into one list
  combined <- do.call(bind_rows, all_pages)
  combined
}

# locations <- get_locations(token)
# 
# locations_joined <- locations |>
#   inner_join(gages, by = join_by(name == name)) 
# 
# results <- locations_joined |>
#   mutate(result = map(id, \(x) get_readings_as_data_frame(token, x))) |>
#   unnest_wider(result) |> 
#   unnest(c(parameterId, unitId, timestamp, value, parm_name)) |>
#   mutate(timestamp = as_datetime(timestamp))

# ---------------------------------------------------------
# UI
# ---------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    tags$style("
      body { margin: 0; }
      .container-fluid { padding: 0 15px; }
    ")
  ),
  
  fluidRow(
    column(12, plotOutput("depth_plot", height = "400px"))
  ),
  
  fluidRow(
    column(12, plotOutput("temp_plot", height = "400px"))
  )
)

# ---------------------------------------------------------
# SERVER
# ---------------------------------------------------------

server <- function(input, output, session) {
  
  # reactive token
  token_val <- reactiveVal(get_access_token(CLIENT_ID, CLIENT_SECRET))
  
  # reactive results table (pre-fetched and flattened)
  ts_data <- reactive({
  
  # fetch locations
  locs <- get_locations(token_val())
  parms <- get_parameter_names(token_val())
  
  # compile list of gages and their ids
  locs_joined <- locs |> inner_join(gages, by = join_by(name == name))
  
  # fetch readings for each location and flatten
  locs_joined |>
    mutate(result = map(id, \(x) get_readings_paginated(token_val(), x, parms))) |>
    unnest(result) |> 
    unnest(c(parameterId, unitId, timestamp, value, parm_name)) |>
    mutate(timestamp = as_datetime(timestamp)) |>
    mutate(parm_name_modified = case_when(
      parm_name == "Temperature" ~
        if_else(type == "vulink", "Air Temperature", "Water Temperature"),
      TRUE ~ parm_name
    ))
    })
  
  observe({
    df <- ts_data()
    print(head(df, 20))       # prints first 20 rows
    print(dim(df))            # prints number of rows and columns
    print(colnames(df))       # prints column names
  })
  
  # ----------------------------
  # Depth plot
  # ----------------------------
  # Depth plot
  output$depth_plot <- renderPlot({
    df <- ts_data() |> 
      filter(tolower(parm_name) == "depth") |> 
      arrange(name, timestamp)
    
    if(nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = timestamp, y = value, color = site, group = name)) +
      geom_line() +
      theme_minimal() +
      labs(y = "Depth", x = NULL, color = "Gage") +
      scale_color_brewer(palette = "Paired")
  })
  
  # Temperature plot
  output$temp_plot <- renderPlot({
    df <- ts_data() |> 
      filter(tolower(parm_name) == "temperature") |> 
      arrange(name, timestamp)
    
    if(nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = timestamp, y = value, color = site, group = name, linetype = parm_name_modified)) +
      geom_line(linewidth = 0.8) +
      theme_minimal() +
      labs(y = "Temperature", x = NULL, color = "Gage") +
      scale_color_brewer(palette = "Paired")
  })
  
}


# ---------------------------------------------------------
# RUN APP
# ---------------------------------------------------------

shinyApp(ui, server)
