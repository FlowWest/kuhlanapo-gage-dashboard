# app.R - HydroVu Shiny app with GitHub RDS + stale-while-revalidate cache

library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(tibble)
library(tidyr)
library(janitor)
library(ggrepel)
library(scales)

## CONFIG ======================================================================

CACHE_TTL <- 24 * 60 * 60   # 24 hours
cache_dir <- file.path(getwd(), ".cache", "hydrovu")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

cache_data_file <- file.path(cache_dir, "gage_data.rds")
cache_lock_file <- file.path(cache_dir, "refresh.lock")

# GitHub raw URL
rds_url <- "https://github.com/flowwest/kuhlanapo-gage-dashboard/raw/main/data/gage_data.rds"

# Gage definitions
gages <- tribble(
  ~code,   ~site,                 ~name,           ~type,
  "MC-01","Lower Manning Creek", "2025SGMC01",    "troll",
  "MC-01","Lower Manning Creek", "2025SGMC01_VL", "vulink",
  "MC-03","Upper Manning Creek", "2025SGMC03",    "troll",
  "MC-03","Upper Manning Creek", "2025SGMC03_VL", "vulink",
  "MC-02","Secondary Channel",   "2025SGMC02",    "troll",
  "MC-02","Secondary Channel",   "2025SGMC02_VL", "vulink"
) |> mutate(across(everything(), as.character))

empty_ts_schema <- tibble(
  code = character(),
  site = character(),
  name = character(),
  type = character(),
  parameterId = character(),
  unitId = character(),
  parm_name = character(),
  timestamp = as.POSIXct(character(), tz = "UTC"),
  value = numeric()
)

## HELPERS =====================================================================

read_cached_data <- function() {
  if (!file.exists(cache_data_file)) return(NULL)
  tryCatch(readRDS(cache_data_file), error = function(e) NULL)
}

write_cache <- function(df) {
  tmp <- paste0(cache_data_file, ".tmp")
  saveRDS(df, tmp)
  file.rename(tmp, cache_data_file)
}

cache_is_stale <- function() {
  if (!file.exists(cache_data_file)) return(TRUE)
  difftime(Sys.time(), file.info(cache_data_file)$mtime, units = "secs") > CACHE_TTL
}

refresh_cache_async <- function() {
  if (file.exists(cache_lock_file)) return(invisible(FALSE))
  writeLines(as.character(Sys.getpid()), cache_lock_file)
  
  future::future({
    on.exit(unlink(cache_lock_file), add = TRUE)
    
    # download latest
    tmp_file <- tempfile(fileext = ".rds")
    tryCatch({
      download.file(rds_url, tmp_file, mode = "wb", quiet = TRUE)
      new_data <- readRDS(tmp_file)
      
      # combine with existing cached data
      old_data <- read_cached_data()
      if (!is.null(old_data)) {
        combined <- bind_rows(old_data, new_data) |>
          distinct(name, timestamp, parm_name, .keep_all = TRUE)
      } else {
        combined <- new_data
      }
      
      write_cache(combined)
    }, error = function(e) {
      warning("Cache refresh failed: ", conditionMessage(e))
    })
    TRUE
  })
}

## UI ==========================================================================

ui <- fluidPage(
  fluidRow(column(12, plotOutput("depth_plot", height = 400))),
  fluidRow(column(12, plotOutput("temp_plot",  height = 400)))
)

## SERVER ======================================================================

server <- function(input, output, session) {
  
  ts_data <- reactive({
    # download once if missing
    if (!file.exists(cache_data_file)) {
      download.file(rds_url, cache_data_file, mode = "wb", quiet = TRUE)
    }
    
    df <- read_cached_data()
    if (cache_is_stale()) refresh_cache_async()
    
    df
  })
  
  df_pivot <- reactive({
    df <- ts_data() |>
      filter(parm_name %in% c("Depth", "Temperature")) |>
      mutate(parm_name_modified = case_when(
               parm_name == "Temperature" & type == "vulink" ~ "Air Temperature",
               parm_name == "Temperature" ~ "Water Temperature",
               TRUE ~ parm_name
             )) |>
      mutate(value = case_when(
        parm_name == "Depth" ~ if_else(value > 0, value / 0.3048, 0),
        parm_name == "Temperature" ~ value * 9/5 + 32)) |>
      select(code, site, timestamp, parm_name_modified, value) |>
      pivot_wider(names_from = parm_name_modified, values_from = value) |>
      clean_names() |>
      mutate(water_temperature = if_else(depth > 0, water_temperature, NA)) |>
      glimpse()
  })
  
  df_latest <- reactive({
    df_pivot() |>
      group_by(code, site) |>
      filter(timestamp == max(timestamp)) |>
      ungroup() |>
      glimpse()
  })
  
  output$depth_plot <- renderPlot({

    df_pivot() |>
      ggplot(aes(x = timestamp, y = depth, color = site)) +
      geom_line() +
      geom_text_repel(aes(y = depth, label = sprintf("%.1f", depth)), data = df_latest(), hjust=1) +
      theme_minimal() +
      labs(y = "Depth (ft)", x = NULL, color = "Gage") +
      scale_color_brewer(palette = "Paired") 
  })
  
  output$temp_plot <- renderPlot({

    df_pivot() |>
      ggplot(aes(x = timestamp, color = site)) +
      geom_line(aes(y = air_temperature, linetype = "Air Temperature")) +
      geom_line(aes(y = water_temperature, linetype = "Water Temperature")) +
      theme_minimal() +
      labs(y = "Temperature (°F)", x = NULL, color = "Gage") +
      scale_color_brewer(palette = "Paired") +
      scale_linetype_manual(
        name = "Parameter",
        values = c("Water Temperature" = "solid", "Air Temperature" = "dashed")
      )
  })
}

shinyApp(ui, server)
