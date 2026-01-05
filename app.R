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
library(plotly)

## CONFIG ======================================================================

INTERACTIVE <- T

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
  fluidRow(column(12, if (!INTERACTIVE) plotOutput("depth_plot", height = 400) else plotlyOutput("depth_plot", height = 400))),
  fluidRow(column(12, if (!INTERACTIVE) plotOutput("temp_plot",  height = 400) else plotlyOutput("temp_plot",  height = 400)))
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
      mutate(site = factor(site, levels = unique(gages$site))) |>
      glimpse()
  })
  
  df_latest <- reactive({
    df_pivot() |>
      group_by(code, site) |>
      filter(timestamp == max(timestamp)) |>
      ungroup() |>
      glimpse()
  })
  
  output$depth_plot <- if (!INTERACTIVE) {
    
    renderPlot({
      base_df  <- df_pivot()
      label_df <- df_latest()
      
      req(nrow(base_df) > 0, nrow(label_df) > 0)
      
      ggplot(base_df, aes(x = timestamp, y = depth, color = site)) +
        geom_line() +
        geom_text_repel(
          data = label_df,
          aes(y = depth, label = sprintf("%.1f", depth)),
          hjust = 1
        ) +
        theme_minimal() +
        labs(y = "Depth (ft)", x = NULL, color = "Gage") +
        scale_color_brewer(palette = "Paired") + 
        theme(
          legend.position = "bottom",
          legend.box = "horizontal"
        )
      
    })
    
  } else {
    
    renderPlotly({
      base_df <- df_pivot()
      req(nrow(base_df) > 0)
      
      plot_ly(
        base_df,
        x = ~timestamp,
        y = ~depth,
        color = ~site,
        type = "scatter",
        mode = "lines",
        hoverinfo = "text+x",
        text = ~paste(site, "\n", "Depth", sprintf("%.1f ft", depth)),
        connectgaps = FALSE
      ) |> layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.25
        ),
        margin = list(b = 80),
        xaxis = list(title = ""),
        yaxis = list(title = "Depth (ft)")
      )
    })
    
  }
  
  output$temp_plot <- if (!INTERACTIVE) {
    
    renderPlot({
      
      base_df <- df_pivot()
      req(nrow(base_df) > 0)
      
      ggplot(base_df, aes(x = timestamp, color = site)) +
        geom_line(aes(y = air_temperature, linetype = "Air Temperature")) +
        geom_line(aes(y = water_temperature, linetype = "Water Temperature")) +
        theme_minimal() +
        labs(
          y = "Temperature (°F)",
          x = NULL,
          color = "Gage",
          linetype = "Parameter"
        ) +
        scale_color_brewer(palette = "Paired") +
        scale_linetype_manual(
          values = c(
            "Water Temperature" = "solid",
            "Air Temperature"   = "dashed"
          )
        ) + 
        theme(
          legend.position = "bottom",
          legend.box = "horizontal"
        )
      
    })
    
  } else {
    
    renderPlotly({
      
      temp_long <- df_pivot() |>
        pivot_longer(
          c(air_temperature, water_temperature),
          names_to = "parameter",
          values_to = "temperature"
        ) |>
        mutate(parameter = case_when(
          parameter == "air_temperature" ~ "Air Temperature",
          parameter == "water_temperature" ~ "Water Temperature"
        ))
      
      req(nrow(temp_long) > 0)
      
      plot_ly(
        temp_long,
        x = ~timestamp,
        y = ~temperature,
        color = ~site,
        linetype = ~parameter,
        text = ~paste(site, "\n", parameter, sprintf("%.1f °F", temperature)),
        hoverinfo = "text+x",
        type = "scatter",
        mode = "lines",
        split = ~interaction(site, parameter),
        name = ~paste0(ifelse(parameter == "Air Temperature", "Air", "Water"), " - ", site),
        connectgaps = FALSE
      ) |> layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.25
        ),
        margin = list(b = 80),
        xaxis = list(title = ""),
        yaxis = list(title = "Temperature (°F)")
      )
      
    })
    
  }
  
}

shinyApp(ui, server)
