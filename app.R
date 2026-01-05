# app.R - HydroVu Shiny app with GitHub RDS + stale-while-revalidate cache + date filter + synced plots

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
library(patchwork)

## CONFIG ======================================================================

INTERACTIVE <- TRUE

CACHE_TTL <- 24 * 60 * 60   # 24 hours
cache_dir <- file.path(getwd(), ".cache", "hydrovu")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

cache_data_file <- file.path(cache_dir, "gage_data.rds")
cache_lock_file <- file.path(cache_dir, "refresh.lock")

rds_url <- "https://github.com/flowwest/kuhlanapo-gage-dashboard/raw/main/data/gage_data.rds"

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
    
    tmp_file <- tempfile(fileext = ".rds")
    tryCatch({
      download.file(rds_url, tmp_file, mode = "wb", quiet = TRUE)
      new_data <- readRDS(tmp_file)
      old_data <- read_cached_data()
      combined <- if (!is.null(old_data)) {
        bind_rows(old_data, new_data) |> distinct(name, timestamp, parm_name, .keep_all = TRUE)
      } else new_data
      write_cache(combined)
    }, error = function(e) warning("Cache refresh failed: ", conditionMessage(e)))
    TRUE
  })
}

## UI ==========================================================================

ui <- fluidPage(
  
  fluidRow(
    column(12,
           dateRangeInput(
             "date_range",
             "Select date range:",
             start = Sys.Date() - 30,
             end = Sys.Date(),
             min = as.Date("2025-12-06"),
             max = Sys.Date() 
           )
    )
  ),
  
  fluidRow(
    column(12,
           if (!INTERACTIVE) plotOutput("combined_plot", height = 800)
           else plotlyOutput("combined_plot", height = 800)
    )
  )
)

## SERVER ======================================================================

server <- function(input, output, session) {
  
  ts_data <- reactive({
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
        parm_name == "Temperature" ~ value * 9/5 + 32
      )) |>
      select(code, site, timestamp, parm_name_modified, value) |>
      pivot_wider(names_from = parm_name_modified, values_from = value) |>
      clean_names() |>
      mutate(water_temperature = if_else(depth > 0, water_temperature, NA)) |>
      mutate(site = factor(site, levels = unique(gages$site))) |>
      mutate(timestamp = with_tz(timestamp, tzone = "America/Los_Angeles"))
  })
  
  # Filter by date range
  filtered_df <- reactive({
    req(df_pivot())
    df <- df_pivot()
    
    if (!is.null(input$date_range)) {
      start_ts <- as.POSIXct(input$date_range[1], tz = "America/Los_Angeles")
      end_ts   <- as.POSIXct(input$date_range[2], tz = "America/Los_Angeles") + hours(23) + minutes(59) + seconds(59)
      
      df <- df |> filter(timestamp >= start_ts, timestamp <= end_ts)
    }
    
    df
  })
  
  # Latest values for depth labels
  df_latest <- reactive({
    filtered_df() |> group_by(code, site) |> filter(timestamp == max(timestamp)) |> ungroup()
  })
  
  output$combined_plot <- if (!INTERACTIVE) {
    
    renderPlot({
      
      base_df  <- filtered_df()
      label_df <- df_latest()
      req(nrow(base_df) > 0, nrow(label_df) > 0)
      
      sites <- unique(base_df$site)
      site_colors <- RColorBrewer::brewer.pal(n = length(sites), name = "Paired")
      names(site_colors) <- sites
      
      # ---- Clean depth data ----
      base_df_depth <- base_df |> filter(!is.na(depth))
      
      # Depth plot
      p_depth <- ggplot(base_df_depth, aes(x = timestamp, y = depth, color = site)) +
        geom_line() +
        geom_text_repel(
          data = label_df |> filter(!is.na(depth)),
          aes(y = depth, label = sprintf("%.1f", depth)),
          hjust = 1
        ) +
        scale_color_manual(values = site_colors, na.value = "grey50") +
        theme_minimal() +
        labs(y = "Depth (ft)", x = NULL, color = "Gage") +
        theme(legend.position = "bottom", legend.box = "horizontal")
      
      # ---- Clean temperature data ----
      temp_long <- base_df |>
        pivot_longer(c(air_temperature, water_temperature),
                     names_to = "parameter",
                     values_to = "temperature") |>
        mutate(
          parameter = case_when(
            parameter == "air_temperature"   ~ "Air Temperature",
            parameter == "water_temperature" ~ "Water Temperature"
          )
        ) |> 
        filter(!is.na(temperature))
      
      # Temperature plot
      p_temp <- ggplot(temp_long, aes(x = timestamp, y = temperature, color = site)) +
        geom_line(aes(linetype = parameter)) +
        scale_color_manual(values = site_colors, na.value = "grey50") +
        scale_linetype_manual(values = c("Water Temperature" = "solid",
                                         "Air Temperature"   = "dashed")) +
        theme_minimal() +
        labs(y = "Temperature (°F)", x = NULL, color = "Gage", linetype = "Parameter") +
        theme(legend.position = "bottom", legend.box = "horizontal")
      
      # ---- Combine with patchwork ----
      p_depth / p_temp + plot_layout(axes = "collect_x")
    })
    
  } else {
    
    renderPlotly({
      
      base_df <- filtered_df()
      req(nrow(base_df) > 0)
      
      sites <- unique(gages$site)
      site_colors <- RColorBrewer::brewer.pal(n = length(sites), name = "Paired")
      names(site_colors) <- sites
      
      # ---- Depth plot ----
      p_depth <- plot_ly()
      for (s in sites) {
        df_s <- base_df |> filter(site == s)
        
        # Depth trace
        p_depth <- add_trace(
          p_depth,
          x = df_s$timestamp,
          y = df_s$depth,
          type = "scatter",
          mode = "lines",
          name = s,
          legendgroup = s,
          line = list(dash = "solid", color = site_colors[s]),
          text = paste(s, "\nDepth", sprintf("%.1f ft", df_s$depth)),
          hoverinfo = "text+x",
          connectgaps = FALSE
        )
      }
      
      # ---- Temperature plot ----
      p_temp <- plot_ly()
      for (s in sites) {
        df_s <- base_df |> filter(site == s)
        
        # Water temp trace (solid)
        p_temp <- add_trace(
          p_temp,
          x = df_s$timestamp,
          y = df_s$water_temperature,
          type = "scatter",
          mode = "lines",
          name = s,
          legendgroup = s,
          showlegend = FALSE,  # merge with depth legend
          line = list(dash = "solid", color = site_colors[s]),
          text = paste(s, "\nWater Temperature", sprintf("%.1f °F", df_s$water_temperature)),
          hoverinfo = "text+x",
          connectgaps = FALSE
        )
        
        # Air temp trace (dotted)
        p_temp <- add_trace(
          p_temp,
          x = df_s$timestamp,
          y = df_s$air_temperature,
          type = "scatter",
          mode = "lines",
          name = paste0(s, " (ambient)"),
          legendgroup = paste0(s, "_ambient"),
          line = list(dash = "dot", color = site_colors[s]),
          text = paste(s, "\nAir Temperature", sprintf("%.1f °F", df_s$air_temperature)),
          hoverinfo = "text+x",
          connectgaps = FALSE
        )
      }
      
      # ---- Stack plots with shared x-axis ----
      subplot(p_depth, p_temp, nrows = 2, shareX = TRUE, titleY = TRUE) |>
        layout(
          margin = list(b = 80),
          dragmode = "zoom",
          xaxis = list(title = "", fixedrange = FALSE),
          yaxis = list(title = "Depth (ft)", fixedrange = TRUE),
          yaxis2 = list(title = "Temperature (°F)", fixedrange = TRUE),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25)
        )
    })
    
  }
  
}

shinyApp(ui, server)
