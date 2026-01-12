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
library(future)

## CONFIG ======================================================================

INTERACTIVE <- TRUE

# refresh cache if >24 hours old or if it's passed 8 AM on a new day
CACHE_TTL <- 24 * 60 * 60
FORCE_REFRESH_TIME_PT <- hm("08:00")

cache_dir <- file.path(getwd(), ".cache", "hydrovu")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

cache_data_file <- file.path(cache_dir, "gage_data.rds")
cache_lock_file <- file.path(cache_dir, "refresh.lock")

rds_url <- "https://github.com/flowwest/kuhlanapo-gage-dashboard/raw/main/data/gage_data.rds"

message(
  sprintf(
    "[cache:init] dir=%s | ttl=%s sec (%.1f hrs) | forced_refresh=07:30 PT",
    normalizePath(cache_dir, winslash = "/", mustWork = FALSE),
    CACHE_TTL,
    CACHE_TTL / 3600
  )
)

message(
  sprintf(
    "[cache:init] data_file=%s",
    normalizePath(cache_data_file, winslash = "/", mustWork = FALSE)
  )
)

## DATA DEFINITIONS =============================================================

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
  if (!file.exists(cache_data_file)) {
    message("[cache:read] no cache file on disk")
    return(NULL)
  }
  
  message("[cache:read] reading cache from disk")
  
  tryCatch(
    readRDS(cache_data_file),
    error = function(e) {
      message("[cache:read] FAILED: ", conditionMessage(e))
      NULL
    }
  )
}

write_cache <- function(df) {
  tmp <- paste0(cache_data_file, ".tmp")
  
  message(
    sprintf(
      "[cache:write] writing %s rows → %s",
      format(nrow(df), big.mark = ","),
      basename(cache_data_file)
    )
  )
  
  saveRDS(df, tmp)
  file.rename(tmp, cache_data_file)
  
  message("[cache:write] write complete")
}

cache_is_stale <- function() {
  
  if (!file.exists(cache_data_file)) {
    message("[cache:check] cache file missing → stale=TRUE")
    return(TRUE)
  }
  
  file_info <- file.info(cache_data_file)
  mtime <- with_tz(file_info$mtime, "America/Los_Angeles")
  now_pt <- with_tz(Sys.time(), "America/Los_Angeles")
  
  # TTL-based staleness
  age_sec <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "secs"))
  ttl_stale <- age_sec > CACHE_TTL
  
  # Forced daily refresh boundary
  today_refresh_time <- as.POSIXct(
    paste(Sys.Date(), FORCE_REFRESH_TIME_PT),
    tz = "America/Los_Angeles"
  )
  
  force_stale <- now_pt >= today_refresh_time && mtime < today_refresh_time
  
  message(
    sprintf(
      paste0(
        "[cache:check] age=%.1f hrs | ttl=%.1f hrs | ",
        "mtime=%s | forced_boundary=%s | ",
        "ttl_stale=%s | forced_stale=%s"
      ),
      age_sec / 3600,
      CACHE_TTL / 3600,
      format(mtime, "%Y-%m-%d %H:%M:%S %Z"),
      format(today_refresh_time, "%Y-%m-%d %H:%M:%S %Z"),
      ttl_stale,
      force_stale
    )
  )
  
  ttl_stale || force_stale
}

refresh_cache_async <- function() {
  
  if (file.exists(cache_lock_file)) {
    message("[cache:refresh] skipped (lock exists)")
    return(invisible(FALSE))
  }
  
  writeLines(as.character(Sys.getpid()), cache_lock_file)
  
  message(
    sprintf(
      "[cache:refresh] started (pid=%s)",
      Sys.getpid()
    )
  )
  
  future::future({
    
    on.exit({
      unlink(cache_lock_file)
      message("[cache:refresh] lock released")
    }, add = TRUE)
    
    tmp_file <- tempfile(fileext = ".rds")
    
    tryCatch({
      
      message("[cache:refresh] downloading upstream RDS")
      download.file(rds_url, tmp_file, mode = "wb", quiet = TRUE)
      
      new_data <- readRDS(tmp_file)
      old_data <- read_cached_data()
      
      message(
        sprintf(
          "[cache:refresh] new_rows=%s | old_rows=%s",
          format(nrow(new_data), big.mark = ","),
          ifelse(is.null(old_data), "0", format(nrow(old_data), big.mark = ","))
        )
      )
      
      combined <- if (!is.null(old_data)) {
        bind_rows(old_data, new_data) |>
          distinct(name, timestamp, parm_name, .keep_all = TRUE)
      } else {
        new_data
      }
      
      message(
        sprintf(
          "[cache:refresh] combined_rows=%s",
          format(nrow(combined), big.mark = ",")
        )
      )
      
      write_cache(combined)
      
      message("[cache:refresh] refresh SUCCESS")
      
    }, error = function(e) {
      message("[cache:refresh] FAILED: ", conditionMessage(e))
    })
    
    TRUE
  })
}

## UI ==========================================================================

ui <- fluidPage(
  
  fluidRow(
    column(
      12,
      dateRangeInput(
        "date_range",
        label = "",
        start = Sys.Date() - 30,
        end = Sys.Date(),
        min = as.Date("2025-12-06"),
        max = Sys.Date()
      )
    )
  ),
  
  fluidRow(
    column(
      12,
      if (!INTERACTIVE)
        plotOutput("combined_plot", height = "90vh")
      else
        plotlyOutput("combined_plot", height = "90vh")
    )
  )
)

## SERVER ======================================================================

server <- function(input, output, session) {
  
  ts_data <- reactive({
    
    if (!file.exists(cache_data_file)) {
      message("[cache:bootstrap] no cache → downloading initial copy")
      download.file(rds_url, cache_data_file, mode = "wb", quiet = TRUE)
    }
    
    df <- read_cached_data()
    
    if (cache_is_stale()) {
      message("[cache:decision] cache is stale → triggering async refresh")
      refresh_cache_async()
    } else {
      message("[cache:decision] cache is fresh → no refresh")
    }
    
    df
  })
  
  df_pivot <- reactive({
    ts_data() |>
      filter(parm_name %in% c("Depth", "Temperature")) |>
      mutate(parm_name_modified = case_when(
        parm_name == "Temperature" & type == "vulink" ~ "Air Temperature",
        parm_name == "Temperature" ~ "Water Temperature",
        TRUE ~ parm_name
      )) |>
      mutate(value = case_when(
        parm_name == "Depth" ~ if_else(value > 0, value / 0.3048, 0),
        parm_name == "Temperature" ~ value * 9 / 5 + 32
      )) |>
      select(code, site, timestamp, parm_name_modified, value) |>
      pivot_wider(names_from = parm_name_modified, values_from = value) |>
      clean_names() |>
      # if troll is freezing, depth reading is invalid
      mutate(depth = if_else(water_temperature > 32, depth, NA)) |>
      # don't show troll temp if there is no water
      mutate(water_temperature = if_else(depth > 0, water_temperature, NA)) |>
      mutate(site = factor(site, levels = unique(gages$site))) |>
      mutate(timestamp = with_tz(timestamp, "America/Los_Angeles"))
  })
  
  filtered_df <- reactive({
    df <- df_pivot()
    req(df)
    
    start_ts <- as.POSIXct(input$date_range[1], tz = "America/Los_Angeles")
    end_ts   <- as.POSIXct(input$date_range[2], tz = "America/Los_Angeles") +
      hours(23) + minutes(59) + seconds(59)
    
    df |> filter(timestamp >= start_ts, timestamp <= end_ts)
  })
  
  df_latest <- reactive({
    filtered_df() |>
      group_by(code, site) |>
      filter(timestamp == max(timestamp)) |>
      ungroup()
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
      
      # ---- Depth traces ----
      p <- plot_ly()
      for (s in sites) {
        df_s <- base_df |> filter(site == s)
        
        # Depth trace
        p <- add_trace(
          p,
          x = df_s$timestamp,
          y = df_s$depth,
          type = "scatter",
          mode = "lines",
          name = s,
          legendgroup = s,
          line = list(dash = "solid", color = site_colors[s]),
          text = paste(s, "\nDepth", sprintf("%.1f ft", df_s$depth)),
          hoverinfo = "text+x",
          connectgaps = FALSE,
          yaxis = "y"
        )
      }
      
      # ---- Temperature traces ----
      for (s in sites) {
        df_s <- base_df |> filter(site == s)
        
        # Water temperature (solid)
        p <- add_trace(
          p,
          x = df_s$timestamp,
          y = df_s$water_temperature,
          type = "scatter",
          mode = "lines",
          name = s,
          legendgroup = s,
          showlegend = FALSE, # merge with depth legend
          line = list(dash = "solid", color = site_colors[s]),
          text = paste(s, "\nWater Temperature", sprintf("%.1f °F", df_s$water_temperature)),
          hoverinfo = "text+x",
          connectgaps = FALSE,
          yaxis = "y2"
        )
        
        # Air temperature (dotted)
        p <- add_trace(
          p,
          x = df_s$timestamp,
          y = df_s$air_temperature,
          type = "scatter",
          mode = "lines",
          name = paste0(s, " (ambient)"),
          legendgroup = paste0(s, "_ambient"),
          line = list(dash = "dot", color = site_colors[s]),
          text = paste(s, "\nAir Temperature", sprintf("%.1f °F", df_s$air_temperature)),
          hoverinfo = "text+x",
          connectgaps = FALSE,
          yaxis = "y2"
        )
      }
      
      # ---- Layout with dynamic vertical sizing ----
      p |> layout(
        dragmode = "zoom",
        xaxis = list(
          title = "",
          domain = c(0, 1),
          side = "top",
          showspikes = TRUE
        ),
        xaxis2 = list(
          overlaying = "x",
          side = "top",
          showticklabels = TRUE
        ),
        yaxis = list(
          title = "Depth (ft)",
          domain = c(0.5, 1),
          fixedrange = TRUE
        ),
        yaxis2 = list(
          title = "Temperature (°F)",
          domain = c(0, 0.5),
          fixedrange = TRUE
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          yref = "container",
          xanchor = "center",
          y = 0,
          automargin = TRUE
        ),
        margin = list(t = 40, b = 60, l = 60, r = 20)
      )
    })
    
    
  }
  
}

shinyApp(ui, server)
