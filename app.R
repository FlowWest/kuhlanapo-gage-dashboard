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

source(here::here("global.R"))

## CONFIG ======================================================================

INTERACTIVE <- TRUE

# refresh cache if >24 hours old or if it's passed 8 AM on a new day
CACHE_TTL <- 24 * 60 * 60
FORCE_REFRESH_TIME_PT <- hm("08:00")

cache_dir_hydrovu <- file.path(getwd(), ".cache", "hydrovu")
dir.create(cache_dir_hydrovu, recursive = TRUE, showWarnings = FALSE)
cache_data_file_hydrovu <- file.path(cache_dir_hydrovu, "gage_data.rds")
cache_lock_file_hydrovu <- file.path(cache_dir_hydrovu, "refresh.lock")
rds_url_hydrovu <- "https://github.com/flowwest/kuhlanapo-gage-dashboard/raw/main/data/gage_data.rds"

usgs_cache_dir <- file.path(getwd(), ".cache", "usgs")
dir.create(usgs_cache_dir, recursive = TRUE, showWarnings = FALSE)
cache_data_file_lakelevel <- file.path(usgs_cache_dir, "usgs_lake_level_11450000.rds")
cache_lock_file_lakelevel <- file.path(usgs_cache_dir, "refresh.lock")
rds_url_lakelevel <- "https://github.com/flowwest/kuhlanapo-gage-dashboard/raw/main/data/usgs_lake_level_11450000.rds"

FORCE_LOCAL <- T

message(
  sprintf(
    "[cache:init] dir=%s | ttl=%s sec (%.1f hrs) | forced_refresh=07:30 PT",
    normalizePath(cache_dir_hydrovu, winslash = "/", mustWork = FALSE),
    CACHE_TTL,
    CACHE_TTL / 3600
  )
)

message(
  sprintf(
    "[cache:init] data_file=%s",
    normalizePath(cache_data_file_hydrovu, winslash = "/", mustWork = FALSE)
  )
)

## DATA DEFINITIONS =============================================================

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

read_cached_data <- function(cache_data_file) {
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

write_cache <- function(df, cache_data_file) {
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

cache_is_stale <- function(cache_data_file) {
  
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

refresh_cache_async <- function(cache_lock_file, rds_url) {
  
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
      old_data <- read_cached_data(cache_data_file_hydrovu)
      
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
      
      write_cache(combined, cache_data_file_hydrovu)
      
      message("[cache:refresh] refresh SUCCESS")
      
    }, error = function(e) {
      message("[cache:refresh] FAILED: ", conditionMessage(e))
    })
    
    TRUE
  })
}

## UI ==========================================================================

ui <- fluidPage(
  
  tags$style(HTML("
  .flow-fullwidth > * {
    width: auto !important;
  }
")),
  
  flowLayout(
    class = "flow-fullwidth",
    style = "width: 95vw;",
    
    dateRangeInput(
      "date_range",
      label = "",
      start = Sys.Date() - 30,
      end = Sys.Date(),
      min = as.Date("2025-12-06"),
      max = Sys.Date()
    ),
    
    shinyWidgets::radioGroupButtons(
      "top_metric",
      label = "",
      choices = c(
        "Depth" = "depth",
        "Water Surface" = "wse_ft_navd88",
        "GW Elev" = "gwe_ft_navd88",
        "GW Depth" = "gw_depth_ft"
      ),
      selected = "depth",
      size = "sm"
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

  top_metric <- reactive({
    switch(
      input$top_metric,
      depth = list(
        col   = "depth",
        label = "Depth (ft)",
        fmt   = function(x) sprintf("%.1f", x)
      ),
      wse_ft_navd88 = list(
        col   = "wse_ft_navd88",
        label = "WSE (ft NAVD88)",
        fmt   = function(x) sprintf("%.2f", x)
      ),
      gw_depth_ft = list(
        col   = "gw_depth_ft",
        label = "Surface to Groundwater Depth (ft)",
        fmt   = function(x) sprintf("%.2f", x)
      ),
      gwe_ft_navd88 = list(
        col   = "gwe_ft_navd88",
        label = "Groundwater Elevation (ft NAVD88)",
        fmt   = function(x) sprintf("%.2f", x)
      )
    )
  })
  
  ts_data <- reactive({
    
    if((file.exists(here::here("data/gage_data.rds"))) && FORCE_LOCAL) {
      message("FORCE_LOCAL is on; using data/gage_data.rds locally")
      return(readRDS(here::here("data/gage_data.rds")))
    }
    
    if (!file.exists(cache_data_file_hydrovu)) {
      message("[cache:bootstrap] no cache → downloading initial copy")
      download.file(rds_url, cache_data_file_hydrovu, mode = "wb", quiet = TRUE)
    }
    
    df <- read_cached_data(cache_data_file_hydrovu)
    
    if (cache_is_stale()) {
      message("[cache:decision] cache is stale → triggering async refresh")
      refresh_cache_async(cache_lock_file_hydrovu, rds_url_hydrovu)
    } else {
      message("[cache:decision] cache is fresh → no refresh")
    }
    
    df
  })
  
  ll_data <- reactive({
    
    if (!file.exists(cache_data_file_lakelevel)) {
      message("[cache:bootstrap] no cache → downloading initial copy")
      download.file(rds_url_lakelevel, cache_data_file_lakelevel, mode = "wb", quiet = TRUE)
    }
    
    df <- read_cached_data(cache_data_file_lakelevel)
    
    if (cache_is_stale(cache_data_file_lakelevel)) {
      message("[cache:decision] cache is stale → triggering async refresh")
      refresh_cache_async(cache_lock_file_lakelevel, rds_url_lakelevel)
    } else {
      message("[cache:decision] cache is fresh → no refresh")
    }
    
    df
  })
  
  df_pivot <- reactive({
    ts_data() |>
      inner_join(sites |> select(code, category)) |>
      filter(parm_name %in% c("Depth", "Temperature")) |>
      mutate(parm_name_modified = case_when(
        parm_name == "Temperature" & type == "vulink" ~ "Air Temperature",
        parm_name == "Temperature" ~ "Water Temperature",
        TRUE ~ parm_name
      )) |>
      # convert units. also, depth readings less than zero are invalid
      mutate(value = case_when(
        parm_name == "Depth" ~ if_else(value > 0, value / 0.3048, 0),
        parm_name == "Temperature" ~ value * 9 / 5 + 32
      )) |>
      select(category, code, site, timestamp, parm_name_modified, value) |>
      pivot_wider(names_from = parm_name_modified, values_from = value) |>
      clean_names() |>
      # if troll is freezing, depth reading is invalid
      group_by(category, code, site) |>
      mutate(depth = if_else((water_temperature > 32) & 
                               coalesce(lag(water_temperature) > 32, TRUE), 
                             depth, NA)) |>
      ungroup() |>
      # don't show troll temp if there is no water
      mutate(water_temperature = if_else(depth > 0, water_temperature, NA)) |>
      mutate(site = factor(site, levels = unique(sensors$site))) |>
      mutate(timestamp = with_tz(timestamp, "America/Los_Angeles")) |>
      #############
      # LAKE LEVELS
      left_join(ll_data() |> select(timestamp, lake_level = value), by = join_by(timestamp)) |>
      ##############################
      # GAGE WATER SURFACE ELEVATION
      left_join(sites |> select(code, twg_elev), by=join_by(code)) |>
      mutate(wse_ft_navd88 = if_else(depth > 0, depth + twg_elev, NA)) |>
      #################################
      # GROUNDWATER DEPTH AND ELEVATION
      # correct piezometer for well depth and calculate piezometer GWE
      inner_join(sensors |> filter(type == "troll") |> select(code, name), by = join_by(code)) |>
      left_join(piezo_meta |> select(name, gse_ft_navd88, tdx_ft_navd88), by = join_by(name)) |>
      # smooth spikes of length one in groundwater depth, eliminate other spikes
      group_by(category, site) |>
      mutate(depth = if_else(category == "Piezometer",
                             case_when((timestamp == min(timestamp)) ~ lead(depth),
                                       (abs(depth - lag(depth)) > 3) & (abs(depth - lead(depth)) > 3) ~ (lag(depth) + lead(depth)) / 2,
                                       (depth > 18) ~ NA,
                                       TRUE ~ depth),
                             depth)) |>
      ungroup() |>
      # calculate groundwater elevation
      mutate(gwe_ft_navd88 = if_else(category == "Piezometer",
                                     tdx_ft_navd88 + depth,
                                     NA),
             gw_depth_ft = if_else(category == "Piezometer",
                                   gse_ft_navd88 - gwe_ft_navd88,
                                   NA)
             ) |>
      select(-name, -gse_ft_navd88, -tdx_ft_navd88) |>
      # join the lake level data
      glimpse()
    
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
  
  output$combined_plot <- renderPlotly({ 
    
    p <- plot_ly() |>
      config(displayModeBar = TRUE)
    
    base_df <- filtered_df()
    req(nrow(base_df) > 0)
    tm <- top_metric()
    ycol <- tm$col
    
    min_lake <- 1320.74 # as defined on USGS Clear Lake Lakeport gage
    max_lake <- min_lake + 7.56
    
    if (ycol %in% c("wse_ft_navd88", "gwe_ft_navd88") & "lake_level" %in% names(base_df)) {
      
      df_ll_plot <- base_df |>
        select(timestamp, lake_level) |>
        distinct()  # collapse duplicates so only one trace
      
      # ---- Lake level traces ----
      p <- add_trace(
        p, 
        x = c(min(df_ll_plot$timestamp), max(df_ll_plot$timestamp)),   # line from start to end
        y = c(min_lake, min_lake),
        type = "scatter",
        mode = "lines",
        line = list(color = "rgba(255,255,255,0)"),
        hoverinfo = "text",
        name = "Min Lake",
        legendgroup = "lake_level",
        text = "Min Lake",
        showlegend = FALSE
      )
      p <- add_trace(
        p,
        x = df_ll_plot$timestamp,
        y = df_ll_plot$lake_level,
        type = "scatter",
        mode = "none",         # no line markers
        fill = "tonexty",      # fill area down to y=0
        fillcolor = "rgba(0,0,255,0.2)",  # semi-transparent blue
        name = "Lake Level (USGS)",
        legendgroup = "lake_level",
        text = paste0("Lake Level (ft NAVD88): ", sprintf("%.2f", df_ll_plot$lake_level)),
        hoverinfo = "text+x",
        yaxis = "y"
      ) 
      p <- add_trace(
        p, 
        x = c(min(df_ll_plot$timestamp), max(df_ll_plot$timestamp)),   # line from start to end
        y = c(max_lake, max_lake),
        type = "scatter",
        mode = "lines",
        line = list(dash = "dash", color = "rgba(0,0,255,0.2)"),
        hoverinfo = "text",
        name = "Full Lake",
        legendgroup = "lake_level",
        text = "Full Lake",
        showlegend = TRUE
      )
      
      
    }
    
    if(ycol %in% c("depth", "wse_ft_navd88")) {
      
      min_y <- switch(input$top_metric,
                      "depth" = 0,
                      "wse_ft_navd88" = min(min_lake, min(base_df[[ycol]], na.rm=T)) )
      max_y <- switch(input$top_metric,
                      "depth" = max(base_df[[ycol]], na.rm=T),
                      "wse_ft_navd88" = max(base_df[[ycol]], na.rm=T))
      
      # ---- Surface water depth/WSE traces ----
      for (s in sites_stage$code) {
        df_s <- base_df |> filter(code == s)
        
        p <- add_trace(
          p,
          x = df_s$timestamp,
          y = df_s[[ycol]],
          type = "scatter",
          mode = "lines",
          name = paste0(site_labels[[s]], ", ", site_descrips[[s]]),
          legendgroup = "channel",
          line = list(dash = "solid", color = site_colors[s]),
          text = paste(
            site_labels[[s]],
            tm$label,
            tm$fmt(df_s[[ycol]])
          ),
          hoverinfo = "text+x",
          connectgaps = FALSE,
          yaxis = "y"
        )
      }
      
      # ---- Temperature traces ----
      for (s in sites_stage$code) {
        df_s <- base_df |> filter(code == s)
        
        # Water temperature (solid)
        p <- add_trace(
          p,
          x = df_s$timestamp,
          y = df_s$water_temperature,
          type = "scatter",
          mode = "lines",
          name = paste0(site_labels[[s]], ", ", site_descrips[[s]]),
          legendgroup = "channel", #s,
          showlegend = FALSE, # merge with depth legend
          line = list(dash = "solid", color = site_colors[s]),
          text = paste(site_labels[[s]], "\nWater Temperature", sprintf("%.1f °F", df_s$water_temperature)),
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
          name = paste0(site_labels[[s]], " (ambient temp.)"),
          legendgroup = "ambient", #paste0(s, "_ambient"),
          line = list(dash = "dot", color = site_colors[s]),
          text = paste(site_labels[[s]], "\nAir Temperature", sprintf("%.1f °F", df_s$air_temperature)),
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
          title = tm$label,
          domain = c(0.5, 1),
          fixedrange = TRUE,
          range = c(min_y, max_y)
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
  
  }  else if(top_metric()$col %in% c("gw_depth_ft", "gwe_ft_navd88")) {
      
      base_df <- filtered_df()
      req(nrow(base_df) > 0)
      
      # ---- Depth traces ----
      tm <- top_metric()
      ycol <- tm$col
      
      placeholder_value <- mean(base_df[[ycol]], na.rm=T)
      
      min_y <- switch(input$top_metric,
                      "gw_depth_ft" = max(base_df[[ycol]], na.rm=T),
                      "gse_ft_navd88" = min(base_df[[ycol]], na.rm=T))
      message("!!!", min(base_df[[ycol]], na.rm=T))
      max_y <- switch(input$top_metric,
                      "gw_depth_ft" = min(0, base_df[[ycol]], na.rm=T),
                      "gse_ft_navd88" = max(base_df[[ycol]], na.rm=T))

      for (s in sites_piezo$code) {
        df_s <- base_df |> filter(code == s)
        has_data <- nrow(df_s) > 0 && any(!is.na(df_s[[ycol]]))
        
        p <- add_trace(
          p,
          x = if (has_data) df_s$timestamp else Sys.time(),
          y = if (has_data) df_s[[ycol]] else placeholder_value,
          type = "scatter",
          mode = "lines",
          name = paste0(site_labels[[s]], ", ", site_descrips[[s]]),
          legendgroup = substring(s, 1, 4),
          line = list(dash = "solid", color = piezo_colors[s]),
          text = paste(
            site_labels[[s]],
            tm$label,
            tm$fmt(df_s[[ycol]])
          ),
          hoverinfo = "text+x",
          connectgaps = FALSE,
          yaxis = "y"#,
          #visible = if (has_data) TRUE else "legendonly"
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
        yaxis = list(
          title = tm$label,
          domain = c(0, 1),
          fixedrange = TRUE,
          range = c(min_y, max_y)
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
  }
  
 })
  
}

shinyApp(ui, server)
