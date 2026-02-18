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

nasa_cache_dir <- file.path(getwd(), ".cache", "nasa")
dir.create(nasa_cache_dir, recursive = TRUE, showWarnings = FALSE)
cache_data_file_precip <- file.path(usgs_cache_dir, "precip_ts.rds")
cache_lock_file_precip <- file.path(usgs_cache_dir, "refresh.lock")
rds_url_precip <- "https://github.com/flowwest/kuhlanapo-gage-dashboard/raw/main/data/precip_ts.rds"

FORCE_LOCAL <- F

ZERO_RUMSEY_NAVD88 <- 1320.74

idw_obj <- readRDS(here::here("data-raw", "idw_precomputed.rds"))
kuhlanapo_bnd <- readRDS(here::here("data", "kuhlanapo_bnd.rds"))

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
    
    uiOutput("date_range_selector"),
    
    uiOutput("top_metric_selector"),

    conditionalPanel(
      condition = "input.top_metric == 'gw_depth_ft' || input.top_metric == 'gwe_ft_navd88'",
      
      shinyWidgets::radioGroupButtons(
        "transect_select",
        label = "",
        choices = c(
          "All Transects" = "all",
          "A" = "A",
          "B" = "B",
          "C" = "C"
        ),
        selected = "all",
        size = "sm"
      )
    ),
    
    conditionalPanel(
      condition = "input.top_metric == 'gw_contour'",
      sliderInput(
        "gw_time",
        "",
        min = as.POSIXct("2025-12-09 00:00", tz = "America/Los_Angeles"), 
        max = as.POSIXct("2026-02-03 23:59", tz = "America/Los_Angeles"),
        value = as.POSIXct("2026-01-01 00:00", tz = "America/Los_Angeles"),
        timeFormat = "%Y-%m-%d %H:%M",
        step = 900, # 1 hour steps
        animate = FALSE,
        ticks = FALSE,
        width = "300px"
      )
    )
  ),
  
  fluidRow(
    column(
      12,
      conditionalPanel(
        condition = "input.top_metric == 'gw_contour'",
        plotOutput("gw_contour_plot", height = "80vh")
      ),
      conditionalPanel(
        condition = "input.top_metric != 'gw_contour'",
        plotlyOutput("combined_plot", height = "90vh")
      )
    )
  )
)

## SERVER ======================================================================

server <- function(input, output, session) {
  
  ################
  # URL PARAMETERS
  ################
  
  # Parse URL query string once at session start
  query <- reactive({
    parseQueryString(session$clientData$url_search)
  })
  
  url_mode <- reactive({
    q <- query()
    if (!is.null(q$mode) && q$mode %in% c("stage", "piezo")) {
      message("URL parameters set mode to ", q$mode)
      q$mode
    } else {
      "default"
    }
  })
  
  url_date_range <- reactive({
    q <- query()
    if (!is.null(q$start) && !is.null(q$end)) {
      c(as.Date(q$start), as.Date(q$end))
    } else {
      NULL
    }
  })
  
  url_show_precip <- reactive({
    q <- query()
    
    if (is.null(q$show_precip)) {
      FALSE
    } else if (q$show_precip == "" || q$show_precip %in% c("1", "true", "TRUE")) {
      TRUE
    } else if (q$show_precip %in% c("0", "false", "FALSE")) {
      FALSE
    } else {
      FALSE
    }
  })
  
  output$date_range_selector <- renderUI({
    
    mode <- url_mode()
    
    dateRangeInput(
      "date_range",
      label = "",
      start = switch(mode,
                     stage = Sys.Date() - 30,
                     piezo = as.Date("2025-12-09"),
                     Sys.Date() - 30),
      end = switch(mode,
                   stage = Sys.Date(),
                   piezo = as.Date("2026-02-03"),
                   Sys.Date()),
      min = as.Date("2025-12-06"),
      max = Sys.Date()
    )
  })
  
  output$top_metric_selector <- renderUI({
    
    mode <- url_mode()
    
    choices <- switch(
      mode,
      stage = c(
        "Depth" = "depth",
        "Water Surface" = "wse_ft_navd88"
      ),
      piezo = c(
        "Elevation"  = "gwe_ft_navd88",
        "Depth to GW" = "gw_depth_ft",
        "Map" = "gw_contour"
      ),
      c(
        "Depth" = "depth",
        "Water Surface" = "wse_ft_navd88"
      )
    )
    
    shinyWidgets::radioGroupButtons(
      inputId = "top_metric",
      label   = "",
      choices = choices,
      selected = choices[[1]],
      size = "sm"
    )
  })
  
  #####
  # APP
  #####

  top_metric <- reactive({
    
    req(input$top_metric)
    
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
  
  # Slider for GW Contour time
  gw_contour_time <- reactive({
    if(input$top_metric != "gw_contour") return(NULL)
    
    df <- filtered_df()
    req(nrow(df) > 0)
    
    slider_min <- min(df$timestamp, na.rm = TRUE)
    slider_max <- max(df$timestamp, na.rm = TRUE)
    
    input$gw_contour_t0 %||% slider_min
  })
  
  ts_data <- reactive({
    
    if((file.exists(here::here("data/gage_data.rds"))) && FORCE_LOCAL) {
      message("FORCE_LOCAL is on; using data/gage_data.rds locally")
      return(readRDS(here::here("data/gage_data.rds")))
    }
    
    if (!file.exists(cache_data_file_hydrovu)) {
      message("[cache:bootstrap] no cache → downloading initial copy")
      download.file(rds_url_hydrovu, cache_data_file_hydrovu, mode = "wb", quiet = TRUE)
    }
    
    df <- read_cached_data(cache_data_file_hydrovu)
    
    if (cache_is_stale(cache_data_file_hydrovu)) {
      message("[cache:decision] cache is stale → triggering async refresh")
      refresh_cache_async(cache_lock_file_hydrovu, rds_url_hydrovu)
    } else {
      message("[cache:decision] cache is fresh → no refresh")
    }
    
    df |> glimpse()
  })
  
  ll_data <- reactive({
    
    if((file.exists(here::here("data/usgs_lake_level-11450000.rds"))) && FORCE_LOCAL) {
      message("FORCE_LOCAL is on; using data/usgs_lake_level-11450000.rds locally")
      return(readRDS(here::here("data/usgs_lake_level-11450000.rds")))
    }

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
  
  precip_data <- reactive({
    
    req(url_show_precip())

    if((file.exists(here::here("data/precip_ts.rds"))) && FORCE_LOCAL) {
      message("FORCE_LOCAL is on; using data/precip_ts.rds locally")
      return(readRDS(here::here("data/precip_ts.rds")))
    }
    
    if (!file.exists(cache_data_file_precip)) {
      message("[cache:bootstrap] no cache → downloading initial copy")
      download.file(rds_url_precip, cache_data_file_precip, mode = "wb", quiet = TRUE)
    }
    
    df <- read_cached_data(cache_data_file_precip)
    
    if (cache_is_stale(cache_data_file_precip)) {
      message("[cache:decision] cache is stale → triggering async refresh")
      refresh_cache_async(cache_lock_file_precip, rds_url_precip)
    } else {
      message("[cache:decision] cache is fresh → no refresh")
    }
    
    df
  })
  
  df_pivot <- reactive({
    req(input$date_range)
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
      select(-name, -gse_ft_navd88, -tdx_ft_navd88)
    })
  
  filtered_df <- reactive({
    df <- df_pivot()
    req(df)
    
    start_ts <- as.POSIXct(input$date_range[1], tz = "America/Los_Angeles")
    end_ts   <- as.POSIXct(input$date_range[2], tz = "America/Los_Angeles") +
      hours(23) + minutes(59) + seconds(59)
    
    df |> filter(timestamp >= start_ts, timestamp <= end_ts)
  })

  filtered_precip_df <- reactive({
    
    req(url_show_precip())
    
    df <- precip_data()
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
  
  sites_piezo_filtered <- reactive({
    
    req(sites_piezo)
    
    transect_map <- list(
      A = c("PZ-A1", "PZ-A2", "PZ-A3"),
      B = c("PZ-B1", "PZ-B2", "PZ-B3", "PZ-B4"),
      C = c("PZ-B2", "PZ-C1", "PZ-C2", "PZ-C3")
    )
    
    if (is.null(input$transect_select) || input$transect_select == "all") {
      return(sites_piezo)
    }
    
    sites_piezo |> 
      filter(code %in% transect_map[[input$transect_select]])
  })
  
  output$combined_plot <- renderPlotly({ 
    
    p <- plot_ly() |>
      config(displayModeBar = TRUE)
    
    base_df <- filtered_df()
    req(nrow(base_df) > 0)
    tm <- top_metric()
    ycol <- tm$col
    req(ycol)
    
    min_lake <- 1320.74 # as defined on USGS Clear Lake Lakeport gage
    max_lake <- min_lake + 7.56
    
    if(url_show_precip()) {
      precip_df <- filtered_precip_df() |> 
        filter(site == "KPD")
    }
    
    if (isTRUE(ycol %in% c("wse_ft_navd88", "gwe_ft_navd88") && "lake_level" %in% names(base_df))) {
      
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
       #  fillcolor = "rgba(0,0,255,0.2)",  # semi-transparent blue
        fillcolor = "rgba(0,0,0,0.2)",
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
        line = list(dash = "dash", color = "rgba(0,0,0,0.2)"),
        hoverinfo = "text",
        name = "Full Lake",
        legendgroup = "lake_level",
        text = "Full Lake",
        showlegend = TRUE
      )
      
      
    }
    
    if(isTRUE(ycol %in% c("depth", "wse_ft_navd88"))) {
      
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
      
  }  else if(top_metric()$col %in% c("gw_depth_ft", "gwe_ft_navd88")) {
      
      placeholder_value <- mean(base_df[[ycol]], na.rm=T)
      
      min_y <- switch(input$top_metric,
                      "gw_depth_ft" = max(base_df[[ycol]], na.rm=T),
                      "gwe_ft_navd88" = min(base_df[[ycol]], na.rm=T))
      max_y <- switch(input$top_metric,
                      "gw_depth_ft" = min(0, base_df[[ycol]], na.rm=T),
                      "gwe_ft_navd88" = max(base_df[[ycol]], na.rm=T))

      for (s in sites_piezo_filtered()$code) {
        df_s <- base_df |> filter(code == s)
        has_data <- nrow(df_s) > 0 && any(!is.na(df_s[[ycol]]))
        
        p <- add_trace(
          p,
          x = if (has_data) df_s$timestamp else input$date_range[2],
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
      }
    
    if(url_show_precip()){    
    
      p <- add_trace(
        p,
        x = precip_df$timestamp,           # left edge of the bar
        y = precip_df$precip_in,
        type = "bar",
        name = "Precipitation",
        legendgroup = "precip",
        text = NULL,                       # remove bar labels
        hovertext = paste0(
          format(precip_df$timestamp, "%b %d, %Y %H:%M"), " - ", format(precip_df$timestamp  + lubridate::hours(1), "%b %d, %Y %H:%M"), "<br>",
          "Precipitation: ", sprintf("%.2f in", precip_df$precip_in)
        ),
        hoverinfo = "text",
        marker = list(color = "rgba(64,64,64,0.5)"),
        width = 3600 * 1000,               # 1 hour in ms
        yaxis = "y3",
        offset = 0                         # align left side of bar with x
      )
    }
      
    y_domain <- if (top_metric()$col %in% c("gw_depth_ft", "gwe_ft_navd88")) {
      if (url_show_precip()) c(0, 0.825) else c(0, 1)
    } else {
      if (url_show_precip()) c(0.3, 0.825) else c(0.3, 1)
    }
    
  # add secondary Rumsey axis where relevant
    if (ycol %in% c("wse_ft_navd88", "gwe_ft_navd88")) {
      
      if (is.infinite(min_y) || is.infinite(max_y) || is.na(min_y) || is.na(max_y)) {
        min_y <- min(base_df[[ycol]], na.rm = TRUE)
        max_y <- max(base_df[[ycol]], na.rm = TRUE)
      }
      
    # rumsey_ticks <- c(seq(0, 10), 7.56)
    rumsey_ticks <- c(pretty(c(min_y, max_y)) - ZERO_RUMSEY_NAVD88, 7.56)
      
    rumsey_axis <- list(
      title = "Rumsey Elevation (ft)",
      overlaying = "y",
      side = "right",
      anchor = "x",
      domain = y_domain,
      range = c(min_y, max_y),
      position = 0.98,
      tickvals = rumsey_ticks + ZERO_RUMSEY_NAVD88,
      ticktext = as.character(sprintf("%.2f", rumsey_ticks)),
      fixedrange = TRUE,
      showticklabels = TRUE,
      showline = TRUE,
      automargin = TRUE
    )

  } else {
    rumsey_axis <- NULL
  }
    
  # add dummy trace so rumsey axis appears
    if (!is.null(rumsey_axis)) {
      p <- add_trace(
        p,
        x = base_df$timestamp[1],
        y = base_df[[ycol]][1],
        type = "scatter",
        mode = "lines",
        line = list(color = "rgba(0,0,0,0)"),
        showlegend = FALSE,
        hoverinfo = "none",
        yaxis = "y4"
      )
    }
    
    
  # ---- Layout with dynamic vertical sizing ----
  layout_args <- list(
    dragmode = "zoom",
    xaxis = list(
      title = "",
      domain = c(0, 1),
      side = "top",
      showspikes = TRUE
    ),
    xaxis2 = list(
      overlaying = "x",
      side = "top"
    ),
    xaxis3 = if (url_show_precip()) list(
      overlaying = "x",
      side = "top"
    ) else NULL,
    yaxis = list(
      title = tm$label,
      domain = y_domain,
      fixedrange = TRUE,
      range = c(min_y, max_y)
    ),
    yaxis2 = list(
      title = "Temperature (°F)",
      domain = c(0, 0.3),
      fixedrange = TRUE
    ),
    yaxis3 = if (url_show_precip()) list(
      title = "Hourly Precip (in)",
      domain = c(0.9, 1.0),
      fixedrange = TRUE
    ) else NULL,
    legend = list(
      orientation = "h",
      x = 0.5,
      yref = "container",
      xanchor = "center",
      y = 0,
      automargin = TRUE
    ),
    margin = list(t = 40, b = 60, l = 60, r = if (!is.null(rumsey_axis)) 60 else 20)
  )
    
  if (!is.null(rumsey_axis)) layout_args$yaxis4 <- rumsey_axis
  
  do.call(layout, c(list(p), layout_args))
  
 })
  
  # Reactive: IDW interpolation at selected time
  gw_contour_df <- reactive({
    req(url_mode() == "piezo")
    req(input$gw_time)
    df <- df_pivot() |> 
      filter(category == "Piezometer") |> 
      transmute(code, timestamp, value = gwe_ft_navd88)
    req(nrow(df) > 0)
    
    # Call IDW function
    out <- interpolate_idw_at_time(idw_obj, df, t0 = input$gw_time, return_matrix=F) 
    
    idw_obj$grid |>
      mutate(value = as.numeric(out))
    
  })
  
  # Output: GW Contour ggplot
  output$gw_contour_plot <- renderPlot({
    req(url_mode() == "piezo")
    req(gw_contour_df())
    
    gw_contour_df() |>
    ggplot(aes(x = x, y = y)) +
      geom_polygon(data = kuhlanapo_bnd, aes(x = X, y = Y), fill = "#eeeeee") +
      geom_contour(aes(z = value, color = after_stat(level)),
                   linewidth = 1,
                   binwidth = 1,
                   na.rm = TRUE)  +
      metR::geom_label_contour(aes(z = value, label = after_stat(level), color = after_stat(level)),
                               binwidth = 1,
                               skip = 0,
                               size = 12 / .pt,
                               na.rm = TRUE)  +
      geom_point(data=idw_obj$sites, aes(fill = id), size = 12, shape = 21) +
      geom_text(data=idw_obj$sites, aes(label = substr(id, 4, 5)), size = 12 / .pt) +
      coord_equal(xlim = c(min(gw_contour_df()$x), max(gw_contour_df()$x)),
                  ylim = c(min(gw_contour_df()$y), max(gw_contour_df()$y))) +
      scale_y_continuous(expand = expansion(add = c(400, 1200))) +
      scale_x_continuous(expand = expansion(add = c(400, 400))) +
      scale_fill_manual(name = "", values = piezo_colors) +
      scale_color_viridis_c(name = "ft NAVD88",
                            limits = c(min(df_pivot()$gwe_ft_navd88, na.rm = T), 
                                       max(df_pivot()$gwe_ft_navd88, na.rm = T))) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
           axis.ticks = element_blank(),
           axis.text = element_blank(),
           axis.title = element_blank(),
           legend.position = "none")
  })
  
}

shinyApp(ui, server)
