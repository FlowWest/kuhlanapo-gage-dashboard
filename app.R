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
  
  flowLayout(

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
          "Depth"        = "depth",
          "Water Surface"   = "wse_ft_navd88",
          "Flow"        = "flow_cfs"
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
        label = "Water Surface Elevation (ft NAVD88)",
        fmt   = function(x) sprintf("%.2f", x)
      ),
      flow_cfs = list(
        col   = "flow_cfs",
        label = "Flow (cfs)",
        fmt   = function(x) scales::comma(round(x))
      )
    )
  })
  
  ts_data <- reactive({
    
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
  
  # load flow rating curve saved by rating_curves.R
  rating_curves <- readRDS(here::here("data", "rating_curves.rds"))
  
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
        parm_name == "Temperature" ~ value * 9 / 5 + 32
      )) |>
      select(code, site, timestamp, parm_name_modified, value) |>
      pivot_wider(names_from = parm_name_modified, values_from = value) |>
      clean_names() |>
      # if troll is freezing, depth reading is invalid
      group_by(code, site) |>
      mutate(depth = if_else((water_temperature > 32) & 
                               coalesce(lag(water_temperature) > 32, TRUE), 
                             depth, NA)) |>
      ungroup() |>
      # don't show troll temp if there is no water
      mutate(water_temperature = if_else(depth > 0, water_temperature, NA)) |>
      mutate(site = factor(site, levels = unique(gages$site))) |>
      mutate(timestamp = with_tz(timestamp, "America/Los_Angeles")) |>
      # apply flow rating curve:
      nest(.by = c(code, site)) |>
      inner_join(sites, by=join_by(code)) |>
      inner_join(enframe(rating_curves, 
                         name = "code", 
                         value = "rating_curve"),
                 by = join_by(code)) |>
      mutate(result = pmap(list(data, rating_curve, twg_elev),
                           \(d, rc, twe) {
                             d |> mutate(wse_ft_navd88 = if_else(depth > 0, depth + twe, NA),
                                         flow_cfs = approx(x = rc$max_depth,
                                                           y = rc$flow_cfs,
                                                           xout = depth,
                                                           rule = 2:2)$y)})) |>
      select(code, site, result) |>
      unnest(result) |>
      mutate(flow_cfs = if_else(depth == 0, 0, flow_cfs))
    
    # join lake level
    df_ll <- ll_data() |>
      select(timestamp, lake_level = value)
    
    df <- df |>
      left_join(df_ll, by = c("timestamp")) |>
      # don't show the flow prediction if lake level exceeds the thalweg
      mutate(flow_cfs = if_else(lake_level >= (wse_ft_navd88 - depth), NA, flow_cfs))
    
    df
    
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
      
      tm <- top_metric()
      ycol <- sym(tm$col)
      
      base_df_top <- base_df |> filter(!is.na(!!ycol))
      
      p_top <- ggplot(base_df_top, aes(x = timestamp, y = !!ycol, color = site)) +
        geom_line() +
        geom_text_repel(
          data = label_df |> filter(!is.na(!!ycol)),
          aes(
            y = !!ycol,
            label = tm$fmt(!!ycol)
          ),
          hjust = 1
        ) +
        scale_color_manual(values = site_colors, na.value = "grey50") +
        theme_minimal() +
        labs(y = tm$label, x = NULL, color = "Gage") +
        theme(legend.position = "bottom", legend.box = "horizontal")
      
      # ---- Combine with patchwork ----
      p_top / p_temp + plot_layout(axes = "collect_x")
    })
    
  } else {
    
    renderPlotly({
      
      base_df <- filtered_df()
      req(nrow(base_df) > 0)
      
      sites <- unique(gages$site)
      site_colors <- RColorBrewer::brewer.pal(n = length(sites), name = "Paired")
      names(site_colors) <- sites
      
      # ---- Depth traces ----
      tm <- top_metric()
      ycol <- tm$col
      
      # min_lake <- 1318.257 + 3.4 # zero rumsey to NAVD88
      min_lake <- 1320.74 # as defined on USGS Clear Lake Lakeport gage
      max_lake <- min_lake + 7.56
      min_y <- if (input$top_metric == "wse_ft_navd88") min_lake else 0
      max_y <- max(base_df[[ycol]], na.rm=T) 
      max_y <- max_y + (max_y - min_y) * 0.05
      
      p <- plot_ly() |>
        config(displayModeBar = TRUE)
      
      if (input$top_metric == "wse_ft_navd88" & "lake_level" %in% names(base_df)) {
        df_ll_plot <- base_df |>
          select(timestamp, lake_level) |>
          distinct()  # collapse duplicates so only one trace
        
        p <- add_trace(
          p,
          x = df_ll_plot$timestamp,
          y = df_ll_plot$lake_level,
          type = "scatter",
          mode = "none",         # no line markers
          fill = "tozeroy",      # fill area down to y=0
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

      for (s in sites) {
        df_s <- base_df |> filter(site == s)
        
        p <- add_trace(
          p,
          x = df_s$timestamp,
          y = df_s[[ycol]],
          type = "scatter",
          mode = "lines",
          name = s,
          legendgroup = s,
          line = list(dash = "solid", color = site_colors[s]),
          text = paste(
            s,
            tm$label,
            tm$fmt(df_s[[ycol]])
          ),
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
    })
    
    
  }
  
}

shinyApp(ui, server)
