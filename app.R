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

# No future plan is set, so cache refreshes run sequentially in the main
# process. plan(multisession) was tried, but on the 1 GB "large" shinyapps.io
# instance the idle workers plus the main process's ~800 MB peak while
# building df_pivot triggered OOM kills -- and a refresh worker would need its
# own copy of gage_data.rds on top of that. Revisit with a larger instance.

cache_dir_hydrovu <- file.path(getwd(), ".cache", "hydrovu")
dir.create(cache_dir_hydrovu, recursive = TRUE, showWarnings = FALSE)
cache_data_file_hydrovu <- file.path(cache_dir_hydrovu, "gage_data.rds")
cache_lock_file_hydrovu <- file.path(cache_dir_hydrovu, "refresh.lock")
rds_url_hydrovu <- "https://github.com/flowwest/kuhlanapo-gage-dashboard/raw/data/data/gage_data.rds"

usgs_cache_dir <- file.path(getwd(), ".cache", "usgs")
dir.create(usgs_cache_dir, recursive = TRUE, showWarnings = FALSE)
cache_data_file_lakelevel <- file.path(usgs_cache_dir, "usgs_lake_level_11450000.rds")
cache_lock_file_lakelevel <- file.path(usgs_cache_dir, "refresh.lock")
rds_url_lakelevel <- "https://github.com/flowwest/kuhlanapo-gage-dashboard/raw/data/data/usgs_lake_level_11450000.rds"

nasa_cache_dir <- file.path(getwd(), ".cache", "nasa")
dir.create(nasa_cache_dir, recursive = TRUE, showWarnings = FALSE)
cache_data_file_precip <- file.path(nasa_cache_dir, "precip_ts.rds")
cache_lock_file_precip <- file.path(nasa_cache_dir, "refresh.lock")
rds_url_precip <- "https://github.com/flowwest/kuhlanapo-gage-dashboard/raw/data/data/precip_ts.rds"

FORCE_LOCAL <- F

ZERO_RUMSEY_NAVD88 <- 1320.74

idw_obj <- readRDS(here::here("data-raw", "idw_precomputed.rds"))
kuhlanapo_bnd <- readRDS(here::here("data", "kuhlanapo_bnd.rds"))

# process-wide store for the shared pivoted series (see pivot_bundle in server)
pivot_cache <- new.env()

# Thin a single series to at most `max_points` before it's added as a plotly
# trace. Wide date ranges (e.g. Jan-present) at native ~15-min cadence across
# up to 10 piezometer traces produced 100k+ points and a 20+MB JSON payload,
# which was slow enough on shinyapps.io's hosted tier to disconnect the
# session -- this buckets by time and averages rather than decimating, so the
# visible trend/noise band is preserved.
downsample_for_plot <- function(df, max_points = 3000) {
  n <- nrow(df)
  if (n <= max_points || n == 0) return(df)

  df <- df |> arrange(timestamp)
  ts <- df$timestamp
  span_secs <- as.numeric(difftime(max(ts), min(ts), units = "secs"))
  bucket_secs <- max(1, ceiling(span_secs / max_points))
  bucket <- floor(as.numeric(ts) / bucket_secs)
  tzone <- attr(ts, "tzone")

  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  other_cols   <- setdiff(names(df), c("timestamp", numeric_cols))

  # vectorized per-bucket mean via rowsum() -- dplyr's summarise(across(...))
  # over ~3000 groups took 3+ sec per series here, which ate the time saved
  # by shrinking the payload; this does the same math at the C level
  mat <- cbind(.ts = as.numeric(ts), as.matrix(df[numeric_cols]))
  na_mask <- is.na(mat)
  mat0 <- mat; mat0[na_mask] <- 0
  sums   <- rowsum(mat0, bucket)
  counts <- rowsum(1 - na_mask * 1, bucket)
  means  <- sums / counts
  means[!is.finite(means)] <- NA_real_

  # buckets are monotonically non-decreasing since df is time-ordered, so
  # the first row per bucket is already in bucket order -- no re-sort needed
  first_df <- df[!duplicated(bucket), other_cols, drop = FALSE]

  out <- as.data.frame(means)
  out$timestamp <- as.POSIXct(out$.ts, origin = "1970-01-01", tz = tzone)
  out$.ts <- NULL
  cbind(out, first_df)[c("timestamp", numeric_cols, other_cols)]
}

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
  location_id = character(),
  code = character(),
  site = character(),
  name = character(),
  type = character(),
  parameter_id = character(),
  unit_id = character(),
  parameter_name = character(),
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

# Downloads `rds_url` and merges it into `cache_data_file`, deduplicating on
# `key_cols` (upstream rows win, so revised values replace cached ones).
# `prep` is applied to both the upstream and cached data before merging.
refresh_cache_async <- function(cache_data_file, cache_lock_file, rds_url,
                                key_cols, prep = identity) {

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
  
  f <- future::future({

    on.exit({
      unlink(cache_lock_file)
      message("[cache:refresh] lock released")
    }, add = TRUE)
    
    tmp_file <- tempfile(fileext = ".rds")
    
    tryCatch({
      
      message("[cache:refresh] downloading upstream RDS")
      download.file(rds_url, tmp_file, mode = "wb", quiet = TRUE)
      
      new_data <- prep(readRDS(tmp_file))
      old_data <- read_cached_data(cache_data_file)
      if (!is.null(old_data)) old_data <- prep(old_data)
      
      message(
        sprintf(
          "[cache:refresh] new_rows=%s | old_rows=%s",
          format(nrow(new_data), big.mark = ","),
          ifelse(is.null(old_data), "0", format(nrow(old_data), big.mark = ","))
        )
      )
      
      combined <- if (!is.null(old_data)) {
        bind_rows(new_data, old_data) |>
          distinct(across(all_of(key_cols)), .keep_all = TRUE)
      } else {
        new_data
      }
      
      message(
        sprintf(
          "[cache:refresh] combined_rows=%s",
          format(nrow(combined), big.mark = ",")
        )
      )
      
      write_cache(combined, cache_data_file)
      
      message("[cache:refresh] refresh SUCCESS")
      
    }, error = function(e) {
      message("[cache:refresh] FAILED: ", conditionMessage(e))
    })
    
    TRUE
  }, packages = "dplyr", seed = NULL)

  # Collect the result once the worker finishes; this relays the worker's
  # [cache:refresh] messages to the app log and frees the worker
  promises::then(
    f,
    onRejected = function(e) message("[cache:refresh] worker FAILED: ", conditionMessage(e))
  )

  invisible(f)
}

## UI ==========================================================================

ui <- bslib::page_fluid(

  # Bootstrap 5 (bslib also adds the width=device-width viewport meta tag)
  theme = bslib::bs_theme(version = 5),
  title = "KuhlaʼNapo Gage Dashboard",

  tags$style(HTML("
  html, body {
    height: 100%;
    margin: 0;
    overflow: hidden;
  }
  /* Pins toolbar + plot to exactly 100% of the iframe's own viewport height,
     so the plot area (flex: 1) always takes 'whatever's left' after the
     toolbar instead of a fixed vh fraction. A fixed vh fraction plus a
     toolbar on top of it is always > 100% of the container, which is what
     was forcing an internal scrollbar when embedded in a fixed-height iframe.
     dvh follows the phone browser's collapsing address bar; vh is the
     fallback for browsers without it. */
  .app-flex-col {
    display: flex;
    flex-direction: column;
    /* minus the container's top margin so the bottom isn't clipped */
    height: calc(100vh - 15px);
    height: calc(100dvh - 15px);
  }
  .container-fluid {
    margin-top: 15px;
  }
  .app-toolbar {
    flex: 0 0 auto;
  }
  .app-plot-area {
    flex: 1 1 auto;
    min-height: 0;
    display: flex;
    flex-direction: column;
  }
  .app-plot-area > div {
    flex: 1 1 auto;
    min-height: 0;
    height: 100%;
  }
  .flow-fullwidth > * {
    width: auto !important;
  }
  /* tighten the toolbar: drop the empty control labels and the form-group
     bottom margins so wrapped rows sit close together */
  .app-toolbar label.control-label:empty,
  .app-toolbar .shiny-input-radiogroup > .control-label,
  .app-toolbar .shiny-input-radiogroup > br {
    display: none;
  }
  .app-toolbar .form-group,
  .app-toolbar .shiny-input-container {
    margin-bottom: 2px !important;
  }
  /* flex rather than inline-block so every control in a row is centered on
     the same line regardless of its own height (the date input is taller) */
  .app-toolbar .shiny-flow-layout {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
  }
  .app-toolbar .shiny-flow-layout > div {
    margin-bottom: 2px;
  }
  .app-toolbar .btn-group {
    flex-wrap: wrap;
  }
  .layer-toggle-row {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    gap: 2px 16px;
    min-height: 30px;
  }
  .layer-toggle-row .shiny-input-container {
    width: auto !important;
    margin: 0 !important;
    min-height: 0;
  }
  .layer-toggle-row .form-check-label {
    white-space: nowrap;
  }

  /* The controls live in a one-panel accordion. On desktop it is invisible
     (header hidden, panel forced open) so the toolbar looks as before; on
     phones the header becomes a tap-to-open 'Controls' bar. */
  .controls-wrap {
    position: relative;
    flex: 0 0 auto;
  }
  .controls-acc {
    --bs-accordion-border-width: 0;
    --bs-accordion-bg: transparent;
  }
  .controls-acc .accordion-body {
    padding: 0;
  }
  .legend-btn {
    display: none;
    position: absolute;
    right: 40px;
    top: 5px;
    z-index: 5;
  }
  @media (min-width: 768px) {
    .controls-acc .accordion-header {
      display: none;
    }
    .controls-acc .accordion-collapse {
      display: block !important;
      height: auto !important;
    }
  }

  @media (max-width: 767.98px) {
    .container-fluid {
      margin-top: 8px;
      padding-left: 10px;
      padding-right: 10px;
    }
    .app-flex-col {
      height: calc(100vh - 8px);
      height: calc(100dvh - 8px);
    }
    .controls-acc .accordion-button {
      padding: 6px 12px;
      font-size: 0.9rem;
    }
    .app-toolbar {
      padding-top: 8px;
    }
    .app-toolbar .flow-fullwidth {
      width: 100% !important;
    }
    .app-toolbar .shiny-flow-layout > div,
    .app-toolbar .shiny-date-range-input {
      width: 100%;
    }
    .app-toolbar .btn-group-container-sw,
    .app-toolbar .btn-group {
      display: flex;
      width: 100%;
    }
    .app-toolbar .btn-group .btn {
      flex: 1 1 auto;
    }
    .layer-toggle-row {
      width: 100%;
    }
    /* the plot's own legend can be toggled on phones (time series only) */
    .controls-wrap.has-series .legend-btn {
      display: inline-block;
    }
  }
")),

  # phone-width flag for the server's plot layouts (see is_mobile())
  tags$script(HTML("
  window.reportMobile = function() {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('is_mobile', window.innerWidth < 768);
    }
  };
  $(document).on('shiny:connected', window.reportMobile);
  $(window).on('resize orientationchange', window.reportMobile);
  // the Legend button only applies to the time series, not the piezo map
  $(document).on('shiny:inputchanged', function(e) {
    if (e.name === 'top_metric') {
      $('.controls-wrap').toggleClass('has-series', e.value !== 'gw_contour');
    }
  });
  ")),

  div(
    class = "app-flex-col",

    div(
      class = "controls-wrap has-series",

      bslib::accordion(
        id = "controls_acc",
        class = "controls-acc",
        open = FALSE,
        bslib::accordion_panel(
          "Controls",
          value = "controls",
          div(
            class = "app-toolbar",
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

              uiOutput("precip_toggle_selector"),

              conditionalPanel(
                condition = "input.top_metric == 'gw_contour'",
                uiOutput("gw_time_selector")
              )
            )
          )
        )
      ),

      # phones only: shows/hides the time series plot's own (plotly) legend
      tags$button(
        type = "button",
        class = "btn btn-sm btn-outline-secondary legend-btn",
        onclick = "var g = document.getElementById('combined_plot'); if (g && g.layout) Plotly.relayout(g, {showlegend: !g.layout.showlegend});",
        "Legend"
      )
    ),

    div(
      class = "app-plot-area",
      conditionalPanel(
        condition = "input.top_metric == 'gw_contour'",
        plotOutput("gw_contour_plot", height = "100%")
      ),
      conditionalPanel(
        condition = "input.top_metric != 'gw_contour'",
        plotlyOutput("combined_plot", height = "100%")
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
  
  # ?smooth (or smooth=true/1) suppresses the daily groundwater cycle in the
  # piezometer series; off by default
  url_smooth <- reactive({
    q <- query()
    !is.null(q$smooth) && q$smooth %in% c("", "1", "true", "TRUE")
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

  # In default mode (no mode= param) precip is a user-toggleable checkbox,
  # seeded from the URL param but overridable once the input renders. In
  # stage/piezo modes there's no toggle shown, so the URL param always wins.
  show_precip <- reactive({
    if (url_mode() == "default") {
      if (is.null(input$show_precip_input)) url_show_precip() else input$show_precip_input
    } else {
      url_show_precip()
    }
  })

  # Temperature panel (stage views only): on by default; ?show_temp=false hides
  # it. Like precip, a user-toggleable switch in default mode, URL-only otherwise.
  url_show_temp <- reactive({
    q <- query()
    !(!is.null(q$show_temp) && q$show_temp %in% c("0", "false", "FALSE"))
  })

  show_temp <- reactive({
    if (url_mode() == "default" && !is.null(input$show_temp_input)) {
      input$show_temp_input
    } else {
      url_show_temp()
    }
  })

  output$precip_toggle_selector <- renderUI({
    req(url_mode() == "default")

    tags$div(
      class = "form-group",
      # matches the empty control-label every other toolbar widget renders
      # above its control, so this row lines up with them vertically
      tags$label(class = "control-label"),
      tags$div(
        class = "layer-toggle-row",
        bslib::input_switch("show_precip_input", "Show Precipitation",
                            value = isolate(url_show_precip())),
        # temperature only exists on the stage / water surface views
        conditionalPanel(
          condition = "input.top_metric == 'depth' || input.top_metric == 'wse_ft_navd88'",
          bslib::input_switch("show_temp_input", "Show Temperature",
                              value = isolate(url_show_temp()))
        )
      )
    )
  })

  output$date_range_selector <- renderUI({

    mode <- url_mode()

    # stage data lags behind Sys.Date() when the upstream refresh is delayed/stale;
    # clamp the rolling window to the latest timestamp actually on hand so it
    # doesn't default to an empty range
    latest_stage_date <- tryCatch(
      as.Date(pivot_bundle()$max_ts),
      error = function(e) Sys.Date()
    )
    if (is.na(latest_stage_date) || is.infinite(as.numeric(latest_stage_date))) {
      latest_stage_date <- Sys.Date()
    }
    latest_stage_date <- min(Sys.Date(), latest_stage_date)

    dateRangeInput(
      "date_range",
      label = "",
      # "piezo" used to default to a hardcoded 2025-12-09 - 2026-02-03
      # window (same staleness bug as the old gw_time slider default) --
      # falls through to the same rolling window as stage/default now
      start = switch(mode,
                     stage = latest_stage_date - 30,
                     latest_stage_date - 30),
      end = switch(mode,
                   stage = latest_stage_date,
                   latest_stage_date),
      min = as.Date("2025-12-06"),
      max = Sys.Date()
    )
  })
  
  output$top_metric_selector <- renderUI({
    
    mode <- url_mode()
    
    choices <- switch(
      mode,
      stage = c(
        "Stage (Depth)" = "depth",
        "Water Surface" = "wse_ft_navd88"
      ),
      piezo = c(
        "GW Elevation"  = "gwe_ft_navd88",
        "Depth to GW" = "gw_depth_ft",
        "Piezo Map" = "gw_contour"
      ),
      # default (no mode param): show both stage and piezo options together
      c(
        "Stage (Depth)" = "depth",
        "Water Surface" = "wse_ft_navd88",
        "GW Elevation"  = "gwe_ft_navd88",
        "Depth to GW" = "gw_depth_ft",
        "Piezo Map" = "gw_contour"
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
  
  # Slider for GW Contour time -- bounded by the actual available piezometer
  # record (not the toolbar's date_range, since gw_contour_df() queries
  # df_pivot() directly rather than filtered_df()), so it stays correct as
  # new data comes in instead of the range being hardcoded and going stale.
  output$gw_time_selector <- renderUI({
    req(input$top_metric == "gw_contour")

    piezo_df <- df_pivot() |> filter(category == "Piezometer")
    piezo_range <- range(piezo_df$timestamp, na.rm = TRUE)
    req(all(is.finite(piezo_range)))

    # default to the latest time where every piezometer still has a
    # reasonably recent reading, rather than the overall latest timestamp --
    # individual trolls fall behind (maintenance, dead battery, etc.), and
    # picking a time only a few of them have data for interpolates from an
    # unrepresentative handful of sites (or nothing at all)
    default_t0 <- piezo_df |>
      filter(!is.na(gwe_ft_navd88)) |>
      summarise(latest = max(timestamp), .by = code) |>
      pull(latest) |>
      min()

    sliderInput(
      "gw_time",
      "",
      min = piezo_range[1],
      max = piezo_range[2],
      value = isolate(input$gw_time) %||% default_t0,
      timeFormat = "%Y-%m-%d %H:%M",
      step = 900, # 15 min steps
      animate = FALSE,
      ticks = FALSE,
      width = "300px"
    )
  })
  
  ts_data <- reactive({
    
    if((file.exists(here::here("data/gage_data.rds"))) && FORCE_LOCAL) {
      message("FORCE_LOCAL is on; using data/gage_data.rds locally")
      return(standardize_hydrovu_cols(readRDS(here::here("data/gage_data.rds"))))
    }

    if (!file.exists(cache_data_file_hydrovu)) {
      message("[cache:bootstrap] no cache → downloading initial copy")
      download.file(rds_url_hydrovu, cache_data_file_hydrovu, mode = "wb", quiet = TRUE)
    }
    
    df <- standardize_hydrovu_cols(read_cached_data(cache_data_file_hydrovu))

    if (cache_is_stale(cache_data_file_hydrovu)) {
      message("[cache:decision] cache is stale → triggering async refresh")
      refresh_cache_async(cache_data_file_hydrovu, cache_lock_file_hydrovu, rds_url_hydrovu,
                          key_cols = c("name", "timestamp", "parameter_name"),
                          prep = standardize_hydrovu_cols)
    } else {
      message("[cache:decision] cache is fresh → no refresh")
    }
    
    df |> glimpse()
  })
  
  ll_data <- reactive({
    
    if((file.exists(here::here("data/usgs_lake_level_11450000.rds"))) && FORCE_LOCAL) {
      message("FORCE_LOCAL is on; using data/usgs_lake_level_11450000.rds locally")
      return(readRDS(here::here("data/usgs_lake_level_11450000.rds")))
    }

    if (!file.exists(cache_data_file_lakelevel)) {
      message("[cache:bootstrap] no cache → downloading initial copy")
      download.file(rds_url_lakelevel, cache_data_file_lakelevel, mode = "wb", quiet = TRUE)
    }
    
    df <- read_cached_data(cache_data_file_lakelevel)
    
    if (cache_is_stale(cache_data_file_lakelevel)) {
      message("[cache:decision] cache is stale → triggering async refresh")
      refresh_cache_async(cache_data_file_lakelevel, cache_lock_file_lakelevel, rds_url_lakelevel,
                          key_cols = c("name", "timestamp", "parm_name"))
    } else {
      message("[cache:decision] cache is fresh → no refresh")
    }

    df
  })
  
  precip_data <- reactive({

    req(show_precip())

    if((file.exists(here::here("data/precip_ts.rds"))) && FORCE_LOCAL) {
      message("FORCE_LOCAL is on; using data/precip_ts.rds locally")
      return(readRDS(here::here("data/precip_ts.rds")))
    }
    
    if (!file.exists(cache_data_file_precip)) {
      message("[cache:bootstrap] no cache → downloading initial copy")
      tryCatch(
        download.file(rds_url_precip, cache_data_file_precip, mode = "wb", quiet = TRUE),
        error = function(e) message("[cache:bootstrap] FAILED: ", conditionMessage(e))
      )
    }

    df <- read_cached_data(cache_data_file_precip)
    
    if (cache_is_stale(cache_data_file_precip)) {
      message("[cache:decision] cache is stale → triggering async refresh")
      refresh_cache_async(cache_data_file_precip, cache_lock_file_precip, rds_url_precip,
                          key_cols = c("site", "timestamp"))
    } else {
      message("[cache:decision] cache is fresh → no refresh")
    }
    
    df
  })
  
  # The cleaned/pivoted series is identical for every viewer, so it is built
  # once per process and shared (keyed on the cache files' mtimes and the
  # smooth option) rather than rebuilt per session: the raw 1.8M-row table plus
  # the pivot copies per session ran the shinyapps.io instance out of memory
  # when a few sessions overlapped. Only the two newest entries are kept.
  pivot_bundle <- reactive({
    key <- paste(
      format(file.mtime(cache_data_file_hydrovu), "%Y%m%d%H%M%OS3"),
      format(file.mtime(cache_data_file_lakelevel), "%Y%m%d%H%M%OS3"),
      url_smooth()
    )
    hit <- pivot_cache[[key]]
    if (!is.null(hit)) {
      # still let a stale cache trigger its async refresh
      if (cache_is_stale(cache_data_file_hydrovu)) invisible(ts_data())
      return(hit)
    }
    ts <- ts_data()
    bundle <- list(
      df = build_df_pivot(ts, ll_data(), smooth = url_smooth()),
      max_ts = max(ts$timestamp, na.rm = TRUE)
    )
    rm(ts); gc()
    old <- setdiff(ls(pivot_cache), key)
    if (length(old) > 1) rm(list = head(old[order(old)], length(old) - 1), envir = pivot_cache)
    assign(key, bundle, envir = pivot_cache)
    bundle
  })

  df_pivot <- reactive({
    req(input$date_range)
    pivot_bundle()$df
  })
  
  filtered_df <- reactive({
    df <- df_pivot()
    req(df)
    
    start_ts <- as.POSIXct(input$date_range[1], tz = "America/Los_Angeles")
    end_ts   <- as.POSIXct(input$date_range[2], tz = "America/Los_Angeles") +
      hours(23) + minutes(59) + seconds(59)
    
    df |> filter(timestamp >= start_ts, timestamp <= end_ts)
  })

  empty_precip_schema <- tibble(
    site = character(),
    timestamp = as.POSIXct(character(), tz = "America/Los_Angeles"),
    precip_in = numeric()
  )

  filtered_precip_df <- reactive({

    req(show_precip())

    df <- precip_data()
    # precip is a supplementary layer - if it failed to load (e.g. upstream
    # file unavailable), skip the layer rather than blocking the whole plot
    if (is.null(df)) return(empty_precip_schema)

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
  
  # TRUE on phone-width screens (< 768px), reported by the page's JS. The plot
  # waits for the first report so it draws once, in the right layout.
  is_mobile <- reactive({
    req(!is.null(input$is_mobile))
    isTRUE(input$is_mobile)
  })

  output$combined_plot <- renderPlotly({

    mob <- is_mobile()

    # on phones: no modebar, and no scroll-wheel zoom
    p <- plot_ly() |>
      config(displayModeBar = !mob, scrollZoom = FALSE, displaylogo = FALSE)
    
    base_df <- filtered_df()
    validate(need(nrow(base_df) > 0, "No data available for the selected date range."))
    tm <- top_metric()
    ycol <- tm$col
    req(ycol)
    
    min_lake <- 1320.74 # as defined on USGS Clear Lake Lakeport gage
    max_lake <- min_lake + 7.56
    
    if(show_precip()) {
      precip_df <- filtered_precip_df() |>
        filter(site == "KPD")
    }
    
    if (isTRUE(ycol %in% c("wse_ft_navd88", "gwe_ft_navd88") && "lake_level" %in% names(base_df))) {
      
      df_ll_plot <- base_df |>
        select(timestamp, lake_level) |>
        filter(!is.na(lake_level)) |>
        # one point per timestamp, in time order: base_df is stacked by site,
        # so without this the series doubles back on itself and the fill
        # polygon self-intersects
        distinct(timestamp, .keep_all = TRUE) |>
        arrange(timestamp)
      
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

      # a zero-span range (e.g. all depths reading exactly 0 in the dry
      # season) is invalid for plotly and silently falls back to an
      # autoranged +/-1 window, which pulls the axis below zero
      if (input$top_metric == "depth" && (!is.finite(max_y) || max_y <= min_y)) {
        max_y <- min_y + 0.1
      }

      # ---- Surface water depth/WSE traces ----
      for (s in sites_stage$code) {
        df_s <- base_df |> filter(code == s) |> downsample_for_plot()
        
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
      if (show_temp()) for (s in sites_stage$code) {
        df_s <- base_df |> filter(code == s) |> downsample_for_plot()
        
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
          xaxis = "x2",
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
          xaxis = "x2",
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
        df_s <- base_df |> filter(code == s) |> downsample_for_plot()
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
    
    if(show_precip()){

      precip_max <- if (nrow(precip_df) > 0) max(precip_df$precip_in, na.rm = TRUE) else 0
      if (!is.finite(precip_max)) precip_max <- 0
      # floor the top of the range so an all-zero (no rain) window doesn't
      # collapse to a zero-span axis, which plotly autoranges below zero
      precip_axis_max <- max(precip_max * 1.1, 0.1)

      p <- add_trace(
        p,
        x = precip_df$timestamp,           # left edge of the bar
        y = precip_df$precip_in,
        type = "bar",
        name = "Precipitation",
        legendgroup = "precip",
        legendrank = 1,                    # legend order follows the panel order, top first
        text = NULL,                       # remove bar labels
        hovertext = paste0(
          format(precip_df$timestamp, "%b %d, %Y %H:%M"), " - ", format(precip_df$timestamp  + lubridate::hours(1), "%b %d, %Y %H:%M"), "<br>",
          "Precipitation: ", sprintf("%.2f in", precip_df$precip_in)
        ),
        hoverinfo = "text",
        marker = list(color = "rgba(64,64,64,0.5)"),
        width = 3600 * 1000,               # 1 hour in ms
        xaxis = "x3",
        yaxis = "y3",
        offset = 0                         # align left side of bar with x
      )
    }
      
    y_domain <- if (top_metric()$col %in% c("gw_depth_ft", "gwe_ft_navd88")) {
      if (show_precip()) c(0, 0.825) else c(0, 1)
    } else {
      lo <- if (show_temp()) 0.3 else 0
      if (show_precip()) c(lo, 0.825) else c(lo, 1)
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
  # one time axis per panel (all matched to the main one) so every panel gets
  # its own vertical gridlines; only the topmost panel's axis shows tick
  # labels, at the top of the whole plot (see label_axis below)
  x_axis <- function(anchor) {
    c(list(
      title = "",
      domain = c(0, 1),
      anchor = anchor,
      showticklabels = FALSE,
      showgrid = TRUE,
      gridcolor = "rgba(0,0,0,0.1)",
      zeroline = FALSE,
      showspikes = TRUE
    ), if (anchor != "y") list(matches = "x"))
  }

  stage_metric <- top_metric()$col %in% c("depth", "wse_ft_navd88") && show_temp()

  layout_args <- list(
    dragmode = "zoom",
    xaxis = x_axis("y"),
    xaxis2 = if (stage_metric) x_axis("y2") else NULL,
    xaxis3 = if (show_precip()) x_axis("y3") else NULL,
    yaxis = list(
      title = tm$label,
      domain = y_domain,
      fixedrange = TRUE,
      range = c(min_y, max_y)
    ),
    yaxis2 = if (stage_metric) list(
      title = "Temperature (\u00b0F)",
      domain = c(0, 0.3),
      fixedrange = TRUE
    ) else NULL,
    yaxis3 = if (show_precip()) list(
      title = "Hourly Precip (in)",
      domain = c(0.9, 1.0),
      fixedrange = TRUE,
      range = c(0, precip_axis_max)
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

  label_axis <- if (show_precip()) "xaxis3" else "xaxis"
  layout_args[[label_axis]]$showticklabels <- TRUE
  layout_args[[label_axis]]$side <- "top"

  if (mob) {
    # phone layout: touch pans instead of box-zooming, upright sparse date
    # labels, tight margins (top keeps room for the date axis), and the
    # legend starts hidden until the 'Legend' button (see the UI) turns it on;
    # tapping a line shows its site name
    layout_args$dragmode <- "pan"
    for (ax in c("xaxis", "xaxis2", "xaxis3")) {
      if (!is.null(layout_args[[ax]])) {
        layout_args[[ax]]$tickangle <- 0
        layout_args[[ax]]$nticks <- 4
      }
    }
    for (ax in c("yaxis", "yaxis2", "yaxis3")) {
      if (!is.null(layout_args[[ax]])) layout_args[[ax]]$automargin <- TRUE
    }
    layout_args$showlegend <- FALSE
    # when shown it overlays the bottom of the plot as a wrapped strip
    layout_args$legend <- list(
      orientation = "h",
      x = 0, xanchor = "left",
      y = 0.01, yanchor = "bottom",
      tracegroupgap = 2,
      font = list(size = 9),
      bgcolor = "rgba(255,255,255,0.93)",
      bordercolor = "rgba(0,0,0,0.2)",
      borderwidth = 1
    )
    layout_args$margin <- list(t = 30, b = 10, l = 40, r = if (!is.null(rumsey_axis)) 40 else 10)
  }

  if (!is.null(rumsey_axis)) layout_args$yaxis4 <- rumsey_axis
  
  do.call(layout, c(list(p), layout_args))
  
 })
  
  # Reactive: IDW interpolation at selected time
  gw_contour_df <- reactive({
    req(input$top_metric == "gw_contour")
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
    req(input$top_metric == "gw_contour")
    req(gw_contour_df())
    validate(need(
      any(!is.na(gw_contour_df()$value)),
      "No piezometer readings close enough to this time to interpolate a map. Try a different time."
    ))

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
      geom_text(data=idw_obj$sites |>
                  mutate(label_color = if_else(id %in% c("PZ-A1", "PZ-B1", "PZ-B2"), "white", "black")),
                aes(label = substr(id, 4, 5), color = I(label_color)), size = 12 / .pt) +
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
  
  # The toolbar sits in a collapsed accordion on phones, and Shiny suspends
  # outputs that are hidden -- but the plots need these inputs to exist
  for (id in c("date_range_selector", "top_metric_selector",
               "precip_toggle_selector", "gw_time_selector")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }

}

shinyApp(ui, server)
