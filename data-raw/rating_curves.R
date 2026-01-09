library(tidyverse)
library(xsmatic)
library(patchwork)

## Define n and slope values for the cross sections

# these are stand-in values for now
gage_meta <- tribble(
  ~code,    ~site,                ~mannings_n, ~slope,
  "MC-01", "Lower Manning Creek", 0.0300,  (1328.062 - 1327.613) / 83.87,
  "MC-03", "Upper Manning Creek", 0.0300,  0.0002,
  "MC-02", "Secondary Channel",   0.0500,  (1332.001 - 1331.27) / 103.08,
) 

## Import the cross sections

xs1 <- tribble(~inc_ft, ~elev_ft,
               0.00, 1331.118,
               6.26, 1330.832,
               5.76, 1329.462,
               5.64, 1328.048,
               4.95, 1327.613,
               7.82, 1328.748,
               1.54, 1330.353,
               1.30, 1331.044) |>
  mutate(sta_ft = cumsum(inc_ft))

xs_data <- tribble(~code, ~df,
                   "MC-01", xs1,
                   "MC-02", read_csv(here::here("data-raw", "xs2_raw_approx_loc.csv")),
                   "MC-03", read_csv(here::here("data-raw", "xs3_interp_approx_loc.csv"))) 

cross_sections_nested <- xs_data |>
  inner_join(gage_meta, by = join_by(code)) |>
  mutate(xs = map(df, \(x) xsmatic::xs_prep(x, sta_ft, elev_ft)))

cross_sections <- cross_sections_nested |>
  select(code, xs) |>
  deframe()

## Derive rating curves (this is a temporary approximation until we have empirical rating curves)

cross_sections |>
  lapply(xsmatic::xs_plot) |>
  wrap_plots() + 
  plot_layout(ncol = 1)

rating_curves_nested <- cross_sections_nested |>
  mutate(rating_curve = pmap(list(xs, slope, mannings_n), xsmatic::xs_rating_curve)) 

rating_curves_df <- rating_curves_nested |>
  select(code, rating_curve) |>
  unnest(rating_curve)

rating_curves <- rating_curves_nested |>
  select(code, rating_curve) |>
  deframe()

rating_curves |> saveRDS(here::here("data-raw", "rating_curves.rds"))
  
rating_curves |>
  lapply(xsmatic::xs_plot_rc) |>
  wrap_plots() + 
  plot_layout(ncol = 1)

## Test applying the rating curves to the data

gage_data <- readRDS(here::here("data", "gage_data.rds"))

df_pivot <- gage_data |>
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
  mutate(water_temperature = if_else(depth > 0, water_temperature, NA)) |>
  mutate(site = factor(site, levels = unique(gages$site))) |>
  mutate(timestamp = with_tz(timestamp, "America/Los_Angeles"))

# df_pivot_with_flow_cfs <-
#   with(rating_curves[["MC-01"]],
#        df_pivot |>
#        mutate(wse_ft_navd88 = depth + first(thalweg_elevation),
#               flow_cfs = coalesce(approx(x = max_depth,
#                                          y = discharge,
#                                          xout = depth,
#                                          rule = 1:2)$y, 0)))

df_pivot |>
  nest(.by = c(code, site)) |>
  inner_join(enframe(rating_curves, 
                     name = "code", 
                     value = "rating_curve"),
             by = join_by(code)) |>
  mutate(result = map2(data, rating_curve,
                       \(d, rc) {
                         with(rc, d |>
                                mutate(wse_ft_navd88 = depth + first(thalweg_elevation),
                                       flow_cfs = coalesce(approx(x = max_depth,
                                                                  y = discharge,
                                                                  xout = depth,
                                                                  rule = 1:2)$y, 0)))})) |>
  select(code, site, result) |>
  unnest(result)

    