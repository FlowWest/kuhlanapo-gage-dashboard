library(tidyverse)
library(Hmisc)
source(here::here("global.R"))

# these are the thalwegs in the model at the XS location that defined the rating curve
# these do not necessarily correspond to the true thalweg at the gage
sites_zero_depth <- tribble(
  ~code, ~model_twg_elev, ~bankfull_depth,
  "MC-01", 1328.22, 1.8, # lower
  "MC-03", 1329.69, 4.6, # upper
  "MC-02", 1331.96, 0.7, # secondary - surveyed twg is 1331.98 but extrapolation to zero on rating curve is 1331.956
)

rc_sites <- tribble(
  ~code,   ~filename,
  "MC-01",  "rc_xs1_lake1323.csv",
  "MC-03",  "rc_xs3_lake1323.csv",
  "MC-02",  "rc_xs2_lake1323.csv",
) |>
  inner_join(sites_zero_depth, by = join_by(code))

rating_curves_nested <-
  rc_sites |>
  mutate(rating_curve = pmap(list(filename, model_twg_elev, bankfull_depth), 
                             \(x, y, z) {
    rc <- 
      here::here("data-raw", x) |>
      read_csv() |>
      bind_rows(tibble_row(flow_cfs = 0,
                           wse_ft_navd88 = y)) |>
      arrange(flow_cfs) |>
      group_by(wse_ft_navd88) |>
      dplyr::summarize(flow_cfs = mean(flow_cfs), .groups = "drop") |>
      mutate(max_depth = wse_ft_navd88 - y) 
    extrap_domain <- seq(max(rc$max_depth), z, 0.1)
    rc |> 
      bind_rows(tibble(max_depth = extrap_domain,
                       flow_cfs = approxExtrap(x = rc$max_depth,
                                               y = rc$flow_cfs,
                                               xout = extrap_domain,
                                               na.rm = T)$y))
      
  })) |>
  select(code, rating_curve) 

rating_curves_df <- rating_curves_nested |>
  select(code, rating_curve) |>
  unnest(rating_curve)

rating_curves <- rating_curves_nested |>
  select(code, rating_curve) |>
  deframe()

rating_curves |> saveRDS(here::here("data", "rating_curves.rds"))

rating_curves_df |>
  ggplot() + 
  geom_line(aes(color = code, 
                x = max_depth, 
                y = flow_cfs,
                linetype = if_else(is.na(wse_ft_navd88), "Extrapolated", "Modeled"))) +
  scale_linetype_manual(name = "Source",
                        values = c("Modeled" = "solid",
                                   "Extrapolated" = "dashed")) +
  theme_minimal() +
  labs(x = "Max Depth (ft)",
       y = "Flow (cfs)") 











