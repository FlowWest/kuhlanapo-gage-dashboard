sites <- tribble(
  ~code,   ~site,                 ~site_label,                  ~site_descrip,            ~twg_elev,
  "MC-01", "Lower Manning Creek", "Manning Creek West Branch",  "5400 ft DS Soda Bay Rd", 1327.83, # lower
  "MC-03", "Upper Manning Creek", "Manning Creek Mainstem",     "3800 ft DS Soda Bay Rd", 1329.63, # upper
  "MC-02", "Secondary Channel",   "Secondary Channel",          "4300 ft DS Soda Bay Rd", 1331.55, # secondary
)

gages <- tribble(
  ~code,   ~name,           ~type,
  "MC-01", "2025SGMC01",    "troll",
  "MC-01", "2025SGMC01_VL", "vulink",
  "MC-03", "2025SGMC03",    "troll",
  "MC-03", "2025SGMC03_VL", "vulink",
  "MC-02", "2025SGMC02",    "troll",
  "MC-02", "2025SGMC02_VL", "vulink"
) |> 
  mutate(across(everything(), as.character)) |>
  inner_join(sites |> select(code, site), by = join_by(code))

site_labels <- sites |>
  select(code, site_label) |>
  deframe()

site_descrips <- sites |>
  select(code, site_descrip) |>
  deframe()
