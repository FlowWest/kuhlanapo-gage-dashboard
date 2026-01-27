sites <- tribble(
  ~category,    ~code,   ~site,                 ~site_label,                  ~site_descrip,            ~twg_elev, 
  "Stage Gage", "MC-01", "Lower Manning Creek", "Manning Creek West Branch",  "5400 ft DS Soda Bay Rd", 1327.83,   
  "Stage Gage", "MC-03", "Upper Manning Creek", "Manning Creek Mainstem",     "3800 ft DS Soda Bay Rd", 1329.63,   
  "Stage Gage", "MC-02", "Secondary Channel",   "Secondary Channel",          "4300 ft DS Soda Bay Rd", 1331.55,   
  "Piezometer", "PZ-A1", "Piezometer A1",       "Piezometer A1",              "X ft from Clear Lake",   NA,        
  "Piezometer", "PZ-A2", "Piezometer A2",       "Piezometer A2",              "X ft from Clear Lake",   NA,        
  "Piezometer", "PZ-A3", "Piezometer A3",       "Piezometer A3",              "X ft from Clear Lake",   NA,        
  "Piezometer", "PZ-B1", "Piezometer B1",       "Piezometer B1",              "X ft from Clear Lake",   NA,        
  "Piezometer", "PZ-B2", "Piezometer B2",       "Piezometer B2",              "X ft from Clear Lake",   NA,        
  "Piezometer", "PZ-B3", "Piezometer B3",       "Piezometer B3",              "X ft from Clear Lake",   NA,        
  "Piezometer", "PZ-B4", "Piezometer B4",       "Piezometer B4",              "X ft from Clear Lake",   NA,        
  "Piezometer", "PZ-C1", "Piezometer C1",       "Piezometer C1",              "X ft from Transect B",   NA,        
  "Piezometer", "PZ-C2", "Piezometer C2",       "Piezometer C2",              "X ft from Transect B",   NA,        
  "Piezometer", "PZ-C3", "Piezometer C3",       "Piezometer C3",              "X ft from Transect B",   NA,        
)

sensors <- tribble(
  ~code,   ~name,           ~type,
  "MC-01", "2025SGMC01",    "troll",
  "MC-01", "2025SGMC01_VL", "vulink",
  "MC-03", "2025SGMC03",    "troll",
  "MC-03", "2025SGMC03_VL", "vulink",
  "MC-02", "2025SGMC02",    "troll",
  "MC-02", "2025SGMC02_VL", "vulink",
  "PZ-A1", "2025PZA01",     "troll",
  "PZ-A2", "2025PZA02",     "troll",
  "PZ-A3", "2025PZA03",     "troll",
  "PZ-B1", "2025PZB01",     "troll",
  "PZ-B2", "2025PZB02",     "troll",
  "PZ-B3", "2025PZB03",     "troll",
  "PZ-B4", "2025PZB04",     "troll",
  "PZ-C1", "2025PZC01",     "troll",
  "PZ-C2", "2025PZC02",     "troll",
  "PZ-C3", "2025PZC03",     "troll",
  "PZ-B4", "2025BAROK01",   "barotroll"
) |> 
  mutate(across(everything(), as.character)) 

gages <- sensors |>
  inner_join(sites |> select(code, site), by =)

site_labels <- sites |>
  select(code, site_label) |>
  deframe()

site_descrips <- sites |>
  select(code, site_descrip) |>
  deframe()

piezo_colors <- tribble(
  ~code, ~color,
  "PZ-A1", "#c90074", 
  "PZ-A2", "#ff731c", 
  "PZ-A3", "#ffd28f", 
  "PZ-B1", "#001284", 
  "PZ-B2", "#007793", 
  "PZ-B3", "#55abf2", 
  "PZ-B4", "#d2e0ff", 
  "PZ-C1", "#47d09c", 
  "PZ-C2", "#acf186", 
  "PZ-C3", "#fcffaa", 
)
