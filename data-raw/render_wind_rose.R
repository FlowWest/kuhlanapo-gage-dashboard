setwd("c:/Users/skylerlewis/Github/kuhlanapo-gage-dashboard")
readRenviron(".Renviron")
if (Sys.getenv("EARTHDATA_USER") == "" && Sys.getenv("EARTHDATA_LOGIN") != "") {
  Sys.setenv(EARTHDATA_USER = Sys.getenv("EARTHDATA_LOGIN"))
}
rmarkdown::render(
  "data-raw/wind_rose_lakeshore.Rmd",
  output_format = "html_document"
)
