
library(RSelenium)
library(here)

year <- 2020
week <- 1
url <- paste0(
  "https://www.census.gov/data/tables/",
  year,
  "/demo/hhp/",
  "hhp",
  week,
  ".html"
)

button_id <- "LeaderBoard1_cmdCSV"

filename <- "household_puslse_week1.csv"
download_location <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
