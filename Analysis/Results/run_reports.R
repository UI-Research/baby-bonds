
library(tidyverse)
library(quarto)

report_places = tribble(
    ~state,   ~geoid,       ~city,
    "CA",     "0653000",    "Oakland",
    "GA",     "1304000",    "Atlanta",
    "MA",     "2507000",    "Boston",
    "MD",     "2404000",    "Baltimore",
    "",       "",           "USA"
)

cities = report_places$city

report_dir = paste0(here::here(), "/Analysis/Results/")

reports = tibble(
    input = paste0(report_dir, "BabyBonds Cities.qmd"),
    output_file = str_glue("BabyBonds {cities}.html"),
    execute_params = map(cities, \(x) list(city=x))
)

setwd(report_dir)
pwalk(reports, quarto_render)
setwd(here::here())

