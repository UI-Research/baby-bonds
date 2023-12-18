# This file reads in longitudinal data from CPS ASEC
library(tidyverse)
library(ipumsr)
# Run this once to save key to .Renviron file
# set_ipums_api_key('Enter your key here')


##########################
# Pull Extracts from IPUMS
##########################
# Note: ethnicity not available pre-1971; school or college attendance (SCHLCOLL) not available pre-1986
years <- 1971:2023

get_cps_asec <- function(year, import=TRUE){
    cps_extract_request <- define_extract_cps(
        description = 'Longitudinal CPS ASEC Data',
        sample = paste0('cps', year, '_03s'),
        variables = c('SEX', 'AGE', 'RACE', 'HISPAN', 'EDUC')
    )

    submitted_extract <- submit_extract(cps_extract_request)
    collection <- submitted_extract$collection
    number <- submitted_extract$number
    downloadable_extract <- wait_for_extract(submitted_extract)
    path_to_asec_files <- download_extract(c(collection, number), download_dir = "data")

}

purrr::map(years, get_cps_asec)

##########################
# Read in Extracts
##########################
# Get all files ending in .xml
asec_files <- fs::dir_ls('data', regexp = '\\.xml$')
asec_df_long <- purrr::map_dfr(asec_files, read_ipums_micro)

write_rds(asec_df_long, 'data/asec_longitudinal.Rds')

