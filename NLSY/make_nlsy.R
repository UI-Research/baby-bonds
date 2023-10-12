
# Execute the script provided by NLSY that reads the data and 
# save them in an RDS file.

library(tidyverse)

# Get the NLSY data path
source('common.R')

# The NLSY script has to be executed in the data folder
setwd(nlsy_data_dir)
source("College-finance2.R")

categories <- vallabels(new_data)
new_data <- qnames(new_data)
categories <- qnames(categories)


# Frame with weights
wtdf = read_table(
  'customweight_nlsy97_651b190a18af1d9d269.dat',
  col_names = c('PUBID_1997','wt'),
  col_types = 'id'
) |>
  mutate(wt=wt/100)

categories = left_join(categories, wtdf, by='PUBID_1997')

setwd(here::here())
saveRDS(categories, "NLSY/NLSY-college-finance.rds")
rm(list=c('new_data', 'categories', 'wtdf', 'qnames', 'varlabels', 'vallabels', 'vallabels_continuous'))

