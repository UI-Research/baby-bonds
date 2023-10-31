
# Execute the script provided by NLSY that reads the data and
# save them in an RDS file.

library(tidyverse)

# Not sure what packages these functions come from - could not run these three lines
categories <- vallabels(new_data)
new_data <- qnames(new_data)
categories <- qnames(categories)


# Frame with weights
wtdf = read_table(
  'https://urbanorg.box.com/shared/static/v8ps9iechzoamynkvkwgymx7iisd1tr8.dat',
  col_names = c('PUBID_1997','wt'),
  col_types = 'id'
) |>
  mutate(wt=wt/100)

categories = left_join(categories, wtdf, by='PUBID_1997')

setwd(here::here())
saveRDS(categories, "NLSY/NLSY-college-finance.rds")
rm(list=c('new_data', 'categories', 'wtdf', 'qnames', 'varlabels', 'vallabels', 'vallabels_continuous'))

