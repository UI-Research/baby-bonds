
# Execute the script provided by NLSY that reads the data and
# save them in an RDS file.

library(tidyverse)

# Load in College-finance2.dat
new_data <- read.table('https://urbanorg.box.com/shared/static/f8tyxcohac1ha11a3mcw0dzq8ywqayyn.dat', sep=' ')
# Source College-finance2.R to clean the data just loaded in.
# NOTE: Will need to comment out the first line of this script and resave to Box each time it is updated.
source('https://urbanorg.box.com/shared/static/wc9eg6bzygf2l7abpjb6sdnd9qyx96tq.r')

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

saveRDS(categories, here::here("NLSY", "NLSY-college-finance.rds"))
rm(list=c('new_data', 'categories', 'wtdf', 'qnames', 'varlabels', 'vallabels', 'vallabels_continuous'))

