
# Execute the script provided by NLSY that reads the data and
# save them in an RDS file.

library(tidyverse)

# Load in College-finance2.dat
new_data <- read.table('https://urbanorg.box.com/shared/static/f8tyxcohac1ha11a3mcw0dzq8ywqayyn.dat', sep=' ')
# Source College-finance2.R to clean the data just loaded in.
# Note: The first 6 lines need to be removed and because the script is so large, doing so
# programmatically is the least intrusive way.
colfin_link = 'https://urbanorg.box.com/shared/static/wc9eg6bzygf2l7abpjb6sdnd9qyx96tq.r'
colfin_r = readLines(colfin_link)
colfin_r = colfin_r[-1:-6]
tc = textConnection(colfin_r)
source(tc)
close(tc)

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

# Note: This data was uploaded from Box and can be read in from there
saveRDS(categories, here::here("NLSY", "NLSY-college-finance.rds"))
rm(list=c('new_data', 'categories', 'wtdf', 'qnames', 'varlabels', 'vallabels', 'vallabels_continuous'))

