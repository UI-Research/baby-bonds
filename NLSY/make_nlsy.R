

library(tidyverse)

setwd(paste(here::here(), "NLSY", sep="/"))
source("College-finance2.R")
setwd(here::here())

rm(new_data)

nlsydf = categories
rm(categories)
saveRDS(nlsydf, "NLSY/NLSY-college-finance2.rds")

