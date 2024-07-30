# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
library(tidyverse)

setwd("ACS")
ddi <- read_ipums_ddi("cps_00119.xml")
data <- read_ipums_micro(ddi)

df = data |>
    filter(AGE_1<=3) |>
    rename_all(tolower) |>
    select(year=year_1, serial=serial_1, asecwt=asecwt_1, race=race_1, offpov=offpov_1, poverty=poverty_1)

df |>
    filter(offpov<=2) |>
    mutate(inpov=(offpov==1)) |>
    group_by(year) |>
    summarise(inpov=weighted.mean(inpov, asecwt))

df |> rename(race0=race) |>
    mutate(
        race = case_when(
            race0==100 ~ "White",
            race0==200 ~ "Black"
        )
)


