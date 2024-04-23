

library(tidyverse)
library(readxl)

datadir = "C:/Users/dcosic/Box/Data/Student Loans/Excel/"

fname_shares = paste0(datadir, "BPS share borrow.xlsx")


read_shares = function(fname)
{
    col_names = c("race", "gender", "income_all", "income1", "income2", "income3")
    col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric")
    range="B6:G18"
    sheets = list(
        list(sheet="Dependent BA",     desc="Dependent BA"),
        list(sheet="Dependent non-BA", desc="Dependent non-BA")
    )
    read_share_sheet = function(x, fname)
    {
        read_excel(
            path=fname_shares,
            sheet=x[['sheet']],
            col_names=col_names,
            col_types=col_types,
            range=range
        ) |>
            filter(!is.na(race)) |>
            mutate(desc=x[['desc']])
    }

    return(bind_rows(map(sheets, read_share_sheet, fname)))
}


shares_df = read_shares(fname_shares)

