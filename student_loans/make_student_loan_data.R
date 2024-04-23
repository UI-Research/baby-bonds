

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
        list(sheet="Dependent BA",     taxstatus="Dependent", educ="BA"),
        list(sheet="Dependent non-BA", taxstatus="Dependent", educ="Some college")
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
            mutate(
                taxstatus=x[['taxstatus']],
                educ=x[['educ']]
            )
    }

    return(bind_rows(map(sheets, read_share_sheet, fname)))
}


shares_df = read_shares(fname_shares)


read_amounts = function()
{
    range="B6:H18"
    col_names=c("race", "gender", "p10", "p25", "p50", "p75", "p90")
    col_types=rep("text", 7)

    fname_amts = list(
        list(fname="BPS amt borrow dep BA.xlsx",       taxstatus="Dependent",   educ="BA"),
        list(fname="BPS amt borrow dep non-BA.xlsx",   taxstatus="Dependent",   educ="Some college"),
        list(fname="BPS amt borrow indep BA.xlsx",     taxstatus="Independent", educ="BA"),
        list(fname="BPS amt borrow indep non-BA.xlsx", taxstatus="Independent", educ="Some college")
    )

    read_amt_file = function(x)
    {
        read_excel(
            path=paste0(datadir, x[['fname']]),
            sheet="All incomes",
            col_names=col_names,
            col_types=col_types,
            range=range
        ) |>
            filter(!is.na(race)) |>
            mutate(
                taxstatus=x[['taxstatus']],
                educ=x[['educ']]
            )
    }

    return(
        bind_rows(map(fname_amts, read_amt_file)) |>
            mutate(across(starts_with('p'), ~gsub(',', '', .x))) |>
            mutate(across(starts_with('p'), ~gsub('\\..+', '', .x))) |>
            mutate(across(starts_with('p'), ~gsub('\\D+', '', .x))) |>
            mutate(across(starts_with('p'), as.numeric))
    )
}

amt_df = read_amounts()


