

library(tidyverse)
library(readxl)

datadir = "C:/Users/dcosic/Box/Data/Student Loans/Excel/"

fname_shares = paste0(datadir, "BPS share borrow.xlsx")


#' Removes non-digit characters from selected columns and converts them to nums
clean_nums = function(data, start_str)
{
    return(data |> mutate(across(starts_with(start_str), ~gsub(',', '', .x))) |>
               mutate(across(starts_with(start_str), ~gsub('\\..+', '', .x))) |>
               mutate(across(starts_with(start_str), ~gsub('\\D+', '', .x))) |>
               mutate(across(starts_with(start_str), as.numeric))
    )
}

recode_race = function(data)
{
    return(
        data |>
            mutate(
                race = case_match(
                    race,
                    "Black or African American" ~ "Black",
                    "Hispanic or Latino"        ~ "Hispanic",
                    .default = race
                )
            )
    )
}

#' Reads data on shares of students with student debt
read_shares = function(fname)
{
    col_names = c("race", "sex", "income_all", "income1", "income2", "income3")
    col_types = rep("text", 6)
    range="B6:G21"
    sheets = list(
        list(sheet="Dependent BA",       taxstatus="Dependent",   educ="BA"),
        list(sheet="Dependent non-BA",   taxstatus="Dependent",   educ="Some college"),
        list(sheet="Independent BA",     taxstatus="Independent", educ="BA"),
        list(sheet="Independent non-BA", taxstatus="Independent", educ="Some college")
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

    return(
        bind_rows(map(sheets, read_share_sheet, fname)) |>
            clean_nums('income') |>
            recode_race()
    )
}

read_amounts = function()
{
    range="B6:H21"
    col_names=c("race", "sex", "p10", "p25", "p50", "p75", "p90")
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
            clean_nums('p') |>
            recode_race()
    )
}





