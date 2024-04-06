
library(tidyverse)
library(broom)

# Get the wage index for the survey year
widx = read.csv(paste0(here::here(), "/windex.csv"))
widx1997 = (widx |> filter(year==1997) |> select(windex) |> pull()) * 1000


#' Returns DYNASIM-compatible model names based on sex and
#'
#' @param data - dataframe with columns sex and race
#'
dyn_get_sexrace_names = function(data)
{

    columns = colnames(data)
    res = NULL

    if( 'sex' %in% columns & 'race' %in% columns ) {
        res = data |>
            mutate(
                sexrace=str_trunc(
                    toupper(
                        paste0(
                            str_trunc(as.character(sex),1,ellipsis=''),
                            race)),
                    8,
                    ellipsis='')
                ) |>
            pull(sexrace)
    }
    else if( 'sex' %in% columns ) {
        res = data |>
            mutate(
                sexrace=str_pad(
                    toupper(as.character(sex)),
                    8,
                    side='right')
            ) |>
            pull(sexrace)
    }
    return(res)
}


dyn_coef_to_df = function(models, sig_level=1)
{

    # Pad variable names to 8 characters and enclose them in
    # single, not fancy, quotation marks
    varnames = toupper(names(coef(models[[1]])))

    varnames = gsub('YEARF',                 'YEAR',         varnames)
    varnames = gsub('\\(INTERCEPT\\)',       'INTERCEPT',    varnames)
    varnames = gsub('ASIAN\\:',              'ASN',          varnames)
    varnames = gsub('\\:PWEALTH(.+)',        'PW\\1',        varnames)
    varnames = gsub('PARED',                 'PED',          varnames)
    varnames = gsub('COLLEGE',               'COL',          varnames)
    varnames = gsub('NO\\>|ZERO\\>',         '0',            varnames)
    varnames = gsub('ONE\\>',                '1',            varnames)
    varnames = gsub('TWO\\>',                '2',            varnames)
    varnames = gsub('THREE\\>',              '3',            varnames)
    varnames = gsub('HIGH-SCHOOL GRADUATE',  'HSDIP',        varnames)
    varnames = gsub('SOME COLLEGE',          'SMCOL',        varnames)
    varnames = gsub('COLLEGE DEGREE',        'BSDEG',        varnames)
    varnames = gsub('GRADUATE DEGREE',       'GRDEG',        varnames)
    varnames = gsub(' ',                '',             varnames)

    varnames = str_trunc(varnames, 8, ellipsis = '')
    varnames = str_pad(varnames, 8, side='right')
    varnames = sQuote(varnames, q=FALSE)

    # Create a dataframe with models' coefficients
    df = as_tibble(map(models, dyn_get_coef, sig_level)) |>
        mutate(
            N=format(row_number(), width=2, justify="right"),
            varname=varnames
        ) |>
        filter(!grepl('.*_NA', varname)) |>
        mutate(
            across(-c(N,varname), ~round(.x, digits=6)),
            across(-c(N,varname), ~format(.x, width=10, digits=6, zero.print="0.")),
            across(-c(N,varname), ~gsub('\\s+NA','  0.      ',.x))
        ) |>
        relocate(N, varname)
    return(df)
}

#' Returns model's coefficients. Those not significant at sig_level are set to zero.
dyn_get_coef = function(model, sig_level=1)
{
    return(coef(model) * as.integer(tidy(model)$p.value<sig_level))
}

#' Uses the same list of variables in all models
#' If a variable does not appear in a model, it has a string of spaces
dyn_uniformize_coef = function(coefs)
{

    varnames=character(0)
    for(i in 1:length(coefs)) {
        varnames = union(varnames, coefs[[i]]$df$varname)
    }

    vardf = tibble(varname=varnames)
    for(i in 1:length(coefs)) {
        df = left_join(vardf, coefs[[i]]$df, by='varname') |>
        mutate(N=format(row_number(), width=2, justify="right")) |>
        mutate(across(-c(N,varname), ~replace_na(.x, '  0.      '))) |>
        relocate(N, varname)
        coefs[[i]]$df = df
    }
    return(coefs)
}

#' Writes coefficients of a list of models into a DYNASIM coefficient file
#'
#' @param df - dataframe with coefficiets
#' @param filename - path of output file
#' @param description - a short description
dyn_write_coef_file = function(df, filename, description)
{

    modnames = colnames(df)[-(1:2)]

    # Number of variables and models
    nv = dim(df)[1]
    ng = length(modnames)

    # File header
    #   - lines starting with ';' are comments
    #   - lines between $COEFINFO and $END are paramaters
    header = paste0(
        ';\n',
        '; ', gsub('\\.CSV', '', toupper(basename(filename))), ' - ', description, '\n',
        '; ', today(), '\n',
        ';\n',
        ';\n',
        ';', paste(rep(" ", 11), collapse=""),
        paste(format(modnames, width=11, justify='right'), collapse=""), '\n',
        paste0('+', paste(rep(rep('=',11), ng+1), collapse=""),'\n'),
        '$COEFINFO\n',
        ' NV = ', format(nv, width=4), ',\n',
        ' NG = ', format(ng, width=4), '\n',
        '$END\n'
    )

    # Write it to file
    cat(header, file=filename)
    write_csv(df, filename, append=TRUE)
}


dyn_wealth_xform = function(var, type="log", inverse=FALSE)
{
    # Offset used for log-transofmration
    log_offset = 3

    if(inverse) {
        # Logarithmic transformation
        if(type == "log") {
            # Divide by the 1997 wage index
            # Cap negative values and shift them to be positive
            # Calculate log
            return((exp({{var}})-log_offset)*widx1997)
        }
        # Inverse hyperbolic sine transformation
        else if(type == "ihs") {
            return(sinh(x)*widx1997)
        }
        else if(type == "log+") {
            return(exp({{var}}*widx1997))
        }
        else if(type == "log-") {
            return(-exp({{var}}*widx1997))
        }
        else if(type == "linear") {
            return({{var}}*widx1997)
        }
        else {
            stop(paste0(type, " not implemented"))
        }
    }

    # Logarithmic transformation
    if(type == "log") {
        # Divide by the 1997 wage index
        # Cap negative values and shift them to be positive
        # Calculate log
        return(log(pmax({{var}}/widx1997,-2.9) + 3))
    }
    # Inverse hyperbolic sine transformation
    else if(type == "ihs") {
        return(asinh(pmax({{var}}/widx1997,-3)))
    }
    else if(type == "log+") {
        return(if_else({{var}}>0, log({{var}}/widx1997),0,NA))
    }
    else if(type == "log-") {
        return(if_else({{var}}<0, log(-{{var}}/widx1997),0,NA))
    }
    else if(type == "linear") {
        return({{var}}/widx1997)
    }
    else {
        stop(paste0(type, " not implemented"))
    }
}

