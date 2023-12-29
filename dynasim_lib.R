

# Get the wage index for the survey year
widx = read.csv(paste0(here::here(), "/windex.csv"))
widx1997 = (widx |> filter(year==1997) |> select(windex) |> pull()) * 1000


#' Returns DYNASIM-compatible model names based on sex and
#'
#' @param data - dataframe with columns sex and race
#'
dyn_get_sexrace_names = function(data)
{
    return(data |>
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
    )
}


dyn_coef_to_df = function(models)
{

    # Pad variable names to 8 characters and enclose them in
    # single, not fancy, quotation marks
    varnames = toupper(names(coef(models[[1]])))

    # Create a dataframe with models' coefficients
    df = as_tibble(map(models, coef)) |>
        mutate(
            N=format(row_number(), width=2, justify="right"),
            varname=varnames
        ) |>
        filter(!grepl('.*_NA', varname)) |>
        mutate(
            varname =gsub('YEARF','YEAR',varname),
            varname =if_else(varname=="(INTERCEPT)","INTERCPT",varname),
            varname = str_trunc(varname,8,ellipsis = ''),
            varname = str_pad(varname,8,side='right'),
            varname = sQuote(varname, q=FALSE),
            across(-c(N,varname), ~round(.x, digits=6)),
            across(-c(N,varname), ~format(.x, width=10, digits=6, zero.print="0.")),
            across(-c(N,varname), ~gsub('NA','  ',.x))
        ) |>
        relocate(N, varname)
    return(df)
}

dyn_merge_results = function(dfs)
{


    for(i in 1:length(dfs)) {

    }


}

#' Writes coefficients of a list of models into a DYNASIM coefficient file
#'
#' @param models - list of models
#' @param filename - path of output file
#' @param description - a short description
dyn_write_coef_file = function(models, filename, description)
{

    # Create a dataframe with models' coefficients
    df = dyn_coef_to_df(models)

    modnames = names(models)

    # Number of variables and models
    nv = dim(df)[1]
    ng = dim(df)[2]

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


dyn_wealth_xform = function(var, type="log")
{
    # Logarithmic transformation
    if(type == "log") {
        # Divide by the 1997 wage index
        # Cap negative values and shift them to be positive
        # Calculate log
        return(log(pmax({{var}}/widx1997,-2.9) + 3))
    }
    # Inverse hyperbolic sine transformation
    else if(type == "ihs") {
        x = pmax({{var}}/widx1997,-3)
        return(log(sqrt(x^2+1)+x))
    }
    else {
        stop(paste0(type, " not implemented"))
    }
}

