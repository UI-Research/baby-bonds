

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
    varnames = gsub('YEARF','YEAR',varnames)
    varnames[1] = 'INTERCPT'
    varnames = str_trunc(varnames,8,ellipsis = '')
    varnames = str_pad(varnames,8,side='right')
    varnames = sQuote(varnames, q=FALSE)

    # Create a dataframe with models' coefficients
    df = as_tibble(map(models, coef)) |>
        mutate(
            N=row_number(),
            varname=varnames,
            across(-c(N,varname), ~round(.x, digits=6)),
            across(-c(N,varname), ~format(.x, width=10, digits=6, zero.print="0."))
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
#' @param varnames - a list of variable names, up to 8 characters long
#' @param modnames - model names, up to 10 characters long
#' @param description - a short description
dyn_write_coef_file = function(models, filename, varnames, modnames, description)
{

    # Create a dataframe with models' coefficients
    df = as_tibble(map(models, coef)) |>
        mutate(
            N=row_number(),
            # Pad variable names to 8 characters and enclose them in
            # single, not fancy, quotation marks
            varname=str_pad(varnames, width=8, side='right'),
            varname=sQuote(varname, q=FALSE),
            across(-c(N,varname), ~round(.x, digits=6)),
            across(-c(N,varname), ~format(.x, width=10, digits=6, zero.print = "0."))
        ) |>
        relocate(N, varname)

    # Number of variables and models
    nv = dim(df)[1]
    ng = length(models)

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
