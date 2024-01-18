# Function to interpolate for each group (ID) in the data
#' @param interp_type: Type of interpolation, either approx for linear or spline for nonlinear
#' @param variable: Either "income" or "networth"
#' @param grouped_df: data frame with year, variable to interpolate, and ID

grouped_interpolate <- function(grouped_df, variable, interp_type = approx) {
    if (variable == 'income'){
        interp <- interp_type(x = grouped_df$year, y = grouped_df$income, xout = grouped_df$year)$y
    }
    else if (variable == 'networth'){
        interp <- interp_type(x = grouped_df$year, y = grouped_df$networth, xout = grouped_df$year)$y
    }
    return(data.frame(year = grouped_df$year, interp) |>
               rename(!!variable := interp))
}


# Function to reverse a time series for backcasting
reverse_ts <- function(y)
{
    ts(rev(y), start=tsp(y)[1L], frequency=frequency(y))
}
# Function to reverse a forecast
reverse_forecast <- function(object)
{
    h <- length(object[["mean"]])
    f <- frequency(object[["mean"]])
    object[["x"]] <- reverse_ts(object[["x"]])
    object[["mean"]] <- ts(rev(object[["mean"]]),
                           end=tsp(object[["x"]])[1L]-1/f, frequency=f)
    object[["lower"]] <- object[["lower"]][h:1L,]
    object[["upper"]] <- object[["upper"]][h:1L,]
    return(object)
}

#' This function creates time series for income and wealth for each ID
#' @param data: A data frame that has been nested such that the column "data"
#' contains data frames with the interpolated income and networth for each ID
#' @param variable: Either "income" or "networth"
create_ts_by_id <- function(data, variable){
    data_filtered <- data |>
        filter(!is.na({{variable}})) |>
        pull({{variable}})

    start <- data |> filter(!is.na({{variable}})) |> head(1) |> pull(year)
    end <- data |> filter(!is.na({{variable}})) |> tail(1) |> pull(year)

    group_ts <- ts(data_filtered, start=start, end=end)
    return(group_ts)
}

#' Extrapolate (forecast and backcast) for a time series
#' @param ts: A time series object
#' @param method: A forecasting method, default's to Holt's Linear Trend
#' @param variable: Either "income" or "networth"
extrap_ts <- function(ts, start, end, variable, id, method=holt, min_len = 5){
    # Return none if ID has fully missing info
    if((variable == 'income' & id %in% missing_ids_income) |
       (variable == 'networth' & id %in% missing_ids_nw)){
        return(c())
    }
    # Checks if the ts is sufficiently long enough to be forecasted
    if (length(na.omit(ts)) < min_len){
        return(c())
    }

    backcast <- c()
    forecast <- c()
    if(start > 1997){
        # Pipe functionality not working...but pipeline is reverse_ts -> holt -> forecast -> reverse_forecast
        cast <- reverse_forecast(
            forecast(
                holt(
                    reverse_ts(ts), h=(start-1997)
                )
            )
        )
        backcast <- cast$mean
    }
    if(end < 2019){
        cast <- forecast(
            holt(ts, h=(2019-end))
        )
        forecast <- cast$mean
    }
    return(c(backcast, na.omit(ts), forecast))
}
