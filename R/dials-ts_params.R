#' Tuning Parameters for Time Series (ts-class) Models
#'
#' @param values A time-based phrase
#'
#' @details
#'
#' Time series models (e.g. `Arima()` and `ets()`) use [stats::ts()] or [forecast::msts()]
#'  to apply seasonality. We can do the same process using the following
#'  general time series parameter:
#'
#'  - `period`: The periodic nature of the seasonality.
#'
#' It's usually best practice to _not_ tune this parameter, but rather set
#' to obvious values based on the seasonality of the data:
#'
#'  - __Daily Seasonality:__ Often used with __hourly data__ (e.g. 24 hourly timestamps per day)
#'  - __Weekly Seasonality:__ Often used with __daily data__ (e.g. 7 daily timestamps per week)
#'  - __Yearly Seasonalty:__ Often used with __weekly, monthly, and quarterly data__
#'    (e.g. 12 monthly observations per year).
#'
#' However, in the event that users want to experiment with period tuning, you
#' can do so with `seasonal_period()`.
#'
#' @examples
#' seasonal_period()
#'
#'
#'
#' @name time_series_params


#' @export
#' @rdname time_series_params
seasonal_period <- function(values = c("none", "daily", "weekly", "yearly")) {
    dials::new_qual_param(
        type     = c("character"),
        values   = values,
        # default  = "none",
        label    = c(period = "Period (Seasonal Frequency)"),
        finalize = NULL
    )
}

# period <- function(range = c(1L, 1L), trans = NULL) {
#     dials::new_quant_param(
#         type = "integer",
#         range = range,
#         inclusive = c(TRUE, TRUE),
#         trans = trans,
#         label = c(period = "Period (Seasonal Frequency)"),
#         finalize = NULL
#     )
# }




