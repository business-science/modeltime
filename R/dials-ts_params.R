#' Tuning Parameters for Time Series (ts-class) Models
#'
#'
#' @inheritParams dials::Laplace
#'
#' @details
#' Time series models (e.g. `Arima()` and `ets()`) use [stats::ts()]
#'  to apply seasonality. We can do the same process using the following
#'  general time series parameter:
#'
#'  - `period`: The periodic nature of the seasonality. Set 1 for non-seasonal.
#'
#' @examples
#' period()
#'
#'
#' @name arima_params


#' @export
#' @rdname arima_params
period <- function(range = c(1L, 12L), trans = NULL) {
    dials::new_quant_param(
        type = "integer",
        range = range,
        inclusive = c(TRUE, TRUE),
        trans = trans,
        label = c(period = "Period (Seasonal Frequency)"),
        finalize = NULL
    )
}

