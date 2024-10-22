#' Tuning Parameters for ARIMA Models
#'
#'
#' @inheritParams dials::Laplace
#'
#' @details
#' The main parameters for ARIMA models are:
#'
#'  - `non_seasonal_ar`: The order of the non-seasonal auto-regressive (AR) terms.
#'  - `non_seasonal_differences`: The order of integration for non-seasonal differencing.
#'  - `non_seasonal_ma`: The order of the non-seasonal moving average (MA) terms.
#'  - `seasonal_ar`: The order of the seasonal auto-regressive (SAR) terms.
#'  - `seasonal_differences`: The order of integration for seasonal differencing.
#'  - `seasonal_ma`: The order of the seasonal moving average (SMA) terms.
#'
#' @examples
#' ets_model()
#'
#' non_seasonal_ar()
#'
#' non_seasonal_differences()
#'
#' non_seasonal_ma()
#'
#'
#' @name arima_params


#' @export
#' @rdname arima_params
non_seasonal_ar <- function(range = c(0L, 5L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(non_seasonal_ar = "Non-seasonal AR Term"),
        finalize  = NULL
    )
}

#' @export
#' @rdname arima_params
non_seasonal_differences <- function(range = c(0L, 2L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(non_seasonal_differences = "Non-seasonal Differencing Term"),
        finalize  = NULL
    )
}

#' @export
#' @rdname arima_params
non_seasonal_ma <- function(range = c(0L, 5L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(non_seasonal_ma = "Non-seasonal MA Term"),
        finalize  = NULL
    )
}

#' @export
#' @rdname arima_params
seasonal_ar <- function(range = c(0L, 2L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(seasonal_ar = "Seasonal AR Term"),
        finalize  = NULL
    )
}

#' @export
#' @rdname arima_params
seasonal_differences <- function(range = c(0L, 1L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(seasonal_differences = "Seasonal Differencing Term"),
        finalize  = NULL
    )
}

#' @export
#' @rdname arima_params
seasonal_ma <- function(range = c(0L, 2L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(seasonal_ma = "Seasonal MA Term"),
        finalize  = NULL
    )
}
