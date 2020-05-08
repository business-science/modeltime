#' Tuning Parameters for ARIMA Models
#'
#'
#' @inheritParams dials::Laplace
#'
#' @details
#' The main parameters for ARIMA models are:
#'
#'  - `period`: The periodic nature of the seasonality. Set 1 for non-seasonal.
#'  - `p`: The order of the non-seasonal auto-regressive (AR) terms.
#'  - `d`: The order of integration for non-seasonal differencing.
#'  - `q`: The order of the non-seasonal moving average (MA) terms.
#'  - `P`: The order of the seasonal auto-regressive (SAR) terms.
#'  - `D`: The order of integration for seasonal differencing.
#'  - `Q`: The order of the seasonal moving average (SMA) terms.
#'
#' @examples
#' p()
#'
#' d()
#'
#' q()
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

#' @export
#' @rdname arima_params
p <- function(range = c(0L, 5L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(p = "Non-seasonal AR Term"),
        finalize  = NULL
    )
}

#' @export
#' @rdname arima_params
d <- function(range = c(0L, 2L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(d = "Non-seasonal Differencing Term"),
        finalize  = NULL
    )
}

#' @export
#' @rdname arima_params
q <- function(range = c(0L, 5L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(q = "Non-seasonal MA Term"),
        finalize  = NULL
    )
}

#' @export
#' @rdname arima_params
P <- function(range = c(0L, 2L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(P = "Seasonal AR Term"),
        finalize  = NULL
    )
}

#' @export
#' @rdname arima_params
D <- function(range = c(0L, 1L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(D = "Seasonal Differencing Term"),
        finalize  = NULL
    )
}

#' @export
#' @rdname arima_params
Q <- function(range = c(0L, 2L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(Q = "Seasonal MA Term"),
        finalize  = NULL
    )
}
