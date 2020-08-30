#' Tuning Parameters for NNETAR Models
#'
#' @inheritParams dials::epochs
#'
#' @details
#' The main parameters for NNETAR models are:
#'
#' - `non_seasonal_ar`: Number of non-seasonal auto-regressive (AR) lags. Often denoted "p" in pdq-notation.
#' - `seasonal_ar`: Number of seasonal auto-regressive (SAR) lags. Often denoted "P" in PDQ-notation.
#' - `hidden_units`: An integer for the number of units in the hidden model.
#' - `num_networks`: Number of networks to fit with different random starting weights. These are then averaged when producing forecasts.
#' - `penalty`: A non-negative numeric value for the amount of weight decay.
#' - `epochs`: An integer for the number of training iterations.
#'
#'
#' @seealso
#' [non_seasonal_ar()], [seasonal_ar()], [dials::hidden_units()], [dials::penalty()], [dials::epochs()]
#'
#' @examples
#'
#' num_networks()
#'
#' @name nnetar_params


#' @export
#' @rdname nnetar_params
num_networks <- function(range = c(1L, 100L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(num_networks = "Number of Neural Networks to Average"),
        finalize  = NULL
    )
}
