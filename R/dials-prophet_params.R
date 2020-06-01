#' Tuning Parameters for Prophet Models
#'
#' @inheritParams dials::learn_rate
#' @param values A character string of possible values.
#'
#' @details
#' The main parameters for Prophet models are:
#'
#' - `growth`: The form of the trend: "linear", or "logistic".
#' - `num_changepoints`: The number of trend changepoints allowed in modeling the trend
#' - `season`:
#'     - The form of the seasonal term: "additive" or "multiplicative".
#'     - See [season()].
#' - "Prior Scale": Controls flexibility of
#'     - _Changepoints:_ `prior_scale_changepoints`
#'     - _Seasonality:_ `prior_scale_seasonality`
#'     - _Holidays:_ `prior_scale_holidays`
#'     - The `log10_trans()` converts priors to a scale from 0.001 to 100,
#'       which effectively weights lower values more heavily than larger values.
#'
#'
#'
#' @examples
#'
#' growth()
#'
#' num_changepoints()
#'
#' season()
#'
#' prior_scale_changepoints()
#'
#' @name prophet_params


#' @export
#' @rdname prophet_params
growth <- function(values = c("linear", "logistic")) {
    dials::new_qual_param(
        type     = c("character"),
        values   = values,
        default  = "linear",
        label    = c(linear = "Growth Trend"),
        finalize = NULL
    )
}

#' @export
#' @rdname prophet_params
num_changepoints <- function(range = c(0L, 50L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(num_changepoints = "Number of Trend Changepoints"),
        finalize  = NULL
    )
}

#' @export
#' @rdname prophet_params
#' @importFrom scales log10_trans
prior_scale_changepoints <- function(range = c(-3, 2), trans = log10_trans()) {
    dials::new_quant_param(
        type      = "double",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(prior_scale_changepoints = "Prior Scale Changepoints"),
        finalize  = NULL
    )
}

#' @export
#' @rdname prophet_params
#' @importFrom scales log10_trans
prior_scale_seasonality <- function(range = c(-3, 2), trans = log10_trans()) {
    dials::new_quant_param(
        type      = "double",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(prior_scale_seasonality = "Prior Scale Seasonality"),
        finalize  = NULL
    )
}

#' @export
#' @rdname prophet_params
#' @importFrom scales log10_trans
prior_scale_holidays <- function(range = c(-3, 2), trans = log10_trans()) {
    dials::new_quant_param(
        type      = "double",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(prior_scale_holidays = "Prior Scale Holidays"),
        finalize  = NULL
    )
}



