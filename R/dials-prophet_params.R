#' Tuning Parameters for Prophet Models
#'
#' @inheritParams dials::learn_rate
#' @param values A character string of possible values.
#'
#' @details
#' The main parameters for Prophet models are:
#'
#' - `growth`: The form of the trend: "linear", or "logistic".
#' - `changepoint_num`: The maximum number of trend changepoints allowed when modeling the trend
#' - `changepoint_range`: The range affects how close the changepoints can go to the end of the time series.
#'   The larger the value, the more flexible the trend.
#' - Yearly, Weekly, and Daily Seasonality:
#'     - _Yearly_: `seasonality_yearly` - Useful when seasonal patterns appear year-over-year
#'     - _Weekly_: `seasonality_weekly` - Useful when seasonal patterns appear week-over-week (e.g. daily data)
#'     - _Daily_: `seasonality_daily` - Useful when seasonal patterns appear day-over-day (e.g. hourly data)
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
#' changepoint_num()
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
        # default  = "linear",
        label    = c(growth = "Growth Trend"),
        finalize = NULL
    )
}

#' @export
#' @rdname prophet_params
changepoint_num <- function(range = c(0L, 50L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(changepoint_num = "Number of Possible Trend Changepoints"),
        finalize  = NULL
    )
}

#' @export
#' @rdname prophet_params
changepoint_range <- function(range = c(0.6, 0.9), trans = NULL) {
    dials::new_quant_param(
        type      = "double",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(changepoint_range = "Range of Trend Changepoints"),
        finalize  = NULL
    )
}

#' @export
#' @rdname prophet_params
seasonality_yearly <- function(values = c(TRUE, FALSE)) {
    dials::new_qual_param(
        type     = c("logical"),
        values   = values,
        # default  = TRUE,
        label    = c(seasonality_yearly = "Use Yearly Seasonality"),
        finalize = NULL
    )
}

#' @export
#' @rdname prophet_params
seasonality_weekly <- function(values = c(TRUE, FALSE)) {
    dials::new_qual_param(
        type     = c("logical"),
        values   = values,
        # default  = TRUE,
        label    = c(seasonality_weekly = "Use Weekly Seasonality"),
        finalize = NULL
    )
}

#' @export
#' @rdname prophet_params
seasonality_daily <- function(values = c(TRUE, FALSE)) {
    dials::new_qual_param(
        type     = c("logical"),
        values   = values,
        # default  = TRUE,
        label    = c(seasonality_daily = "Use Daily Seasonality"),
        finalize = NULL
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



