#' Tuning Parameters for Exponential Smoothing Models
#'
#' @inheritParams dials::Laplace
#' @param values A character string of possible values.
#'
#'
#' @details
#' The main parameters for Exponential Smoothing models are:
#'
#' - `error`: The form of the error term: additive", or "multiplicative".
#'  If the error is multiplicative, the data must be non-negative.
#' - `trend`: The form of the trend term: "additive", "multiplicative" or "none".
#' - `season`: The form of the seasonal term: "additive", "multiplicative" or "none"..
#' - `damping`: Apply damping to a trend: "damped", or "none".
#' - `smooth_level`: This is often called the "alpha" parameter used as the base level smoothing factor for exponential smoothing models.
#' - `smooth_trend`: This is often called the "beta" parameter used as the trend smoothing factor for exponential smoothing models.
#' - `smooth_seasonal`: This is often called the "gamma" parameter used as the seasonal smoothing factor for exponential smoothing models.
#'
#'
#' @examples
#'
#' error()
#'
#' trend()
#'
#' season()
#'
#' @name exp_smoothing_params


#' @export
#' @rdname exp_smoothing_params
error <- function(values = c("additive", "multiplicative")) {
    dials::new_qual_param(
        type     = c("character"),
        values   = values,
        # default  = "additive",
        label    = c(error = "Error Term"),
        finalize = NULL
    )
}

#' @export
#' @rdname exp_smoothing_params
trend <- function(values = c("additive", "multiplicative", "none")) {
    dials::new_qual_param(
        type     = c("character"),
        values   = values,
        # default  = "additive",
        label    = c(trend = "Trend Term"),
        finalize = NULL
    )
}

#' @export
#' @rdname exp_smoothing_params
trend_smooth <- function(values = c("additive", "multiplicative", "none", "additive_damped", "multiplicative_damped")) {
    dials::new_qual_param(
        type     = c("character"),
        values   = values,
        # default  = "additive",
        label    = c(trend = "Trend Term"),
        finalize = NULL
    )
}

#' @export
#' @rdname exp_smoothing_params
season <- function(values = c("additive", "multiplicative", "none")) {
    dials::new_qual_param(
        type     = c("character"),
        values   = values,
        # default  = "additive",
        label    = c(season = "Season Term"),
        finalize = NULL
    )
}

#' @export
#' @rdname exp_smoothing_params
damping <- function(values = c("none", "damped")) {
    dials::new_qual_param(
        type     = c("character"),
        values   = values,
        # default  = "none",
        label    = c(damping = "Damping Term"),
        finalize = NULL
    )
}

#' @export
#' @rdname exp_smoothing_params
damping_smooth <- function(range = c(0, 2), trans = NULL) {
    dials::new_quant_param(
        type      = "double",
        range     = range,
        inclusive = c(TRUE, TRUE),
        label     = c(damping_smooth = "Damping Term"),
        finalize  = NULL
    )
}

#' @export
#' @rdname exp_smoothing_params
smooth_level <- function(range = c(0, 1), trans = NULL) {
    dials::new_quant_param(
        type      = "double",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(smooth_level = "The smoothing factor for the base level (alpha) in exponential models"),
        finalize  = NULL
    )
}

#' @export
#' @rdname exp_smoothing_params
smooth_trend <- function(range = c(0, 1), trans = NULL) {
    dials::new_quant_param(
        type      = "double",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(smooth_trend = "The smoothing factor for the trend level (beta) in exponential models"),
        finalize  = NULL
    )
}

#' @export
#' @rdname exp_smoothing_params
smooth_seasonal <- function(range = c(0, 1), trans = NULL) {
    dials::new_quant_param(
        type      = "double",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(smooth_seasonal = "The smoothing factor for the seasonal level (gamma) in exponential models"),
        finalize  = NULL
    )
}

