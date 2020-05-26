#' Tuning Parameters for Exponential Smoothing Models
#'
#' @param values A character string of possible values.
#'
#' @details
#' The main parameters for Exponential Smoothing models are:
#'
#' - `error`: The form of the error term: additive", or "multiplicative".
#'  If the error is multiplicative, the data must be non-negative.
#' - `trend`: The form of the trend term: "additive", "multiplicative" or "none".
#' - `season`: The form of the seasonal term: "additive", "multiplicative" or "none"..
#' - `damping`: Apply damping to a trend: "damped", or "none".
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
        default  = "additive",
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
        default  = "additive",
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
        default  = "additive",
        label    = c(season = "Season Term"),
        finalize = NULL
    )
}

#' @export
#' @rdname exp_smoothing_params
damping <- function(values = c("damped", "none")) {
    dials::new_qual_param(
        type     = c("character"),
        values   = values,
        default  = "none",
        label    = c(damping = "Damping Term"),
        finalize = NULL
    )
}

