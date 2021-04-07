#' Tuning Parameters for Croston Models
#'
#'
#' @inheritParams dials::Laplace
#'
#' @details
#' The main parameters for Croston models are:
#'
#'  - `alpha`: The order of the non-seasonal auto-regressive (AR) terms.
#'
#' @examples
#' alpha()
#'
#'
#' @name croston_params


#' @export
#' @rdname croston_params
alpha <- function(range = c(0, 1), trans = NULL) {
    dials::new_quant_param(
        type      = "double",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(alpha = "The smoothing parameters of the two applications of SES"),
        finalize  = NULL
    )
}

