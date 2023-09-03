#' Tuning Parameters for TEMPORAL HIERARCHICAL Models
#'
#' @param values A character string of possible values.
#'
#' @details
#' The main parameters for Temporal Hierarchical models are:
#'
#'  - `combination_method`: Combination method of temporal hierarchies.
#'  - `use_model`: Model used for forecasting each aggregation level.
#'
#' @examples
#' combination_method()
#'
#' use_model()
#'
#' @name temporal_hierarchy_params


#' @export
#' @rdname temporal_hierarchy_params
combination_method <- function(values = c("struc", "mse", "ols", "bu", "shr", "sam")) {
    dials::new_qual_param(
        type      = "character",
        values    = values,
        label     = c(method = "Combination method of temporal hierarchies."),
        finalize  = NULL
    )
}


#' @export
#' @rdname temporal_hierarchy_params
use_model <- function() {
    dials::new_qual_param(
        type      = "character",
        values    = c("ets", "arima", "theta", "naive", "snaive"),
        label     = c(method = "Model used for forecasting each aggregation level."),
        finalize  = NULL
    )
}
