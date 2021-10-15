#' Tuning Parameters for ADAM Models
#'
#'
#' @inheritParams dials::Laplace
#'
#' @details
#' The main parameters for ADAM models are:
#'
#'  - `non_seasonal_ar`: The order of the non-seasonal auto-regressive (AR) terms.
#'  - `non_seasonal_differences`: The order of integration for non-seasonal differencing.
#'  - `non_seasonal_ma`: The order of the non-seasonal moving average (MA) terms.
#'  - `seasonal_ar`: The order of the seasonal auto-regressive (SAR) terms.
#'  - `seasonal_differences`: The order of integration for seasonal differencing.
#'  - `seasonal_ma`: The order of the seasonal moving average (SMA) terms.
#'  - `use_constant`: Logical, determining, whether the constant is needed in the model or not.
#'  - `regressors_treatment`: The variable defines what to do with the provided explanatory variables.
#'  - `outliers_treatment`: Defines what to do with outliers.
#'  - `probability_model`: The type of model used in probability estimation.
#'  - `distribution`: What density function to assume for the error term.
#'  - `information_criteria`: The information criterion to use in the model selection / combination procedure.
#'  - `select_order`: If TRUE, then the function will select the most appropriate order.
#'
#'
#' @examples
#' use_constant()
#'
#' regressors_treatment()
#'
#' distribution()
#'
#'
#' @name adam_params


#' @export
#' @return A parameter
#' @rdname adam_params
use_constant <- function() {
    dials::new_qual_param(
        type      = "logical",
        default   = FALSE,
        values    = c(FALSE, TRUE),
        label     = c(use_constant = "Logical, determining, whether the constant is needed in the model or not"),
        finalize  = NULL
    )
}


#' @export
#' @return A parameter
#' @rdname adam_params
regressors_treatment <- function() {
    dials::new_qual_param(
        type      = "character",
        values    = c("use", "select", "adapt"),
        label     = c(regressors_treatment = "The variable defines what to do with the provided explanatory variables."),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname adam_params
outliers_treatment <- function() {
    dials::new_qual_param(
        type      = "character",
        values    = c("use", "select", "ignore"),
        label     = c(outliers_treatment = "Defines what to do with outliers."),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname adam_params
probability_model <- function() {
    dials::new_qual_param(
        type      = "character",
        values    = c("none", "auto", "fixed", "general", "odds-ratio",
                      "inverse-odds-ratio", "direct"),
        label     = c(probability_model = "The type of model used in probability estimation."),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname adam_params
distribution <- function() {
    dials::new_qual_param(
        type      = "character",
        values    = c("default", "dnorm", "dlaplace", "ds", "dgnorm",
                      "dlnorm", "dinvgauss", "dgamma"),
        label     = c(distribution = "What density function to assume for the error term."),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname adam_params
information_criteria <- function() {
    dials::new_qual_param(
        type      = "character",
        values    = c("AICc", "AIC", "BIC", "BICc"),
        label     = c(information_criteria = "The information criterion to use in the model selection / combination procedure."),
        finalize  = NULL
    )
}

#' @export
#' @return A parameter
#' @rdname adam_params
select_order <- function() {
    dials::new_qual_param(
        type      = "logical",
        default   = FALSE,
        values    = c(FALSE, TRUE),
        label     = c(select_order = "If TRUE, then the function will select the most appropriate order."),
        finalize  = NULL
    )
}
