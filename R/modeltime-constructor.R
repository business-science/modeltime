#' Tools for creating modeltime models
#'
#' These functions are used to construct new `modeltime` bridge functions that
#' connect the `tidymodels` infrastructure to time-series models containing date or date-time features.
#'
#' @param class A class name that is used for creating custom printing messages
#' @param models A list containing one or more models
#' @param data A data frame (or tibble) containing 4 columns:
#'  (date column with name that matches input data), .value, .fitted, and .resid.
#' @param extras An optional list that is typically used for transferring preprocessing recipes
#'  to the predict method.
#'
#' @details
#' TODO - Add vignette on extending modeltime
#'
#' @examples
#' library(stats)
#' library(tidyverse)
#' library(lubridate)
#' library(timetk)
#'
#' lm_model <- lm(value ~ as.numeric(date) + hour(date) + wday(date, label = TRUE),
#'                data = taylor_30_min)
#'
#' data = tibble(
#'     date    = taylor_30_min$date, # Important - The column name must match the modeled data
#'     # These are standardized names: .value, .fitted, .resid
#'     .value  = taylor_30_min$value,
#'     .fitted = lm_model$fitted.values %>% as.numeric(),
#'     .resid  = lm_model$residuals %>% as.numeric()
#' )
#'
#' new_modeltime_bridge(
#'     class  = "lm_time_series_impl",
#'     models = list(
#'         model_1 = lm_model
#'     ),
#'     data   = data,
#'     extras = NULL
#' )
#'
#'
#' @export
new_modeltime_bridge <- function(class, models, data, extras = NULL) {

    if (missing(class)) rlang::abort("'class' must be a character vector. This is used to define a print method.")
    if (!is.character(class)) rlang::abort("'class' must be a character vector. This is used to define a print method.")

    if (missing(models)) rlang::abort("'models' should be a list.")
    if (!is.list(models)) rlang::abort("'models' should be a list.")

    if (missing(data)) rlang::abort("'data' should be a data frame (or tibble) containing 4 columns: (date column with name that matches input data), .value, .fitted, and .resid.")
    if (!is.data.frame(data)) rlang::abort("'data' should be a data frame (or tibble) containing 4 columns: (date column with name that matches input data), .value, .fitted, and .resid.")
    if (!all(c(".value", ".fitted", ".resid") %in% names(data))) {
        rlang::abort("'data' should be a data frame (or tibble) containing 4 columns: (date column with name that matches input data), .value, .fitted, and .resid.")
    }

    if (!is.null(extras)) {
        if (!is.list(extras)) rlang::abort("'extras' should be a list. It's often used for adding preprocessing recipes.")
    }

    # CONSTRUCTOR
    ret <- list(
        models = models,
        data   = data,
        extras = extras
    )

    class(ret) <- c(class, "modeltime_bridge")

    return(ret)

}

#' @export
print.modeltime_bridge <- function(x, ...) {
    print(x$models)
    invisible(x)
}
