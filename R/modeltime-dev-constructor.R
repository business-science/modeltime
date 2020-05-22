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
#' @param desc An optional model description to appear when printing your modeltime objects
#'
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
#'     models = list(model_1 = lm_model),
#'     data   = data,
#'     extras = NULL
#' )
#'
#'
#' @export
new_modeltime_bridge <- function(class, models, data, extras = NULL, desc = NULL) {

    if (missing(class)) rlang::abort("'class' must be a character vector. This is used to define a print method.")
    if (!is.character(class)) rlang::abort("'class' must be a character vector. This is used to define a print method.")

    msg <- "'models' should be a list:\n1. The first model should named 'model_1'.\n2. Subsequent models should be named 'model_2' and so on."
    if (missing(models)) rlang::abort(paste0("'models' is missing.\n\n", msg))
    if (!is.list(models)) rlang::abort(paste0("'models' is not a list().\n\n", msg))
    if (!all(stringr::str_detect(names(models), pattern = "^model_"))) rlang::abort(paste0("'model' has bad list names. Try naming 'model_1'.\n\n", msg))

    msg <- "'data' should be a data frame (or tibble) containing 4 columns:\n1. date column (with name that matches input data)\n2. .value (these are the original values your model predicts from)\n3. .fitted (these are your model's in-sample predictions)\n4. .resid (these are your errors)"
    if (missing(data)) rlang::abort(paste0("'data' is missing.\n\n", msg))
    if (!is.data.frame(data)) rlang::abort(paste0("'data' is not a data.frame\n\n", msg))
    if (ncol(data) != 4) rlang::abort(paste0("'data' does not have 4 columns\n\n", msg))
    if (!all(c(".value", ".fitted", ".resid") %in% names(data))) {
        rlang::abort(paste0("Column names don't contain: .value, .fitted, and .resid.\n\n", msg))
    }

    msg <- "'extras' should be a list. It's often used for adding preprocessing recipes."
    if (!is.null(extras)) {
        if (!is.list(extras)) rlang::abort(msg)
    }

    msg <- "'desc' should be a single character value. It's often used for printing a description of your model using a print method."
    if (!is.null(desc)) {
        if (!is.character(desc)) rlang::abort(paste0("'desc' is not of class character.\n", msg))
        if (length(desc) != 1) rlang::abort(paste0("'desc' length is not 1.\n", msg))
    }

    # CONSTRUCTOR
    ret <- list(
        models = models,
        data   = data,
        extras = extras,
        desc   = desc
    )

    class(ret) <- c(class, "modeltime_bridge")

    return(ret)

}

#' @export
print.modeltime_bridge <- function(x, ...) {
    if (!is.null(x$desc)) cat(paste0(x$desc, "\n"))
    print(x$models)
    invisible(x)
}
