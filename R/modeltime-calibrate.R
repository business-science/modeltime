#' Preparation for forecasting
#'
#'
#'
#' @param object A fitted model object that is either:
#' 1. A workflow that has been fit by [fit.workflow()] or
#' 2. A parsnip model that has been fit using [fit.model_spec()]
#' 3. A modeltime table that has been created using [modeltime_table()]
#' @param new_data A test data set `tibble` containing future information (timestamps and actual values).
#' @param quiet Hide errors (`TRUE`, the default), or display them as they occur?
#' @param ... Additional arguments passed to [modeltime_forecast()].
#'
#'
#' @return A `mdl_time_tbl` with `.calibration_data` added
#'
#' @details
#'
#' The results of calibration are used for:
#' - __Forecast Confidence Interval Estimation__: The out of sample residual data is used to calculate the
#'   confidence interval. Refer to [modeltime_forecast()].
#' - __Accuracy Calculations:__ The out of sample actual and prediction values are used to calculate
#'   performance metrics. Refer to [modeltime_accuracy()]
#'
#' The calibration steps include:
#'
#' 1. If not a Modeltime Table, objects are converted to Modeltime Tables internally
#' 2. Two Columns are added:
#'   - `.type`: Indicates the sample type. Only "Test" is currently available.
#'   - `.calibration_data`: Contains a tibble with Actual Values, Predictions and Residuals
#'    calculated from `new_data` (Test Data)
#'
#'
#' @examples
#' library(tidyverse)
#' library(lubridate)
#' library(timetk)
#' library(parsnip)
#' library(rsample)
#'
#' # Data
#' m750 <- m4_monthly %>% filter(id == "M750")
#'
#' # Split Data 80/20
#' splits <- initial_time_split(m750, prop = 0.9)
#'
#' # --- MODELS ---
#'
#' # Model 1: auto_arima ----
#' model_fit_no_boost <- arima_reg() %>%
#'     set_engine(engine = "auto_arima") %>%
#'     fit(value ~ date, data = training(splits))
#'
#' # Model 2: arima_boost ----
#' model_fit_boosted <- arima_boost(
#'     min_n = 2,
#'     learn_rate = 0.015
#' ) %>%
#'     set_engine(engine = "auto_arima_xgboost") %>%
#'     fit(value ~ date + as.numeric(date) + month(date, label = TRUE),
#'         data = training(splits))
#'
#' # ---- MODELTIME TABLE ----
#'
#' models_tbl <- modeltime_table(
#'     model_fit_no_boost,
#'     model_fit_boosted
#' )
#'
#' # ---- ACCURACY ----
#'
#' models_tbl %>%
#'     modeltime_calibrate(new_data = testing(splits)) %>%
#'     modeltime_accuracy()
#'
#' # ---- FORECAST ----
#'
#' models_tbl %>%
#'     modeltime_calibrate(new_data = testing(splits)) %>%
#'     modeltime_forecast(
#'         new_data    = testing(splits),
#'         actual_data = m750
#'     )
#'
#'
#' @name modeltime_calibrate
NULL

#' @export
#' @rdname modeltime_calibrate
modeltime_calibrate <- function(object, new_data = NULL,
                                quiet = TRUE, ...) {
    UseMethod("modeltime_calibrate")
}

#' @export
modeltime_calibrate.default <- function(object, new_data = NULL,
                                        quiet = TRUE, ...) {
    glubort("Received an object of class: {class(object)[1]}. Expected an object of class:\n 1. 'workflow' - That has been fitted (trained).\n 2. 'model_fit' - A fitted parsnip model.\n 3. 'mdl_time_tbl' - A Model Time Table made with 'modeltime_table()'.")
}

#' @export
modeltime_calibrate.model_spec <- function(object, new_data = NULL,
                                           quiet = TRUE, ...) {
    rlang::abort("Model spec must be trained using the 'fit()' function.")
}

#' @export
modeltime_calibrate.model_fit <- function(object, new_data = NULL,
                                          quiet = TRUE, ...) {

    ret <- modeltime_table(object) %>%
        modeltime_calibrate(new_data = new_data, quiet = quiet, ...)

    message("Converting to Modeltime Table.")

    return(ret)

}

#' @export
modeltime_calibrate.workflow <- function(object, new_data = NULL,
                                         quiet = TRUE, ...) {

    # Checks
    if (!object$trained) {
        rlang::abort("Workflow must be trained using the 'fit()' function.")
    }

    ret <- modeltime_table(object) %>%
        modeltime_calibrate(new_data = new_data, quiet = quiet, ...)

    message("Converting to Modeltime Table.")

    return(ret)

}

#' @export
modeltime_calibrate.mdl_time_tbl <- function(object, new_data = NULL,
                                             quiet = TRUE, ...) {
    data <- object

    safe_calc_residuals <- purrr::safely(calc_residuals, otherwise = NA, quiet = quiet)

    ret <- data %>%
        dplyr::ungroup() %>%
        dplyr::mutate(.nested.col = purrr::map(
            .x         = .model,
            .f         = function(obj) {
                ret <- safe_calc_residuals(
                    obj,
                    test_data = new_data
                )

                ret <- ret %>% purrr::pluck("result")

                return(ret)
            })
        ) %>%
        # dplyr::select(-.model) %>%
        tidyr::unnest(cols = .nested.col)

    if (".nested.col" %in% names(ret)) {
        ret <- ret %>%
            dplyr::select(-.nested.col)
    }

    if (!"mdl_time_tbl" %in% class(ret)) {
        class(ret) <- c("mdl_time_tbl", class(ret))
    }

    return(ret)
}



# UTILITIES ----

calc_residuals <- function(object, test_data = NULL, ...) {

    model_fit <- object

    # Training Metrics
    train_metrics_tbl <- tibble::tibble()

    # Testing Metrics
    test_metrics_tbl <- tibble::tibble()
    if (!is.null(test_data)) {

        predictions_tbl <- object %>%
            mdl_time_forecast(
                new_data      = test_data,
                actual_data   = test_data,
                conf_interval = NULL,
                ...
            )

        test_metrics_prepped_tbl <- predictions_tbl %>%
            tidyr::pivot_wider(names_from = .key, values_from = .value) %>%
            tidyr::drop_na() %>%
            tibble::add_column(.type = "Test", .before = 1) %>%
            dplyr::group_by(.type)

        test_metrics_tbl <- test_metrics_prepped_tbl %>%
            dplyr::summarize(.calibration_data = list(
                    tibble::tibble(
                        .actual     = actual,
                        .prediction = prediction,
                        .residuals  = actual - prediction
                    )
                )
            ) %>%
            dplyr::ungroup()

    }

    metrics_tbl <- dplyr::bind_rows(train_metrics_tbl, test_metrics_tbl)

    return(metrics_tbl)
}


