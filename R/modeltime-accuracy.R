# MODELTIME ACCURACY ----

#' Calculate Accuracy Metrics
#'
#' This is a wrapper for `yardstick` that simplifies time series regression accuracy metric
#' calculations from a fitted `workflow` (trained workflow) or `model_fit` (trained parsnip model).
#'
#' @param object A Modeltime Table
#' @param new_data A `tibble` to predict and calculate residuals on.
#'  If provided, overrides any calibration data.
#' @param metric_set A `yardstick::metric_set()` that is used to summarize one or more
#'  forecast accuracy (regression) metrics.
#' @param quiet Hide errors (`TRUE`, the default), or display them as they occur?
#' @param ... Not currently used
#'
#'
#' @return A tibble with accuracy estimates.
#'
#' @details
#'
#' The following accuracy metrics are included by default via [default_forecast_accuracy_metric_set()]:
#'
#' - MAE - Mean absolute error, [mae()]
#' - MAPE - Mean absolute percentage error, [mape()]
#' - MASE  - Mean absolute scaled error, [mase()]
#' - SMAPE - Symmetric mean absolute percentage error, [smape()]
#' - RMSE  - Root mean squared error, [rmse()]
#' - RSQ   - R-squared, [rsq()]
#'
#'
#'
#' @examples
#' library(tidymodels)
#' library(tidyverse)
#' library(lubridate)
#' library(timetk)
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
#' model_fit_arima <- arima_reg() %>%
#'     set_engine(engine = "auto_arima") %>%
#'     fit(value ~ date, data = training(splits))
#'
#'
#' # ---- MODELTIME TABLE ----
#'
#' models_tbl <- modeltime_table(
#'     model_fit_arima
#' )
#'
#' # ---- ACCURACY ----
#'
#' models_tbl %>%
#'     modeltime_calibrate(new_data = testing(splits)) %>%
#'     modeltime_accuracy(
#'         metric_set = metric_set(mae, rmse, rsq)
#'     )
#'
#'
#' @name modeltime_accuracy
NULL

#' @export
#' @rdname modeltime_accuracy
modeltime_accuracy <- function(object, new_data = NULL,
                               metric_set = default_forecast_accuracy_metric_set(),
                               quiet = TRUE, ...) {
    if (!is_calibrated(object)) {
       if (is.null(new_data)) {
           rlang::abort("Modeltime Table must be calibrated (see 'modeltime_calibrate()') or include 'new_data'.")
       }
    }

    UseMethod("modeltime_accuracy")
}

#' @export
modeltime_accuracy.default <- function(object, new_data = NULL,
                                       metric_set = default_forecast_accuracy_metric_set(),
                                       quiet = TRUE, ...) {
    rlang::abort(stringr::str_glue("Received an object of class: {class(object)[1]}. Expected an object of class:\n 1. 'mdl_time_tbl' - A Model Time Table made with 'modeltime_table()' and calibrated with 'modeltime_calibrate()'."))
}


#' @export
modeltime_accuracy.mdl_time_tbl <- function(object, new_data = NULL,
                                            metric_set = default_forecast_accuracy_metric_set(),
                                            quiet = TRUE, ...) {
    data <- object

    metrics <- metric_set

    # Handle New Data ----
    if (!is.null(new_data)) {
        data <- data %>%
            modeltime_calibrate(new_data = new_data)
    }


    # Accuracy Calculation ----
    safe_calc_accuracy <- purrr::safely(calc_accuracy_2, otherwise = NA, quiet = quiet)

    ret <- data %>%
        dplyr::ungroup() %>%
        dplyr::mutate(.nested.col = purrr::map(
            .x         = .calibration_data,
            .f         = function(.data) {
                ret <- safe_calc_accuracy(
                    test_data  = .data,
                    metric_set = metrics,
                    ...
                )

                ret <- ret %>% purrr::pluck("result")

                return(ret)
            })
        ) %>%
        dplyr::select(-.model, -.calibration_data) %>%
        tidyr::unnest(cols = .nested.col)

    if (".nested.col" %in% names(ret)) {
        ret <- ret %>%
            dplyr::select(-.nested.col)
    }

    return(ret)
}

# DEFAULT FORECAST ACCURACY METRIC SET ----

#' Forecast Accuracy Metrics Sets
#'
#'
#' This is a wrapper for [metric_set()] with several common forecast / regression
#' accuracy metrics included. These are the default time series accuracy
#' metrics used with [modeltime_accuracy()].
#'
#' @param ... Add additional `yardstick` metrics
#'
#' @details
#'
#' The primary purpose is to use the default accuracy metrics to calculate the following
#' forecast accuracy metrics using [modeltime_accuracy()]:
#' - MAE   - Mean absolute error, [mae()]
#' - MAPE  - Mean absolute percentage error, [mape()]
#' - MASE  - Mean absolute scaled error, [mase()]
#' - SMAPE - Symmetric mean absolute percentage error, [smape()]
#' - RMSE  - Root mean squared error, [rmse()]
#' - RSQ   - R-squared, [rsq()]
#'
#' Adding additional metrics is possible via `...`.
#'
#' @seealso
#' - [metric_tweak()] - For modifying `yardstick` metrics
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' library(timetk)
#' library(yardstick)
#'
#' fake_data <- tibble(
#'     y    = c(1:12, 2*1:12),
#'     yhat = c(1 + 1:12, 2*1:12 - 1)
#' )
#'
#' # ---- HOW IT WORKS ----
#'
#' # Default Forecast Accuracy Metric Specification
#' default_forecast_accuracy_metric_set()
#'
#' # Create a metric summarizer function from the metric set
#' calc_default_metrics <- default_forecast_accuracy_metric_set()
#'
#' # Apply the metric summarizer to new data
#' calc_default_metrics(fake_data, y, yhat)
#'
#' # ---- ADD MORE PARAMETERS ----
#'
#' # Can create a version of mase() with seasonality = 12 (monthly)
#' mase12 <- metric_tweak(mase, m = 12)
#'
#' # Add it to the default metric set
#' my_metric_set <- default_forecast_accuracy_metric_set(mase12)
#' my_metric_set
#'
#' # Apply the newly created metric set
#' my_metric_set(fake_data, y, yhat)
#'
#'
#' @export
#' @importFrom yardstick mae mape mase smape rmse rsq
default_forecast_accuracy_metric_set <- function(...) {
    yardstick::metric_set(
        mae,
        mape,
        mase,
        smape,
        rmse,
        rsq,
        ...
    )
}


# SUMMARIZE ACCURACY ----

#' Summarize Accuracy Metrics
#'
#' This is an internal function used by `modeltime_accuracy()`.
#'
#' @inheritParams modeltime_accuracy
#' @param data  A `data.frame` containing the truth and estimate columns.
#' @param truth The column identifier for the true results (that is numeric).
#' @param estimate The column identifier for the predicted results (that is also numeric).
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#'
#' predictions_tbl <- tibble(
#'     group = c("model 1", "model 1", "model 1",
#'               "model 2", "model 2", "model 2"),
#'     truth = c(1, 2, 3,
#'               1, 2, 3),
#'     estimate = c(1.2, 2.0, 2.5,
#'                  0.9, 1.9, 3.3)
#' )
#'
#' predictions_tbl %>%
#'     group_by(group) %>%
#'     summarize_accuracy_metrics(
#'         truth, estimate,
#'         metric_set = default_forecast_accuracy_metric_set()
#'     )
#'
#'
#' @export
summarize_accuracy_metrics <- function(data, truth, estimate, metric_set) {

    data_tbl <- data

    truth_expr    <- rlang::enquo(truth)
    estimate_expr <- rlang::enquo(estimate)

    metric_summarizer_fun <- metric_set

    group_nms <- dplyr::group_vars(data_tbl)

    data_tbl %>%
        metric_summarizer_fun(!! truth_expr, !! estimate_expr) %>%
        dplyr::select(-.estimator) %>%

        dplyr::group_by(!!! rlang::syms(group_nms)) %>%
        dplyr::mutate(.metric = make.unique(.metric, sep = "_")) %>%
        dplyr::ungroup() %>%

        tidyr::pivot_wider(
            names_from  = .metric,
            values_from = .estimate
        )

}

# METRIC TWEAK ----

#' Modify Yardstick Metric Functions
#'
#' Used to modify `yardstick` functions, which have parameters that
#' need to be adjusted.
#'
#' @param .f A yardstick function (e.g. `mase`)
#' @param ... Parameters to overload (.e.g. `m = 1`)
#'
#' @details
#'
#' This function was created to help users quickly modify
#' the [default_forecast_accuracy_metric_set()] using existing `yardstick`
#' functions with parameters that require adjustment.
#'
#' An example is [yardstick::mase()], which has a parameter `m = 1`.
#' The `m` parameter identifies the seasonality for the MASE calculation.
#' Users often need to adjust this to the dominant seasonality. This can be
#' quickly accomplished using code:
#'
#' ``` r
#' mase12 <- metric_tweak(mase, m = 12)
#' ```
#'
#' @examples
#' library(modeltime)
#' library(yardstick)
#' library(tibble)
#'
#' fake_data <- tibble(
#'     y    = c(1:12, 2*1:12),
#'     yhat = c(1 + 1:12, 2*1:12 - 1)
#' )
#'
#' # Make new mase12 metric
#' mase12 <- metric_tweak(mase, m = 12)
#'
#' # Add to metric set
#' my_metric_set <- default_forecast_accuracy_metric_set(
#'     mase12
#' )
#' my_metric_set
#'
#' # Apply metric set to fake_data
#' my_metric_set(fake_data, y, yhat)
#'
#'
#'
#' @export
metric_tweak <- function(.f, ...) {

    f_attrs <- attributes(.f)

    ret <- purrr::partial(.f = .f, ...)

    attributes(ret) <- f_attrs

    return(ret)

}

# UTILITIES ----

calc_accuracy_2 <- function(train_data = NULL, test_data = NULL, metric_set, ...) {

    metrics <- metric_set

    # Training Metrics
    train_metrics_tbl <- tibble::tibble()

    # Testing Metrics
    test_metrics_tbl <- tibble::tibble()
    if (!is.null(test_data)) {

        test_metrics_tbl <- test_data %>%
            summarize_accuracy_metrics(
                truth      = .actual,
                estimate   = .prediction,
                metric_set = metrics
            ) %>%
            dplyr::ungroup()

    }

    metrics_tbl <- dplyr::bind_rows(train_metrics_tbl, test_metrics_tbl)

    return(metrics_tbl)
}
