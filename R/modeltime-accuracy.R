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
           rlang::abort("Modeltime Table must be calibrated (see 'modeltime_calbirate()') or include 'new_data'.")
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
                    metric_set = metric_set,
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

# DEFAULT METRIC SET ----

#' Forecast Accuracy Metrics Sets
#'
#'
#' This is a wrapper for [metric_set()] with several common forecast / regression
#' accuracy metrics included. These are the default time series accuracy
#' metrics used with [modeltime_accuracy()].
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
#' @examples
#' library(tibble)
#' library(dplyr)
#' library(timetk)
#'
#' set.seed(1)
#' data <- tibble(
#'     time  = tk_make_timeseries("2020", by = "sec", length_out = 10),
#'     y     = 1:10 + rnorm(10),
#'     y_hat = 1:10 + rnorm(10)
#' )
#'
#' # Default Metric Specification
#' default_forecast_accuracy_metric_set()
#'
#' # Create a metric summarizer function from the metric set
#' calc_default_metrics <- default_forecast_accuracy_metric_set()
#'
#' # Apply the metric summarizer to new data
#' calc_default_metrics(data, y, y_hat)
#'
#' @export
#' @importFrom yardstick mae mape mase smape rmse rsq
default_forecast_accuracy_metric_set <- function() {
    yardstick::metric_set(
        mae,
        mape,
        mase,
        smape,
        rmse,
        rsq
    )
}

# UTILITIES ----

calc_accuracy_2 <- function(train_data, test_data = NULL, metric_set, ...) {

    # Training Metrics
    train_metrics_tbl <- tibble::tibble()

    # if (is.null(train_data)) {
    #     metrics_tbl <- tibble::tibble(
    #         .type = "Training"
    #     )
    # } else {
    #     metrics_tbl <- train_data %>%
    #         tibble::add_column(.type = "Training", .before = 1) %>%
    #         dplyr::group_by(.type) %>%
    #         summarize_accuracy_metrics(.value, .fitted, metric_set) %>%
    #         dplyr::ungroup()
    # }

    # if (!is.null(train_data)) {
    #     train_metrics_tbl <- train_data %>%
    #         tibble::add_column(.type = "Training", .before = 1) %>%
    #         dplyr::group_by(.type) %>%
    #         summarize_accuracy_metrics(.value, .fitted, metric_set) %>%
    #         dplyr::ungroup()
    # }

    # Testing Metrics
    test_metrics_tbl <- tibble::tibble()
    if (!is.null(test_data)) {

        test_metrics_tbl <- test_data %>%
            summarize_accuracy_metrics(.actual, .prediction, metric_set) %>%
            dplyr::ungroup()

    }

    metrics_tbl <- dplyr::bind_rows(train_metrics_tbl, test_metrics_tbl)

    return(metrics_tbl)
}

# calc_accuracy <- function(object, train_data, test_data = NULL, metric_set, ...) {
#
#     model_fit <- object
#
#
#     # Training Metrics
#     train_metrics_tbl <- tibble::tibble()
#
#     # if (is.null(train_data)) {
#     #     metrics_tbl <- tibble::tibble(
#     #         .type = "Training"
#     #     )
#     # } else {
#     #     metrics_tbl <- train_data %>%
#     #         tibble::add_column(.type = "Training", .before = 1) %>%
#     #         dplyr::group_by(.type) %>%
#     #         summarize_accuracy_metrics(.value, .fitted, metric_set) %>%
#     #         dplyr::ungroup()
#     # }
#
#     # if (!is.null(train_data)) {
#     #     train_metrics_tbl <- train_data %>%
#     #         tibble::add_column(.type = "Training", .before = 1) %>%
#     #         dplyr::group_by(.type) %>%
#     #         summarize_accuracy_metrics(.value, .fitted, metric_set) %>%
#     #         dplyr::ungroup()
#     # }
#
#     # Testing Metrics
#     test_metrics_tbl <- tibble::tibble()
#     if (!is.null(test_data)) {
#
#         predictions_tbl <- object %>%
#             modeltime_forecast(
#                 new_data      = test_data,
#                 actual_data   = test_data,
#                 conf_interval = NULL,
#                 ...
#             )
#
#
#         test_metrics_prepped_tbl <- predictions_tbl %>%
#             tidyr::pivot_wider(names_from = .key, values_from = .value) %>%
#             tidyr::drop_na() %>%
#             tibble::add_column(.type = "Test", .before = 1) %>%
#             dplyr::group_by(.type)
#
#         # test_metrics_residuals_tbl <- test_metrics_prepped_tbl %>%
#         #     dplyr::summarize(residuals = list(actual - prediction)) %>%
#         #     dplyr::ungroup()
#
#         test_metrics_tbl <- test_metrics_prepped_tbl %>%
#             summarize_accuracy_metrics(actual, prediction, metric_set) %>%
#             dplyr::ungroup()
#
#         # test_metrics_tbl <- dplyr::left_join(
#         #     test_metrics_residuals_tbl,
#         #     test_metrics_accuracy_tbl,
#         #     by = ".type")
#
#     }
#
#     metrics_tbl <- dplyr::bind_rows(train_metrics_tbl, test_metrics_tbl)
#
#     return(metrics_tbl)
# }

summarize_accuracy_metrics <- function(data, truth, estimate, metric_set) {

    truth_expr    <- rlang::enquo(truth)
    estimate_expr <- rlang::enquo(estimate)

    metric_summarizer_fun <- metric_set

    data %>%
        metric_summarizer_fun(!! truth_expr, !! estimate_expr) %>%
        dplyr::select(-.estimator) %>%
        # mutate(.metric = toupper(.metric)) %>%
        tidyr::pivot_wider(names_from = .metric, values_from = .estimate)

}
