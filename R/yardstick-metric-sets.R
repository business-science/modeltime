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
#' - [yardstick::metric_tweak()] - For modifying `yardstick` metrics
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
#' mase12 <- metric_tweak(.name = "mase12", .fn = mase, m = 12)
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
#' @importFrom yardstick mae mape mase smape rmse rsq metric_tweak
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


# UTILITIES ----

calc_accuracy_2 <- function(train_data = NULL, test_data = NULL, metric_set, by_id = FALSE, ...) {

    metrics <- metric_set

    # Training Metrics
    train_metrics_tbl <- tibble::tibble()

    # Testing Metrics
    test_metrics_tbl <- tibble::tibble()

    # Check by_id
    if (by_id) {
        if (".id" %in% names(test_data)) {
            test_data <- test_data %>%
                dplyr::group_by(.id)
        } else {
            rlang::warn("The `.id` column in calibration data was not detected. Global accuracy is being returned.")
        }

    }

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
