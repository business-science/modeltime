# DEFAULT FORECAST ACCURACY METRIC SET ----

#' Forecast Accuracy Metrics Sets
#'
#' This is a wrapper for `metric_set()` with several common forecast / regression
#' accuracy metrics included. These are the default time series accuracy
#' metrics used with [modeltime_accuracy()].
#'
#' @param ... Add additional `yardstick` metrics
#'
#' @details
#'
#' # Default Forecast Accuracy Metric Set
#'
#' The primary purpose is to use the default accuracy metrics to calculate the following
#' forecast accuracy metrics using [modeltime_accuracy()]:
#'
#' - MAE   - Mean absolute error, `mae()`
#' - MAPE  - Mean absolute percentage error, `mape()`
#' - MASE  - Mean absolute scaled error, `mase()`
#' - SMAPE - Symmetric mean absolute percentage error, `smape()`
#' - RMSE  - Root mean squared error, `rmse()`
#' - RSQ   - R-squared, `rsq()`
#'
#' Adding additional metrics is possible via `...`.
#'
#' # Extended Forecast Accuracy Metric Set
#'
#' Extends the default metric set by adding:
#'
#' - MAAPE - Mean Arctangent Absolute Percentage Error, [maape()].
#'   MAAPE is designed for intermittent data where MAPE returns `Inf`.
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
#' @name metric_sets
NULL

#' @importFrom yardstick mae mape mase smape rmse rsq
#' @export
#' @rdname metric_sets
default_forecast_accuracy_metric_set <- function(...) {
    yardstick::metric_set(
        yardstick::mae,
        yardstick::mape,
        yardstick::mase,
        yardstick::smape,
        yardstick::rmse,
        yardstick::rsq,
        ...
    )
}

# EXTENDED FORECAST ACCURACY METRIC SET ----

#' @importFrom yardstick mae mape mase smape rmse rsq
#' @export
#' @rdname metric_sets
extended_forecast_accuracy_metric_set <- function(...) {
    yardstick::metric_set(
        yardstick::mae,
        yardstick::mape,
        maape,
        yardstick::mase,
        yardstick::smape,
        yardstick::rmse,
        yardstick::rsq,
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
#'     dplyr::group_by(group) %>%
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
        metric_summarizer_fun(!!truth_expr, !!estimate_expr) %>%
        dplyr::select(-dplyr::any_of(".estimator")) %>%
        dplyr::group_by(!!!rlang::syms(group_nms)) %>%
        dplyr::mutate(.metric = make.unique(.metric, sep = "_")) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(
            names_from  = .metric,
            values_from = .estimate
        )
}

# -------------------------------------------------------------------------
# yardstick 1.2.0+ compatibility helpers (no deprecation warnings)
# -------------------------------------------------------------------------

.yardstick_numeric_summarizer <- function(metric_nm, metric_fn,
                                          data, truth, estimate,
                                          na_rm = TRUE,
                                          case_weights = NULL,
                                          ...) {
    ys_ver <- tryCatch(utils::packageVersion("yardstick"),
                       error = function(e) numeric_version("0.0.0"))

    if (!is.na(ys_ver) && ys_ver >= "1.2.0") {
        # New API: explicitly pass case_weights (NULL by default)
        yardstick::numeric_metric_summarizer(
            name         = metric_nm,
            fn           = metric_fn,
            data         = data,
            truth        = {{ truth }},
            estimate     = {{ estimate }},
            na_rm        = na_rm,
            case_weights = case_weights,
            fn_options   = list()
        )
    } else {
        # Legacy API (no case_weights argument)
        yardstick::metric_summarizer(
            metric_nm = metric_nm,
            metric_fn = metric_fn,
            data      = data,
            truth     = {{ truth }},
            estimate  = {{ estimate }},
            na_rm     = na_rm
        )
    }
}

.yardstick_check_and_remove_missing <- function(truth, estimate, na_rm = TRUE) {
    ys_ver <- tryCatch(utils::packageVersion("yardstick"),
                       error = function(e) numeric_version("0.0.0"))

    if (!is.na(ys_ver) && ys_ver >= "1.2.0") {
        yardstick::check_numeric_metric(truth, estimate, case_weights = NULL)
        if (isTRUE(na_rm) && yardstick::yardstick_any_missing(truth, estimate)) {
            rm <- yardstick::yardstick_remove_missing(
                truth, estimate, case_weights = NULL
            )
            list(truth = rm$truth, estimate = rm$estimate)
        } else {
            list(truth = truth, estimate = estimate)
        }
    } else {
        yardstick::metric_vec_template(truth = truth, estimate = estimate, na_rm = na_rm)
        list(truth = truth, estimate = estimate)
    }
}

# ---- MAAPE vector metric ------------------------------------------------------

#' Mean Arctangent Absolute Percentage Error
#' @param truth Numeric vector of ground-truth values.
#' @param estimate Numeric vector of predicted values.
#' @param na_rm Remove missing values before computation.
#' @param ... Unused.
#' @export
maape_vec <- function(truth, estimate, na_rm = TRUE, ...) {
    rlang::check_installed("TSrepr")

    chk <- .yardstick_check_and_remove_missing(truth, estimate, na_rm = na_rm)
    truth    <- chk$truth
    estimate <- chk$estimate

    TSrepr::maape(truth, estimate)
}

# ---- MAAPE data.frame metric --------------------------------------------------

#' Mean Arctangent Absolute Percentage Error
#' @param data A data frame.
#' @export
maape <- function(data, ...) {
    UseMethod("maape")
}
maape <- yardstick::new_numeric_metric(maape, direction = "minimize")

#' @export
maape.data.frame <- function(data, truth, estimate, na_rm = TRUE,
                             case_weights = NULL, ...) {
    .yardstick_numeric_summarizer(
        metric_nm     = "maape",
        metric_fn     = maape_vec,
        data          = data,
        truth         = {{ truth }},
        estimate      = {{ estimate }},
        na_rm         = na_rm,
        case_weights  = case_weights
    )
}


