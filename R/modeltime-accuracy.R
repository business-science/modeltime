# MODELTIME ACCURACY ----

#' Calculate Accuracy Metrics
#'
#' This is a wrapper for `yardstick` that simplifies accuracy metric
#' calculations from a fitted `workflow` (trained workflow) or `model_fit` (trained parsnip model).
#'
#' @param object A fitted model object that is either (1) a workflow that has been fit by [fit.workflow()] or
#'  (2) a parsnip model that has been fit using [fit.model_spec()]
#' @param new_data A `tibble` containing future information (timestamps and actual values).
#' @param metric_set A [metric_set()] that is used to summarize one or more
#'  forecast accuracy (regression) metrics.
#' @param ... Not currently used.
#'
#'
#' @return A tibble with accuracy estimates.
#'
#' @details
#'
#' The following accuracy metrics are included by default via [default_forecast_accuracy_metric_set()]:
#'
#' - MAE - Mean absolute error, [mae_vec()]
#' - MAPE - Mean absolute percentage error, [mape_vec()]
#' - MASE  - Mean absolute scaled errror, [mase_vec()]
#' - SMAPE - Symmetric mean absolute percentage error, [smape_vec()]
#' - RMSE  - Root mean squared error, [rmse_vec()]
#' - RSQ   - R-squared, [rsq_vec()]
#'
#'
#'
#' @examples
#' library(dplyr)
#' library(parsnip)
#' library(rsample)
#' library(yardstick)
#' library(timetk)
#' library(modeltime)
#'
#' # Data
#' m750 <- m4_monthly %>% filter(id == "M750")
#'
#' # Split Data 80/20
#' splits <- initial_time_split(m750, prop = 0.8)
#'
#' # Model Spec
#' model_spec <- arima_reg(
#'         period                   = 12,
#'         non_seasonal_ar          = 3,
#'         non_seasonal_differences = 1,
#'         non_seasonal_ma          = 3,
#'         seasonal_ar              = 1,
#'         seasonal_differences     = 0,
#'         seasonal_ma              = 1
#'     ) %>%
#'     set_engine("arima")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#'
#' # --- ACCURACY ---
#'
#' # Default accuracy metrics
#' model_fit %>%
#'     modeltime_accuracy(new_data = testing(splits))
#'
#' # Supply new accuracy metrics
#' model_fit %>%
#'     modeltime_accuracy(
#'         new_data   = testing(splits),
#'         metric_set = metric_set(mae, rmse)
#'     )
#'
#' @name modeltime_accuracy
NULL

#' @export
#' @rdname modeltime_accuracy
modeltime_accuracy <- function(object, new_data = NULL,
                               metric_set = default_forecast_accuracy_metric_set(), ...) {
    UseMethod("modeltime_accuracy")
}

#' @export
modeltime_accuracy.default <- function(object, new_data = NULL,
                                       metric_set = default_forecast_accuracy_metric_set(), ...) {
    rlang::abort(stringr::str_glue("Received an object of class: {class(object)[1]}. Expected an object of class 'workflow' that has been fitted (trained) or 'model_fit' (a fitted parsnip model)."))
}

#' @export
modeltime_accuracy.model_spec <- function(object, new_data = NULL,
                                          metric_set = default_forecast_accuracy_metric_set(), ...) {
    rlang::abort("Model spec must be trained using the 'fit()' function.")
}

#' @export
modeltime_accuracy.model_fit <- function(object, new_data = NULL,
                                         metric_set = default_forecast_accuracy_metric_set(), ...) {

    data <- object$fit$data

    ret <- calc_accuracy(object, train_data = data, test_data = new_data, metric_set = metric_set, ...)

    return(ret)

}

#' @export
modeltime_accuracy.workflow <- function(object, new_data = NULL,
                                        metric_set = default_forecast_accuracy_metric_set(), ...) {

    # Checks
    if (!object$trained) {
        rlang::abort("Workflow must be trained using the 'fit()' function.")
    }

    data <- object$fit$fit$fit$data

    ret <- calc_accuracy(object, train_data = data, test_data = new_data, metric_set = metric_set, ...)

    return(ret)

}

#' @export
modeltime_accuracy.mdl_time_tbl <- function(object, new_data = NULL,
                                            metric_set = default_forecast_accuracy_metric_set(), ...) {
    data <- object

    ret <- data %>%
        dplyr::ungroup() %>%
        dplyr::mutate(.nested.col = purrr::map(
            .x         = .model,
            .f         = function(obj) modeltime_accuracy(

                object     = obj,
                new_data   = new_data,
                metric_set = metric_set,
                ...

            )
        )) %>%
        dplyr::select(-.model) %>%
        tidyr::unnest(cols = .nested.col)
    # ret <- data

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
#' - MASE  - Mean absolute scaled errror, [mase()]
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

calc_accuracy <- function(object, train_data, test_data = NULL, metric_set, ...) {

    model_fit <- object

    metrics_tbl <- train_data %>%
        tibble::add_column(.type = "Training", .before = 1) %>%
        dplyr::group_by(.type) %>%
        summarize_accuracy_metrics(.value, .fitted, metric_set) %>%
        dplyr::ungroup()

    if (!is.null(test_data)) {

        predictions_tbl <- object %>%
            modeltime_forecast(
                new_data      = test_data,
                actual_data   = test_data,
                conf_interval = NULL
            )

        test_metrics_tbl <- predictions_tbl %>%
            tidyr::pivot_wider(names_from = .id, values_from = .value) %>%
            tibble::add_column(.type = "Test", .before = 1) %>%
            dplyr::group_by(.type) %>%
            summarize_accuracy_metrics(actual, prediction, metric_set) %>%
            dplyr::ungroup()

        metrics_tbl <- dplyr::bind_rows(metrics_tbl, test_metrics_tbl)

    }

    return(metrics_tbl)
}

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
