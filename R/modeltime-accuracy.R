#' Calculate Accuracy Metrics
#'
#' This is a wrapper for `yardstick` that simplifies accuracy metric
#' calculations from a fitted `workflow` (trained workflow) or `model_fit` (trained parsnip model).
#'
#' @param object A fitted model object that is either (1) a workflow that has been fit by [fit.workflow()] or
#'  (2) a parsnip model that has been fit using [fit.model_spec()]
#' @param new_data A `tibble` containing future information .
#' @param ... Not currently used.
#'
#'
#' @return A tibble with accuracy estimates.
#'
#' @details
#'
#' The following accuracy metrics are included by default:
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
#'     set_engine("forecast::Arima")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#'
#' # --- ACCURACY ---
#'
#' model_fit %>%
#'     modeltime_accuracy(new_data = testing(splits))
#'
#' @name modeltime_accuracy
NULL

#' @export
#' @rdname modeltime_accuracy
modeltime_accuracy <- function(object, new_data = NULL, ...) {
    UseMethod("modeltime_accuracy")
}

#' @export
modeltime_accuracy.default <- function(object, new_data = NULL, ...) {
    rlang::abort(stringr::str_glue("Received an object of class: {class(object)[1]}. Expected an object of class 'workflow' that has been fitted (trained) or 'model_fit' (a fitted parsnip model)."))
}

#' @export
modeltime_accuracy.model_spec <- function(object, new_data = NULL, ...) {
    rlang::abort("Model spec must be trained using the 'fit()' function.")
}

#' @export
modeltime_accuracy.model_fit <- function(object, new_data = NULL, ...) {

    data <- object$fit$data

    ret <- calc_accuracy(object, train_data = data, test_data = new_data, ...)

    return(ret)

}

#' @export
modeltime_accuracy.workflow <- function(object, new_data = NULL, ...) {

    # Checks
    if (!object$trained) {
        rlang::abort("Workflow must be trained using the 'fit()' function.")
    }

    data <- object$fit$fit$fit$data

    ret <- calc_accuracy(object, train_data = data, test_data = new_data, ...)

    return(ret)

}

# UTILITIES ----

calc_accuracy <- function(object, train_data, test_data = NULL, ...) {

    model_fit <- object

    metrics_tbl <- train_data %>%
        tibble::add_column(.type = "Training", .before = 1) %>%
        dplyr::group_by(.type) %>%
        summarize_regr_metrics(.value, .fitted) %>%
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
            summarize_regr_metrics(actual, prediction) %>%
            dplyr::ungroup()

        metrics_tbl <- dplyr::bind_rows(metrics_tbl, test_metrics_tbl)

    }

    return(metrics_tbl)
}

summarize_regr_metrics <- function(data, truth, estimate) {

    truth_expr    <- rlang::enquo(truth)
    estimate_expr <- rlang::enquo(estimate)

    data %>%
        dplyr::summarize(
            MAE   = yardstick::mae_vec(!! truth_expr, !! estimate_expr),
            MAPE  = yardstick::mape_vec(!! truth_expr, !! estimate_expr),
            MASE  = yardstick::mase_vec(!! truth_expr, !! estimate_expr),
            SMAPE = yardstick::smape_vec(!! truth_expr, !! estimate_expr),
            RMSE  = yardstick::rmse_vec(!! truth_expr, !! estimate_expr),
            RSQ   = yardstick::rsq_vec(!! truth_expr, !! estimate_expr)
        )
}
