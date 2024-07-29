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
#' @param acc_by_id Should a global or local model accuracy be produced? (Default: FALSE)
#'
#'  - When `FALSE`, a global model accuracy is provided.
#'
#'  - If `TRUE`, a local accuracy is provided group-wise for each time series ID.
#'    To enable local accuracy, an `id` must be provided during `modeltime_calibrate()`.
#'
#' @param quiet Hide errors (`TRUE`, the default), or display them as they occur?
#' @param ... If `new_data` is provided, these parameters are passed to `modeltime_calibrate()`
#'
#'
#' @return A tibble with accuracy estimates.
#'
#' @details
#'
#' The following accuracy metrics are included by default via [default_forecast_accuracy_metric_set()]:
#'
#' - MAE - Mean absolute error, `mae()`
#' - MAPE - Mean absolute percentage error, `mape()`
#' - MASE  - Mean absolute scaled error, `mase()`
#' - SMAPE - Symmetric mean absolute percentage error, `smape()`
#' - RMSE  - Root mean squared error, `rmse()`
#' - RSQ   - R-squared, `rsq()`
#'
#'
#'
#' @examples
#' library(tidymodels)
#' library(dplyr)
#' library(lubridate)
#' library(timetk)
#'
#'
#' # Data
#' m750 <- m4_monthly %>% filter(id == "M750")
#'
#' # Split Data 80/20
#' splits <- initial_time_split(m750, prop = 0.8)
#'
#' # --- MODELS ---
#'
#' # Model 1: prophet ----
#' model_fit_prophet <- prophet_reg() %>%
#'     set_engine(engine = "prophet") %>%
#'     fit(value ~ date, data = training(splits))
#'
#'
#' # ---- MODELTIME TABLE ----
#'
#' models_tbl <- modeltime_table(
#'     model_fit_prophet
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
                               acc_by_id = FALSE,
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
                                       acc_by_id = FALSE,
                                       quiet = TRUE, ...) {
    rlang::abort(stringr::str_glue("Received an object of class: {class(object)[1]}. Expected an object of class:\n 1. 'mdl_time_tbl' - A Model Time Table made with 'modeltime_table()' and calibrated with 'modeltime_calibrate()'."))
}


#' @export
modeltime_accuracy.mdl_time_tbl <- function(object, new_data = NULL,
                                            metric_set = default_forecast_accuracy_metric_set(),
                                            acc_by_id = FALSE,
                                            quiet = TRUE, ...) {
    data <- object

    metrics <- metric_set

    # Handle New Data ----
    if (!is.null(new_data)) {
        data <- data %>%
            modeltime_calibrate(new_data = new_data, ...)
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
                    by_id      = acc_by_id,
                    ...
                )

                ret <- ret %>% purrr::pluck("result")

                return(ret)
            })
        ) %>%
        dplyr::select(-.model, -.calibration_data) %>%
        tidyr::unnest(cols = .nested.col)

    # MAPE/MAAPE Check: Intermittent Series

    if ("mape" %in% names(ret)) {
        if (any(is.na(ret$mape) | is.infinite(ret$mape))){
            cli::cli_alert_info(cli::col_yellow("We have detected a possible intermittent series, you can change the default metric set to the extended_forecast_accuracy_metric_set() containing the MAAPE metric, which is more appropriate for this type of series."))
        }
    }


    if (".nested.col" %in% names(ret)) {
        ret <- ret %>%
            dplyr::select(-.nested.col)
    }

    # Check to see if errored
    if (ncol(ret) <= 3) {
        rlang::warn("modeltime_accuracy(): It looks like no accuracy metrics were returned. Try running with `modeltime_accuracy(quiet = FALSE)` to return error messages.")

        # MASE Check: forecast must be greater than 1
        n_forecast <- nrow(data$.calibration_data[[1]])
        if (n_forecast == 1) {
            if ("mase" %in% names(attr(metric_set, "metrics"))) {
                rlang::warn("modeltime_accuracy(): Some metrics like `mase()` cannot be calculated with a single forecast observation. Try removing `mase` from the metrics by using: `modeltime_accuracy(metric_set = yardstick::metric_set( mae, mape, smape, rmse, rsq ))`.")
            }
        }

    }

    return(ret)
}


