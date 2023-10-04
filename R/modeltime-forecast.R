# MODELTIME FORECAST ----

#' Forecast future data
#'
#' The goal of `modeltime_forecast()` is to simplify the process of
#' forecasting future data.
#'
#' @param object A Modeltime Table
#' @param new_data A `tibble` containing future information to forecast.
#'  If `NULL`, forecasts the calibration data.
#' @param h The forecast horizon (can be used instead of `new_data` for
#'  time series with no exogenous regressors).
#'  Extends the calibration data `h` periods into the future.
#' @param actual_data Reference data that is combined with the output tibble and given a `.key = "actual"`
#' @param conf_interval An estimated confidence interval based on the calibration data.
#'  This is designed to estimate future confidence from _out-of-sample prediction error_.
#' @param conf_by_id Whether or not to produce confidence interval estimates by an ID feature.
#'
#'  - When `FALSE`, a global model confidence interval is provided.
#'
#'  - If `TRUE`, a local confidence interval is provided group-wise for each time series ID.
#'    To enable local confidence interval, an `id` must be provided during `modeltime_calibrate()`.
#'
#' @param conf_method Algorithm used to produce confidence intervals. All CI's are Conformal Predictions. Choose one of:
#'
#'  - `conformal_default`: Uses `qnorm()` to compute quantiles from out-of-sample (test set) residuals.
#'
#'  - `conformal_split`: Uses the split method split conformal inference method described by Lei _et al_ (2018)
#'
#' @param keep_data Whether or not to keep the `new_data` and `actual_data` as extra columns in the results.
#'  This can be useful if there is an important feature in the `new_data` and `actual_data` needed
#'  when forecasting.
#'  Default: `FALSE`.
#' @param arrange_index Whether or not to sort the index in rowwise chronological order (oldest to newest) or to
#'  keep the original order of the data.
#'  Default: `FALSE`.
#' @param ... Not currently used
#'
#'
#' @return
#' A tibble with predictions and time-stamp data. For ease of plotting and calculations,
#'  the column names are transformed to:
#'
#' - `.key`: Values labeled either "prediction" or "actual"
#' - `.index`: The timestamp index.
#' - `.value`: The value being forecasted.
#'
#' Additionally, if the Modeltime Table has been previously calibrated using [modeltime_calibrate()],
#' you will gain confidence intervals.
#' - `.conf_lo`: The lower limit of the confidence interval.
#' - `.conf_hi`: The upper limit of the confidence interval.
#'
#' Additional descriptive columns are included:
#' - `.model_id`: Model ID from the Modeltime Table
#' - `.model_desc`: Model Description from the Modeltime Table
#'
#' Unnecessary columns are _dropped_ to save space:
#' - `.model`
#' - `.calibration_data`
#'
#' @details
#'
#' The `modeltime_forecast()` function prepares a forecast for visualization with
#' with [plot_modeltime_forecast()]. The forecast is controlled by `new_data` or `h`,
#' which can be combined with existing data (controlled by `actual_data`).
#' Confidence intervals are included if the incoming Modeltime Table has been
#' calibrated using [modeltime_calibrate()].
#' Otherwise confidence intervals are not estimated.
#'
#' __New Data__
#'
#' When forecasting you can specify future data using `new_data`.
#' This is a future tibble with date column and columns for xregs
#'  extending the trained dates and exogonous regressors (xregs) if used.
#'
#'    - __Forecasting Evaluation Data__: By default, the `new_data` will use the `.calibration_data`
#'      if `new_data` is not provided.
#'      This is the equivalent of using `rsample::testing()` for getting test data sets.
#'    - __Forecasting Future Data__: See `timetk::future_frame()` for creating future tibbles.
#'    - __Xregs__: Can be used with this method
#'
#'
#' __H (Horizon)__
#'
#' When forecasting, you can specify `h`. This is a phrase like "1 year",
#' which extends the `.calibration_data` (1st priority) or the `actual_data` (2nd priority)
#' into the future.
#'    - __Forecasting Future Data__: All forecasts using `h` are
#'      ___extended after the calibration data or actual_data___.
#'
#'    - Extending `.calibration_data` - Calibration data is given 1st priority, which is
#'      desirable _after refitting_ with [modeltime_refit()].
#'      Internally, a call is made to `timetk::future_frame()` to
#'      expedite creating new data using the date feature.
#'    - Extending `actual_data` - If `h` is provided, and the modeltime table has not been
#'      calibrated, the "actual_data" will be extended into the future. This is useful
#'      in situations where you want to go directly from `modeltime_table()` to `modeltime_forecast()`
#'      without calibrating or refitting.
#'    - __Xregs__: Cannot be used because future data must include new xregs.
#'      If xregs are desired, build a future data frame and use `new_data`.
#'
#' __Actual Data__
#'
#' This is reference data that contains the true values of the time-stamp data.
#' It helps in visualizing the performance of the forecast vs the actual data.
#'
#' When `h` is used and the Modeltime Table has _not been calibrated_, then the
#' actual data is extended into the future periods that are defined by `h`.
#'
#' __Confidence Interval Estimation__
#'
#' Confidence intervals (`.conf_lo`, `.conf_hi`) are estimated based on the normal estimation of
#' the testing errors (out of sample) from [modeltime_calibrate()].
#' The out-of-sample error estimates are then carried through and
#' applied to applied to any future forecasts.
#'
#' The confidence interval can be adjusted with the `conf_interval` parameter. The algorithm used
#' to produce confidence intervals can be changed with the `conf_method` parameter.
#'
#' _Conformal Default Method:_
#'
#' When `conf_method = "conformal_default"` (default), this method uses `qnorm()`
#' to produce a 95% confidence interval by default. It estimates a normal (Gaussian distribution)
#' based on the out-of-sample errors (residuals).
#'
#' The confidence interval is _mean-adjusted_, meaning that if the mean of the residuals
#' is non-zero, the confidence interval is adjusted to widen the interval to capture
#' the difference in means.
#'
#' _Conformal Split Method:_
#'
#' When `conf_method = "conformal_split`, this method uses the split conformal inference method
#' described by Lei _et al_ (2018). This is also implemented in the `probably` R package's
#' `int_conformal_split()` function.
#'
#' _What happens to the confidence interval after refitting models?_
#'
#' Refitting has no affect on the confidence interval since this is calculated independently of
#' the refitted model. New observations typically improve
#' future accuracy, which in most cases makes the out-of-sample confidence intervals conservative.
#'
#' __Keep Data__
#'
#' Include the new data (and actual data) as extra columns with the results of the model forecasts.
#' This can be helpful when the new data includes information useful to the forecasts.
#' An example is when forecasting _Panel Data_ and the new data contains
#' ID features related to the time series group that the forecast belongs to.
#'
#' __Arrange Index__
#'
#' By default, `modeltime_forecast()` keeps the original order of the data.
#' If desired, the user can sort the output by `.key`, `.model_id` and `.index`.
#'
#' @references
#' Lei, Jing, et al. "Distribution-free predictive inference for regression."
#' _Journal of the American Statistical Association_ 113.523 (2018): 1094-1111.
#'
#' @examples
#' library(dplyr)
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
#' # ---- CALIBRATE ----
#'
#' calibration_tbl <- models_tbl %>%
#'     modeltime_calibrate(new_data = testing(splits))
#'
#' # ---- ACCURACY ----
#'
#' calibration_tbl %>%
#'     modeltime_accuracy()
#'
#' # ---- FUTURE FORECAST ----
#'
#' calibration_tbl %>%
#'     modeltime_forecast(
#'         new_data    = testing(splits),
#'         actual_data = m750
#'     )
#'
#' # ---- ALTERNATIVE: FORECAST WITHOUT CONFIDENCE INTERVALS ----
#' # Skips Calibration Step, No Confidence Intervals
#'
#' models_tbl %>%
#'     modeltime_forecast(
#'         new_data    = testing(splits),
#'         actual_data = m750
#'     )
#'
#' # ---- KEEP NEW DATA WITH FORECAST ----
#' # Keeps the new data. Useful if new data has information
#' #  like ID features that should be kept with the forecast data
#'
#' calibration_tbl %>%
#'     modeltime_forecast(
#'         new_data      = testing(splits),
#'         keep_data     = TRUE
#'     )
#'
#' @name modeltime_forecast
NULL

#' @export
#' @rdname modeltime_forecast
modeltime_forecast <- function(object, new_data = NULL, h = NULL, actual_data = NULL,
                               conf_interval = 0.95, conf_by_id = FALSE, conf_method = "conformal_default",
                               keep_data = FALSE, arrange_index = FALSE, ...) {

    # Required arguments & messages
    if (is.null(new_data) && is.null(h)) {
        if (all(c(".type", ".calibration_data") %in% names(object))) {
            message("Using '.calibration_data' to forecast.")
        } else if (!is.null(actual_data)) {
            message("Using 'actual_data' to forecast. This may not be desirable for sequence models such as ARIMA.")
        } else {
            rlang::abort("Forecast requires either: \n - 'new_data' \n - 'h'")
        }
    }

    # Horizon, h: Checks
    if (!is.null(h)) {

        # Check for .calibration data or actual data if using `h`
        # - h needs this to extend from
        using_actual <- FALSE
        if (!all(c(".type", ".calibration_data") %in% names(object))) {
            if (is.null(actual_data)) {
               rlang::abort("Forecasting with 'h' requires one of: \n - '.calibration_data' (see '?modeltime_calibrate()') \n - 'actual_data'")
            }
            using_actual <- TRUE
        }

        # Ensure no overlapping timestamps if using `h`
        # - `h` doesn't know how to handle overlapping time series
        abort_msg <- "modeltime_forecast(): Overlapping dates detected indicating time series groups. 'h' cannot be used to forecast. Try using 'new_data' that has been extended using `timetk::future_frame()`."
        if (using_actual) {
            validate_no_overlapping_dates(
                data          = actual_data,
                abort_message = abort_msg
            )
        } else {
            calib_data <- object %>%
                purrr::pluck(".calibration_data", 1)

            validate_no_overlapping_dates(
                data          = calib_data,
                abort_message = abort_msg
            )
        }
    }

    if (!conf_method %in% c("conformal_default", "conformal_split")) {
        rlang::abort('conf_method must be one of "conformal_default", "conformal_split"')
    }

    UseMethod("modeltime_forecast")
}

#' @export
modeltime_forecast.default <- function(object, new_data = NULL, h = NULL, actual_data = NULL,
                                       conf_interval = 0.95, conf_by_id = FALSE,  conf_method = "conformal_default",
                                       keep_data = FALSE, arrange_index = FALSE, ...) {
    cli::cli_abort(c("Received an object of class: {.obj_type_friendly {object}}.",
                     "Expected an object of class:",
                     "1. 'mdl_time_tbl' - A Model Time Table made with 'modeltime_table()' and calibrated with 'modeltime_calibrate()'."))
}

#' @export
modeltime_forecast.mdl_time_tbl <- function(object, new_data = NULL, h = NULL, actual_data = NULL,
                                            conf_interval = 0.95, conf_by_id = FALSE, conf_method = "conformal_default",
                                            keep_data = FALSE, arrange_index = FALSE, ...) {

    data <- object

    n_models <- data$.model_id %>% unique() %>% length()

    # HANDLE CALIBRATION DATA
    if (!all(c(".type", ".calibration_data") %in% names(data))) {
        # cli::cli_abort("Expecting columns '.type' and '.calibration_data'. Try running 'modeltime_calibrate()' before using 'modeltime_forecast()'.")
        conf_interval = NULL
        data <- data %>%
            dplyr::mutate(
                .type = NA,
                .calibration_data = NA
            )
    } else {
        data_calibration <- data %>%
            dplyr::select(.model_id, .calibration_data)
    }

    # CREATE FORECAST ----

    # Compute first model with actual data
    ret_1 <- data %>%
        dplyr::ungroup() %>%
        dplyr::slice(1) %>%
        safe_modeltime_forecast_map(
            new_data      = new_data,
            h             = h,
            actual_data   = actual_data,
            keep_data     = if (conf_by_id) TRUE else keep_data,
            arrange_index = arrange_index,
            bind_actual   = TRUE # Rowwise binds the actual data during the forecast
        )

    if ("actual" %in% unique(ret_1$.key)) {
        ret_1 <- ret_1 %>%
            dplyr::mutate(.model_desc = ifelse(.key == "actual", "ACTUAL", .model_desc)) %>%
            dplyr::mutate(.model_id   = ifelse(.key == "actual", NA_integer_, .model_id))
    }

    # Compute subsequent models without actual data
    ret_2 <- tibble::tibble()

    if (n_models > 1) {

        ret_2 <- data %>%
            dplyr::ungroup() %>%
            dplyr::slice(2:dplyr::n()) %>%
            safe_modeltime_forecast_map(
                new_data      = new_data,
                h             = h,
                actual_data   = actual_data,
                keep_data     = if (conf_by_id) TRUE else keep_data,
                arrange_index = arrange_index,
                bind_actual   = FALSE # Skips iterative rowwise binding of actual_data
            )

    }

    # If errors occur, .nested.col remains - needs removed
    if (".nested.col" %in% names(ret_1)) {
        ret_1 <- ret_1 %>%
            dplyr::select(-.nested.col)
    }

    if (".nested.col" %in% names(ret_2)) {
        ret_2 <- ret_2 %>%
            dplyr::select(-.nested.col)
    }

    ret <- dplyr::bind_rows(ret_1, ret_2)

    # ADD CONF INTERVALS ----
    if (!is.null(conf_interval)) {

        if (conf_by_id) {

            # Check 5th column exists
            id_col_found <- data_calibration$.calibration_data[[1]] %>% names() %>% length() == 5

            # Apply CI by ID if possible
            if (id_col_found) {

                id_col <- data_calibration$.calibration_data[[1]] %>% names() %>% purrr::pluck(5)

                ret <- ret %>%
                    safe_conf_interval_map_by_id(
                        data_calibration,
                        conf_interval = conf_interval,
                        conf_method   = conf_method,
                        id            = !! id_col
                    ) %>%
                    dplyr::select(".model_id", ".model_desc", ".key", ".index", ".value", ".conf_lo", ".conf_hi", dplyr::all_of(names(new_data)))

                # Remove unnecessary columns if `keep_data = FALSE`. Required to keep the id column.
                if (!keep_data) {
                    ret <- ret %>%
                        dplyr::select(.model_id:.conf_hi, dplyr::all_of(id_col))
                }

            } else {

                rlang::warn("The 'id' column in calibration data was not detected. Global Confidence Interval is being returned.")

                ret <- ret %>%
                    safe_conf_interval_map(data_calibration, conf_interval = conf_interval) %>%
                    dplyr::relocate(dplyr::starts_with(".conf_"), .after = .value)

                # Remove unnecessary columns if `keep_data = FALSE`
                if (!keep_data) {
                    ret <- ret %>%
                        dplyr::select(.model_id:.conf_hi)
                }
            }



        } else {
            ret <- ret %>%
                safe_conf_interval_map(
                    data_calibration,
                    conf_interval = conf_interval,
                    conf_method   = conf_method
                ) %>%
                dplyr::relocate(dplyr::starts_with(".conf_"), .after = .value)

            # Remove unnecessary columns if `keep_data = FALSE`
            if (!keep_data) {
                ret <- ret %>%
                    dplyr::select(.model_id:.conf_hi)
            }
        }

    }

    # REMOVE ANY EXTRA-ACTUAL DATA ----
    # - Happens when ensembling

    ret <- ret %>%
        dplyr::filter(.model_desc == "ACTUAL" | .key == "prediction")


    # STRUCTURE ----
    class(ret) <- c("mdl_forecast_tbl", class(ret))

    attr(ret, "conf_interval") <- conf_interval
    attr(ret, "conf_method")   <- conf_method
    attr(ret, "conf_by_id")    <- conf_by_id

    return(ret)
}

#' @export
print.mdl_forecast_tbl <- function(x, ...) {

    # Collect inputs
    conf_interval <- attr(x, 'conf_interval')
    conf_method   <- attr(x, 'conf_method')
    conf_by_id    <- attr(x, 'conf_by_id')

    conf_by_id_desc <- if (conf_by_id) {
        "LOCAL CONFIDENCE"
    } else {
        "GLOBAL CONFIDENCE"
    }

    cat("# Forecast Results\n")
    cat("  ")
    cli::cli_text(cli::col_grey("Conf Method: {conf_method} | Conf Interval: {conf_interval} | Conf By ID: {conf_by_id} ({conf_by_id_desc})"))
    # cli::cli_rule()
    class(x) <- class(x)[!(class(x) %in% c("mdl_forecast_tbl"))]
    print(x, ...)
}


# SAFE FORECAST MAPPERS ----

safe_modeltime_forecast_map <- function(data, new_data = NULL, h = NULL, actual_data = NULL, bind_actual = TRUE, keep_data = FALSE, arrange_index = FALSE, ...) {

    safe_modeltime_forecast <- purrr::safely(mdl_time_forecast, otherwise = NA, quiet = FALSE)

    data %>%
        dplyr::mutate(.nested.col = purrr::map2(
            .x         = .model,
            .y         = .calibration_data,
            .f         = function(obj, cal) {

                ret <- safe_modeltime_forecast(
                    obj, cal,
                    new_data      = new_data,
                    h             = h,
                    actual_data   = actual_data,
                    bind_actual   = bind_actual,
                    keep_data     = keep_data,
                    arrange_index = arrange_index
                )

                err <- ret$error

                ret <- ret$result

                # if (!is.null(error)) warning(err)

                return(ret)
            })
        ) %>%
        # Drop unnecessary columns
        dplyr::select(-.model, -.type, -.calibration_data) %>%
        tidyr::unnest(cols = .nested.col)
}


# SAFE CONF INTERVAL MAPPERS ----

safe_conf_interval_map <- function(data, data_calibration, conf_interval, conf_method) {

    empty_ci_tbl <- tibble::tibble(
        .conf_lo = NA,
        .conf_hi = NA
    )

    if (conf_method == "conformal_default") {
        safe_ci <- purrr::safely(
            conformal_default_func,
            otherwise = empty_ci_tbl,
            quiet = FALSE
        )
    }
    if (conf_method == "conformal_split") {
        safe_ci <- purrr::safely(
            conformal_split_func,
            otherwise = empty_ci_tbl,
            quiet = FALSE
        )
    }

    data %>%
        dplyr::group_by(.model_id) %>%
        tidyr::nest() %>%
        dplyr::left_join(data_calibration, by = ".model_id") %>%
        dplyr::mutate(.ci = purrr::map2(data, .calibration_data, .f = function(data_1, data_2) {
            res <- safe_ci(data_1, data_2, conf_interval = conf_interval)
            res %>% purrr::pluck("result")
        })
        ) %>%
        dplyr::select(-.calibration_data) %>%
        tidyr::unnest(cols = c(data, .ci)) %>%
        dplyr::ungroup()
}

safe_conf_interval_map_by_id <- function(data, data_calibration, conf_interval, id, conf_method) {

    empty_ci_tbl <- tibble::tibble(
        .conf_lo = NA,
        .conf_hi = NA
    )

    if (conf_method == "conformal_default") {
        safe_ci <- purrr::safely(
            conformal_default_func,
            otherwise = empty_ci_tbl,
            quiet = FALSE
        )
    }
    if (conf_method == "conformal_split") {
        safe_ci <- purrr::safely(
            conformal_split_func,
            otherwise = empty_ci_tbl,
            quiet = FALSE
        )
    }

    forecast_nested_tbl <- data %>%
        tibble::rowid_to_column(var = "..rowid") %>%
        dplyr::group_by(.model_id, !! rlang::ensym(id)) %>%
        tidyr::nest()

    calibration_nested_tbl <- data_calibration %>%
        tidyr::unnest(.calibration_data) %>%
        dplyr::group_by(.model_id, !! rlang::ensym(id)) %>%
        tidyr::nest() %>%
        dplyr::rename(.calibration_data = data)

    # print(forecast_nested_tbl)
    # print(calibration_nested_tbl)

    forecast_nested_tbl %>%
        dplyr::left_join(
            calibration_nested_tbl
            ,
            by = names(forecast_nested_tbl)[1:2]
        ) %>%
        dplyr::mutate(.ci = purrr::map2(data, .calibration_data, .f = function(data_1, data_2) {
            res <- safe_ci(data_1, data_2, conf_interval = conf_interval)
            res %>% purrr::pluck("result")
        })
        ) %>%
        dplyr::select(-.calibration_data) %>%
        tidyr::unnest(cols = c(data, .ci)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(..rowid) %>%
        dplyr::select(
            -..rowid
            # , - !! rlang::ensym(id)
        )
}

# * DEFAULT CI METHOD: QUANTILE OF NORMAL DISTRIBUTION AROUND PREDICTIONS ----
conformal_default_func <- function(data_1, data_2, conf_interval) {

    # Collect absolute residuals
    ret <- tryCatch({

        residuals <- c(data_2$.residuals, -data_2$.residuals)
        s <- stats::sd(residuals)

        ci_tbl <- data_1 %>%
            dplyr::mutate(
                .conf_lo = stats::qnorm((1-conf_interval)/2, mean = .value, sd = s),
                .conf_hi = stats::qnorm((1+conf_interval)/2, mean = .value, sd = s)
            ) %>%
            dplyr::select(.conf_lo, .conf_hi)

        ci_tbl

    }, error = function(e) {
        tibble::tibble(
            .conf_lo = NA,
            .conf_hi = NA
        )
    })

    return(ret)

}

# * CONFORMAL PREDICTION VIA SPLIT METHOD ----
# https://github.com/tidymodels/probably/blob/c46326651109fb2ebd1b3762b3cb086cfb96ac88/R/conformal_infer_split.R#L99
conformal_split_func <- function(data_1, data_2, conf_interval) {

    # Collect absolute residuals
    ret <- tryCatch({

        residuals        <- data_2$.residuals
        residuals_sorted <- residuals %>%
            abs() %>%
            sort(decreasing = FALSE)

        n     <- nrow(data_2)
        q_ind <- ceiling(conf_interval * n)
        q_val <- residuals_sorted[q_ind]

        ci_tbl <- data_1 %>%
            dplyr::mutate(
                .conf_lo = .value - q_val,
                .conf_hi = .value + q_val
            ) %>%
            dplyr::select(.conf_lo, .conf_hi)

        ci_tbl

    }, error = function(e) {
        tibble::tibble(
            .conf_lo = NA,
            .conf_hi = NA
        )
    })

    return(ret)

}


# FORECAST UTILITIES ----

#' Modeltime Forecast Helpers
#'
#' Used for low-level forecasting of modeltime, parnsip and workflow models.
#' These functions are not intended for user use.
#'
#' @inheritParams modeltime_forecast
#' @param calibration_data Data that has been calibrated from a testing set
#' @param bind_actual Logical. Whether or not to skip rowwise binding of `actual_data``
#'
#' @return A tibble with forecast features
#'
#' @keywords internal
#'
#' @export
mdl_time_forecast <- function(object, calibration_data, new_data = NULL, h = NULL, actual_data = NULL, bind_actual = TRUE, keep_data = FALSE, arrange_index = FALSE, ...) {
    UseMethod("mdl_time_forecast", object)
}

#' @export
mdl_time_forecast.model_fit <- function(object, calibration_data, new_data = NULL, h = NULL, actual_data = NULL, bind_actual = TRUE, keep_data = FALSE, arrange_index = FALSE, ...) {

    calib_provided <- FALSE
    h_provided     <- FALSE

    # MODEL OBJECT

    # If no 'new_data', forecast 'calibration_data'
    if (is.null(new_data) && is.null(h)) {
        if (is.data.frame(calibration_data)) {
            new_data <- calibration_data
            calib_provided <- TRUE
        } else if (is.data.frame(actual_data)) {
            new_data <- actual_data
        } else {
            rlang::abort("Forecast requires 'new_data', 'calibration_data', or 'actual_data'.")
        }
    }

    # Convert 'h' to 'new_data'
    if (!is.null(h)) {
        if (is.data.frame(calibration_data)) {
            tryCatch({
                # Suppress date selection
                suppressMessages(new_data <- timetk::future_frame(calibration_data, .length_out = h, ...))
            }, error = function(e) {
                rlang::abort("Attempt to extend '.calibration_data' into the future using 'h' has failed.")
            })
        } else if (is.data.frame(actual_data)) {
            tryCatch({
                # Suppress date selection
                suppressMessages(new_data <- timetk::future_frame(actual_data, .length_out = h, ...))
            }, error = function(e) {
                rlang::abort("Attempt to extend 'actual_data' into the future using 'h' has failed.")
            })
        } else {
            rlang::abort("Forecast requires 'new_data', '.calibration_data', or 'actual_data'.")
        }

        h_provided <- TRUE
    }

    # For combining new data
    actual_data_unprocessed <- actual_data
    new_data_unprocessed    <- new_data

    # Setup data for predictions
    nms_time_stamp_predictors <- timetk::tk_get_timeseries_variables(new_data)[1]
    time_stamp_predictors_tbl <- new_data %>%
        dplyr::select(!! rlang::sym(nms_time_stamp_predictors)) %>%
        dplyr::rename(.index = !! rlang::sym(nms_time_stamp_predictors))

    # PREDICTIONS ----

    # Comment this out ----
    # obj <<- object
    # print({
    #     list(
    #         object   = object,
    #         class    = class(object),
    #         new_data = new_data
    #     )
    # })

    modeltime_forecast <- tryCatch({

        if (detect_net(object) && inherits(object, "recursive")) {
            predictions_tbl <- object %>% predict.recursive(new_data = new_data)
        } else if (detect_net(object) && inherits(object, "recursive_panel")) {
            predictions_tbl <- object %>% predict.recursive_panel(new_data = new_data)
        } else {
            predictions_tbl <- object %>% stats::predict(new_data = new_data)
        }

        modeltime_forecast <- predictions_tbl %>%
            dplyr::bind_cols(time_stamp_predictors_tbl)

    }, error = function(e) {
        if (any(c(h_provided, calib_provided))) {
            # Most likely issue: need to provide external regressors
            cli::cli_abort("Problem occurred during prediction. Most likely cause is missing external regressors. Try using 'new_data' and supply a dataset containing all required columns. {e}")
        } else {
            cli::cli_abort("Problem occurred during prediction. {e}")
        }
    })

    # Format data
    data_formatted <- modeltime_forecast %>%
        dplyr::mutate(.key = "prediction", .before = 0)

    # COMBINE ACTUAL DATA

    if (!is.null(actual_data) && bind_actual) {

        # setup
        nms_final     <- names(data_formatted)

        # print(nms_final)

        if (length(object$preproc$y_var) > 0) {
            fit_interface <-  "formula"
        } else {
            fit_interface <- "xy"
        }

        if (fit_interface == "formula") {
            nm_target <- object$preproc$y_var

            # Get the index and target variable symbol
            idx        <- actual_data %>% timetk::tk_index()
            target_sym <- rlang::sym(object$preproc$y_var)

            # Set ID & Index
            actual_data <- actual_data %>%
                dplyr::mutate(.key = "actual") %>%
                dplyr::mutate(.index = idx)

            # Common for Data.Frame Style - Apply transformation to target variable if needed
            # If preproc, rename first variable the target name
            pp_names <- names(object$preproc)
            if (any(pp_names == "terms") | any(pp_names == "x_var")) {
                formula_lhs <- find_formula_lhs(object$preproc)
                if (!is.null(formula_lhs)) {
                    actual_data <- actual_data %>%
                        dplyr::mutate(!! target_sym := eval(formula_lhs))
                } else {
                    warning(call. = FALSE, paste0("Cannot determine if transformation is required on 'actual_data'"))
                }
            }

            # Problem with Formula Style (e.g. stats::lm() ):
            # - Need to search model for formula LHS to know transformation
            # - Use find_formula_lhs() to search first level for formula
            if (object$spec$method$fit$interface == "formula") {

                formula_lhs <- find_formula_lhs(object$fit)
                if (!is.null(formula_lhs)) {
                    actual_data <- actual_data %>%
                        dplyr::mutate(!! target_sym := eval(formula_lhs))
                } else {
                    warning(call. = FALSE, paste0("Cannot determine if transformation is required on 'actual_data'"))
                }
            }

            # Issure #228 - fix `.pred_res`
            data_formatted <- actual_data %>%
                dplyr::bind_rows(data_formatted)

            if (".pred_res" %in% colnames(data_formatted)) {
                data_formatted <- data_formatted %>%
                    dplyr::rename(.pred = .pred_res)
            }

            if (".pred_res" %in% nms_final) {
                nms_final <- stringr::str_replace(nms_final, ".pred_res", ".pred")
            }

            data_formatted <- data_formatted %>%
                dplyr::mutate(.pred = ifelse(is.na(.pred), !! target_sym, .pred)) %>%
                dplyr::select(!!! rlang::syms(nms_final))

        } else {
            # XY Interface

            rlang::abort("XY Interface not yet implemented for 'modeltime_forecast()'. Try using the Formula Interface with `fit()` or a `workflow()`.")

            # actual_data <- prepare_data(object, actual_data) %>%
            #     tibble::as_tibble() %>%
            #     dplyr::mutate(.key = "actual") %>%
            #     dplyr::rename(.index = !! rlang::sym(nms_time_stamp_predictors))

        }

        # Issue #228 - fix .pred_res
        if (".pred_res" %in% colnames(data_formatted)) {
            data_formatted <- data_formatted %>%
                dplyr::rename(.pred = .pred_res)
        }

        if (".pred_res" %in% nms_final) {
            nms_final <- stringr::str_replace(nms_final, ".pred_res", ".pred")
        }

        data_formatted <- data_formatted %>%
            dplyr::select(!!! rlang::syms(nms_final))

    }

    # FINALIZE
    ret <- data_formatted %>%
        dplyr::select(.key, .index, .value = .pred) %>%
        dplyr::mutate(.key = factor(.key, levels = c("actual", "prediction")))

    # Keep Data
    act_tbl  <- NULL
    pred_tbl <- NULL
    if (keep_data) {

        if (!is.null(actual_data) && bind_actual) {
            act_tbl <- ret %>%
                dplyr::filter(.key == "actual") %>%
                dplyr::bind_cols(actual_data_unprocessed)
        }

        pred_tbl <- ret %>%
            dplyr::filter(.key == "prediction") %>%
            dplyr::bind_cols(new_data_unprocessed)

        ret <- dplyr::bind_rows(act_tbl, pred_tbl)

    }

    if (arrange_index) {
        ret <- ret %>%
            dplyr::arrange(.key, .index)
    }

    return(ret)

}

#' @export
mdl_time_forecast.workflow <- function(object, calibration_data, new_data = NULL, h = NULL, actual_data = NULL, bind_actual = TRUE, keep_data = FALSE, arrange_index = FALSE, ...) {

    calib_provided <- FALSE
    h_provided     <- FALSE

    # Checks
    if (!object$trained) {
        rlang::abort("Workflow must be trained using the 'fit()' function.")
    }

    # WORKFLOW MOLD

    # Contains $predictors, $outcomes, $blueprint
    # mld <- object %>% workflows::extract_mold()

    # UPGRADE MOLD (TEMP FIX) ----
    # - models built with hardhat <1.0.0 have issue
    #   https://github.com/tidymodels/hardhat/issues/200
    preprocessor <- workflows::extract_preprocessor(object)
    mld          <- hardhat::mold(preprocessor, preprocessor$template)

    # NEW DATA

    # If no 'new_data', forecast 'calibration_data' or 'actual_data'
    if (is.null(new_data) && is.null(h)) {
        if (is.data.frame(calibration_data)) {
            new_data <- calibration_data
            calib_provided <- TRUE
        } else if (is.data.frame(actual_data)) {
            new_data <- actual_data
        } else {
            rlang::abort("Forecast requires 'new_data', 'calibration_data', or 'actual_data'.")
        }
    }

    # Convert 'h' to 'new_data'
    if (!is.null(h)) {
        if (is.data.frame(calibration_data)) {
            tryCatch({
                # Suppress date selection
                suppressMessages(new_data <- timetk::future_frame(calibration_data, .length_out = h, ...))
            }, error = function(e) {
                rlang::abort("Attempt to extend '.calibration_data' into the future using 'h' has failed.")
            })
        } else if (is.data.frame(actual_data)) {
            tryCatch({
                # Suppress date selection
                suppressMessages(new_data <- timetk::future_frame(actual_data, .length_out = h, ...))
            }, error = function(e) {
                rlang::abort("Attempt to extend 'actual_data' into the future using 'h' has failed.")
            })
        } else {
            rlang::abort("Forecast requires 'new_data', '.calibration_data', or 'actual_data'.")
        }

        h_provided <- TRUE
    }

    # For combining new data
    actual_data_unprocessed <- actual_data
    new_data_unprocessed    <- new_data

    # Issue - Forge processes all recipe steps at once, so need to have outcomes
    # Reference: https://tidymodels.github.io/hardhat/articles/forge.html

    # Prep for Forge (Assign dummy y_var if needed)
    y_var <- names(mld$outcomes)
    if (!any(y_var %in% names(new_data))) {
        new_data[,y_var] <- 1000
    }

    # Apply forge just to get predictors
    new_data_forged <- tryCatch({
        hardhat::forge(new_data, mld$blueprint, outcomes = TRUE)
    }, error = function(e) {
        if (any(c(h_provided, calib_provided))) {
            # Most likely issue: need to provide external regressors
            cli::cli_abort("Problem occurred in getting predictors from new data. Most likely cause is missing external regressors. Try using 'new_data' and supply a dataset containing all required columns. {e}")
        } else {
            cli::cli_abort("Problem occurred getting predictors from new data. {e}")
        }
    })

    # TIMESTAMP + PREDICTORS

    nms_time_stamp_predictors <- timetk::tk_get_timeseries_variables(new_data_forged$predictors)[1]

    if (!is.na(nms_time_stamp_predictors)) {
        time_stamp_predictors_tbl <- new_data_forged$predictors %>%
            dplyr::select(!! rlang::sym(nms_time_stamp_predictors)) %>%
            dplyr::rename(.index = !! rlang::sym(nms_time_stamp_predictors))
    } else {
        # Most ML algorithms won't have time stamp
        idx <- new_data %>% timetk::tk_index()

        if (length(idx) == nrow(new_data_forged$predictors)) {
            # IF index and predictors are same length, all good
            time_stamp_predictors_tbl <- new_data_forged$predictors %>%
                dplyr::mutate(.index = idx)
        } else {
            # Test to see if missing data is being dropped (happens with step_naomit)
            new_data_missing_removed_tbl <- new_data %>%
                tibble::rowid_to_column(var = "..id") %>%
                tidyr::drop_na()

            if (nrow(new_data_forged$predictors) == nrow(new_data_missing_removed_tbl)) {
                # If row counts are identical, subset idx by retained rows
                time_stamp_predictors_tbl <- new_data_forged$predictors %>%
                    dplyr::mutate(.index = idx[new_data_missing_removed_tbl$..id])
            } else {
                cli::cli_abort("Problem occurred combining processed data with timestamps. Most likely cause is rows being added or removed during preprocessing. Try imputing missing values to retain the full number of rows.")
            }
        }
    }

    # FORGE NEW DATA -----

    # Fix - When ID is dummied
    id <- object$fit$fit$spec$id
    df_id <- NULL
    if (!is.null(id)) {
        df_id = new_data %>% dplyr::select(dplyr::all_of(id))
    }

    # Issue - hardhat::forge defaults to outcomes = FALSE, which creates an error at predict.workflow()
    forged    <- hardhat::forge(new_data, mld$blueprint, outcomes = TRUE)
    new_data  <- forged$predictors
    fit       <- object$fit$fit

    # PREDICTIONS ----

    # Comment this out ----
    # obj <<- object
    # print({
    #     list(
    #         object   = object,
    #         class    = class(object),
    #         new_data = new_data,
    #         id       = id,
    #         df_id    = df_id
    #     )
    # })

    # Fix - When ID is dummied
    df <- new_data
    if (!is.null(id)) {
        if (!id %in% names(new_data)) {
            df <- new_data %>%
                dplyr::bind_cols(df_id)
            fit$spec$remove_id <- TRUE
        }
    }

    # PREDICT
    if (detect_net(fit) && inherits(fit, "recursive")) {
        data_formatted <- fit %>% predict.recursive(new_data = df)
    } else if (detect_net(fit) && inherits(fit, "recursive_panel")) {
        data_formatted <- fit %>% predict.recursive_panel(new_data = df)
    } else {
        data_formatted <- fit %>% stats::predict(new_data = df)
    }

    data_formatted <- data_formatted %>%
        dplyr::bind_cols(time_stamp_predictors_tbl) %>%
        dplyr::mutate(.key = "prediction", .before = 0)


    # COMBINE ACTUAL DATA

    if (!is.null(actual_data) && bind_actual) {

        nms_final <- names(data_formatted)

        # mld <- object %>% workflows::extract_mold()

        actual_data_forged <- hardhat::forge(new_data = actual_data, blueprint = mld$blueprint, outcomes = TRUE)

        actual_data_prepped <- actual_data_forged$outcomes %>%
            dplyr::bind_cols(actual_data_forged$predictors) %>%
            dplyr::mutate(.key = "actual")

        # ---- BUG: if NROWs before/after don't match ----
        # - This situation is likely due to missing values in predictors
        # - Predictors are irrelevant to Actual Data, so the solution implemented fills
        #   missing predictors with a downup strategy. Filling reduces the likelihood that
        #   rows will be dropped.

        nrow_before <- nrow(actual_data)
        nrow_after  <- nrow(actual_data_prepped)
        idx_actual  <- timetk::tk_index(actual_data)

        if (nrow_after == nrow_before) {

            actual_data_prepped <- actual_data_prepped %>%
                dplyr::mutate(.index = idx_actual)

        } else if (nrow_after < nrow_before) {
            # message(stringr::str_glue("Transformations are resulting in a reduced number of rows in actual data.
            #                           Attempting to reconcile:
            #                           - Filling missing predictors in actual data to prevent NA values from causing rows to be dropped."))

            # Try to reconcile by filling in missing data
            # - Most likely cause is NA's being dropped by step_naomit()

            actual_data_fill_missing <- actual_data %>%
                tidyr::fill(dplyr::everything(), -names(actual_data_forged$outcomes), .direction = c("downup"))

            actual_data_reconcile_forged <- hardhat::forge(
                new_data = actual_data_fill_missing,
                blueprint = mld$blueprint,
                outcomes = TRUE
            )

            actual_data_reconcile_prepped <- actual_data_reconcile_forged$outcomes %>%
                dplyr::bind_cols(actual_data_reconcile_forged$predictors) %>%
                dplyr::mutate(.key = "actual")

            if (nrow(actual_data_reconcile_prepped) == nrow(actual_data) ) {
                # message("Reconciliation successful.")

                actual_data_prepped <- actual_data_reconcile_prepped %>%
                    dplyr::mutate(.index = idx_actual)

            } else {
                rlang::warn("Could not reconcile actual data. To reconcile, please remove actual data from modeltime_forecast() and add manually using bind_rows().")
                actual_data_prepped <- NULL
            }

        } else {
            rlang::warn("Transformations are causing your actual data to increase in size. To reconcile, please remove actual data from modeltime_forecast() and add manually using bind_rows().")
            actual_data_prepped <- NULL
        }

        # Combine Actual Data with Data Formatted
        if (!is.null(actual_data_prepped)) {
            target_sym <- rlang::sym(names(actual_data_prepped)[1])

            # Issue #228 - fix .pred_res
            data_formatted <- actual_data_prepped %>%
                dplyr::bind_rows(data_formatted)


            if (".pred_res" %in% colnames(data_formatted)) {
                data_formatted <- data_formatted %>%
                    dplyr::rename(.pred = .pred_res)
            }

            if (".pred_res" %in% nms_final) {
                nms_final <- stringr::str_replace(nms_final, ".pred_res", ".pred")
            }

            data_formatted <- data_formatted %>%
                dplyr::mutate(.pred = ifelse(is.na(.pred), !! target_sym, .pred)) %>%
                dplyr::select(!!! rlang::syms(nms_final))
        }

    }

    # FINALIZE

    # Issue #228 - fix .pred_res
    if (".pred_res" %in% colnames(data_formatted)) {
        data_formatted <- data_formatted %>%
            dplyr::rename(.pred = .pred_res)
    }

    ret <- data_formatted %>%
        dplyr::select(.key, .index, .value = .pred) %>%
        dplyr::mutate(.key = factor(.key, levels = c("actual", "prediction")))

    # Keep Data
    act_tbl  <- NULL
    pred_tbl <- NULL
    if (keep_data) {

        if (!is.null(actual_data) && bind_actual) {
            act_tbl <- ret %>%
                dplyr::filter(.key == "actual") %>%
                dplyr::bind_cols(actual_data_unprocessed)
        }

        pred_tbl <- ret %>%
            dplyr::filter(.key == "prediction") %>%
            dplyr::bind_cols(new_data_unprocessed)

        ret <- dplyr::bind_rows(act_tbl, pred_tbl)

    }

    if (arrange_index) {
        ret <- ret %>%
            dplyr::arrange(.key, .index)
    }

    return(ret)

}



detect_net <- function(object){
    if (inherits(object, "_fishnet") || inherits(object, "_elnet") || inherits(object, "_multnet") || inherits(object, "_lognet")){
        res <- TRUE
    } else {
        res <- FALSE
    }
    return(res)
}
