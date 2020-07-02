#' Forecast future data
#'
#' The goal of `modeltime_forecast()` is to simplify the process of
#' forecasting future data.
#'
#' @param object A Modeltime Table that has been calibrated with [modeltime_calibrate()]
#' @param new_data A `tibble` containing future information to forecast.
#'  If `NULL`, forecasts the calibration data.
#' @param h The forecast horizon (can be used instead of `new_data` for
#'  time series with no exogenous regressors).
#'  Extends the calibration data `h` periods into the future.
#' @param actual_data Reference data that is combined with the output tibble and given a `.key = "actual"`
#' @param conf_interval An estimated confidence interval based on the in-sample residuals
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
#' The key parameters are (controlled by `new_data` or `h`) and
#' combining with existing data (controlled by `actual_data`) in preparation
#' for visualization with [plot_modeltime_forecast()].
#'
#'
#' __Specifying New Data or Horizon (h)__
#'
#' When forecasting you can specify future data using:
#'
#' 1. `new_data`: This is a future tibble with date column and columns for xregs
#'  extending the trained dates and exogonous regressors (xregs) if used.
#'    - __Forecasting Evaluation Data__: By default, the `new_data` will use the `.calibration_data`
#'      if `new_data` is not provided.
#'      This is the equivalent of using `rsample::testing()` for getting test data sets.
#'    - __Forecasting Future Data__: See [future_frame()] for creating future tibbles.
#'    - __Xregs__: Can be used with this method
#'
#'
#' 2. `h`: This is a phrase like "1 year", which extends the `.calibration_data` into the future.
#'    - __Forecasting Future Data__: All forecasts using `h` are
#'      ___extended after the calibration data___, which is
#'      desirable _after refitting_ with [modeltime_refit()].
#'      Internally, a call is made to [future_frame()] to
#'      expedite creating new data using the date feature.
#'    - __Xregs__: Cannot be used because future data must include new xregs.
#'
#' __Actual Data__
#'
#' This is reference data that contains the true values of the time-stamp data.
#' It helps in visualizing the performance of the forecast vs the actual data.
#'
#' __Confidence Interval Estimation__
#'
#' Confidence intervals (`.conf_lo`, `.conf_hi`) are estimated based on the normal estimation of
#' the testing errors (out of sample) from [modeltime_calibrate()].
#' The out-of-sample error estimates are then carried through and
#' applied to applied to any future forecasts.
#'
#' The confidence interval can be adjusted with the `conf_interval` parameter. An
#' 80% confidence interval estimates a normal (Gaussian distribution) that assumes that
#' 80% of the future data will fall within the upper and lower confidence limits.
#'
#' The confidence interval is _mean-adjusted_, meaning that if the mean of the residuals
#' is non-zero, the confidence interval is adjusted to widen the interval to capture
#' the difference in means.
#'
#' Refitting has no affect on the confidence interval since this is calculated independently of
#' the refitted model (on data with a smaller sample size). New observations typically improve
#' future accuracy, which in most cases makes the out-of-sample confidence intervals conservative.
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
#' # ---- FORECAST ----
#'
#' calibration_tbl %>%
#'     modeltime_forecast(
#'         new_data    = testing(splits),
#'         actual_data = m750
#'     )
#'
#' @name modeltime_forecast
NULL

#' @export
#' @rdname modeltime_forecast
modeltime_forecast <- function(object, new_data = NULL, h = NULL, actual_data = NULL, conf_interval = 0.95, ...) {

    # Checks

    # Check calibration data
    if (!all(c(".type", ".calibration_data") %in% names(object))) {
        glubort("Expecting columns '.type' and '.calibration_data'. Try running 'modeltime_calibrate()' before using 'modeltime_forecast()'.")
    }

    # Check New Data
    if (is.null(new_data) && is.null(h)) {
        message("'new_data' is missing. Using '.calibration_data' to forecast.")
    }

    UseMethod("modeltime_forecast")
}

#' @export
modeltime_forecast.default <- function(object, new_data = NULL, h = NULL, actual_data = NULL, conf_interval = 0.95, ...) {
    glubort("Received an object of class: {class(object)[1]}. Expected an object of class:\n 1. 'mdl_time_tbl' - A Model Time Table made with 'modeltime_table()' and calibrated with 'modeltime_calibrate()'.")

}

#' @export
modeltime_forecast.mdl_time_tbl <- function(object, new_data = NULL, h = NULL, actual_data = NULL, conf_interval = 0.95, ...) {

    data <- object

    n_models <- data$.model_id %>% unique() %>% length()

    # HANDLE CALIBRATION DATA
    data_calibration <- data %>%
        dplyr::select(.model_id, .calibration_data)

    # CREATE FORECAST ----

    # Compute first model with actual data
    ret_1 <- data %>%
        dplyr::ungroup() %>%
        dplyr::slice(1) %>%
        safe_modeltime_forecast_map(
            new_data    = new_data,
            h           = h,
            actual_data = actual_data
        )

    if ("actual" %in% unique(ret_1$.key)) {
        ret_1 <- ret_1 %>%
            dplyr::mutate(.model_desc = ifelse(.key == "actual", "ACTUAL", .model_desc)) %>%
            dplyr::mutate(.model_id = ifelse(.key == "actual", NA_integer_, .model_id))
    }

    # Compute subsequent models without actual data
    ret_2 <- tibble::tibble()

    if (n_models > 1) {
        ret_2 <- data %>%
            dplyr::ungroup() %>%
            dplyr::slice(2:dplyr::n()) %>%
            safe_modeltime_forecast_map(
                new_data    = new_data,
                h           = h,
                actual_data = NULL # Don't pass actual_data
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
        ret <- ret %>%
            safe_conf_interval_map(data_calibration, conf_interval = conf_interval)
    }

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
#'
#' @return A tibble with forecast features
#'
#' @keywords internal
#'
#' @export
mdl_time_forecast <- function(object, calibration_data, new_data = NULL, h = NULL, actual_data = NULL, ...) {
    UseMethod("mdl_time_forecast", object)
}

#' @export
mdl_time_forecast.model_fit <- function(object, calibration_data, new_data = NULL, h = NULL, actual_data = NULL, ...) {

    # MODEL OBJECT

    # If no 'new_data', forecast 'calibration_data'
    if (is.null(new_data) && is.null(h)) {
        new_data <- calibration_data
    }

    # Convert 'h' to 'new_data'
    if (!is.null(h)) {
        # Suppress date selection
        tryCatch({
            suppressMessages(new_data <- timetk::future_frame(calibration_data, .length_out = h, ...))
        }, error = function(e) {
            rlang::abort("'h' requires a 'modeltime' model fitted with a date feature to extend into the future. 'parsnip' models do not contain this.")
        })

    }

    nms_time_stamp_predictors <- timetk::tk_get_timeseries_variables(new_data)[1]
    time_stamp_predictors_tbl <- new_data %>%
        dplyr::select(!! rlang::sym(nms_time_stamp_predictors)) %>%
        dplyr::rename(.index = !! rlang::sym(nms_time_stamp_predictors))

    modeltime_forecast <- object %>%
        stats::predict(new_data = new_data) %>%
        dplyr::bind_cols(time_stamp_predictors_tbl)

    data_formatted <- modeltime_forecast %>%
        dplyr::mutate(.key = "prediction") %>%
        dplyr::select(.key, dplyr::everything())

    # COMBINE ACTUAL DATA

    if (!is.null(actual_data)) {

        # setup
        nms_final     <- names(data_formatted)

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

            data_formatted <- data_formatted %>%
                dplyr::bind_rows(actual_data) %>%
                dplyr::mutate(.pred = ifelse(is.na(.pred), !! target_sym, .pred))

        } else {
            # XY Interface

            rlang::abort("XY Interface not yet implemented for 'modeltime_forecast()'. Try using the Formula Interface with `fit()` or a `workflow()`.")

            # actual_data <- prepare_data(object, actual_data) %>%
            #     tibble::as_tibble() %>%
            #     dplyr::mutate(.key = "actual") %>%
            #     dplyr::rename(.index = !! rlang::sym(nms_time_stamp_predictors))

        }

        data_formatted <- data_formatted %>%
            dplyr::select(!!! rlang::syms(nms_final))

    }

    # FINALIZE
    ret <- data_formatted %>%
        dplyr::rename(.value = .pred) %>%
        dplyr::select(.key, .index, .value) %>%
        dplyr::mutate(.key = factor(.key, levels = c("actual", "prediction"))) %>%
        dplyr::arrange(.key, .index)

    return(ret)

}

#' @export
mdl_time_forecast.workflow <- function(object, calibration_data, new_data = NULL, h = NULL, actual_data = NULL, ...) {

    # Checks
    if (!object$trained) {
        rlang::abort("Workflow must be trained using the 'fit()' function.")
    }

    # WORKFLOW MOLD

    # Contains $predictors, $outcomes, $blueprint
    mld <- object %>% workflows::pull_workflow_mold()

    # NEW DATA

    # If no 'new_data' and no 'h', forecast 'calibration_data'
    if (is.null(new_data) && is.null(h)) {
        new_data <- calibration_data
    }

    # Convert 'h' to 'new_data'
    if (!is.null(h)) {
        # Suppress date selection
        tryCatch({
            suppressMessages(new_data <- timetk::future_frame(calibration_data, .length_out = h, ...))
        }, error = function(e) {
            rlang::abort("'h' requires a 'modeltime' model fitted with a date feature to extend into the future. 'parsnip' models do not contain this.")
        })

    }

    # Issue - Forge processes all recipe steps at once, so need to have outcomes
    # Reference: https://tidymodels.github.io/hardhat/articles/forge.html

    # Prep for Forge (Assign dummy y_var if needed)
    y_var <- names(mld$outcomes)
    if (!any(y_var %in% names(new_data))) {
        new_data[,y_var] <- 1000
    }

    # Apply forge, just to get predictors
    new_data_forged <- hardhat::forge(new_data = new_data, blueprint = mld$blueprint, outcomes = TRUE)

    nms_time_stamp_predictors <- timetk::tk_get_timeseries_variables(new_data_forged$predictors)[1]

    if (!is.na(nms_time_stamp_predictors)) {
        time_stamp_predictors_tbl <- new_data_forged$predictors %>%
            dplyr::select(!! rlang::sym(nms_time_stamp_predictors)) %>%
            dplyr::rename(.index = !! rlang::sym(nms_time_stamp_predictors))
    } else {
        idx <- new_data %>% timetk::tk_index()
        time_stamp_predictors_tbl <- new_data_forged$predictors %>%
            dplyr::mutate(.index = idx)
    }


    # FORGE NEW DATA

    # Issue - hardhat::forge defaults to outcomes = FALSE, which creates an error at predict.workflow()

    # modeltime_forecast <- object %>%
    #     stats::predict(new_data = new_data) %>%
    #     dplyr::bind_cols(time_stamp_predictors_tbl)

    blueprint <- object$pre$mold$blueprint
    forged    <- hardhat::forge(new_data, blueprint, outcomes = TRUE)
    new_data  <- forged$predictors

    fit <- object$fit$fit

    # PREDICT

    data_formatted <- fit %>%
        stats::predict(new_data = new_data) %>%
        dplyr::bind_cols(time_stamp_predictors_tbl) %>%
        dplyr::mutate(.key = "prediction") %>%
        dplyr::select(.key, dplyr::everything())


    # COMBINE ACTUAL DATA

    if (!is.null(actual_data)) {

        nms_final <- names(data_formatted)

        mld <- object %>% workflows::pull_workflow_mold()

        actual_data_forged <- hardhat::forge(new_data = actual_data, blueprint = mld$blueprint, outcomes = TRUE)

        actual_data_prepped <- actual_data_forged$outcomes %>%
            dplyr::bind_cols(actual_data_forged$predictors) %>%
            dplyr::mutate(.key = "actual") %>%
            dplyr::mutate(.index = timetk::tk_index(actual_data))

        target_sym <- rlang::sym(names(actual_data_prepped)[1])

        data_formatted <- data_formatted %>%
            dplyr::bind_rows(actual_data_prepped) %>%
            dplyr::mutate(.pred = ifelse(is.na(.pred), !! target_sym, .pred))

        data_formatted <- data_formatted %>%
            dplyr::select(!!! rlang::syms(nms_final))

    }

    # FINALIZE
    ret <- data_formatted %>%
        dplyr::rename(.value = .pred) %>%
        dplyr::select(.key, .index, .value) %>%
        dplyr::mutate(.key = factor(.key, levels = c("actual", "prediction"))) %>%
        dplyr::arrange(.key, .index)

    return(ret)

}


# SAFE FORECAST MAPPERS ----

safe_modeltime_forecast_map <- function(data, new_data = NULL, h = NULL, actual_data = NULL, ...) {

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
                    actual_data   = actual_data
                    # ,
                    # ...
                )

                ret <- ret %>% purrr::pluck("result")

                return(ret)
            })
        ) %>%
        # Drop unnecessary columns
        dplyr::select(-.model, -.type, -.calibration_data) %>%
        tidyr::unnest(cols = .nested.col)
}


# SAFE CONF INTERVAL MAPPERS ----

safe_conf_interval_map <- function(data, data_calibration, conf_interval) {

    safe_ci <- purrr::safely(
        centered_residuals,
        otherwise = tibble::tibble(
            .conf_lo = NA,
            .conf_hi = NA
        ),
        quiet = FALSE)

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


centered_residuals <- function(data_1, data_2, conf_interval) {

    # Collect absolute residuals
    ret <- tryCatch({

        residuals <- c(data_2$.residuals, -data_2$.residuals)
        s <- stats::sd(residuals)

        data_1 %>%
            dplyr::mutate(
                .conf_lo = stats::qnorm((1-conf_interval)/2, mean = .value, sd = s),
                .conf_hi = stats::qnorm((1+conf_interval)/2, mean = .value, sd = s)
            ) %>%
            dplyr::select(.conf_lo, .conf_hi)

    }, error = function(e) {
        tibble::tibble(
            .conf_lo = NA,
            .conf_hi = NA
        )
    })

    return(ret)

}


# # Normal Conf Interval
# normal_ci_mean_shifted <- function(data, conf_interval = 0.95) {
#
#     x <- data$.residuals
#
#     probs      <- 0.5 + c(-conf_interval/2, conf_interval/2)
#     quantile_x <- stats::quantile(x, prob = probs, na.rm = TRUE)
#     iq_range   <- quantile_x[[2]] - quantile_x[[1]]
#     limits     <- quantile_x + iq_range * c(-1, 1)
#
#     ci_lo_vec  <- limits[1]
#     ci_hi_vec  <- limits[2]
#
#     # Apply mean shift
#     suppressWarnings({
#         mu <- mean(x, na.rm = T)
#     })
#
#     ci_lo_vec_shifted <- min(ci_lo_vec, ci_lo_vec - mu)
#     ci_hi_vec_shifted <- max(ci_hi_vec, ci_hi_vec - mu)
#
#     # Tibble
#     ret <- tibble::tibble(
#         .conf_lo = ci_lo_vec_shifted,
#         .conf_hi = ci_hi_vec_shifted
#     )
#
#     return(ret)
# }
#
# # Normal Conf Interval
# residual_regression <- function(data, conf_interval = 0.95) {
#
#     model <- tryCatch({
#         stats::lm(data$.residuals ~ seq_along(data$.residuals))
#     }, error = function(e) {
#         NULL
#     })
#
#     sd_residuals <- stats::sd(model$residuals)
#
#     alpha   <- (1-conf_interval)/2
#     z_score <- abs(qnorm(alpha))
#
#     # Tibble
#     ret <- tibble::tibble(
#         .conf_lo = (-z_score) * sd_residuals,
#         .conf_hi = (z_score) * sd_residuals
#     )
#
#     return(ret)
#
#
# }


