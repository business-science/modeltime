#' Forecast future data
#'
#' This is a wrapper for `predict()` that is simplifies forecasting
#' future data from a fitted `workflow` (trained workflow) or `model_fit` (trained parsnip model).
#'
#' @param object A fitted model object that is either (1) a workflow that has been fit by [fit.workflow()] or
#'  (2) a parsnip model that has been fit using [fit.model_spec()]
#' @param new_data A `tibble` containing future information to forecast.
#' @param h The forecast horizon (can be used instead of `new_data` for
#'  time series with no exogenous regressors).
#' @param conf_interval An estimated confidence interval based on the in-sample residuals
#' @param actual_data Data that is combined with the output tibble and given an `.id = "actual"`
#' @param ... Additional arguments passed to [future_frame()] for use with the `h` forecast horizon
#'
#'
#' @return
#' A tibble with predictions and time-stamp data. For ease of plotting and calculations,
#'  the column names are transformed to:
#'
#' - `.id`: Values labeled either "prediction" or "actual"
#' - `.index`: The timestamp index.
#' - `.value`: The value being forecasted.
#' - `.conf_lo`: The lower limit of the confidence interval.
#' - `.conf_hi`: The upper limit of the confidence interval.
#'
#' @details
#'
#' The goal of `modeltime_forecast()` is to simplify the process of
#' forecasting future data (controlled by `new_data` or `h`) and
#' combining with existing data (controlled by `actual_data`).
#'
#' __Specifying Future Data__
#'
#' When forecasting without external regressors, meaning that features are dependent on the
#' date feature alone, you can specify future data using:
#'
#' 1. `h = "3 years" or "36 months" or 36`:
#' 2. `new_data = tibble with date column extending the trained dates`
#'
#' __Interfaces__
#'
#' There are 2 interfaces:
#'
#' 1. Fitted Parsnip Model
#' 2. Fitted Workflow
#'
#' _Interface 1: Fitted Parsnip Model (`model_fit` class)_
#'
#' - Currently, only the __formula format__ is supported (e.g. `model_fit <- model_spec %>% fit(y ~ date)`).
#' - New data and actual data are processed according to the formula (e.g. `fit(log(y) ~ date)` will
#' result in a log transformation applied to future data and new data)
#'
#' _Interface 2: Fitted Workflow (`workflow` with `$trained = TRUE`)_
#'
#' - Currently, only the __recipe format__ is supported. However, `tidymodels/workflows` Issue #34
#' will correct issues with indicators, which prevents the __formula format__.
#' - Transformations are applied according to the `recipe`. New data is forged with `hardhat::forge`.
#'
#' _Confidence Interval Estimation_
#'
#' Confidence intervals are estimated based on the normal estimation of the training errors.
#' The confidence interval can be adjusted with the `conf_interval` parameter. An
#' 80% confidence interval estimates a normal (gaussian distribution) that assumes that
#' 80% of the future data will fall within the upper and lower confidence limits.
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
#' m750
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
#' # --- PRODUCE FORECAST ---
#'
#' # Using new_data
#' model_fit %>%
#'     modeltime_forecast(new_data = testing(splits))
#'
#' # Using horizon, h
#' model_fit %>%
#'     modeltime_forecast(h = "3 years")
#'
#' # Combining forecast with actual values
#' model_fit %>%
#'     modeltime_forecast(h = "3 years", actual_data = training(splits))
#'
#' @name modeltime_forecast
NULL

#' @export
#' @rdname modeltime_forecast
modeltime_forecast <- function(object, new_data = NULL, h = NULL, conf_interval = 0.8, actual_data = NULL, ...) {
    UseMethod("modeltime_forecast")
}

#' @export
modeltime_forecast.default <- function(object, new_data = NULL, h = NULL, conf_interval = 0.8, actual_data = NULL, ...) {
    rlang::abort(stringr::str_glue("Received an object of class: {class(object)[1]}. Expected an object of class 'workflow' that has been fitted (trained) or 'model_fit' (a fitted parsnip model). "))
}

#' @export
modeltime_forecast.model_spec <- function(object, new_data = NULL, h = NULL, conf_interval = 0.8, actual_data = NULL, ...) {
    rlang::abort("Model spec must be trained using the 'fit()' function.")
}

#' @export
modeltime_forecast.model_fit <- function(object, new_data = NULL, h = NULL, conf_interval = 0.8, actual_data = NULL, ...) {

    # MODEL OBJECT

    if (!is.null(h)) {
        # Suppress date selection
        tryCatch({
            suppressMessages(new_data <- timetk::future_frame(object$fit$data, .length_out = h, ...))
        }, error = function(e) {
            rlang::abort("No valid date or date-time column found in the model. 'h' requires a date column to extend into the future.")
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
        dplyr::mutate(.id = "prediction") %>%
        dplyr::select(.id, dplyr::everything())

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
                dplyr::mutate(.id = "actual") %>%
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

            rlang::abort("XY Interface not yet implemented for 'modeltime_forecast()'. Try using the Formula Interface with `fit.model_spec()`.")

            # actual_data <- prepare_data(object, actual_data) %>%
            #     tibble::as_tibble() %>%
            #     dplyr::mutate(.id = "actual") %>%
            #     dplyr::rename(.index = !! rlang::sym(nms_time_stamp_predictors))

        }

        data_formatted <- data_formatted %>%
            dplyr::select(!!! rlang::syms(nms_final))

    }

    # FINALIZE
    ret <- data_formatted %>%
        dplyr::rename(.value = .pred) %>%
        dplyr::select(.id, .index, .value) %>%
        dplyr::mutate(.id = factor(.id, levels = c("actual", "prediction"))) %>%
        dplyr::arrange(.id, .index)

    # ADD CONF INTERVAL
    residuals  <- object$fit$data$.resid
    ret        <- add_conf_interval(ret, residuals, conf_interval, bootstrap = FALSE)

    return(ret)

}

#' @export
modeltime_forecast.workflow <- function(object, new_data = NULL, h = NULL, conf_interval = 0.8, actual_data = NULL, ...) {

    # Checks
    if (!object$trained) {
        rlang::abort("Workflow must be trained using the 'fit()' function.")
    }

    # WORKFLOW MOLD

    # Contains $predictors, $outcomes, $blueprint
    mld <- object %>% workflows::pull_workflow_mold()

    if (!is.null(h)) {
        # Suppress date selection
        tryCatch({
            suppressMessages(new_data <- timetk::future_frame(mld$predictors, .length_out = h, ...))
        }, error = function(e) {
            rlang::abort("No valid date or date-time column found in the workflow. 'h' requires a date column to extend into the future.")
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
    time_stamp_predictors_tbl <- new_data_forged$predictors %>%
        dplyr::select(!! rlang::sym(nms_time_stamp_predictors)) %>%
        dplyr::rename(.index = !! rlang::sym(nms_time_stamp_predictors))


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
        dplyr::mutate(.id = "prediction") %>%
        dplyr::select(.id, dplyr::everything())


    # COMBINE ACTUAL DATA

    if (!is.null(actual_data)) {

        nms_final <- names(data_formatted)

        mld <- object %>% workflows::pull_workflow_mold()

        actual_data_forged <- hardhat::forge(new_data = actual_data, blueprint = mld$blueprint, outcomes = TRUE)

        actual_data <- actual_data_forged$outcomes %>%
            dplyr::bind_cols(actual_data_forged$predictors) %>%
            dplyr::mutate(.id = "actual") %>%
            dplyr::rename(.index = !! rlang::sym(nms_time_stamp_predictors))

        target_sym <- rlang::sym(names(actual_data)[1])

        data_formatted <- data_formatted %>%
            dplyr::bind_rows(actual_data) %>%
            dplyr::mutate(.pred = ifelse(is.na(.pred), !! target_sym, .pred))

        data_formatted <- data_formatted %>%
            dplyr::select(!!! rlang::syms(nms_final))

    }

    # FINALIZE
    ret <- data_formatted %>%
        dplyr::rename(.value = .pred) %>%
        dplyr::select(.id, .index, .value) %>%
        dplyr::mutate(.id = factor(.id, levels = c("actual", "prediction"))) %>%
        dplyr::arrange(.id, .index)

    # ADD CONF INTERVALS
    residuals <- object$fit$fit$fit$data$.resid
    ret       <- add_conf_interval(ret, residuals, conf_interval, bootstrap = FALSE)

    return(ret)

}

#' @export
modeltime_forecast.mdl_time_tbl <- function(object, new_data = NULL, h = NULL, conf_interval = 0.8, actual_data = NULL, ...) {

    data <- object

    safe_modeltime_forecast <- purrr::safely(modeltime_forecast, otherwise = NA, quiet = FALSE)

    # Compute first model with actual data
    ret_1 <- data %>%
        dplyr::ungroup() %>%
        dplyr::slice(1) %>%
        dplyr::mutate(.nested.col = purrr::map(
            .x         = .model,
            .f         = function(obj) {

                ret <- safe_modeltime_forecast(
                    obj,
                    new_data      = new_data,
                    h             = h,
                    conf_interval = NULL,
                    actual_data   = actual_data,
                    ...
                )

                ret <- ret %>% purrr::pluck("result")

                return(ret)
            })
        ) %>%
        dplyr::select(-.model) %>%
        tidyr::unnest(cols = .nested.col)

    if ("actual" %in% unique(ret_1$.id)) {
        ret_1 <- ret_1 %>%
            mutate(.model_desc = ifelse(.id == "actual", "ACTUAL", .model_desc)) %>%
            mutate(.model_id = ifelse(.id == "actual", NA_integer_, .model_id))
    }

    # Compute subsequent models without actual data
    ret_2 <- data %>%
        dplyr::slice(2:dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(.nested.col = purrr::map(
            .x         = .model,
            .f         = function(obj) {

                ret <- safe_modeltime_forecast(
                    obj,
                    new_data      = new_data,
                    h             = h,
                    conf_interval = NULL,
                    actual_data   = NULL,
                    ...
                )

                ret <- ret %>% purrr::pluck("result")

                return(ret)
            })
        ) %>%
        dplyr::select(-.model) %>%
        tidyr::unnest(cols = .nested.col)

    if (".nested.col" %in% names(ret_1)) {
        ret_1 <- ret_1 %>%
            dplyr::select(-.nested.col)
    }

    if (".nested.col" %in% names(ret_2)) {
        ret_2 <- ret_2 %>%
            dplyr::select(-.nested.col)
    }

    ret <- dplyr::bind_rows(ret_1, ret_2)

    return(ret)
}

# CONFIDENCE INTERVAL ESTIMATION ----

add_conf_interval <- function(data, residuals, conf_interval, bootstrap = FALSE) {

    if (!is.null(conf_interval)) {

        if (conf_interval >= 1 | conf_interval <= 0.5) {
            rlang::abort("conf_interval must be between 0.5 and 0.95")
        }

        # Calculate limits
        if (bootstrap) {
            # Reference: https://blog.methodsconsultants.com/posts/understanding-bootstrap-confidence-interval-output-from-the-r-boot-package/
            # TODO
        } else {
            # Assume normal
            limits_tbl <- normal_ci(residuals, conf_interval)
        }

        data <- data %>%
            dplyr::mutate(
                .conf_lo = ifelse(.id == "prediction", .value + limits_tbl$conf_lo, NA),
                .conf_hi = ifelse(.id == "prediction", .value + limits_tbl$conf_hi, NA)
            )
    }

    return(data)

}

normal_ci <- function(residuals, conf_interval = 0.8) {

    probs      <- 0.5 + c(-conf_interval/2, conf_interval/2)
    quantile_x <- stats::quantile(residuals, prob = probs, na.rm = TRUE)
    iq_range   <- quantile_x[[2]] - quantile_x[[1]]
    limits     <- quantile_x + iq_range * c(-1, 1)

    ret <- tibble::tibble(
        conf_lo = limits[1],
        conf_hi = limits[2]
    )

    return(ret)
}

# bootstrap_ci <- function(residuals, conf_interval = 0.8, times = 500) {
#
#     data  <- tibble(.resid = residuals)
#     probs <- 0.5 + c(-conf_interval/2, conf_interval/2)
#
#     ret <- replicate(times, data, simplify = FALSE) %>%
#         bind_rows(.id = ".id") %>%
#         mutate(.id = as_factor(.id)) %>%
#         group_by(.id) %>%
#         sample_n(size = times, replace = TRUE) %>%
#         ungroup() %>%
#         summarize(
#             conf_lo = quantile(.resid, prob = probs[1], na.rm = TRUE),
#             conf_hi = quantile(.resid, prob = probs[2], na.rm = TRUE)
#         )
#
#     return(ret)
#
# }


# PARSNIP HELPERS ----

find_formula_lhs <- function(object) {

    check_formula_tbl <- object %>%
        purrr::map_dfr(~ rlang::is_formula(.)) %>%
        tidyr::gather() %>%
        dplyr::filter(value)

    formula_found <- FALSE
    if (nrow(check_formula_tbl) == 1) {
        formula_found <- TRUE
    }

    lhs <- NULL
    if (formula_found) {
        lhs <- rlang::f_lhs(object[[check_formula_tbl$key]])
    }

    return(lhs)
}

