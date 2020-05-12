#' Forecast future data
#'
#' This is a wrapper for `predict()` that is useful for forecasting
#' future data from a fitted `workflow` (trained workflows object) or `model_fit` (trained parsnip model).
#'
#' @param object A fitted model object that is either (1) a workflow that has been fit by [fit.workflow()] or
#'  (2) a parsnip model that has been fit using [fit.model_spec()]
#' @param new_data A `tibble` containing future information to forecast.
#' @param h The forecast horizon (can be used instead of `new_data` for
#'  time series with no exogenous regressors).
#' @param actual_data Data that is combined with the output tibble and given an `.id = "actual"`
#' @param ... Additional arguments passed to [future_frame()] for use with the `h` forecast horizon
#'
#'
#' @return A tibble with predictions and time-stamp data.
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
#'     set_engine("forecast")
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
modeltime_forecast <- function(object, new_data = NULL, h = NULL, actual_data = NULL, ...) {
    UseMethod("modeltime_forecast")
}

#' @export
modeltime_forecast.default <- function(object, new_data = NULL, h = NULL, actual_data = NULL, ...) {
    rlang::abort(stringr::str_glue("Received an object of class: {class(object)[1]}. Expected an object of class 'workflow' that has been fitted (trained) or 'model_fit' (a fitted parsnip model). "))
}

#' @export
modeltime_forecast.model_spec <- function(object, new_data = NULL, h = NULL, actual_data = NULL, ...) {
    rlang::abort("Model spec must be trained using the 'fit()' function.")
}

#' @export
modeltime_forecast.workflow <- function(object, new_data = NULL, h = NULL, actual_data = NULL, ...) {

    # Checks
    if (!object$trained) {
        rlang::abort("Workflow must be trained using the 'fit()' function.")
    }

    # WORKFLOW MOLD ----

    # Contains $predictors, $outcomes, $blueprint
    mld <- object %>%
        workflows::pull_workflow_mold()

    if (!is.null(h)) {
        # Suppress date selection
        tryCatch({
            suppressMessages(new_data <- timetk::future_frame(mld$predictors, .length_out = h, ...))
        }, error = function(e) {
            rlang::abort("No valid date or date-time column found in the workflow. 'h' requires a date column to extend into the future.")
        })

    }

    new_data_forged <- hardhat::forge(new_data = new_data, blueprint = mld$blueprint, outcomes = FALSE)

    nms_time_stamp_predictors <- timetk::tk_get_timeseries_variables(new_data_forged$predictors)[1]
    time_stamp_predictors_tbl <- new_data_forged$predictors %>%
        dplyr::select(!! rlang::sym(nms_time_stamp_predictors)) %>%
        dplyr::rename(.index = !! rlang::sym(nms_time_stamp_predictors))

    modeltime_forecast <- object %>%
        stats::predict(new_data = new_data_forged$predictors) %>%
        dplyr::bind_cols(time_stamp_predictors_tbl)

    data_formatted <- modeltime_forecast %>%
        dplyr::mutate(.id = "prediction") %>%
        dplyr::select(.id, dplyr::everything())

    # COMBINE ACTUAL DATA ----

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

    # FINALIZE ----
    data_formatted %>%
        dplyr::rename(.value = .pred) %>%
        dplyr::select(.id, .index, .value) %>%
        dplyr::arrange(.index, .id) %>%
        dplyr::mutate(.id = factor(.id, levels = c("actual", "prediction")))

}

#' @export
modeltime_forecast.model_fit <- function(object, new_data = NULL, h = NULL, actual_data = NULL, ...) {

    # MODEL OBJECT ----

    if (!is.null(h)) {
        # Suppress date selection
        tryCatch({
            suppressMessages(new_data <- timetk::future_frame(object$fit$index, .length_out = h, ...))
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

    # COMBINE ACTUAL DATA ----

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

            # Applies any preprocessing
            actual_data <- prepare_data(object, actual_data) %>% tibble::as_tibble()

            # Set ID & Index
            actual_data <- actual_data %>%
                dplyr::mutate(.id = "actual") %>%
                dplyr::rename(.index = !! rlang::sym(nms_time_stamp_predictors)) %>%
                dplyr::rename(!! object$preproc$y_var := 1)

            # Get the target variable symbol
            target_sym <- rlang::sym(object$preproc$y_var)

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

    # FINALIZE ----
    data_formatted %>%
        dplyr::rename(.value = .pred) %>%
        dplyr::select(.id, .index, .value) %>%
        dplyr::arrange(.index, .id) %>%
        dplyr::mutate(.id = factor(.id, levels = c("actual", "prediction")))

}


# PARSNIP HELPERS ----

prepare_data <- function(object, new_data) {
    fit_interface <- object$spec$method$fit$interface

    pp_names <- names(object$preproc)
    if (any(pp_names == "terms") | any(pp_names == "x_var")) {
        # Translation code
        if (fit_interface == "formula") {
            new_data <- convert_xy_to_form_new(object$preproc, new_data)
        } else {
            new_data <- convert_form_to_xy_new(object$preproc, new_data)$x
        }
    }
    switch(
        fit_interface,
        none = new_data,
        data.frame = as.data.frame(new_data),
        matrix = as.matrix(new_data),
        new_data
    )
}

#' @importFrom stats na.pass
convert_form_to_xy_new <- function(object, new_data, na.action = na.pass,
                                   composition = "data.frame") {
    if (!(composition %in% c("data.frame", "matrix")))
        rlang::abort("`composition` should be either 'data.frame' or 'matrix'.")

    mod_terms <- object$terms
    # mod_terms <- delete.response(mod_terms)

    # Calculate offset(s). These can show up in-line in the formula
    # (in multiple places) and might also be as its own argument. If
    # there is more than one offset, we add them together.

    offset_cols <- attr(mod_terms, "offset")

    # If offset was done at least once in-line
    if (!is.null(offset_cols)) {
        offset <- rep(0, nrow(new_data))
        for (i in offset_cols)
            offset <- offset +
                rlang::eval_tidy(attr(mod_terms, "variables")[[i + 1]], new_data) # use na.action here and below?
    } else offset <- NULL

    if (!is.null(object$offset_expr)) {
        if (is.null(offset))
            offset <- rep(0, nrow(new_data))
        offset <- offset + rlang::eval_tidy(object$offset_expr, new_data)
    }

    new_data <-
        stats::model.frame(
            mod_terms,
            new_data,
            na.action = na.action,
            xlev = object$xlevels)

    cl <- attr(mod_terms, "dataClasses")
    if (!is.null(cl))
        stats::.checkMFClasses(cl, new_data)

    if(object$options$indicators) {
        new_data <- stats::model.matrix(mod_terms, new_data, contrasts.arg = object$contrasts)
    }

    new_data <- new_data[, colnames(new_data) != "(Intercept)", drop = FALSE]

    if (composition == "data.frame")
        new_data <- as.data.frame(new_data)
    else {
        if (will_make_matrix(new_data))
            new_data <- as.matrix(new_data)
    }
    list(x = new_data, offset = offset)
}

convert_xy_to_form_new <- function(object, new_data) {
    new_data <- new_data[, c(object$x_var), drop = FALSE]
    if (!is.data.frame(new_data))
        new_data <- as.data.frame(new_data)
    new_data
}

will_make_matrix <- function(y) {
    if (is.matrix(y) | is.vector(y))
        return(FALSE)
    cls <- unique(unlist(lapply(y, class)))
    if (length(cls) > 1)
        return(FALSE)
    can_convert <-
        vapply(y, function(x)
            is.atomic(x) & !is.factor(x), logical(1))
    all(can_convert)
}
