#' Preparation for confidence interval estimation during forecasting
#'
#'
#' @param object A fitted model object that is either:
#' 1. A workflow that has been fit by [fit.workflow()] or
#' 2. A parsnip model that has been fit using [fit.model_spec()]
#' 3. A modeltime table that has been created using [modeltime_table()]
#' @param new_data A test data set `tibble` containing future information (timestamps and actual values).
#' @param quiet Hide errors (`TRUE`, the default), or display them as they occur?
#' @param ... Additional arguments passed to [modeltime_forecast()].
#'
#'
#' @return A `mdl_time_tbl` with test residuals added.
#'
#' @details
#'
#' The pre-forecasting steps include:
#'
#' 1. Objects are converted to modeltime tables internally
#' 2. Out of sample residuals are calculated from `new_data`
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
#'     set_engine("arima")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#'
#' # --- Calibration ---
#'
#' # Converts to Modeltime Table and adds out of sample residuals
#' model_fit %>%
#'     modeltime_calibrate(new_data = testing(splits))
#'
#'
#' @name modeltime_calibrate
NULL

#' @export
#' @rdname modeltime_calibrate
modeltime_calibrate <- function(object, new_data = NULL,
                                quiet = TRUE, ...) {
    UseMethod("modeltime_calibrate")
}

#' @export
modeltime_calibrate.default <- function(object, new_data = NULL,
                                        quiet = TRUE, ...) {
    glubort("Received an object of class: {class(object)[1]}. Expected an object of class:\n 1. 'workflow' - That has been fitted (trained).\n 2. 'model_fit' - A fitted parsnip model.\n 3. 'mdl_time_tbl' - A Model Time Table made with 'modeltime_table()'.")
}

#' @export
modeltime_calibrate.model_spec <- function(object, new_data = NULL,
                                           quiet = TRUE, ...) {
    rlang::abort("Model spec must be trained using the 'fit()' function.")
}

#' @export
modeltime_calibrate.model_fit <- function(object, new_data = NULL,
                                          quiet = TRUE, ...) {

    ret <- calc_residuals(object, test_data = new_data, quiet = quiet, ...)

    # Convert to mdl_time_tbl
    ret <- ret %>%
        tibble::rowid_to_column(var = ".model_id") %>%
        dplyr::mutate(.model = list(object)) %>%
        dplyr::mutate(.model_desc = get_model_description(object)) %>%
        dplyr::select(.model_id, .model, .model_desc, dplyr::everything())

    class(ret) <- c("mdl_time_tbl", class(ret))

    return(ret)

}

#' @export
modeltime_calibrate.workflow <- function(object, new_data = NULL,
                                         quiet = TRUE, ...) {

    # Checks
    if (!object$trained) {
        rlang::abort("Workflow must be trained using the 'fit()' function.")
    }

    ret <- calc_residuals(object, test_data = new_data, ...)

    # Convert to mdl_time_tbl
    ret <- ret %>%
        tibble::rowid_to_column(var = ".model_id") %>%
        dplyr::mutate(.model = list(object)) %>%
        dplyr::mutate(.model_desc = get_model_description(object)) %>%
        dplyr::select(.model_id, .model, .model_desc, dplyr::everything())

    class(ret) <- c("mdl_time_tbl", class(ret))

    return(ret)

}

#' @export
modeltime_calibrate.mdl_time_tbl <- function(object, new_data = NULL,
                                             quiet = TRUE, ...) {
    data <- object

    safe_calc_residuals <- purrr::safely(calc_residuals, otherwise = NA, quiet = quiet)

    ret <- data %>%
        dplyr::ungroup() %>%
        dplyr::mutate(.nested.col = purrr::map(
            .x         = .model,
            .f         = function(obj) {
                ret <- safe_calc_residuals(
                    obj,
                    test_data = new_data
                )

                ret <- ret %>% purrr::pluck("result")

                return(ret)
            })
        ) %>%
        # dplyr::select(-.model) %>%
        tidyr::unnest(cols = .nested.col)

    if (".nested.col" %in% names(ret)) {
        ret <- ret %>%
            dplyr::select(-.nested.col)
    }

    if (!"mdl_time_tbl" %in% class(ret)) {
        class(ret) <- c("mdl_time_tbl", class(ret))
    }

    return(ret)
}



# UTILITIES ----

calc_residuals <- function(object, test_data = NULL, ...) {

    model_fit <- object

    # Training Metrics
    train_metrics_tbl <- tibble::tibble()

    # Testing Metrics
    test_metrics_tbl <- tibble::tibble()
    if (!is.null(test_data)) {

        predictions_tbl <- object %>%
            modeltime_forecast(
                new_data      = test_data,
                actual_data   = test_data,
                conf_interval = NULL,
                ...
            )

        test_metrics_prepped_tbl <- predictions_tbl %>%
            tidyr::pivot_wider(names_from = .key, values_from = .value) %>%
            tidyr::drop_na() %>%
            tibble::add_column(.type = "Test", .before = 1) %>%
            dplyr::group_by(.type)

        test_metrics_tbl <- test_metrics_prepped_tbl %>%
            dplyr::summarize(.residuals = list(actual - prediction)) %>%
            dplyr::ungroup()

    }

    metrics_tbl <- dplyr::bind_rows(train_metrics_tbl, test_metrics_tbl)

    return(metrics_tbl)
}


