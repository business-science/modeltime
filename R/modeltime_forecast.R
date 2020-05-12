#' Forecast future data
#'
#' This is a wrapper for `predict()` that is useful for forecasting
#' future data from a fitted `workflow` (trained workflows object) or `model_fit` (trained parsnip model).
#'
#' @param object A fitted model object that is either (1) a workflow that has been fit by [fit.workfow()] or
#'  (2) a parsnip model that has been fit using [fit.model_spec()]
#' @param new_data A `tibble` containing future information to forecast
#' @param h The forecast horizon (can be used instead of `new_data` for
#'  time series with no exogenous regressors).
#' @param actual_data Data is combined with the output tibble and given an `.id = "actual"`
#' @param ... Additional arguments passed to [future_frame()] for use with the `h` forecast horizon
#'
#' @details
#'
#' TODO
#'
#'
#'
#' @return A tibble with predictions and time-stamp data.
#'
#'
#' @examples
#'
#' # TODO
#'
#'
#'
#' @name modeltime_forecast
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

# #' @export
# modeltime_forecast.model_fit <- function(object, new_data = NULL, h = NULL, actual_data = NULL, ...) {
#
#     # WORKFLOW MOLD ----
#
#     # Contains $model, $index
#     model <- object %>%
#         dplyr::pluck("fit")
#
#     if (!is.null(h)) {
#         # Suppress date selection
#         tryCatch({
#             index_tbl <- tibble::tibble(.index = model$index)
#             suppressMessages(new_data <- timetk::future_frame(index_tbl, .length_out = h, ...))
#         }, error = function(e) {
#             rlang::abort("No valid date or date-time column found in the model. 'h' requires a date column to extend into the future.")
#         })
#
#     }
#
#     nms_time_stamp_predictors <- timetk::tk_get_timeseries_variables(new_data)[1]
#     time_stamp_predictors_tbl <- new_data %>%
#         dplyr::select(!! rlang::sym(nms_time_stamp_predictors)) %>%
#         dplyr::rename(.index = !! rlang::sym(nms_time_stamp_predictors))
#
#     modeltime_forecast <- object %>%
#         predict(new_data = ) %>%
#         dplyr::bind_cols(time_stamp_predictors_tbl)
#
#     data_formatted <- modeltime_forecast %>%
#         mutate(.id = "prediction") %>%
#         select(.id, dplyr::everything())
#
#     # COMBINE ACTUAL DATA ----
#
#     if (!is.null(actual_data)) {
#
#         nms_final <- names(data_formatted)
#
#         mld <- object %>% workflows::pull_workflow_mold()
#
#         actual_data_forged <- hardhat::forge(new_data = actual_data, blueprint = mld$blueprint, outcomes = TRUE)
#
#         actual_data <- bind_cols(actual_data_forged$outcomes, actual_data_forged$predictors) %>%
#             dplyr::mutate(.id = "actual") %>%
#             dplyr::rename(.index = !! rlang::sym(nms_time_stamp_predictors))
#
#         target_sym <- rlang::sym(names(actual_data)[1])
#
#         data_formatted <- data_formatted %>%
#             dplyr::bind_rows(actual_data) %>%
#             dplyr::mutate(.pred = ifelse(is.na(.pred), !! target_sym, .pred))
#
#         data_formatted <- data_formatted %>%
#             dplyr::select(!!! rlang::syms(nms_final))
#
#     }
#
#     # FINALIZE ----
#     data_formatted %>%
#         dplyr::rename(.value = .pred) %>%
#         dplyr::select(.id, .index, .value) %>%
#         dplyr::arrange(.index, .id) %>%
#         dplyr::mutate(.id = factor(.id, levels = c("actual", "prediction")))
#
# }
