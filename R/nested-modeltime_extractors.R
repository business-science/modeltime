# NESTED TEST ACCURACY / TEST FORECAST / ERROR REPORTING ----

#' Log Extractor Functions for Modeltime Nested Tables
#'
#' @description
#' Extract logged information calculated during the `modeltime_nested_fit()`,
#' `modeltime_nested_select_best()`, and `modeltime_nested_refit()` processes.
#'
#' @param object A nested modeltime table
#' @param .include_actual Whether or not to include the actual data in the extracted forecast.
#'  Default: TRUE.
#' @param .id_subset Can supply a vector of id's to extract forcasts for one or more id's,
#'  rather than extracting all forecasts. If `NULL`, extracts forecasts for all id's.
#' @param .row_id The row number to extract from the nested data.
#'
#'
#' @name log_extractors

#' @export
#' @rdname log_extractors
extract_nested_test_accuracy <- function(object) {
    attr(object, "accuracy_tbl")
}

#' @export
#' @rdname log_extractors
extract_nested_test_forecast <- function(object, .include_actual = TRUE, .id_subset = NULL) {

    ret <- attr(object, "test_forecast_tbl")

    fit_col       <- attr(object, 'fit_column')
    time_elapsed  <- attr(object, 'time_elapsed')
    error_tbl     <- attr(object, 'error_tbl')
    conf_interval <- attr(object, 'conf_interval')
    conf_method   <- attr(object, 'conf_method')
    if (is.null(conf_method)) {conf_method <- "conformal_default"}


    if (!is.null(ret)) {
        ret <- fcast_extract(
            fcast_tbl       = ret,
            .include_actual = .include_actual,
            .id_subset      = .id_subset,
            .id_text        = attr(object, "id")
        )

        # STRUCTURE

        class(ret) <- c("mdl_nested_forecast_tbl", class(ret))

        attr(ret, "fit_column")       <- fit_col
        attr(ret, "conf_interval")    <- conf_interval
        attr(ret, "conf_method")      <- conf_method
        attr(ret, "error_tbl")        <- error_tbl
        attr(ret, "time_elapsed")     <- time_elapsed
    }

    return(ret)

}

#' @export
#' @rdname log_extractors
extract_nested_error_report <- function(object) {
    attr(object, "error_tbl")
}

#' @export
#' @rdname log_extractors
extract_nested_best_model_report <- function(object) {
    attr(object, "best_selection_tbl")
}

#' @export
#' @rdname log_extractors
extract_nested_future_forecast <- function(object, .include_actual = TRUE, .id_subset = NULL) {

    ret <- attr(object, "future_forecast_tbl")

    fit_col       <- attr(object, 'fit_column')
    time_elapsed  <- attr(object, 'time_elapsed')
    error_tbl     <- attr(object, 'error_tbl')
    conf_interval <- attr(object, 'conf_interval')
    conf_method   <- attr(object, 'conf_method')
    if (is.null(conf_method)) {conf_method <- "conformal_default"}

    if (!is.null(ret)) {
        ret <- fcast_extract(
            fcast_tbl       = ret,
            .include_actual = .include_actual,
            .id_subset      = .id_subset,
            .id_text        = attr(object, "id")
        )

        # STRUCTURE

        class(ret) <- c("mdl_nested_forecast_tbl", class(ret))

        attr(ret, "fit_column")       <- fit_col
        attr(ret, "conf_interval")    <- conf_interval
        attr(ret, "conf_method")      <- conf_method
        attr(ret, "error_tbl")        <- error_tbl
        attr(ret, "time_elapsed")     <- time_elapsed
    }

    return(ret)
}

#' @export
#' @rdname log_extractors
extract_nested_modeltime_table <- function(object, .row_id = 1) {
    object %>%
        dplyr::slice(.row_id) %>%
        dplyr::select(1, .modeltime_tables) %>%
        tidyr::unnest(.modeltime_tables)
}


#' @export
#' @rdname log_extractors
extract_nested_train_split <- function(object, .row_id = 1) {

    actual_data <- object$.actual_data[[.row_id]]
    split_list  <- object$.splits[[.row_id]]

    actual_data %>% dplyr::slice(split_list$idx_train)
}

#' @export
#' @rdname log_extractors
extract_nested_test_split <- function(object, .row_id = 1) {

    actual_data <- object$.actual_data[[.row_id]]
    split_list  <- object$.splits[[.row_id]]

    actual_data %>% dplyr::slice(split_list$idx_test)
}


# HELPERS ----

fcast_extract <- function(fcast_tbl, .include_actual = TRUE, .id_subset = NULL, .id_text = NULL) {

    ret <- fcast_tbl

    if (all(c(".key", .id_text) %in% names(ret))) {

        actual_tbl <- NULL
        if (!.include_actual) {
            ret <- ret %>%
                dplyr::filter(.key != "actual")
        }

        if (!is.null(.id_subset)) {
            ret <- ret %>%
                dplyr::filter(!! rlang::sym(.id_text) %in% .id_subset)
        }

    }

    return(ret)
}
