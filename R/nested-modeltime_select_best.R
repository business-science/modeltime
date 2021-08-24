

# NESTED SELECT BEST ----

#' Select the Best Models from Nested Modeltime Table
#'
#' @description
#' Finds the best models for each time series group in a Nested Modeltime Table using
#' a `metric` that the user specifies.
#'
#' - Logs the best results, which can be accessed with [extract_nested_best_model_report()]
#' - If `filter_test_forecasts = TRUE`, updates the test forecast log, which can be accessed
#'   [extract_nested_test_forecast()]
#'
#' @param object A Nested Modeltime Table
#' @param metric A metric to minimize or maximize. By default available metrics are:
#'
#' - "rmse" (default)
#' - "mae"
#' - "mape"
#' - "mase"
#' - "smape"
#' - "rsq"
#'
#' @param minimize Whether to minimize or maximize. Default: TRUE (minimize).
#' @param filter_test_forecasts Whether or not to update the test forecast log to
#'  filter only the best forecasts. Default: TRUE.
#'
#'
#' @export
modeltime_nested_select_best <- function(object, metric = "rmse", minimize = TRUE,
                                         filter_test_forecasts = TRUE) {

    # CHECKS ----

    if (!inherits(object, "nested_mdl_time")) rlang::abort("object must be a Nested Modeltime Table. Try using `modeltime_nested_fit()`.")

    acc_tbl <- object %>%
        extract_nested_test_accuracy()

    if (is.null(acc_tbl)) {
        rlang::abort("Accuracy table is not found. Try using `modeltime_nested_fit()`.")
    }

    if (!metric[[1]] %in% names(acc_tbl)) {
        rlang::abort(stringr::str_glue("metric: {metric[[1]]} is not detected. Please review the accuracy table with `extract_nested_test_accuracy()`."))
    }

    # Handle inputs ----
    id_text <- attr(object, "id")
    id_expr <- rlang::sym(id_text)

    metric_expr <- rlang::sym(metric[[1]])

    metric_fun <- ifelse(
        minimize,
        function(x) {suppressWarnings(min(x, na.rm = TRUE))},
        function(x) {suppressWarnings(max(x, na.rm = TRUE))}
    )

    # Select best from accuracy ----
    best_model_by_id_tbl <- acc_tbl %>%
        dplyr::group_by(!! id_expr) %>%

        dplyr::filter( (!! metric_expr) == metric_fun(!! metric_expr) ) %>%

        dplyr::slice(1) %>%
        dplyr::ungroup()

    best_model_by_id_tbl <- object %>%
        dplyr::select(!! id_expr) %>%
        dplyr::left_join(
            best_model_by_id_tbl,
            by = c(id_text)
        )

    # Check for NA's
    any_missing <- any(is.na(best_model_by_id_tbl$.model_id))

    if (any_missing) {

        # Warn if any missing
        rlang::warn(
            stringr::str_glue("Best Model Selection: Some time series did not have a best local model. Review with `extract_nested_best_model_report()`.")
        )

        # if Model ID provided, fill missing with models
        # if (!is.null(fill_model_id)) {
        #
        #     fill_model_id
        #
        #     best_model_by_id_tbl <- best_model_by_id_tbl %>%
        #         replace_na(replace = list(
        #             .model_id   = fill_model_id[1],
        #             .model_desc = "NULL"
        #         ))
        # }

    }



    # # Use best overall ----
    #
    # if (any_missing && use_best_overall_on_error) {
    #
    #     best_overall_tbl <- best_model_by_id_tbl %>%
    #         dplyr::count(.model_id, .model_desc, sort = TRUE) %>%
    #         dplyr::slice(1)
    #
    #     model_id_best_overall <- best_overall_tbl %>%
    #         dplyr::pull(.model_id)
    #
    #     model_desc_best_overall <- best_overall_tbl %>%
    #         dplyr::pull(.model_desc)
    #
    #     if (!is.na(model_id_best_overall)) {
    #         best_model_by_id_tbl <- best_model_by_id_tbl %>%
    #             replace_na(replace = list(
    #                 .model_id   = model_id_best_overall,
    #                 .model_desc = model_desc_best_overall
    #             ))
    #     }
    #
    # }

    # Log best selections ----

    attr(object, "best_selection_tbl")  <- best_model_by_id_tbl

    best_model_by_id_tbl <- best_model_by_id_tbl %>%
        dplyr::select(!! id_expr, .model_id)


    # Update Modeltime Tables ----
    modeltime_tables_tbl <- object %>%

        dplyr::select(!! id_expr, .modeltime_tables) %>%
        tidyr::unnest(.modeltime_tables) %>%
        dplyr::right_join(best_model_by_id_tbl, by = c(id_text, ".model_id")) %>%

        tidyr::nest(.modeltime_tables = -(!! id_expr) ) %>%
        dplyr::mutate(.modeltime_tables = purrr::map(.modeltime_tables, function(x) {
            class(x) <- c("mdl_time_tbl", class(x))
            x
        }))

    object$.modeltime_tables <- modeltime_tables_tbl$.modeltime_tables


    # Filter Forecasts ----

    if (filter_test_forecasts) {

        # Updated Test Forecast
        test_forecast_tbl <- object %>%
            extract_nested_test_forecast()

        if (!is.null(test_forecast_tbl)) {

            test_actual <- test_forecast_tbl %>%
                dplyr::filter(.model_desc == "ACTUAL")

            test_forecast <- test_forecast_tbl %>%
                dplyr::right_join(best_model_by_id_tbl, by = c(id_text, ".model_id"))

            test_forecast_tbl <- dplyr::bind_rows(
                test_actual,
                test_forecast
            )

            attr(object, "test_forecast_tbl")  <- test_forecast_tbl

        }

        # Updated Test Forecast
        future_forecast_tbl <- object %>%
            extract_nested_future_forecast()

        if (!is.null(future_forecast_tbl)) {

            future_actual <- future_forecast_tbl %>%
                dplyr::filter(.model_desc == "ACTUAL")

            future_forecast <- future_forecast_tbl %>%
                dplyr::right_join(best_model_by_id_tbl, by = c(id_text, ".model_id"))

            future_forecast_tbl <- dplyr::bind_rows(
                future_actual,
                future_forecast
            )

            attr(object, "future_forecast_tbl")  <- future_forecast_tbl

        }

    }




    return(object)

}

