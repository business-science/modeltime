


#' Prepared Nested Modeltime Data
#'
#' @description
#' A set of functions to simplify preparation of nested data for
#' iterative (nested) forecasting with Nested Modeltime Tables.
#'
#' @param .data A data frame or tibble containing time series data. The data should have:
#'
#' - identifier (.id_var): Identifying one or more time series groups
#' - date variable (.date_var): A date or date time column
#' - target variable (.value): A column containing numeric values that is to be forecasted
#'
#' @param .id_var An id column
#' @param .date_var A date or datetime column
#' @param .length_out Varies based on the function:
#'
#' - `extend_timeseries()`: Defines how far into the future to extend the
#'   time series by each time series group.
#' - `nest_timeseries()`: Defines which observations should be split into the `.future_data`.
#'
#' @param .length_test Defines the length of the test split for evaluation.
#' @param .length_train Defines the length of the training split for evaluation.
#' @param ... Additional arguments passed to the helper function. See details.
#'
#' @details
#'
#' Preparation of nested time series follows a 3-Step Process:
#'
#' ### Step 1: Extend the Time Series
#'
#' `extend_timeseries()`: A wrapper for [timetk::future_frame()] that extends a time series
#'   group-wise into the future.
#'
#'   - The group column is specified by `.id_var`.
#'   - The date column is specified by `.date_var`.
#'   - The length into the future is specified with `.length_out`.
#'
#' ### Step 2: Nest the Time Series
#'
#' `nest_timeseries()`: A helper for nesting your data into `.actual_data` and `.future_data`.
#'
#'   - The group column is specified by `.id_var`
#'   - The `.length_out` defines the length of the `.future_data`. The remaining data is
#'     converted to the `.actual_data`
#'
#'   The result is a "nested data frame".
#'
#' ### Step 3: Split the Actual Data into Train/Test Splits
#'
#' `split_nested_timeseries()`: A wrapper for [timetk::time_series_split()] that generates
#'   training/testing splits from the `.actual_data` column.
#'
#'   - The `.length_test` is the primary argument that identifies the size of the
#'     testing sample. This is typically the same size as the `.future_data`.
#'   - The `.length_train` is an optional size of the training data.
#'   - The `...` (dots) are additional arguments that can be passed to [timetk::time_series_split()].
#'
#' @examples
#'
#' library(tidyverse)
#' library(timetk)
#' library(modeltime)
#'
#' walmart_sales_weekly %>%
#'     select(id, Date, Weekly_Sales) %>%
#'     set_names(c("id", "date", "value")) %>%
#'
#'     # Extends the time series by id
#'     extend_timeseries(
#'         .id_var     = id,
#'         .date_var   = date,
#'         .length_out = 52
#'     ) %>%
#'
#'     # Nests the time series into .actual_data and .future_data
#'     nest_timeseries(
#'         .id_var     = id,
#'         .length_out = 52
#'     ) %>%
#'
#'     # Adds a column .splits that contains training/testing samples
#'     split_nested_timeseries(
#'         .length_test = 52
#'     )
#'
#'
#'
#' @name prep_nested


#' @export
#' @rdname prep_nested
extend_timeseries <- function(.data, .id_var, .date_var, .length_out) {

    # val_expr <- rlang::enquo(.value)
    id_expr  <- rlang::enquo(.id_var)

    # if (rlang::quo_is_missing(val_expr)) rlang::abort("`.value` is missing with no default. This should be a target variable.")
    if (rlang::quo_is_missing(id_expr)) rlang::abort("`.id_var` is missing with no default. This should be a column that identifies time series groupings.")

    # CHECKS

    # missing_data_tbl <- .data %>%
    #     dplyr::filter(is.na(!! val_expr))
    #
    # if (nrow(missing_data_tbl) > 0) {
    #     col_name <- rlang::quo_name(val_expr)
    #     cli::cli_h2("Missing Target Value Report:")
    #     cli::cli_alert_danger(stringr::str_glue("The following data has missing values in the `{col_name}` column."))
    #     print(missing_data_tbl)
    #     rlang::warn(
    #         stringr::str_glue("Missing data detected in `{col_name}` .value column. Please fix by filling missing values.")
    #     )
    # }

    # EXTEND

    .data %>%
        dplyr::group_by(!! enquo(.id_var)) %>%
        timetk::future_frame(
            .date_var   = !! rlang::enquo(.date_var),
            .length_out = .length_out,
            .bind_data  = TRUE
        ) %>%
        dplyr::ungroup()
}



#' @export
#' @rdname prep_nested
nest_timeseries <- function(.data, .id_var, .length_out) {

    id_var_expr    <- rlang::enquo(.id_var)

    # SPLIT FUTURE AND ACTUAL DATA

    future_data_tbl <- .data %>%
        panel_tail(id = !!id_var_expr, n = .length_out)

    groups <- future_data_tbl$id %>% unique() %>% length()

    n_group <- .data %>%
        dplyr::group_by(!!id_var_expr) %>%
        dplyr::summarise(n = dplyr::n() - (dim(future_data_tbl)[1] / groups) )

    actual_data_tbl <- .data %>%
        dplyr::inner_join(n_group, by = rlang::quo_name(id_var_expr)) %>%
        dplyr::group_by(!!id_var_expr) %>%
        dplyr::slice(seq(dplyr::first(n))) %>%
        dplyr::ungroup() %>%
        dplyr::select(-n)


    # CHECKS
    if (nrow(future_data_tbl) == 0) {
        rlang::warn("Future Data is `NULL`. Try using `extend_timeseries()` to add future data.")
    }

    # NEST

    ret_1 <- actual_data_tbl %>%
        tidyr::nest(.actual_data = - (!! id_var_expr))

    ret_2 <- future_data_tbl %>%
        tidyr::nest(.future_data = - (!! id_var_expr))

    # JOIN

    id_col_text <- names(ret_1)[[1]]

    ret <- dplyr::left_join(ret_1, ret_2, by = id_col_text)

    return(ret)


}

#' @export
#' @rdname prep_nested
split_nested_timeseries <- function(.data, .length_test, .length_train = NULL, ...) {

    if (rlang::is_missing(.length_test)) rlang::abort("`.length_test` is missing. Provide a value for the time series length of the test split. ")

    if (!".actual_data" %in% names(.data)) rlang::abort("`.actual_data` column is not found. Try using `nest_timeseries()` to create a nested data frame with columns `.actual_data` and `.future_data`.")

    id_text <- names(.data)[1]

    cum <- FALSE
    if (is.null(.length_train)) {
        cum <- TRUE
        .length_train <- 5
    }

    suppressMessages({
        .data %>%
            dplyr::mutate(.splits = purrr::map2(.actual_data, !!rlang::ensym(id_text), .f = function(x, i) {

                # print(i)

                tryCatch({
                    timetk::time_series_split(
                        x,
                        initial    = .length_train,
                        assess     = .length_test,
                        cumulative = cum,
                        ...
                    )

                }, error = function(e) {
                    # rlang::warn("Problem with: {as.character(i)}")
                    error_msg = stringr::str_c(as.character(e), collapse = '. ')

                    rlang::warn(stringr::str_glue("Problem Splitting ID: {i} | Returing <NULL> split | {error_msg}"))
                    NULL
                })


            }))
    })

}
