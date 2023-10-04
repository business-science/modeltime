


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
#' @param .length_future Varies based on the function:
#'
#' - `extend_timeseries()`: Defines how far into the future to extend the
#'   time series by each time series group.
#' - `nest_timeseries()`: Defines which observations should be split into the `.future_data`.
#'
#' @param .length_actual Can be used to slice the `.actual_data` to a most recent number of observations.
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
#'   - The length into the future is specified with `.length_future`.
#'   - The `...` are additional parameters that can be passed to [timetk::future_frame()]
#'
#' ### Step 2: Nest the Time Series
#'
#' `nest_timeseries()`: A helper for nesting your data into `.actual_data` and `.future_data`.
#'
#'   - The group column is specified by `.id_var`
#'   - The `.length_future` defines the length of the `.future_data`.
#'   - The remaining data is converted to the `.actual_data`.
#'   - The `.length_actual` can be used to slice the `.actual_data` to a most recent number of observations.
#'
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
#' ### Helpers
#'
#' [`extract_nested_train_split()`] and [`extract_nested_test_split()`] are used to simplify extracting
#' the training and testing data from the actual data. This can be helpful when making
#' preprocessing recipes using the `recipes` package.
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#'
#'
#' nested_data_tbl <- walmart_sales_weekly %>%
#'     select(id, date = Date, value = Weekly_Sales) %>%
#'
#'     # Step 1: Extends the time series by id
#'     extend_timeseries(
#'         .id_var     = id,
#'         .date_var   = date,
#'         .length_future = 52
#'     ) %>%
#'
#'     # Step 2: Nests the time series into .actual_data and .future_data
#'     nest_timeseries(
#'         .id_var     = id,
#'         .length_future = 52
#'     ) %>%
#'
#'     # Step 3: Adds a column .splits that contains training/testing indices
#'     split_nested_timeseries(
#'         .length_test = 52
#'     )
#'
#' nested_data_tbl
#'
#' # Helpers: Getting the Train/Test Sets
#' extract_nested_train_split(nested_data_tbl, .row_id = 1)
#'
#' @name prep_nested
NULL

#' @export
#' @rdname prep_nested
extend_timeseries <- function(.data, .id_var, .date_var, .length_future, ...) {

    # val_expr <- rlang::enquo(.value)
    id_expr   <- rlang::enquo(.id_var)
    date_expr <- rlang::enquo(.date_var)

    # if (rlang::quo_is_missing(val_expr)) rlang::abort("`.value` is missing with no default. This should be a target variable.")
    if (rlang::quo_is_missing(id_expr)) rlang::abort("`.id_var` is missing with no default. This should be a column that identifies time series groupings.")
    if (rlang::quo_is_missing(date_expr)) rlang::abort("`.date_var` is missing with no default. This should be a column that identifies time series date or datetime column.")

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
        dplyr::arrange(!! date_expr) %>%
        timetk::future_frame(
            .date_var      = !! date_expr,
            .length_out    = .length_future,
            .bind_data     = TRUE,
            ...
        ) %>%
        dplyr::ungroup()
}



#' @export
#' @rdname prep_nested
nest_timeseries <- function(.data, .id_var, .length_future, .length_actual = NULL) {

    id_var_expr    <- rlang::enquo(.id_var)

    # SPLIT FUTURE AND ACTUAL DATA

    .data <- .data %>%
        tibble::rowid_to_column(var = "..rowid")

    future_data_tbl <- .data %>%
        panel_tail(id = !!id_var_expr, n = .length_future)

    actual_data_tbl <- .data %>%
        dplyr::anti_join(future_data_tbl, by = "..rowid")

    # REMOVE ROWID
    future_data_tbl <- future_data_tbl %>% dplyr::select(-..rowid)
    actual_data_tbl <- actual_data_tbl %>% dplyr::select(-..rowid)

    # groups <- future_data_tbl$id %>% unique() %>% length()
    #
    # n_group <- .data %>%
    #     dplyr::group_by(!!id_var_expr) %>%
    #     dplyr::summarise(n = dplyr::n() - (dim(future_data_tbl)[1] / groups) )
    #
    # actual_data_tbl <- .data %>%
    #     dplyr::inner_join(n_group, by = rlang::quo_name(id_var_expr)) %>%
    #     dplyr::group_by(!!id_var_expr) %>%
    #     dplyr::slice(seq(dplyr::first(n))) %>%
    #     dplyr::ungroup() %>%
    #     dplyr::select(-n)

    if (!is.null(.length_actual)) {
        actual_data_tbl <- actual_data_tbl %>%
            dplyr::group_by(!! id_var_expr) %>%
            dplyr::slice_tail(n = .length_actual) %>%
            dplyr::ungroup()
    }


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

    # cum <- FALSE
    # if (is.null(.length_train)) {
    #     cum <- TRUE
    #     .length_train <- 5
    # }

    suppressMessages({
        .data %>%
            dplyr::mutate(.splits = purrr::map2(.actual_data, !!rlang::ensym(id_text), .f = function(x, i) {

                # print(i)

                tryCatch({

                    make_ts_splits(x, .length_test = .length_test, .length_train = .length_train)

                }, error = function(e) {
                    # rlang::warn("Problem with: {as.character(i)}")
                    error_msg = stringr::str_c(as.character(e), collapse = '. ')

                    rlang::warn(stringr::str_glue("Problem Splitting ID: {i} | Returing <NULL> split | {error_msg}"))
                    NULL
                })


            }))
    })

}



# Helpers ----

#' Generate a Time Series Train/Test Split Indicies
#'
#' Makes fast train/test split indicies for time series.
#'
#' @param .data A data frame containing ordered time seried data (ascending)
#' @param .length_test The number of rows to include in the test set
#' @param .length_train Optional. The number of rows to include in the training set.
#'  If NULL, returns all remaining row indicies.
#'
#' @return A list containing train_idx and test_idx
#'
#' @keywords internal
#' @export
#'
make_ts_splits <- function(.data, .length_test, .length_train = NULL) {

    idx <- seq(1, nrow(.data))

    # print("idx")
    # print(idx)

    idx_test <- utils::tail(idx, n = .length_test)

    # print("idx_test")
    # print(idx_test)

    idx_train <- seq_len(idx_test[1]-1)

    # print("idx_train")
    # print(idx_train)

    if (!is.null(.length_train)) {
        idx_train <- utils::tail(idx_train, n = .length_train)
    }

    ret <- list(
        idx_train = idx_train,
        idx_test  = idx_test
    )

    class(ret) <- c("ts_split_indicies")

    return(ret)
}

#' @export
print.ts_split_indicies <- function(x, ...) {
    print(stringr::str_glue("split [{length(x$idx_train)}|{length(x$idx_test)}|{length(x$idx_test)+length(x$idx_train)}]"))
}

# Extract Train / Test Splits from Nested Modeltime Data
#
# These function simplify extracting training and testing data, which is
# useful when developing preprocessing recipes for models using the `recipes` package.
#
# @param .data Data that has been nested and split with [nest_timeseries()] and [split_nested_timeseries()].
# @param .row_id The row number to extract from the nested data.
#
# @name extract_split


