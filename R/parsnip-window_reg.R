# WINDOW FORECAST MODEL ----

#' General Interface for Window Forecast Models
#'
#' `window_reg()` is a way to generate a _specification_ of a window model
#'  before fitting and allows the model to be created using
#'  different R packages as backends.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param id An optional quoted column name (e.g. "id") for
#'  identifying multiple time series (i.e. panel data).
#' @param window_function A function / formula:
#'
#'  - If a function, e.g. `mean`, the function is used with
#'    any additional arguments, `...` in `set_engine()`.
#'  - If a formula, e.g. `~ mean(., na.rm = TRUE)`, it is converted to a function.
#'
#'  This syntax allows you to create very compact anonymous functions.
#' @param window_size A window to apply the window function. By default,
#'  the window uses the full data set, which is rarely the best choice.
#'
#'
#' @details
#'
#' A time series window regression applies a `window_function` to a
#' window of the data (last N observations).
#'
#'  - The function can return a scalar (single value) or multiple values()
#'  - Common use cases:
#'     - __Moving Average Forecasts:__ Forecast forward a 20-day average
#'     - __Weighted Average Forecasts:__ Exponentially weighting the most recent observations
#'     - __Median Forecasts:__ Forecasting forward a 20-day median
#'     - __Repeating Forecasts:__ Simulating a Seasonal Naive Forecast by
#'       broadcasting the last 12 observations of a monthly dataset into the future
#'
#' A time series window regression is derived using `window_reg()`.
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "window" (default) - Performs a Window Forecast applying a `window_function`
#'    to a window of size defined by `window_size`
#'
#'
#' @section Engine Details:
#'
#' __window (default engine)__
#'
#' - The engine uses [window_fit_impl()]
#'
#' @section Fit Details:
#'
#' __Date and Date-Time Variable__
#'
#' It's a requirement to have a date or date-time variable as a predictor.
#' The `fit()` interface accepts date and date-time features and handles them internally.
#'
#' - `fit(y ~ date)`
#'
#
#' __ID features (Multiple Time Series, Panel Data)__
#'
#'  The `id` parameter is populated using the `fit()` or `fit_xy()` function:
#'
#'  _ID Example:_ Suppose you have 3 features:
#'
#'  1. `y` (target)
#'  2. `date` (time stamp),
#'  3. `series_id` (a unique identifer that identifies each time series in your data).
#'
#'  The `series_id` can be passed to the `window_reg()` using
#'  `fit()`:
#'
#'  - `window_reg(id = "series_id")` specifes that the `series_id` column should be used
#'    to identify each time series.
#'  - `fit(y ~ date + series_id)` will pass `series_id` on to the underlying functions.
#'
#' __Window Function Specification (window_function)__
#'
#' You can specify a function / formula using `purrr` syntax.
#'
#'  - If a function, e.g. `mean`, the function is used with
#'    any additional arguments, `...` in `set_engine()`.
#'  - If a formula, e.g. `~ mean(., na.rm = TRUE)`, it is converted to a function.
#'
#'  This syntax allows you to create very compact anonymous functions.
#'
#' __Window Size Specification (window_size)__
#'
#' The period can be non-seasonal (`window_size = 1 or "none"`) or
#' yearly seasonal (e.g. For monthly time stamps, `window_size = 12`, `window_size = "12 months"`, or `window_size = "yearly"`).
#' There are 3 ways to specify:
#'
#' 1. `window_size = "all"`: A seasonal period is selected based on the periodicity of the data (e.g. 12 if monthly)
#' 2. `window_size = 12`: A numeric frequency. For example, 12 is common for monthly data
#' 3. `window_size = "1 year"`: A time-based phrase. For example, "1 year" would convert to 12 for monthly data.
#'
#'
#' __External Regressors (Xregs)__
#'
#' These models are univariate. No xregs are used in the modeling process.
#'
#'
#' @seealso [fit.model_spec()], [set_engine()]
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
#' # ---- SINGLE TIME SERIES ----
#'
#' # Model Spec
#' model_spec <- window_reg(
#'         window_function = median,
#'         window_size     = 12
#'     ) %>%
#'     # Extra parameters passed as: set_engine(...)
#'     set_engine("window", na.rm = TRUE)
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#'
#' # A single mean. No ID column since an ID was not provided.
#' model_fit
#'
#'
#' # ---- PANEL DATA ----
#'
#' # Weighted Average Forecast
#' model_spec <- window_reg(
#'         # Specify the ID column for Panel Data
#'         id = "id",
#'         # Create a Weighted Average
#'         window_function = ~ sum(tail(.x, 3) * c(0.1, 0.3, 0.6)),
#'         window_size     = 12
#'     ) %>%
#'     set_engine("window")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date + id, data = training(splits))
#'
#' # A single weighted value.
#' # An ID column identifies which series this belongs to.
#' model_fit
#'
#' # ---- BROADCASTING PANELS ----
#'
#' # Simulating a Seasonal Naive Forecast by
#' # broadcasted model the last 12 observations into the future
#' model_spec <- window_reg(
#'         id = "id",
#'         window_function = ~ tail(.x, 12),
#'         window_size     = Inf
#'     ) %>%
#'     set_engine("window")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date + id, data = training(splits))
#'
#' # A sequence of observations are returned.
#' # The sequence will be broadcasted (repeated) during prediction.
#' model_fit
#'
#' # The sequence is broadcasted (repeated) during prediction
#' predict(model_fit, testing(splits))
#'
#' @export
window_reg <- function(mode = "regression",
                      id = NULL,
                      window_function = NULL,
                      window_size = NULL
                      ) {

    args <- list(
        id               = rlang::enquo(id),
        window_function  = rlang::enquo(window_function),
        window_size      = rlang::enquo(window_size)
    )

    parsnip::new_model_spec(
        "window_reg",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )

}

#' @export
print.window_reg <- function(x, ...) {
    cat("Window Forecast Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)

    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }

    invisible(x)
}

#' @export
#' @importFrom stats update
update.window_reg <- function(object, parameters = NULL,
                              id = NULL,
                              window_function = NULL,
                              window_size = NULL,
                              fresh = FALSE, ...) {

    parsnip::update_dot_check(...)

    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(
        id               = rlang::enquo(id),
        window_function  = rlang::enquo(window_function),
        window_size      = rlang::enquo(window_size)
    )

    args <- parsnip::update_main_parameters(args, parameters)

    if (fresh) {
        object$args <- args
    } else {
        null_args <- purrr::map_lgl(args, parsnip::null_value)
        if (any(null_args))
            args <- args[!null_args]
        if (length(args) > 0)
            object$args[names(args)] <- args
    }

    parsnip::new_model_spec(
        "window_reg",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.window_reg <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'window'` for translation.")
        engine <- "window"
    }
    x <- parsnip::translate.default(x, engine, ...)

    x
}




# WINDOW ----

#' Low-Level Window Forecast
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param id An optional ID feature to identify different time series. Should be a quoted name.
#' @param window_function A function to apply to the window
#' @param window_size The period to apply the window function to
#' @param ... Not currently used
#'
#' @export
window_fit_impl <- function(x, y, id = NULL,
                            window_function = NULL,
                            window_size = "all",
                            ...) {


    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome    <- y
    predictor  <- x

    # WINDOW FUNCTION
    if (is.null(window_function)) {
        message("window_reg: Using mean() forecast")
        window_function <- mean
    }

    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- parse_index_from_data(predictor)
    if (window_size == "all") {
        message("window_reg: Using window_size = Inf")
        period = Inf
    } else {
        period    <- parse_period_from_index(index_tbl, window_size)
    }
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)


    # VALUE COLUMN
    value_tbl <- tibble::tibble(value = y)

    # PREPARE GROUPS
    is_grouped <- FALSE
    id_tbl     <- NULL
    if (!is.null(id)) {
        is_grouped <- TRUE
        id_tbl <- x %>% dplyr::select(dplyr::all_of(id))
    }

    constructed_tbl <- dplyr::bind_cols(id_tbl, index_tbl, value_tbl)

    if (is_grouped) {
        window_model <- constructed_tbl %>%
            dplyr::group_by(!! rlang::sym(id))
    } else {
        window_model <- constructed_tbl
    }

    window_model <- window_model %>%
        dplyr::arrange(dplyr::all_of(idx_col)) %>%
        dplyr::slice_tail(n = period) %>%
        dplyr::summarise(
            dplyr::across(value, .fns = window_function, ...),
            .groups = "drop") %>%
        dplyr::ungroup()

    # return(window_model)

    # RETURN A NEW MODELTIME BRIDGE

    # Class - Add a class for the model
    class <- "window_fit_impl"

    # Models - Insert model_1 and model_2 into a list
    models <- list(
        model_1 = window_model
    )

    # Data - Start with index tbl and add .actual, .fitted, and .residuals columns
    data <- index_tbl %>%
        dplyr::mutate(
            .actual    =  y,
            .fitted    =  NA,
            .residuals = .actual - .fitted
        )

    # Extras - Pass on transformation recipe
    extras <- list(
        id                = id,
        idx_column        = idx_col,
        value_column      = "value",
        constructed_tbl   = constructed_tbl,
        is_grouped        = is_grouped,
        period            = period
    )

    # Model Description - Gets printed to describe the high-level model structure
    desc <- glue::glue("WINDOW [{period}]")

    # Create new model
    modeltime::new_modeltime_bridge(
        class  = class,
        models = models,
        data   = data,
        extras = extras,
        desc   = desc
    )

}

#' @export
print.window_fit_impl <- function(x, ...) {
    cat(x$desc)
    cat("\n")
    cat("--------")
    cat("\nModel: \n")
    print(x$models$model_1)
    invisible(x)
}

#' Bridge prediction function for window Models
#'
#' @inheritParams parsnip::predict.model_fit
#'
#' @export
window_predict_impl <- function(object, new_data) {

    # PREPARE INPUTS
    model           <- object$models$model_1
    id              <- object$extras$id
    idx_col         <- object$extras$idx_col
    is_grouped      <- object$extras$is_grouped

    if (!is_grouped) {
        h     <- nrow(new_data)
        preds <- rep_len(model$value, length.out = h)
    } else {
        preds <- make_grouped_predictions(model, new_data, id_col = id, idx_col = idx_col)
        # # print("Start")
        #
        # window_nested_tbl <- model %>%
        #     dplyr::group_by(!! rlang::sym(id)) %>%
        #     tidyr::nest(.window_values = value) %>%
        #     dplyr::ungroup()
        #
        # # print("window Nested")
        # # print(window_nested_tbl)
        #
        # new_data_nested_tbl <- new_data %>%
        #     dplyr::select(!! rlang::sym(id), !! rlang::sym(idx_col)) %>%
        #     dplyr::group_by(!! rlang::sym(id)) %>%
        #     tidyr::nest(.idx_values = !! rlang::sym(idx_col)) %>%
        #     dplyr::ungroup()
        #
        # # print("Data Nested")
        # # print(new_data_nested_tbl)
        #
        # data_joined_tbl <- new_data_nested_tbl %>%
        #     dplyr::left_join(window_nested_tbl, by = id)
        #
        # # print("Data Joined")
        # # print(data_joined_tbl)
        #
        # data_joined_tbl <- data_joined_tbl %>%
        #     dplyr::mutate(.final_values = purrr::map2(
        #         .x = .idx_values, .y = .window_values, .f = function(x, y) {
        #
        #             ret <- tryCatch({
        #                 tibble::tibble(value = rep_len(y$value, length.out = nrow(x)))
        #             }, error = function(e) {
        #                 tibble::tibble(value = rep_len(NA, length.out = nrow(x)))
        #             })
        #
        #             return(ret)
        #
        #         })) %>%
        #     dplyr::select(-.window_values) %>%
        #     tidyr::unnest(cols = c(.idx_values, .final_values))
        #
        # # print(data_joined_tbl)
        #
        # preds <- data_joined_tbl$value
    }

    return(preds)

}

#' @export
predict.window_fit_impl <- function(object, new_data, ...) {
    window_predict_impl(object, new_data, ...)
}
