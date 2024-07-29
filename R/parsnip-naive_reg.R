

#' General Interface for NAIVE Forecast Models
#'
#' `naive_reg()` is a way to generate a _specification_ of an NAIVE or SNAIVE model
#'  before fitting and allows the model to be created using
#'  different packages.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param id An optional quoted column name (e.g. "id") for
#'  identifying multiple time series (i.e. panel data).
#' @param seasonal_period SNAIVE only. A seasonal frequency. Uses "auto" by default.
#'  A character phrase of "auto" or time-based phrase of "2 weeks"
#'  can be used if a date or date-time variable is provided.
#'  See Fit Details below.
#'
#'
#' @details
#'
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `naive_reg()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "naive" (default) - Performs a NAIVE forecast
#'  - "snaive" - Performs a Seasonal NAIVE forecast
#'
#'
#' @section Engine Details:
#'
#' __naive (default engine)__
#'
#' - The engine uses [naive_fit_impl()]
#' - The NAIVE implementation uses the last observation and forecasts this value forward.
#' - The `id` can be used to distinguish multiple time series contained in
#'   the data
#' - The `seasonal_period` is not used but provided for consistency with the SNAIVE
#'   implementation
#'
#'  __snaive (default engine)__
#'
#' - The engine uses [snaive_fit_impl()]
#' - The SNAIVE implementation uses the last seasonal series in the data
#'   and forecasts this sequence of observations forward
#' - The `id` can be used to distinguish multiple time series contained in
#'   the data
#' - The `seasonal_period` is used to determine how far back to define the repeated
#'   series. This can be a numeric value (e.g. 28) or a period (e.g. "1 month")
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
#'  The `series_id` can be passed to the `naive_reg()` using
#'  `fit()`:
#'
#'  - `naive_reg(id = "series_id")` specifes that the `series_id` column should be used
#'    to identify each time series.
#'  - `fit(y ~ date + series_id)` will pass `series_id` on to the underlying
#'    naive or snaive functions.
#'
#' __Seasonal Period Specification (snaive)__
#'
#' The period can be non-seasonal (`seasonal_period = 1 or "none"`) or
#' yearly seasonal (e.g. For monthly time stamps, `seasonal_period = 12`, `seasonal_period = "12 months"`, or `seasonal_period = "yearly"`).
#' There are 3 ways to specify:
#'
#' 1. `seasonal_period = "auto"`: A seasonal period is selected based on the periodicity of the data (e.g. 12 if monthly)
#' 2. `seasonal_period = 12`: A numeric frequency. For example, 12 is common for monthly data
#' 3. `seasonal_period = "1 year"`: A time-based phrase. For example, "1 year" would convert to 12 for monthly data.
#'
#' __External Regressors (Xregs)__
#'
#' These models are univariate. No xregs are used in the modeling process.
#'
#'
#' @seealso `fit.model_spec()`, `set_engine()`
#'
#' @examples
#' library(dplyr)
#' library(parsnip)
#' library(rsample)
#' library(timetk)
#'
#' # Data
#' m750 <- m4_monthly %>% filter(id == "M750")
#' m750
#'
#' # Split Data 80/20
#' splits <- initial_time_split(m750, prop = 0.8)
#'
#' # ---- NAIVE ----
#'
#' # Model Spec
#' model_spec <- naive_reg() %>%
#'     set_engine("naive")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#' model_fit
#'
#'
#' # ---- SEASONAL NAIVE ----
#'
#' # Model Spec
#' model_spec <- naive_reg(
#'         id = "id",
#'         seasonal_period = 12
#'     ) %>%
#'     set_engine("snaive")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date + id, data = training(splits))
#' model_fit
#'
#' @export
naive_reg <- function(mode = "regression",
                      id = NULL,
                      seasonal_period = NULL
                      ) {

    args <- list(
        id               = rlang::enquo(id),
        seasonal_period  = rlang::enquo(seasonal_period)
    )

    parsnip::new_model_spec(
        "naive_reg",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )

}

#' @export
print.naive_reg <- function(x, ...) {
    cat("Naive Forecast Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)

    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }

    invisible(x)
}

#' @export
#' @importFrom stats update
update.naive_reg <- function(object, parameters = NULL,
                             id = NULL, seasonal_period = NULL,
                             fresh = FALSE, ...) {

    eng_args <- parsnip::update_engine_parameters(object$eng_args, fresh, ...)

    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(
        id               = rlang::enquo(id),
        seasonal_period  = rlang::enquo(seasonal_period)
    )

    args <- parsnip::update_main_parameters(args, parameters)

    if (fresh) {
        object$args <- args
        object$eng_args <- eng_args
    } else {
        null_args <- purrr::map_lgl(args, parsnip::null_value)
        if (any(null_args))
            args <- args[!null_args]
        if (length(args) > 0)
            object$args[names(args)] <- args
        if (length(eng_args) > 0)
            object$eng_args[names(eng_args)] <- eng_args
    }

    parsnip::new_model_spec(
        "naive_reg",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.naive_reg <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'naive'` for translation.")
        engine <- "naive"
    }
    x <- parsnip::translate.default(x, engine, ...)

    x
}


# NAIVE ----

#' Low-Level NAIVE Forecast
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param id An optional ID feature to identify different time series. Should be a quoted name.
#' @param seasonal_period Not used for NAIVE forecast but here for consistency with SNAIVE
#' @param ... Not currently used
#'
#' @keywords internal
#' @export
naive_fit_impl <- function(x, y, id = NULL, seasonal_period = "auto", ...) {


    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome    <- y
    predictor  <- x

    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- parse_index_from_data(predictor)
    # period    <- parse_period_from_index(index_tbl, seasonal_period)
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
        naive_model <- constructed_tbl %>%
            dplyr::group_by(!! rlang::sym(id)) %>%
            dplyr::arrange(dplyr::pick(dplyr::all_of(idx_col))) %>%
            dplyr::slice_tail(n = 1) %>%
            dplyr::ungroup()
    } else {
        naive_model <- constructed_tbl %>%
            dplyr::arrange(dplyr::pick(dplyr::all_of(idx_col))) %>%
            dplyr::slice_tail(n = 1) %>%
            dplyr::ungroup()
    }

    # RETURN A NEW MODELTIME BRIDGE

    # Class - Add a class for the model
    class <- "naive_fit_impl"

    # Models - Insert model_1 and model_2 into a list
    models <- list(
        model_1 = naive_model
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
        is_grouped        = is_grouped
    )

    # Model Description - Gets printed to describe the high-level model structure
    desc <- "NAIVE"

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
print.naive_fit_impl <- function(x, ...) {
    cat(x$desc)
    cat("\n")
    cat("--------")
    cat("\nModel: \n")
    print(x$models$model_1)
    invisible(x)
}

#' Bridge prediction function for NAIVE Models
#'
#' @inheritParams parsnip::predict.model_fit
#'
#' @keywords internal
#' @export
naive_predict_impl <- function(object, new_data) {

    # PREPARE INPUTS
    model           <- object$models$model_1
    id              <- object$extras$id
    idx_col         <- object$extras$idx_col
    is_grouped      <- object$extras$is_grouped

    if (!is_grouped) {
        h     <- nrow(new_data)
        preds <- rep(model$value, h)
    } else {
        lookup_tbl <- model %>%
            dplyr::select(- (!! idx_col))

        preds <- new_data %>%
            dplyr::select(!! rlang::sym(id), !! rlang::sym(idx_col)) %>%
            dplyr::left_join(lookup_tbl, by = id) %>%
            dplyr::pull(value)
    }

    return(preds)

}


#' @export
predict.naive_fit_impl <- function(object, new_data, ...) {
    naive_predict_impl(object, new_data, ...)
}

# SNAIVE ----

#' Low-Level SNAIVE Forecast
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param id An optional ID feature to identify different time series. Should be a quoted name.
#' @param seasonal_period The seasonal period to forecast into the future
#' @param ... Not currently used
#'
#' @keywords internal
#' @export
snaive_fit_impl <- function(x, y, id = NULL, seasonal_period = "auto", ...) {


    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome    <- y
    predictor  <- x

    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- parse_index_from_data(predictor)
    period    <- parse_period_from_index(index_tbl, seasonal_period)
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
        snaive_model <- constructed_tbl %>%
            dplyr::group_by(!! rlang::sym(id)) %>%
            dplyr::arrange(dplyr::pick(dplyr::all_of(idx_col))) %>%
            dplyr::slice_tail(n = period) %>%
            dplyr::ungroup()
    } else {
        snaive_model <- constructed_tbl %>%
            dplyr::arrange(dplyr::pick(dplyr::all_of(idx_col))) %>%
            dplyr::slice_tail(n = period) %>%
            dplyr::ungroup()
    }

    # RETURN A NEW MODELTIME BRIDGE

    # Class - Add a class for the model
    class <- "snaive_fit_impl"

    # Models - Insert model_1 and model_2 into a list
    models <- list(
        model_1 = snaive_model
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
    desc <- glue::glue("SNAIVE [{period}]")

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
print.snaive_fit_impl <- function(x, ...) {
    cat(x$desc)
    cat("\n")
    cat("--------")
    cat("\nModel: \n")
    print(x$models$model_1)
    invisible(x)
}

#' Bridge prediction function for SNAIVE Models
#'
#' @inheritParams parsnip::predict.model_fit
#'
#' @keywords internal
#' @export
snaive_predict_impl <- function(object, new_data) {

    # PREPARE INPUTS
    model           <- object$models$model_1
    id              <- object$extras$id
    idx_col         <- object$extras$idx_col
    is_grouped      <- object$extras$is_grouped

    if (!is_grouped) {
        h     <- nrow(new_data)
        preds <- rep_len(model$value, length.out = h)
    } else {
        model <- model %>% dplyr::select(- (!! idx_col))
        preds <- make_grouped_predictions(model, new_data, id_col = id, idx_col = idx_col)
    }

    return(preds)

}

#' @export
predict.snaive_fit_impl <- function(object, new_data, ...) {
    snaive_predict_impl(object, new_data, ...)
}


