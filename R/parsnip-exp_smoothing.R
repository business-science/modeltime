#' General Interface for Exponential Smoothing State Space Models
#'
#' `exp_smoothing()` is a way to generate a _specification_ of an Exponential Smoothing model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `forecast`.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param period A seasonal frequency. Uses "auto" by default.
#'  A character phrase of "auto" or time-based phrase of "2 weeks"
#'  can be used if a date or date-time variable is provided.
#'  See Fit Details below.
#' @param error The form of the error term: "auto", "additive", or "multiplicative".
#'  If the error is multiplicative, the data must be non-negative.
#' @param trend The form of the trend term: "auto", "additive", "multiplicative" or "none".
#' @param season The form of the seasonal term: "auto", "additive", "multiplicative" or "none"..
#' @param damping Apply damping to a trend: "auto", "damped", or "none".
#'
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `exp_smoothing()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "ets" (default) - Connects to [forecast::ets()]
#'
#' @section Engine Details:
#'
#' The standardized parameter names in `modeltime` can be mapped to their original
#' names in each engine:
#'
#' ```{r echo = FALSE}
#' # parsnip::convert_args("exp_smoothing")
#' tibble::tribble(
#'     ~ "modeltime", ~ "forecast::ets",
#'     "period()", "ts(frequency)",
#'     "error(), trend(), season()", "model",
#'     "damping()", "damped"
#' ) %>% knitr::kable()
#' ```
#'
#' Other options can be set using `set_engine()`.
#'
#' __ets (default engine)__
#'
#' The engine uses [forecast::ets()].
#'
#' Function Parameters:
#' ```{r echo = FALSE}
#' str(forecast::ets)
#' ```
#' The main arguments are `model` and `damped` are defined using:
#' - `error()` = "auto", "additive", and "multiplicative" are converted to  "Z", "A", and "M"
#' - `trend()` = "auto", "additive", "multiplicative", and "none" are converted to "Z","A","M" and "N"
#' - `season()` = "auto", "additive", "multiplicative", and "none" are converted to "Z","A","M" and "N"
#' - `damping()` - "auto", "damped", "none" are converted to NULL, TRUE, FALSE
#'
#' By default, all arguments are set to "auto" to perform automated Exponential Smoothing using
#' _in-sample data_ following the underlying `forecast::ets()` automation routine.
#'
#' Other options and argument can be set using `set_engine()`.
#'
#' Parameter Notes:
#' - `xreg` - This model is not set up to use exogenous regressors. Only univariate
#'  models will be fit.
#'
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
#' _Period Specification_
#'
#' The period can be non-seasonal (`period = 1`) or seasonal (e.g. `period = 12` or `period = "12 months"`).
#' There are 3 ways to specify:
#'
#' 1. `period = "auto"`: A period is selected based on the periodicity of the data (e.g. 12 if monthly)
#' 2. `period = 12`: A numeric frequency. For example, 12 is common for monthly data
#' 3. `period = "1 year"`: A time-based phrase. For example, "1 year" would convert to 12 for monthly data.
#'
#'
#' __Univariate:__
#'
#' For univariate analysis, you must include a date or date-time feature. Simply use:
#'
#'  - Formula Interface (recommended): `fit(y ~ date)` will ignore xreg's.
#'  - XY Interface: `fit_xy(x = data[,"date"], y = data$y)` will ignore xreg's.
#'
#' __Multivariate (xregs, Exogenous Regressors)__
#'
#' This model is not set up for use with exogenous regressors.
#'
#'
#' @seealso [fit.exp_smoothing()], [set_engine()]
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
#' # ---- AUTO ETS ----
#'
#' # Model Spec - The default parameters are all set
#' # to "auto" if none are provided
#' model_spec <- exp_smoothing() %>%
#'     set_engine("ets")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#' model_fit
#'
#'
#' # ---- STANDARD ETS ----
#'
#' # Model Spec
#' model_spec <- exp_smoothing(
#'         period  = 12,
#'         error   = "multiplicative",
#'         trend   = "additive",
#'         season  = "multiplicative"
#'     ) %>%
#'     set_engine("ets")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#' model_fit
#'
#' @export
exp_smoothing <- function(mode = "regression", period = NULL,
                          error = NULL, trend = NULL, season = NULL, damping = NULL) {

    args <- list(
        period   = rlang::enquo(period),
        error    = rlang::enquo(error),
        trend    = rlang::enquo(trend),
        season   = rlang::enquo(season),
        damping  = rlang::enquo(damping)
    )

    parsnip::new_model_spec(
        "exp_smoothing",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )

}

#' @export
print.exp_smoothing <- function(x, ...) {
    cat("Exponential Smoothing State Space Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)

    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }

    invisible(x)
}

#' @export
#' @importFrom stats update
update.exp_smoothing <- function(object, parameters = NULL,
                                 period = NULL,
                                 error = NULL, trend = NULL, season = NULL, damping = NULL,
                                 fresh = FALSE, ...) {

    parsnip::update_dot_check(...)

    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(
        period   = rlang::enquo(period),
        error    = rlang::enquo(error),
        trend    = rlang::enquo(trend),
        season   = rlang::enquo(season),
        damping  = rlang::enquo(damping)
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
        "exp_smoothing",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.exp_smoothing <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'ets'` for translation.")
        engine <- "ets"
    }
    x <- parsnip::translate.default(x, engine, ...)

    x
}


# FIT - ets -----

#' Low-Level Exponential Smoothing function for translating modeltime to forecast
#'
#' @inheritParams exp_smoothing
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param ... Additional arguments passed to `forecast::ets`
#'
#' @export
ets_fit_impl <- function(x, y, period = "auto",
                         error = "auto", trend = "auto",
                         season = "auto", damping = "auto", ...) {

    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome    <- y
    predictor  <- x

    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- parse_index_from_data(predictor)
    period    <- parse_period_from_index(index_tbl, period)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)

    outcome   <- stats::ts(outcome, frequency = period)

    # CHECK PARAMS
    if (!error %in% c("auto", "additive", "multiplicative")) {
        rlang::abort("'error' must be one of 'auto', 'additive', or 'multiplicative'.")
    }
    if (!trend %in% c("auto", "additive", "multiplicative", "none")) {
        rlang::abort("'trend' must be one of 'auto', 'additive', 'multiplicative', or 'none'.")
    }
    if (!season %in% c("auto", "additive", "multiplicative", "none")) {
        rlang::abort("'season' must be one of 'auto', 'additive', 'multiplicative', or 'none'.")
    }
    if (!damping %in% c("auto", "damped", "none")) {
        rlang::abort("'damping' must be one of 'auto', 'damped', or 'none'.")
    }

    # CONVERT PARAMS
    error_ets <- switch(
        error,
        "auto"           = "Z",
        "additive"       = "A",
        "multiplicative" = "M"
    )
    trend_ets <- switch(
        trend,
        "auto"           = "Z",
        "additive"       = "A",
        "multiplicative" = "M",
        "none"           = "N"
    )
    season_ets <- switch(
        season,
        "auto"           = "Z",
        "additive"       = "A",
        "multiplicative" = "M",
        "none"           = "N"
    )
    damping_ets <- switch(
        damping,
        "auto"           = NULL,
        "damped"         = TRUE,
        "none"           = FALSE
    )

    model_ets <- stringr::str_c(error_ets, trend_ets, season_ets)

    # XREGS - NOT USED FOR forecast::ets()

    # FIT
    fit_ets <- forecast::ets(outcome, model = model_ets, damped = damping_ets, ...)

    # RETURN
    new_modeltime_bridge(
        class = "ets_fit_impl",

        # Models
        models = list(
            model_1 = fit_ets
        ),

        # Data
        data = tibble::tibble(
            !! idx_col  := idx,
            .value      =  as.numeric(fit_ets$x),
            .fitted     =  as.numeric(fit_ets$fitted),
            .resid      =  as.numeric(fit_ets$residuals)
        ),

        # Preprocessing Recipe (prepped) - Used in predict method
        extras = NULL,

        desc = fit_ets$method[1]
    )

}

#' @export
print.ets_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}


# PREDICT ----

#' @export
predict.ets_fit_impl <- function(object, new_data, ...) {
    ets_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for Exponential Smoothing models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `forecast::ets()`
#'
#' @export
ets_predict_impl <- function(object, new_data, ...) {

    # PREPARE INPUTS
    model       <- object$models$model_1
    idx_train   <- object$data %>% timetk::tk_index()
    h_horizon   <- nrow(new_data)

    # XREG
    # - No xregs for forecast::ets()

    # PREDICTIONS
    preds_forecast <- forecast::forecast(model, h = h_horizon, ...)

    # Return predictions as numeric vector
    preds <- tibble::as_tibble(preds_forecast) %>% purrr::pluck(1)

    return(preds)

}










