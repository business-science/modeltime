# EXPONENTIAL SMOOTHING ----

#' General Interface for Exponential Smoothing State Space Models
#'
#' @description
#' `exp_smoothing()` is a way to generate a _specification_ of an Exponential Smoothing model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `forecast`. Several algorithms are implemented:
#'
#'  - ETS - Automated Exponential Smoothing
#'  - CROSTON - Croston's forecast is a special case
#'     of Exponential Smoothing for intermittent demand
#'  - Theta - A special case of Exponential Smoothing with Drift that
#'     performed well in the M3 Competition
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param seasonal_period A seasonal frequency. Uses "auto" by default.
#'  A character phrase of "auto" or time-based phrase of "2 weeks"
#'  can be used if a date or date-time variable is provided.
#'  See Fit Details below.
#' @param error The form of the error term: "auto", "additive", or "multiplicative".
#'  If the error is multiplicative, the data must be non-negative.
#' @param trend The form of the trend term: "auto", "additive", "multiplicative" or "none".
#' @param season The form of the seasonal term: "auto", "additive", "multiplicative" or "none".
#' @param damping Apply damping to a trend: "auto", "damped", or "none".
#' @param smooth_level This is often called the "alpha" parameter used as the base level smoothing
#'  factor for exponential smoothing models.
#' @param smooth_trend This is often called the "beta" parameter used as the trend smoothing
#'  factor for exponential smoothing models.
#' @param smooth_seasonal This is often called the "gamma" parameter used as the seasonal smoothing
#'  factor for exponential smoothing models.
#'
#' @details
#'
#' Models can be created using the following _engines_:
#'
#'  - "ets" (default) - Connects to [forecast::ets()]
#'  - "croston" - Connects to [forecast::croston()]
#'  - "theta" - Connects to [forecast::thetaf()]
#'  - "smooth_es" - Connects to [smooth::es()]
#'
#' @section Engine Details:
#'
#' The standardized parameter names in `modeltime` can be mapped to their original
#' names in each engine:
#'
#' ```{r echo = FALSE}
#' # parsnip::convert_args("exp_smoothing")
#'tibble::tribble(
#'    ~ "modeltime", ~ "forecast::ets", ~ "forecast::croston()", ~ "forecast::thetaf()", ~ "smooth::es()",
#'    "seasonal_period()", "ts(frequency)", "ts(frequency)", "ts(frequency)", "ts(frequency)",
#'    "error(), trend(), season()", "model ('ZZZ')", "NA", "NA", "model('ZZZ')",
#'    "damping()", "damped (NULL)", "NA", "NA", "phi",
#'    "smooth_level()", "alpha (NULL)", "alpha (0.1)", "NA", "persistence(alpha)",
#'    "smooth_trend()", "beta (NULL)", "NA", "NA", "persistence(beta)",
#'    "smooth_seasonal()", "gamma (NULL)", "NA", "NA", "persistence(gamma)"
#') %>% knitr::kable()
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
#' - `smooth_level()`, `smooth_trend()`, and `smooth_seasonal()` are
#'    automatically determined if not provided. They are mapped to "alpha", "beta" and "gamma", respectively.
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
#' __croston__
#'
#' The engine uses [forecast::croston()].
#'
#' Function Parameters:
#' ```{r echo = FALSE}
#' str(forecast::croston)
#' ```
#' The main arguments are defined using:
#' - `smooth_level()`: The "alpha" parameter
#'
#' Parameter Notes:
#' - `xreg` - This model is not set up to use exogenous regressors. Only univariate
#'   models will be fit.
#'
#' __theta__
#'
#' The engine uses [forecast::thetaf()]
#'
#' Parameter Notes:
#' - `xreg` - This model is not set up to use exogenous regressors. Only univariate
#'   models will be fit.
#'
#'
#' __smooth_es__
#'
#' The engine uses [smooth::es()].
#'
#' Function Parameters:
#' ```{r echo = FALSE, eval = rlang::is_installed("smooth")}
#' str(smooth::es)
#' ```
#' The main arguments `model` and `phi` are defined using:
#' - `error()` = "auto", "additive" and "multiplicative" are converted to "Z", "A" and "M"
#' - `trend()` = "auto", "additive", "multiplicative", "additive_damped", "multiplicative_damped" and "none" are converted to "Z", "A", "M", "Ad", "Md" and "N".
#' - `season()` = "auto", "additive", "multiplicative", and "none" are converted "Z", "A","M" and "N"
#' - `damping()` - Value of damping parameter. If NULL, then it is estimated.
#' - `smooth_level()`, `smooth_trend()`, and `smooth_seasonal()` are
#'    automatically determined if not provided. They are mapped to "persistence"("alpha", "beta" and "gamma", respectively).
#'
#' By default, all arguments are set to "auto" to perform automated Exponential Smoothing using
#' _in-sample data_ following the underlying `smooth::es()` automation routine.
#'
#' Other options and argument can be set using `set_engine()`.
#'
#' Parameter Notes:
#' - `xreg` - This is supplied via the parsnip / modeltime `fit()` interface
#'  (so don't provide this manually). See Fit Details (below).
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
#' _Seasonal Period Specification_
#'
#' The period can be non-seasonal (`seasonal_period = 1` or `"none"`) or seasonal (e.g. `seasonal_period = 12` or `seasonal_period = "12 months"`).
#' There are 3 ways to specify:
#'
#' 1. `seasonal_period = "auto"`: A period is selected based on the periodicity of the data (e.g. 12 if monthly)
#' 2. `seasonal_period = 12`: A numeric frequency. For example, 12 is common for monthly data
#' 3. `seasonal_period = "1 year"`: A time-based phrase. For example, "1 year" would convert to 12 for monthly data.
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
#'  Just for `smooth` engine.
#'
#'  The `xreg` parameter is populated using the `fit()` or `fit_xy()` function:
#'
#'  - Only `factor`, `ordered factor`, and `numeric` data will be used as xregs.
#'  - Date and Date-time variables are not used as xregs
#'  - `character` data should be converted to factor.
#'
#'  _Xreg Example:_ Suppose you have 3 features:
#'
#'  1. `y` (target)
#'  2. `date` (time stamp),
#'  3. `month.lbl` (labeled month as a ordered factor).
#'
#'  The `month.lbl` is an exogenous regressor that can be passed to the `arima_reg()` using
#'  `fit()`:
#'
#'  - `fit(y ~ date + month.lbl)` will pass `month.lbl` on as an exogenous regressor.
#'  - `fit_xy(data[,c("date", "month.lbl")], y = data$y)` will pass x, where x is a data frame containing `month.lbl`
#'   and the `date` feature. Only `month.lbl` will be used as an exogenous regressor.
#'
#'  Note that date or date-time class values are excluded from `xreg`.
#'
#'
#' @seealso `fit.model_spec()`, `set_engine()`
#'
#' @examplesIf rlang::is_installed("smooth")
#' library(dplyr)
#' library(parsnip)
#' library(rsample)
#' library(timetk)
#' library(smooth)
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
#'         seasonal_period  = 12,
#'         error            = "multiplicative",
#'         trend            = "additive",
#'         season           = "multiplicative"
#'     ) %>%
#'     set_engine("ets")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#' model_fit
#'
#'
#' # ---- CROSTON ----
#' \donttest{
#' # Model Spec
#' model_spec <- exp_smoothing(
#'         smooth_level = 0.2
#'     ) %>%
#'     set_engine("croston")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#' model_fit
#' }
#'
#'
#'
#' # ---- THETA ----
#' \donttest{
#' #' # Model Spec
#' model_spec <- exp_smoothing() %>%
#'     set_engine("theta")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#' model_fit
#' }
#'
#'
#'
#'
#' #' # ---- SMOOTH ----
#' \donttest{
#' #' # Model Spec
#' model_spec <- exp_smoothing(
#'                seasonal_period  = 12,
#'                error            = "multiplicative",
#'                trend            = "additive_damped",
#'                season           = "additive"
#'          ) %>%
#'     set_engine("smooth_es")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(value ~ date, data = training(splits))
#' model_fit
#' }
#'
#' @export
exp_smoothing <- function(mode = "regression", seasonal_period = NULL,
                          error = NULL, trend = NULL, season = NULL, damping = NULL,
                          smooth_level = NULL, smooth_trend = NULL, smooth_seasonal = NULL
                          ) {

    args <- list(
        seasonal_period   = rlang::enquo(seasonal_period),
        error             = rlang::enquo(error),
        trend             = rlang::enquo(trend),
        season            = rlang::enquo(season),
        damping           = rlang::enquo(damping),
        smooth_level      = rlang::enquo(smooth_level),
        smooth_trend      = rlang::enquo(smooth_trend),
        smooth_seasonal   = rlang::enquo(smooth_seasonal)
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
                                 seasonal_period = NULL,
                                 error = NULL, trend = NULL, season = NULL, damping = NULL,
                                 smooth_level = NULL, smooth_trend = NULL, smooth_seasonal = NULL,
                                 fresh = FALSE, ...) {

    eng_args <- parsnip::update_engine_parameters(object$eng_args, fresh, ...)

    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(
        seasonal_period   = rlang::enquo(seasonal_period),
        error             = rlang::enquo(error),
        trend             = rlang::enquo(trend),
        season            = rlang::enquo(season),
        damping           = rlang::enquo(damping),
        smooth_level      = rlang::enquo(smooth_level),
        smooth_trend      = rlang::enquo(smooth_trend),
        smooth_seasonal   = rlang::enquo(smooth_seasonal)
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


# ETS -----

#' Low-Level Exponential Smoothing function for translating modeltime to forecast
#'
#' @inheritParams exp_smoothing
#' @inheritParams forecast::ets
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param period A seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param ... Additional arguments passed to `forecast::ets`
#'
#' @keywords internal
#' @export
ets_fit_impl <- function(x, y, period = "auto",
                         error = "auto", trend = "auto",
                         season = "auto", damping = "auto",
                         alpha = NULL, beta = NULL, gamma = NULL, ...) {

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

    error  <- tolower(error)
    trend  <- tolower(trend)
    season <- tolower(season)

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
    fit_ets <- forecast::ets(outcome, model = model_ets, damped = damping_ets,
                             alpha = alpha, beta = beta, gamma = gamma, ...)

    # RETURN
    new_modeltime_bridge(
        class = "ets_fit_impl",

        # Models
        models = list(
            model_1 = fit_ets
        ),

        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            !! idx_col  := idx,
            .actual      =  as.numeric(fit_ets$x),
            .fitted      =  as.numeric(fit_ets$fitted),
            .residuals   =  as.numeric(fit_ets$residuals)
        ),

        # Preprocessing Recipe (prepped) - Used in predict method
        extras = NULL,

        # Description
        desc = fit_ets$method[1]
    )

}

#' @export
print.ets_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}



#' @export
predict.ets_fit_impl <- function(object, new_data, ...) {
    ets_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for Exponential Smoothing models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `forecast::ets()`
#'
#' @keywords internal
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

# SMOOTH -----

#' Low-Level Exponential Smoothing function for translating modeltime to forecast
#'
#' @inheritParams exp_smoothing
#' @inheritParams forecast::ets
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param period A seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param ... Additional arguments passed to `smooth::es`
#'
#' @keywords internal
#' @export
smooth_fit_impl <- function(x, y, period = "auto",
                         error = "auto", trend = "auto",
                         season = "auto", damping = NULL,
                         alpha = NULL, beta = NULL, gamma = NULL, ...) {

    args <- list(...)

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

    # XREGS
    # Clean names, get xreg recipe, process predictors
    xreg_recipe <- create_xreg_recipe(predictor, prepare = TRUE)
    xreg_matrix <- juice_xreg_recipe(xreg_recipe, format = "matrix")

    outcome   <- stats::ts(outcome, frequency = period)

    error  <- tolower(error)
    trend  <- tolower(trend)
    season <- tolower(season)

    # CHECK PARAMS
    if (!error %in% c("auto", "additive", "multiplicative")) {
        rlang::abort("'error' must be one of 'auto', 'additive', or 'multiplicative'.")
    }
    if (!trend %in% c("auto", "additive", "multiplicative", "none", "additive_damped", "multiplicative_damped")) {
        rlang::abort("'trend' must be one of 'auto', 'additive', 'multiplicative', 'none', 'additive_damped' or 'multiplicative_damped'.")
    }
    if (!season %in% c("auto", "additive", "multiplicative", "none")) {
        rlang::abort("'season' must be one of 'auto', 'additive', 'multiplicative', or 'none'.")
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
        "auto"                  = "Z",
        "additive"              = "A",
        "multiplicative"        = "M",
        "none"                  = "N",
        "additive_damped"       = "Ad",
        "multiplicative_damped" = "Md"
    )
    season_ets <- switch(
        season,
        "auto"           = "Z",
        "additive"       = "A",
        "multiplicative" = "M",
        "none"           = "N"
    )


    model_ets <- stringr::str_c(error_ets, trend_ets, season_ets)

    #If the model is passed through set_engine, we use this option (this is because there are 30 options
    #and not all of them are included in the "standard" options).
    if (any(names(args) == "model")){
        model_ets <- args$model
        args$model <- NULL
    }

    persistence <- c(alpha, beta, gamma)

    # FIT

    if (!is.null(xreg_matrix)){
        fit_ets <- smooth::es(outcome,
                              model = model_ets,
                              persistence = persistence,
                              phi = damping,
                              xreg = xreg_matrix,
                              ...)
    } else {
        fit_ets <- smooth::es(outcome,
                              model = model_ets,
                              persistence = persistence,
                              phi = damping,
                              ...)
    }

    # RETURN
    new_modeltime_bridge(
        class = "smooth_fit_impl",

        # Models
        models = list(
            model_1 = fit_ets
        ),

        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            !! idx_col  := idx,
            .actual      =  greybox::actuals(fit_ets) %>% as.numeric(),
            .fitted      =  as.numeric(fit_ets$fitted),
            .residuals   =  as.numeric(fit_ets$residuals)
        ),

        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(xreg_matrix = xreg_matrix,
                      xreg_recipe = xreg_recipe),

        # Description
        desc = fit_ets$model

    )

}

#' @export
print.smooth_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}


#' @export
predict.smooth_fit_impl <- function(object, new_data, ...) {
    smooth_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for Exponential Smoothing models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `smooth::es()`
#'
#' @keywords internal
#' @export
smooth_predict_impl <- function(object, new_data, ...) {

    # PREPARE INPUTS
    model       <- object$models$model_1
    idx_train   <- object$data %>% timetk::tk_index()
    h_horizon   <- nrow(new_data)
    xreg_recipe <- object$models$extras$xreg_recipe

    #PREDICTIONS

    if (is.null(xreg_recipe)){
        preds <- as.numeric(greybox::forecast(model, h = h_horizon, ...)$mean)
    } else {
        xreg_matrix <- bake_xreg_recipe(xreg_recipe, new_data, format = "matrix")
        preds <- as.numeric(greybox::forecast(model, h = h_horizon, newdata = xreg_matrix, ...)$mean)
    }

    return(preds)

}

# CROSTON ----

#' Low-Level Exponential Smoothing function for translating modeltime to forecast
#'
#' @inheritParams exp_smoothing
#' @inheritParams forecast::croston
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param ... Additional arguments passed to `forecast::ets`
#'
#' @keywords internal
#' @export
croston_fit_impl <- function(x, y, alpha = 0.1, ...){

    others <- list(...)

    outcome    <- y # Comes in as a vector
    predictors <- x # Comes in as a data.frame (dates and possible xregs)

    fit_croston <- forecast::croston(y = outcome, h = length(outcome), alpha = alpha, ...)

    # 2. Predictors - Handle Dates
    index_tbl <- modeltime::parse_index_from_data(predictors)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)

    modeltime::new_modeltime_bridge(

        class  = "croston_fit_impl",

        models = list(model_1 = fit_croston),

        data   = tibble::tibble(
            !! idx_col := idx,
            .actual    = as.numeric(fit_croston$x),
            .fitted    = as.numeric(fit_croston$fitted),
            .residuals = as.numeric(fit_croston$residuals)
        ),

        extras = list(
            outcome = outcome,
            alpha   = alpha,
            others  = others
        ), # Can add xreg preprocessors here
        desc   = stringr::str_c("Croston Method")
    )
}

#' @export
print.croston_fit_impl <- function(x, ...) {
    if (!is.null(x$desc)) cat(paste0(x$desc,"\n"))
    cat("---\n")
    invisible(x)
}

#' @export
predict.croston_fit_impl <- function(object, new_data, ...) {
    croston_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for CROSTON models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `stats::predict()`
#'
#' @keywords internal
#' @export
croston_predict_impl <- function(object, new_data, ...) {
    # PREPARE INPUTS
    model         <- object$models$model_1
    outcome       <- object$extras$outcome
    alpha         <- object$extras$alpha
    others        <- object$extras$others

    h_horizon      <- nrow(new_data)

    preds <- tryCatch({

        pred_forecast <- forecast::forecast(model, h = h_horizon)

        as.numeric(pred_forecast$mean)

    }, error = function(e) {
        fit <- forecast::croston(y = outcome, h = h_horizon, alpha = alpha)

        as.numeric(fit$mean)

    })

    return(preds)
}


# THETA ----

#' Low-Level Exponential Smoothing function for translating modeltime to forecast
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param ... Additional arguments passed to `forecast::ets`
#'
#' @keywords internal
#' @export
theta_fit_impl <- function(x, y, ...){

    others <- list(...)

    outcome    <- y # Comes in as a vector
    predictors <- x # Comes in as a data.frame (dates and possible xregs)

    fit_theta <- forecast::thetaf(y = outcome, h = length(outcome), ...)

    # 2. Predictors - Handle Dates
    index_tbl <- modeltime::parse_index_from_data(predictors)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)

    modeltime::new_modeltime_bridge(

        class  = "theta_fit_impl",

        models = list(model_1 = fit_theta),

        data   = tibble::tibble(
            !! idx_col := idx,
            .actual    = as.numeric(fit_theta$x),
            .fitted    = as.numeric(fit_theta$fitted),
            .residuals = as.numeric(fit_theta$residuals)
        ),

        extras = list(
            outcome = outcome,
            others  = others
        ), # Can add xreg preprocessors here
        desc   = stringr::str_c("Theta Method")
    )
}

#' @export
print.theta_fit_impl <- function(x, ...) {
    if (!is.null(x$desc)) cat(paste0(x$desc,"\n"))
    cat("---\n")
    # print(tibble::as_tibble(x$models$model_1))
    invisible(x)
}

#' @export
predict.theta_fit_impl <- function(object, new_data, ...) {
    theta_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for THETA models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `stats::predict()`
#'
#' @keywords internal
#' @export
theta_predict_impl <- function(object, new_data, ...) {
    # PREPARE INPUTS
    model         <- object$models$model_1
    outcome       <- object$extras$outcome
    others        <- object$extras$others

    h_horizon      <- nrow(new_data)

    preds <- tryCatch({

        pred_forecast <- forecast::forecast(model, h = h_horizon)

        as.numeric(pred_forecast$mean)

    }, error = function(e) {
        fit <- forecast::thetaf(y = outcome, h = h_horizon)

        as.numeric(fit$mean)

    })

    return(preds)
}




