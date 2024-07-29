# PROPHET REG ----

#' General Interface for PROPHET Time Series Models
#'
#' `prophet_reg()` is a way to generate a _specification_ of a PROPHET model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `prophet`.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param growth String 'linear' or 'logistic' to specify a linear or logistic trend.
#' @param changepoint_num Number of potential changepoints to include for modeling trend.
#' @param changepoint_range Adjusts the flexibility of the trend component by limiting to a percentage of data
#'  before the end of the time series. 0.80 means that a changepoint cannot exist after the first 80% of the data.
#' @param seasonality_yearly One of "auto", TRUE or FALSE. Toggles on/off a seasonal component that
#'  models year-over-year seasonality.
#' @param seasonality_weekly One of "auto", TRUE or FALSE. Toggles on/off a seasonal component that
#'  models week-over-week seasonality.
#' @param seasonality_daily One of "auto", TRUE or FALSE. Toggles on/off a seasonal componet that
#'  models day-over-day seasonality.
#' @param season 'additive' (default) or 'multiplicative'.
#' @param prior_scale_changepoints Parameter modulating the flexibility of the
#'  automatic changepoint selection. Large values will allow many changepoints,
#'  small values will allow few changepoints.
#' @param prior_scale_seasonality Parameter modulating the strength of the
#'  seasonality model. Larger values allow the model to fit larger seasonal
#'  fluctuations, smaller values dampen the seasonality.
#' @param prior_scale_holidays Parameter modulating the strength of the holiday components model,
#'  unless overridden in the holidays input.
#' @param logistic_cap When growth is logistic, the upper-bound for "saturation".
#' @param logistic_floor When growth is logistic, the lower-bound for "saturation".
#'
#'
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `prophet_reg()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "prophet" (default) - Connects to [prophet::prophet()]
#'
#' __Main Arguments__
#'
#' The main arguments (tuning parameters) for the model are:
#'
#' - `growth`: String 'linear' or 'logistic' to specify a linear or logistic trend.
#' - `changepoint_num`: Number of potential changepoints to include for modeling trend.
#' - `changepoint_range`: Range changepoints that adjusts how close to the end
#'    the last changepoint can be located.
#' - `season`: 'additive' (default) or 'multiplicative'.
#' - `prior_scale_changepoints`: Parameter modulating the flexibility of the
#'   automatic changepoint selection. Large values will allow many changepoints,
#'   small values will allow few changepoints.
#' - `prior_scale_seasonality`: Parameter modulating the strength of the
#'  seasonality model. Larger values allow the model to fit larger seasonal
#'  fluctuations, smaller values dampen the seasonality.
#' - `prior_scale_holidays`: Parameter modulating the strength of the holiday components model,
#'  unless overridden in the holidays input.
#' - `logistic_cap`: When growth is logistic, the upper-bound for "saturation".
#' - `logistic_floor`: When growth is logistic, the lower-bound for "saturation".
#'
#' These arguments are converted to their specific names at the
#'  time that the model is fit.
#'
#' Other options and argument can be
#'  set using `set_engine()` (See Engine Details below).
#'
#' If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#'
#' @section Engine Details:
#'
#' The standardized parameter names in `modeltime` can be mapped to their original
#' names in each engine:
#'
#' ```{r echo = FALSE}
#' tibble::tribble(
#'     ~ "modeltime", ~ "prophet",
#'     "growth", "growth ('linear')",
#'     "changepoint_num", "n.changepoints (25)",
#'     "changepoint_range", "changepoints.range (0.8)",
#'     "seasonality_yearly", "yearly.seasonality ('auto')",
#'     "seasonality_weekly", "weekly.seasonality ('auto')",
#'     "seasonality_daily", "daily.seasonality ('auto')",
#'     "season", "seasonality.mode ('additive')",
#'     "prior_scale_changepoints", "changepoint.prior.scale (0.05)",
#'     "prior_scale_seasonality", "seasonality.prior.scale (10)",
#'     "prior_scale_holidays", "holidays.prior.scale (10)",
#'     "logistic_cap", "df$cap (NULL)",
#'     "logistic_floor", "df$floor (NULL)"
#' ) %>% knitr::kable()
#' ```
#'
#' Other options can be set using `set_engine()`.
#'
#'
#' __prophet__
#'
#' The engine uses [prophet::prophet()].
#'
#' Function Parameters:
#' ```{r echo = FALSE}
#' str(prophet::prophet)
#' ```
#'
#' Parameter Notes:
#' - `df`: This is supplied via the parsnip / modeltime `fit()` interface
#'  (so don't provide this manually). See Fit Details (below).
#' - `holidays`: A data.frame of holidays can be supplied via `set_engine()`
#' - `uncertainty.samples`: The default is set to 0 because the prophet
#'  uncertainty intervals are not used as part of the Modeltime Workflow.
#'  You can override this setting if you plan to use prophet's uncertainty tools.
#'
#' Regressors:
#' - Regressors are provided via the `fit()` or `recipes` interface, which passes
#'   regressors to `prophet::add_regressor()`
#' - Parameters can be controlled in `set_engine()` via: `regressors.prior.scale`, `regressors.standardize`,
#'   and `regressors.mode`
#' - The regressor prior scale implementation default is `regressors.prior.scale = 1e4`, which deviates from
#'   the `prophet` implementation (defaults to holidays.prior.scale)
#'
#' Logistic Growth and Saturation Levels:
#' - For `growth = "logistic"`, simply add numeric values for `logistic_cap` and / or
#'   `logistic_floor`. There is _no need_ to add additional columns
#'   for "cap" and "floor" to your data frame.
#'
#' Limitations:
#' - `prophet::add_seasonality()` is not currently implemented. It's used to
#'  specify non-standard seasonalities using fourier series. An alternative is to use
#'  `step_fourier()` and supply custom seasonalities as Extra Regressors.
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
#'
#' __Univariate (No Extra Regressors):__
#'
#' For univariate analysis, you must include a date or date-time feature. Simply use:
#'
#'  - Formula Interface (recommended): `fit(y ~ date)` will ignore xreg's.
#'  - XY Interface: `fit_xy(x = data[,"date"], y = data$y)` will ignore xreg's.
#'
#' __Multivariate (Extra Regressors)__
#'
#'  Extra Regressors parameter is populated using the `fit()` or `fit_xy()` function:
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
#' # ---- PROPHET ----
#'
#' # Model Spec
#' model_spec <- prophet_reg() %>%
#'     set_engine("prophet")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#' model_fit
#'
#'
#' @export
prophet_reg <- function(mode = "regression",
                        growth = NULL, changepoint_num = NULL, changepoint_range = NULL,
                        seasonality_yearly = NULL, seasonality_weekly = NULL, seasonality_daily = NULL, season = NULL,
                        prior_scale_changepoints = NULL, prior_scale_seasonality = NULL, prior_scale_holidays = NULL,
                        logistic_cap = NULL, logistic_floor = NULL
                        ) {

    args <- list(
        # Prophet
        growth                    = rlang::enquo(growth),
        changepoint_num           = rlang::enquo(changepoint_num),
        changepoint_range         = rlang::enquo(changepoint_range),
        seasonality_yearly        = rlang::enquo(seasonality_yearly),
        seasonality_weekly        = rlang::enquo(seasonality_weekly),
        seasonality_daily         = rlang::enquo(seasonality_daily),
        season                    = rlang::enquo(season),
        prior_scale_changepoints  = rlang::enquo(prior_scale_changepoints),
        prior_scale_seasonality   = rlang::enquo(prior_scale_seasonality),
        prior_scale_holidays      = rlang::enquo(prior_scale_holidays),
        logistic_cap              = rlang::enquo(logistic_cap),
        logistic_floor            = rlang::enquo(logistic_floor)
    )

    parsnip::new_model_spec(
        "prophet_reg",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )

}

#' @export
print.prophet_reg <- function(x, ...) {
    cat("PROPHET Regression Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)

    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }

    invisible(x)
}

#' @export
#' @importFrom stats update
update.prophet_reg <- function(object, parameters = NULL,
                               growth = NULL, changepoint_num = NULL, changepoint_range = NULL,
                               seasonality_yearly = NULL, seasonality_weekly = NULL, seasonality_daily = NULL, season = NULL,
                               prior_scale_changepoints = NULL, prior_scale_seasonality = NULL, prior_scale_holidays = NULL,
                               logistic_cap = NULL, logistic_floor = NULL,
                               fresh = FALSE, ...) {

    eng_args <- parsnip::update_engine_parameters(object$eng_args, fresh, ...)

    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(
        # Prophet
        growth                    = rlang::enquo(growth),
        changepoint_num           = rlang::enquo(changepoint_num),
        changepoint_range         = rlang::enquo(changepoint_range),
        seasonality_yearly        = rlang::enquo(seasonality_yearly),
        seasonality_weekly        = rlang::enquo(seasonality_weekly),
        seasonality_daily         = rlang::enquo(seasonality_daily),
        season                    = rlang::enquo(season),
        prior_scale_changepoints  = rlang::enquo(prior_scale_changepoints),
        prior_scale_seasonality   = rlang::enquo(prior_scale_seasonality),
        prior_scale_holidays      = rlang::enquo(prior_scale_holidays),
        logistic_cap              = rlang::enquo(logistic_cap),
        logistic_floor            = rlang::enquo(logistic_floor)
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
        "prophet_reg",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.prophet_reg <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'prophet'` for translation.")
        engine <- "prophet"
    }
    x <- parsnip::translate.default(x, engine, ...)

    x
}


# FIT - prophet -----

#' Low-Level PROPHET function for translating modeltime to PROPHET
#'
#' @inheritParams prophet::prophet
#' @inheritParams prophet_reg
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param regressors.prior.scale Float scale for the normal prior.
#'  Default is 10,000.
#'  Gets passed to `prophet::add_regressor(prior.scale)`
#' @param regressors.standardize Bool, specify whether this regressor will be
#'  standardized prior to fitting.
#'  Can be 'auto' (standardize if not binary), True, or False.
#'  Gets passed to `prophet::add_regressor(standardize)`.
#' @param regressors.mode Optional, 'additive' or 'multiplicative'.
#'  Defaults to `seasonality.mode`.
#' @param ... Additional arguments passed to `prophet::prophet`
#'
#' @keywords internal
#' @export
prophet_fit_impl <- function(x, y,
                             growth = "linear",
                             n.changepoints = 25,
                             changepoint.range = 0.8,
                             yearly.seasonality = "auto",
                             weekly.seasonality = "auto",
                             daily.seasonality  = "auto",
                             seasonality.mode = "additive",
                             changepoint.prior.scale = 0.05,
                             seasonality.prior.scale = 10,
                             holidays.prior.scale = 10,
                             regressors.prior.scale = 1e4,
                             regressors.standardize = "auto",
                             regressors.mode = NULL,
                             logistic_cap = NULL,
                             logistic_floor = NULL,
                             ...) {

    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome    <- y
    predictor  <- x

    growth <- tolower(growth)

    if (!growth[1] %in% c("linear", "logistic")) {
        message("growth must be 'linear' or 'logistic'. Defaulting to 'linear'.")
        growth <- 'linear'
    }

    if (!seasonality.mode[1] %in% c("additive", "multiplicative")) {
        message("seasonality.mode must be 'additive' or 'multiplicative'. Defaulting to 'additive'.")
        seasonality.mode <- 'additive'
    }

    if (growth == "logistic") {
        if (all(c(is.null(logistic_cap), is.null(logistic_floor)))) {
            cli::cli_abort("Capacities must be supplied for `growth = 'logistic'`. Try specifying at least one of 'logistic_cap' or 'logistic_floor'")
        }
    }

    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- parse_index_from_data(predictor)
    # period    <- parse_period_from_index(index_tbl, period)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)

    # XREGS
    # Clean names, get xreg recipe, process predictors
    xreg_recipe <- create_xreg_recipe(predictor, prepare = TRUE)
    xreg_tbl    <- juice_xreg_recipe(xreg_recipe, format = "tbl")

    # FIT

    # Construct Data Frame
    df <- tibble::tibble(
        y  = outcome,
        ds = idx
    ) %>%
        dplyr::bind_cols(xreg_tbl)

    # Construct model
    m <- prophet::prophet(
        growth = growth,
        n.changepoints = n.changepoints,
        changepoint.range = changepoint.range,
        yearly.seasonality = yearly.seasonality,
        weekly.seasonality = weekly.seasonality,
        daily.seasonality = daily.seasonality,
        seasonality.mode = seasonality.mode,
        changepoint.prior.scale = changepoint.prior.scale,
        seasonality.prior.scale = seasonality.prior.scale,
        holidays.prior.scale = holidays.prior.scale,
        ...
    )

    # Add regressors
    xreg_nms <- names(xreg_tbl)
    if (length(xreg_nms) > 0) {
        for (nm in xreg_nms) {
            m <- prophet::add_regressor(m, name = nm,
                                        prior.scale = regressors.prior.scale,
                                        standardize = regressors.standardize,
                                        mode        = regressors.mode
                                        )
        }
    }

    # Add seasonalities ??
    # TODO

    # Add logistic cap / floor
    if (growth == "logistic") {
        df$cap   <- logistic_cap
        df$floor <- logistic_floor
    }

    # Fit model
    m_fit <- prophet::fit.prophet(m, df)

    # In-sample Predictions
    fitted <- stats::predict(m_fit, df) %>% dplyr::pull(yhat)

    # Description
    desc <- "PROPHET"
    if (length(xreg_nms) > 0) {
        desc <- stringr::str_c(desc, " w/ Regressors")
    }

    # RETURN
    new_modeltime_bridge(
        class = "prophet_fit_impl",

        # Models
        models = list(
            model_1 = m_fit
        ),

        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            !! idx_col  := idx,
            .actual      =  as.numeric(outcome),
            .fitted      =  fitted,
        ) %>%
            dplyr::mutate(
                .residuals = .actual - .fitted
            ),

        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(
            xreg_recipe = xreg_recipe,
            logistic_params = list(
                growth         = growth,
                logistic_cap   = logistic_cap,
                logistic_floor = logistic_floor
            )
        ),

        # Description - Convert arima model parameters to short description
        desc = desc
    )

}

#' @export
print.prophet_fit_impl <- function(x, ...) {

    n_xregs <- length(x$models$model_1$extra_regressors)

    prophet_model <- x$models$model_1

    logistic_params <- x$extras$logistic_params
    if (is.null(logistic_params$logistic_cap)) {
        cap <- "NULL"
    } else {
        cap <- logistic_params$logistic_cap
    }
    if (is.null(logistic_params$logistic_floor)) {
        floor <- "NULL"
    } else {
        floor <- logistic_params$logistic_floor
    }

    msg <- stringr::str_glue(
        "{x$desc} Model
         - growth: '{prophet_model$growth}'
         - n.changepoints: {prophet_model$n.changepoints}
         - changepoint.range: {prophet_model$changepoint.range}
         - yearly.seasonality: '{prophet_model$yearly.seasonality}'
         - weekly.seasonality: '{prophet_model$weekly.seasonality}'
         - daily.seasonality: '{prophet_model$daily.seasonality}'
         - seasonality.mode: '{prophet_model$seasonality.mode}'
         - changepoint.prior.scale: {prophet_model$changepoint.prior.scale}
         - seasonality.prior.scale: {prophet_model$seasonality.prior.scale}
         - holidays.prior.scale: {prophet_model$holidays.prior.scale}
         - logistic_cap: {cap}
         - logistic_floor: {floor}
         - extra_regressors: {n_xregs}
         ")

    print(msg)
    invisible(x)
}


# PREDICT ----

#' @export
predict.prophet_fit_impl <- function(object, new_data, ...) {
    prophet_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for PROPHET models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `prophet::predict()`
#'
#' @keywords internal
#' @export
prophet_predict_impl <- function(object, new_data, ...) {

    # PREPARE INPUTS
    model           <- object$models$model_1
    idx_future      <- new_data %>% timetk::tk_index()
    xreg_recipe     <- object$extras$xreg_recipe
    logistic_params <- object$extras$logistic_params

    # XREG
    n_xregs  <- length(object$models$model_1$extra_regressors)
    if (n_xregs > 0) {
        xreg_tbl <- bake_xreg_recipe(xreg_recipe, new_data, format = "tbl")
    }

    # Construct Future Frame
    df <- tibble::tibble(
        ds = idx_future
    )
    if (n_xregs > 0) {
        df <- df %>%
            dplyr::bind_cols(xreg_tbl)
    }
    if (logistic_params$growth == "logistic") {
        df$cap   <- logistic_params$logistic_cap
        df$floor <- logistic_params$logistic_floor
    }


    # PREDICTIONS
    preds_forecast <- stats::predict(model, df)

    # Return predictions as numeric vector
    preds <- preds_forecast %>% dplyr::pull(yhat)

    return(preds)

}
