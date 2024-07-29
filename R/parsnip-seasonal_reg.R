#' General Interface for Multiple Seasonality Regression Models (TBATS, STLM)
#'
#' `seasonal_reg()` is a way to generate a _specification_ of an
#'  Seasonal Decomposition model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `forecast`.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param seasonal_period_1 (required) The primary seasonal frequency.
#'  Uses `"auto"` by default.
#'  A character phrase of "auto" or time-based phrase of "2 weeks"
#'  can be used if a date or date-time variable is provided.
#'  See Fit Details below.
#' @param seasonal_period_2 (optional) A second seasonal frequency.
#'  Is `NULL` by default.
#'  A character phrase of "auto" or time-based phrase of "2 weeks"
#'  can be used if a date or date-time variable is provided.
#'  See Fit Details below.
#' @param seasonal_period_3 (optional) A third seasonal frequency.
#'  Is `NULL` by default.
#'  A character phrase of "auto" or time-based phrase of "2 weeks"
#'  can be used if a date or date-time variable is provided.
#'  See Fit Details below.
#'
#'
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `seasonal_reg()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "tbats" - Connects to `forecast::tbats()`
#'  - "stlm_ets" - Connects to `forecast::stlm()`, `method = "ets"`
#'  - "stlm_arima" - Connects to `forecast::stlm()`, `method = "arima"`
#'
#'
#' @section Engine Details:
#'
#' The standardized parameter names in `modeltime` can be mapped to their original
#' names in each engine:
#'
#' ```{r echo = FALSE}
#' # parsnip::convert_args("seasonal_reg")
#' tibble::tribble(
#'     ~ "modeltime", ~ "forecast::stlm", ~ "forecast::tbats",
#'     "seasonal_period_1, seasonal_period_2, seasonal_period_3", "msts(seasonal.periods)", "msts(seasonal.periods)"
#' ) %>% knitr::kable()
#' ```
#'
#' Other options can be set using `set_engine()`.
#'
#' The engines use `forecast::stlm()`.
#'
#' Function Parameters:
#' ```{r echo = FALSE}
#' str(forecast::stlm)
#' ```
#'
#' __tbats__
#'
#'  - __Method:__ Uses `method = "tbats"`, which by default is auto-TBATS.
#'  - __Xregs:__ Univariate. Cannot accept Exogenous Regressors (xregs). Xregs are ignored.
#'
#' __stlm_ets__
#'
#'  - __Method:__ Uses `method = "stlm_ets"`, which by default is auto-ETS.
#'  - __Xregs:__ Univariate. Cannot accept Exogenous Regressors (xregs). Xregs are ignored.
#'
#'
#' __stlm_arima__
#'
#'  - __Method:__ Uses `method = "stlm_arima"`, which by default is auto-ARIMA.
#'  - __Xregs:__ Multivariate. Can accept Exogenous Regressors (xregs).
#'
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
#' _Seasonal Period Specification_
#'
#' The period can be non-seasonal (`seasonal_period = 1 or "none"`) or
#' yearly seasonal (e.g. For monthly time stamps, `seasonal_period = 12`, `seasonal_period = "12 months"`, or `seasonal_period = "yearly"`).
#' There are 3 ways to specify:
#'
#' 1. `seasonal_period = "auto"`: A seasonal period is selected based on the periodicity of the data (e.g. 12 if monthly)
#' 2. `seasonal_period = 12`: A numeric frequency. For example, 12 is common for monthly data
#' 3. `seasonal_period = "1 year"`: A time-based phrase. For example, "1 year" would convert to 12 for monthly data.
#'
#'
#' __Univariate (No xregs, Exogenous Regressors):__
#'
#' For univariate analysis, you must include a date or date-time feature. Simply use:
#'
#'  - Formula Interface (recommended): `fit(y ~ date)` will ignore xreg's.
#'  - XY Interface: `fit_xy(x = data[,"date"], y = data$y)` will ignore xreg's.
#'
#' __Multivariate (xregs, Exogenous Regressors)__
#'
#'  - The `tbats` engine _cannot_ accept Xregs.
#'  - The `stlm_ets` engine _cannot_ accept Xregs.
#'  - The `stlm_arima` engine _can_ accept Xregs
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
#'  The `month.lbl` is an exogenous regressor that can be passed to the `seasonal_reg()` using
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
#' taylor_30_min
#'
#' # Split Data 80/20
#' splits <- initial_time_split(taylor_30_min, prop = 0.8)
#'
#' # ---- STLM ETS ----
#'
#' # Model Spec
#' model_spec <- seasonal_reg() %>%
#'     set_engine("stlm_ets")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#' model_fit
#'
#'
#' # ---- STLM ARIMA ----
#'
#' # Model Spec
#' model_spec <- seasonal_reg() %>%
#'     set_engine("stlm_arima")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#' model_fit
#'
#' @export
seasonal_reg <- function(mode = "regression",
                            seasonal_period_1 = NULL, seasonal_period_2 = NULL, seasonal_period_3 = NULL) {

    args <- list(
        seasonal_period_1           = rlang::enquo(seasonal_period_1),
        seasonal_period_2           = rlang::enquo(seasonal_period_2),
        seasonal_period_3           = rlang::enquo(seasonal_period_3)
    )

    parsnip::new_model_spec(
        "seasonal_reg",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )

}

#' @export
print.seasonal_reg <- function(x, ...) {
    cat("Seasonal Regression Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)

    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }

    invisible(x)
}

#' @export
#' @importFrom stats update
update.seasonal_reg <- function(object, parameters = NULL,
                                seasonal_period_1 = NULL, seasonal_period_2 = NULL, seasonal_period_3 = NULL,
                                fresh = FALSE, ...) {

    eng_args <- parsnip::update_engine_parameters(object$eng_args, fresh, ...)

    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(
        seasonal_period_1           = rlang::enquo(seasonal_period_1),
        seasonal_period_2           = rlang::enquo(seasonal_period_2),
        seasonal_period_3           = rlang::enquo(seasonal_period_3)
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
        "seasonal_reg",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.seasonal_reg <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'tbats'` for translation.")
        engine <- "tbats"
    }
    x <- parsnip::translate.default(x, engine, ...)

    x
}

# FIT - TBATS -----

#' Low-Level tbats function for translating modeltime to forecast
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param period_1 (required) First seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param period_2 (optional) First seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param period_3 (optional) First seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param use.parallel `TRUE/FALSE` indicates whether or not to use parallel processing.
#' @param ... Additional arguments passed to `forecast::tbats()`
#'
#' @keywords internal
#' @export
tbats_fit_impl <- function(x, y, period_1 = "auto", period_2 = NULL, period_3 = NULL, use.parallel = length(y) > 1000, ...) {

    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome    <- y
    predictor  <- x

    if (is.null(period_1) || period_1 == "none" || period_1 <=1) {
        cli::cli_abort("The 'seasonal_period_1' must be greater than 1 (i.e. have seasonality). Try increasing the seasonality.")
    }

    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- parse_index_from_data(predictor)

    period_1  <- parse_period_from_index(index_tbl, period_1)
    if (!is.null(period_2)) period_2 <- parse_period_from_index(index_tbl, period_2)
    if (!is.null(period_3)) period_3 <- parse_period_from_index(index_tbl, period_3)

    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)

    # XREGS - NOT USED FOR TBATS METHOD
    # Clean names, get xreg recipe, process predictors
    # xreg_recipe <- create_xreg_recipe(predictor, prepare = TRUE)
    # xreg_matrix <- juice_xreg_recipe(xreg_recipe, format = "matrix")
    if (ncol(predictor) > 1) {
        message("External regressors (xregs) detected. TBATS is a univariate method. Ignoring xregs.")
    }

    # FIT
    outcome   <- forecast::msts(outcome,
                                seasonal.periods = c(period_1, period_2, period_3))
    fit_tbats <- forecast::tbats(y = outcome, use.parallel = use.parallel, ...)

    # RETURN
    new_modeltime_bridge(
        class = "tbats_fit_impl",

        # Models
        models = list(
            model_1 = fit_tbats
        ),

        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            !! idx_col  := idx,
            .actual      =  as.numeric(y),
            .fitted      =  as.numeric(fit_tbats$fitted.values),
            .residuals   =  as.numeric(y) - as.numeric(fit_tbats$fitted.values)
        ),

        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(),

        # Description - Convert tbats model parameters to short description
        desc = get_tbats_description(fit_tbats)
    )

}

#' @export
print.tbats_fit_impl <- function(x, ...) {
    model <- x$models$model_1
    print(model)
    invisible(x)
}




# PREDICT - TBATS ----
# - auto.arima produces an Arima model

#' @export
predict.tbats_fit_impl <- function(object, new_data, ...) {
    tbats_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for ARIMA models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `forecast::forecast()`
#'
#' @keywords internal
#' @export
tbats_predict_impl <- function(object, new_data, ...) {

    # PREPARE INPUTS
    model       <- object$models$model_1
    h_horizon   <- nrow(new_data)

    # XREG
    # NOT REQUIRED FOR ETS.
    # xreg_recipe <- object$extras$xreg_recipe
    # xreg_matrix <- bake_xreg_recipe(xreg_recipe, new_data, format = "matrix")

    # PREDICTIONS
    preds_forecast <- forecast::forecast(model, h = h_horizon, ...)

    # Return predictions as numeric vector
    preds <- as.numeric(preds_forecast$mean)

    return(preds)

}


# FIT - STLM ETS -----

#' Low-Level stlm function for translating modeltime to forecast
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param period_1 (required) First seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param period_2 (optional) First seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param period_3 (optional) First seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param ... Additional arguments passed to `forecast::stlm()`
#'
#' @keywords internal
#' @export
stlm_ets_fit_impl <- function(x, y, period_1 = "auto", period_2 = NULL, period_3 = NULL, ...) {

    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome    <- y
    predictor  <- x

    if (is.null(period_1) || period_1 == "none" || period_1 <=1) {
        cli::cli_abort("The 'seasonal_period_1' must be greater than 1 (i.e. have seasonality). Try increasing the seasonality.")
    }

    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- parse_index_from_data(predictor)

    period_1  <- parse_period_from_index(index_tbl, period_1)
    if (!is.null(period_2)) period_2 <- parse_period_from_index(index_tbl, period_2)
    if (!is.null(period_3)) period_3 <- parse_period_from_index(index_tbl, period_3)

    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)

    # XREGS - NOT USED FOR ETS METHOD
    # Clean names, get xreg recipe, process predictors
    # xreg_recipe <- create_xreg_recipe(predictor, prepare = TRUE)
    # xreg_matrix <- juice_xreg_recipe(xreg_recipe, format = "matrix")
    if (ncol(predictor) > 1) {
        message("External regressors (xregs) detected. STLM + ETS is a univariate method. Ignoring xregs.")
    }

    # FIT
    outcome  <- forecast::msts(outcome, seasonal.periods = c(period_1, period_2, period_3))
    fit_stlm <- forecast::stlm(y = outcome, method = "ets", ...)

    # RETURN
    new_modeltime_bridge(
        class = "stlm_ets_fit_impl",

        # Models
        models = list(
            model_1 = fit_stlm
        ),

        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            !! idx_col  := idx,
            .actual      =  as.numeric(fit_stlm$x),
            .fitted      =  as.numeric(fit_stlm$fitted),
            .residuals   =  as.numeric(fit_stlm$residuals)
        ),

        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(),

        # Description - Convert arima model parameters to short description
        desc = stringr::str_c("SEASONAL DECOMP: ", fit_stlm$model$method)
    )

}

#' @export
print.stlm_ets_fit_impl <- function(x, ...) {
    model <- x$models$model_1$model

    cat(x$desc)
    # cat("\n")
    # print(model$call)
    cat("\n\n")
    print(
        tibble::tibble(
            aic    = model$aic,
            bic    = model$bic,
            aicc   = model$aicc,
            loglik = model$loglik,
            mse    = model$mse
        )
    )
    invisible(x)
}




# PREDICT - STLM ETS ----
# - auto.arima produces an Arima model

#' @export
predict.stlm_ets_fit_impl <- function(object, new_data, ...) {
    stlm_ets_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for ARIMA models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `forecast::forecast()`
#'
#' @keywords internal
#' @export
stlm_ets_predict_impl <- function(object, new_data, ...) {

    # PREPARE INPUTS
    model       <- object$models$model_1
    h_horizon   <- nrow(new_data)

    # XREG
    # NOT REQUIRED FOR ETS.
    # xreg_recipe <- object$extras$xreg_recipe
    # xreg_matrix <- bake_xreg_recipe(xreg_recipe, new_data, format = "matrix")

    # PREDICTIONS
    preds_forecast <- forecast::forecast(model, h = h_horizon, ...)

    # Return predictions as numeric vector
    preds <- as.numeric(preds_forecast$mean)

    return(preds)

}


# FIT - STLM ARIMA -----

#' Low-Level stlm function for translating modeltime to forecast
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param period_1 (required) First seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param period_2 (optional) First seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param period_3 (optional) First seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param ... Additional arguments passed to `forecast::stlm()`
#'
#' @keywords internal
#' @export
stlm_arima_fit_impl <- function(x, y, period_1 = "auto", period_2 = NULL, period_3 = NULL, ...) {

    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome    <- y
    predictor  <- x

    if (is.null(period_1) || period_1 == "none" || period_1 <=1) {
        cli::cli_abort("The 'seasonal_period_1' must be greater than 1 (i.e. have seasonality). Try increasing the seasonality.")
    }

    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- parse_index_from_data(predictor)

    period_1  <- parse_period_from_index(index_tbl, period_1)
    if (!is.null(period_2)) period_2 <- parse_period_from_index(index_tbl, period_2)
    if (!is.null(period_3)) period_3 <- parse_period_from_index(index_tbl, period_3)

    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)

    # XREGS - NOT USED FOR ETS METHOD
    # Clean names, get xreg recipe, process predictors
    xreg_recipe <- create_xreg_recipe(predictor, prepare = TRUE, one_hot = FALSE)
    xreg_matrix <- juice_xreg_recipe(xreg_recipe, format = "matrix")

    # FIT
    outcome  <- forecast::msts(outcome, seasonal.periods = c(period_1, period_2, period_3))

    if (!is.null(xreg_matrix)) {
        fit_stlm   <- forecast::stlm(outcome, method = "arima", xreg = xreg_matrix, ...)
    } else {
        fit_stlm   <- forecast::stlm(outcome, method = "arima", ...)
    }

    # RETURN
    new_modeltime_bridge(
        class = "stlm_arima_fit_impl",

        # Models
        models = list(
            model_1 = fit_stlm
        ),

        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            !! idx_col  := idx,
            .actual      =  as.numeric(fit_stlm$x),
            .fitted      =  as.numeric(fit_stlm$fitted),
            .residuals   =  as.numeric(fit_stlm$residuals)
        ),

        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(
            xreg_recipe = xreg_recipe
        ),

        # Description - Convert arima model parameters to short description
        desc = stringr::str_c("SEASONAL DECOMP: ", get_arima_description(fit_stlm$model))
    )

}

#' @export
print.stlm_arima_fit_impl <- function(x, ...) {
    model <- x$models$model_1$model

    cat(x$desc)
    # cat("\n")
    # print(model$call)
    cat("\n\n")
    print(model)
    # print(
    #     tibble::tibble(
    #         aic    = model$aic,
    #         bic    = model$bic,
    #         aicc   = model$aicc,
    #         loglik = model$loglik,
    #         mse    = model$mse
    #     )
    # )
    invisible(x)
}




# PREDICT - STLM ARIMA ----
# - auto.arima produces an Arima model

#' @export
predict.stlm_arima_fit_impl <- function(object, new_data, ...) {
    stlm_arima_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for ARIMA models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `forecast::forecast()`
#'
#' @keywords internal
#' @export
stlm_arima_predict_impl <- function(object, new_data, ...) {

    # PREPARE INPUTS
    model       <- object$models$model_1
    h_horizon   <- nrow(new_data)

    # XREG
    # NOT REQUIRED FOR ETS.
    xreg_recipe <- object$extras$xreg_recipe
    xreg_matrix <- bake_xreg_recipe(xreg_recipe, new_data, format = "matrix")

    # PREDICTIONS
    if (!is.null(xreg_matrix)) {
        preds_forecast <- forecast::forecast(model, h = h_horizon, xreg = xreg_matrix, ...)
    } else {
        preds_forecast <- forecast::forecast(model, h = h_horizon, ...)
    }

    # Return predictions as numeric vector
    preds <- as.numeric(preds_forecast$mean)

    return(preds)

}







