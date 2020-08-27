#' General Interface for NNETAR Regression Models
#'
#' `nnetar_reg()` is a way to generate a _specification_ of an NNETAR model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `forecast`.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param seasonal_period A seasonal frequency. Uses "auto" by default.
#'  A character phrase of "auto" or time-based phrase of "2 weeks"
#'  can be used if a date or date-time variable is provided.
#'  See Fit Details below.
#' @param non_seasonal_ar The order of the non-seasonal auto-regressive (AR) terms. Often denoted "p" in pdq-notation.
#' @param non_seasonal_differences The order of integration for non-seasonal differencing. Often denoted "d" in pdq-notation.
#' @param non_seasonal_ma The order of the non-seasonal moving average (MA) terms. Often denoted "q" in pdq-notation.
#' @param seasonal_ar The order of the seasonal auto-regressive (SAR) terms. Often denoted "P" in PDQ-notation.
#' @param seasonal_differences The order of integration for seasonal differencing. Often denoted "D" in PDQ-notation.
#' @param seasonal_ma The order of the seasonal moving average (SMA) terms. Often denoted "Q" in PDQ-notation.
#'
#'
#'
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `nnetar_reg()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "nnetar" (default) - Connects to [forecast::nnetar()]
#'
#' __Main Arguments__
#'
#' The main arguments (tuning parameters) for the model are:
#'
#'  - `seasonal_period`: The periodic nature of the seasonality. Uses "auto" by default.
#'  - `non_seasonal_ar`: The order of the non-seasonal auto-regressive (AR) terms.
#'  - `non_seasonal_differences`: The order of integration for non-seasonal differencing.
#'  - `non_seasonal_ma`: The order of the non-seasonal moving average (MA) terms.
#'  - `seasonal_ar`: The order of the seasonal auto-regressive (SAR) terms.
#'  - `seasonal_differences`: The order of integration for seasonal differencing.
#'  - `seasonal_ma`: The order of the seasonal moving average (SMA) terms.
#'
#' These arguments are converted to their specific names at the
#'  time that the model is fit.
#'
#' Other options and argument can be
#'  set using `set_engine()` (See Engine Details below).
#'
#'  If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#'
#' @section Engine Details:
#'
#' The standardized parameter names in `modeltime` can be mapped to their original
#' names in each engine:
#'
#' ```{r echo = FALSE}
#' # parsnip::convert_args("nnetar_reg")
#' tibble::tribble(
#'     ~ "modeltime", ~ "forecast::auto.arima", ~ "forecast::Arima",
#'     "seasonal_period", "ts(frequency)", "ts(frequency)",
#'     "non_seasonal_ar, non_seasonal_differences, non_seasonal_ma", "max.p, max.d, max.q", "order = c(p,d,q)",
#'     "seasonal_ar, seasonal_differences, seasonal_ma", "max.P, max.D, max.Q", "seasonal = c(P,D,Q)"
#' ) %>% knitr::kable()
#' ```
#'
#' Other options can be set using `set_engine()`.
#'
#' __auto_arima (default engine)__
#'
#' The engine uses [forecast::auto.arima()].
#'
#' Function Parameters:
#' ```{r echo = FALSE}
#' str(forecast::auto.arima)
#' ```
#' The _MAXIMUM_ nonseasonal NNETAR terms (`max.p`, `max.d`, `max.q`) and
#' seasonal NNETAR terms (`max.P`, `max.D`, `max.Q`) are provided to
#' [forecast::auto.arima()] via `nnetar_reg()` parameters.
#' Other options and argument can be set using `set_engine()`.
#'
#' Parameter Notes:
#' - All values of nonseasonal pdq and seasonal PDQ are maximums.
#'  The `forecast::auto.arima()` model will select a value using these as an upper limit.
#' - `xreg` - This is supplied via the parsnip / modeltime `fit()` interface
#'  (so don't provide this manually). See Fit Details (below).
#'
#' __arima__
#'
#' The engine uses [forecast::Arima()].
#'
#' Function Parameters:
#' ```{r echo = FALSE}
#' str(forecast::nnetar)
#' ```
#'
#' The nonseasonal NNETAR terms (`order`) and seasonal NNETAR terms (`seasonal`)
#' are provided to [forecast::Arima()] via `nnetar_reg()` parameters.
#' Other options and argument can be set using `set_engine()`.
#'
#' Parameter Notes:
#' - `xreg` - This is supplied via the parsnip / modeltime `fit()` interface
#'  (so don't provide this manually). See Fit Details (below).
#' - `method` - The default is set to "ML" (Maximum Likelihood).
#'  This method is more robust at the expense of speed and possible
#'  selections may fail unit root inversion testing. Alternatively, you can add `method = "CSS-ML"` to
#'  evaluate Conditional Sum of Squares for starting values, then Maximium Likelihood.
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
#'  The `month.lbl` is an exogenous regressor that can be passed to the `nnetar_reg()` using
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
#' # ---- NNETAR ----
#'
#' # Model Spec
#' model_spec <- nnetar_reg() %>%
#'     set_engine("auto_arima")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#' model_fit
#'
#'
#'
#' @export
nnetar_reg <- function(mode = "regression", seasonal_period = NULL,
                       non_seasonal_ar = NULL, seasonal_ar = NULL,
                       hidden_units = NULL, num_networks = NULL,
                       penalty = NULL, epochs = NULL) {

    args <- list(
        seasonal_period           = rlang::enquo(seasonal_period),
        non_seasonal_ar           = rlang::enquo(non_seasonal_ar),
        seasonal_ar               = rlang::enquo(seasonal_ar),
        hidden_units              = rlang::enquo(hidden_units),
        num_networks              = rlang::enquo(num_networks),
        penalty                   = rlang::enquo(penalty),
        epochs                    = rlang::enquo(epochs)
    )

    parsnip::new_model_spec(
        "nnetar_reg",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )

}

#' @export
print.nnetar_reg <- function(x, ...) {
    cat("Neural Network Auto Regression (NNETAR) Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)

    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }

    invisible(x)
}

#' @export
#' @importFrom stats update
update.nnetar_reg <- function(object, parameters = NULL,
                             seasonal_period = NULL,
                             non_seasonal_ar = NULL, seasonal_ar = NULL,
                             hidden_units = NULL, num_networks = NULL,
                             penalty = NULL, epochs = NULL,
                             fresh = FALSE, ...) {

    parsnip::update_dot_check(...)

    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(
        seasonal_period           = rlang::enquo(seasonal_period),
        non_seasonal_ar           = rlang::enquo(non_seasonal_ar),
        seasonal_ar               = rlang::enquo(seasonal_ar),
        hidden_units              = rlang::enquo(hidden_units),
        num_networks              = rlang::enquo(num_networks),
        penalty                   = rlang::enquo(penalty),
        epochs                    = rlang::enquo(epochs)
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
        "nnetar_reg",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.nnetar_reg <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'nnetar'` for translation.")
        engine <- "nnetar"
    }
    x <- parsnip::translate.default(x, engine, ...)

    x
}


# FIT - NNETAR -----

#' Low-Level NNETAR function for translating modeltime to forecast
#'
#' @inheritParams forecast::nnetar
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param period A seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param ... Additional arguments passed to `forecast::nnetar`
#'
#' @export
nnetar_fit_impl <- function(x, y, period = "auto",
                            p = 1, P = 1, size = 10, repeats = 20,
                            decay = 0, maxit = 100,
                            ...) {

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

    # FIT
    outcome    <- stats::ts(outcome, frequency = period)
    fit_nnetar <- forecast::nnetar(y = outcome,
                                   # forecast args
                                   p = p, P = P, size = size, repeats = repeats, xreg = xreg_matrix,
                                   # nnet args
                                   decay = decay, maxit = maxit,
                                   ...)

    # RETURN
    new_modeltime_bridge(
        class = "nnetar_fit_impl",

        # Models
        models = list(
            model_1 = fit_nnetar
        ),

        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            !! idx_col  := idx,
            .actual      =  as.numeric(fit_nnetar$x),
            .fitted      =  as.numeric(fit_nnetar$fitted),
            .residuals   =  as.numeric(fit_nnetar$residuals)
        ),

        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(
            xreg_recipe = xreg_recipe
        ),

        # Description - Convert arima model parameters to short description
        desc = fit_nnetar$method
    )

}

#' @export
print.nnetar_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}


# PREDICT ----

#' @export
predict.nnetar_fit_impl <- function(object, new_data, ...) {
    nnetar_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for ARIMA models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `forecast::forecast()`
#'
#' @export
nnetar_predict_impl <- function(object, new_data, ...) {

    # PREPARE INPUTS
    model       <- object$models$model_1
    idx_train   <- object$data %>% timetk::tk_index()
    xreg_recipe <- object$extras$xreg_recipe
    h_horizon   <- nrow(new_data)

    # XREG
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










