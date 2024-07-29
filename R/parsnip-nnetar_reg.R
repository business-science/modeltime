# NNETAR ----

#' General Interface for NNETAR Regression Models
#'
#' `nnetar_reg()` is a way to generate a _specification_ of an NNETAR model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `forecast`.
#'
#' @inheritParams arima_reg
#' @inheritParams parsnip::mlp
#' @param num_networks Number of networks to fit with different random starting weights.
#'  These are then averaged when producing forecasts.
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
#' The main arguments (tuning parameters) for the model are the parameters in
#'  `nnetar_reg()` function. These arguments are converted to their specific names at the
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
#' # parsnip::convert_args("nnetar_reg")
#' tibble::tribble(
#'     ~ "modeltime", ~ "forecast::nnetar",
#'     "seasonal_period", "ts(frequency)",
#'     "non_seasonal_ar", "p (1)",
#'     "seasonal_ar", "P (1)",
#'     "hidden_units", "size (10)",
#'     "num_networks", "repeats (20)",
#'     "epochs", "maxit (100)",
#'     "penalty", "decay (0)"
#' ) %>% knitr::kable()
#' ```
#'
#' Other options can be set using `set_engine()`.
#'
#' __nnetar__
#'
#' The engine uses [forecast::nnetar()].
#'
#' Function Parameters:
#' ```{r echo = FALSE}
#' str(forecast::nnetar)
#' ```
#'
#' Parameter Notes:
#' - `xreg` - This is supplied via the parsnip / modeltime `fit()` interface
#'  (so don't provide this manually). See Fit Details (below).
#' - `size` - Is set to 10 by default. This differs from the `forecast` implementation
#' - `p` and `P` - Are set to 1 by default.
#' - `maxit` and `decay` are `nnet::nnet` parameters that are exposed in the `nnetar_reg()` interface.
#'   These are key tuning parameters.
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
#' # ---- NNETAR ----
#'
#' # Model Spec
#' model_spec <- nnetar_reg() %>%
#'     set_engine("nnetar")
#'
#' # Fit Spec
#' set.seed(123)
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

    eng_args <- parsnip::update_engine_parameters(object$eng_args, fresh, ...)

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
#' @param decay Parameter for weight decay. Default 0.
#' @param maxit Maximum number of iterations. Default 100.
#' @param ... Additional arguments passed to `forecast::nnetar`
#'
#' @keywords internal
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
#' @keywords internal
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










