#' General Interface for ARIMA Regression Models
#'
#' `arima_reg()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `forecast`.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param period A seasonal frequency. If none is present, use 1.
#'  A character phrase of "auto" or time-based phrase of "2 weeks"
#'  can be used if a date or date-time variable is provided.
#'  See Fit Details below.
#' @param p The order of the non-seasonal auto-regressive (AR) terms.
#' @param d The order of integration for non-seasonal differencing.
#' @param q The order of the non-seasonal moving average (MA) terms.
#' @param P The order of the seasonal auto-regressive (SAR) terms.
#' @param D The order of integration for seasonal differencing.
#' @param Q The order of the seasonal moving average (SMA) terms.
#'
#'
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `arima_reg()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - __R__: "forecast" (default)
#'
#' __Main Arguments__
#'
#' The main arguments (tuning parameters) for the model are:
#'
#'  - `period`: The periodic nature of the seasonality. If none is present, use 1.
#'  - `p`: The order of the non-seasonal auto-regressive (AR) terms.
#'  - `d`: The order of integration for non-seasonal differencing.
#'  - `q`: The order of the non-seasonal moving average (MA) terms.
#'  - `P`: The order of the seasonal auto-regressive (SAR) terms.
#'  - `D`: The order of integration for seasonal differencing.
#'  - `Q`: The order of the seasonal moving average (SMA) terms.
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
#' # parsnip::convert_args("arima_reg")
#' tibble::tribble(
#'     ~ modeltime, ~ forecast,
#'     "period", "ts(frequency)",
#'     "p, d, q", "order = c(p,d,q)",
#'     "P, D, Q", "seasonal = c(P,D,Q)"
#' ) %>% knitr::kable()
#' ```
#'
#' Other options can be set using `set_engine()`.
#'
#' __forecast__
#'
#' The order and seasonal terms are provided via `arima_reg()` parameters.
#' Other options and argument can be set using `set_engine()`.
#'
#' ```{r echo = FALSE}
#' # forecast::Arima() parameters
#' str(forecast::Arima)
#' ```
#' Parameter Notes:
#' - `xreg` - This is supplied via the parsnip / modeltime `fit()` interface
#'  (so don't provide this manually). See Fit Details (below).
#'
#'
#' @section Fit Details:
#'
#' __Date and Date-Time Variable__
#'
#' It's very common to work with date and date-time variables when working with time series.
#' The `fit()` interface accepts date and date-time features and handles them internally.
#'
#' _Period Specification_
#'
#' When `period = "auto" or "12 months"`, the `fit()` interface will require a date or date-time
#' feature. You can specify one using the format:
#'
#' - `fit(y ~ date)`
#'
#' When `period = 1 or 12`, it can be used as a tuning parameter.
#'  No date or date-time feature is required.
#'
#'
#' __Univariate (No xreg's):__
#'
#' For univariate analysis, simply use:
#'
#'  - `fit(y ~ 1)` will ignore xreg.
#'  - `fit_xy(x = NULL, y)` will ignore xreg.
#'
#'  Alternatively, you can have a date or date-time feature in the `fit()` interface, and this
#'  will still result in a univariate analysis. The additional benefit is using `period = "auto"`
#'  to automate the `period` assignment.
#'
#'  - `fit(y ~ date)` is a univariate analysis if date is a date or date time feature.
#'  - `fit_xy(x = dplyr::select(data, date), y)` is a univariate analysis if date is a date or date time feature.
#'
#' __xreg (Exogenous Regressors)__
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
#'  - `fit_xy(x, y)` will pass x, where x is a data frame containing `month.lbl`
#'   and possibly the `date` feature. Only `month.lbl` will be used as an exogenous regressor.
#'
#'  Note that date or date-time class values are excluded from `xreg`.
#'
#'
#'
#' @seealso [fit.arima_reg()], [set_engine()]
#'
#' @examples
#' # TODO
#'
#' @export
arima_reg <- function(mode = "regression", period = NULL,
                      order_ar = NULL, order_differences = NULL, order_ma = NULL,
                      order_seasonal_ar = NULL, order_seasonal_differences = NULL, order_seasonal_ma = NULL) {

    args <- list(
        period                     = rlang::enquo(period),
        order_ar                   = rlang::enquo(order_ar),
        order_differences          = rlang::enquo(order_differences),
        order_ma                   = rlang::enquo(order_ma),
        order_seasonal_ar          = rlang::enquo(order_seasonal_ar),
        order_seasonal_differences = rlang::enquo(order_seasonal_differences),
        order_seasonal_ma          = rlang::enquo(order_seasonal_ma)
    )

    parsnip::new_model_spec(
        "arima_reg",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )

}

#' @export
print.arima_reg <- function(x, ...) {
    cat("ARIMA Regression Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)

    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }

    invisible(x)
}

#' @export
#' @importFrom stats update
update.arima_reg <- function(object, parameters = NULL,
                             period = NULL, order_ar = NULL, order_differences = NULL, order_ma = NULL,
                             order_seasonal_ar = NULL, order_seasonal_differences = NULL, order_seasonal_ma = NULL,
                             fresh = FALSE, ...) {

    parsnip::update_dot_check(...)

    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(
        period                     = rlang::enquo(period),
        order_ar                   = rlang::enquo(order_ar),
        order_differences          = rlang::enquo(order_differences),
        order_ma                   = rlang::enquo(order_ma),
        order_seasonal_ar          = rlang::enquo(order_seasonal_ar),
        order_seasonal_differences = rlang::enquo(order_seasonal_differences),
        order_seasonal_ma          = rlang::enquo(order_seasonal_ma)
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
        "arima_reg",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.arima_reg <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'forecast'` for translation.")
        engine <- "forecast"
    }
    x <- parsnip::translate.default(x, engine, ...)

    x
}


