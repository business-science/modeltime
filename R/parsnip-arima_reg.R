#' General Interface for ARIMA Regression Models
#'
#' `arima_reg()` is a way to generate a _specification_ of an ARIMA model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `forecast`.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param period A seasonal frequency. If none is present, use 1.
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
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `arima_reg()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "forecast::auto.arima" (default)
#'  - "forecast::Arima"
#'
#' __Main Arguments__
#'
#' The main arguments (tuning parameters) for the model are:
#'
#'  - `period`: The periodic nature of the seasonality. If none is present, use 1.
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
#' # parsnip::convert_args("arima_reg")
#' tibble::tribble(
#'     ~ "modeltime", ~ "forecast::auto.arima", ~ "forecast::Arima",
#'     "period", "ts(frequency)", "ts(frequency)",
#'     "non_seasonal_ar, non_seasonal_differences, non_seasonal_ma", "max.p, max.d, max.q", "order = c(p,d,q)",
#'     "seasonal_ar, seasonal_differences, seasonal_ma", "max.P, max.D, max.Q", "seasonal = c(P,D,Q)"
#' ) %>% knitr::kable()
#' ```
#'
#' Other options can be set using `set_engine()`.
#'
#' __forecast::auto.arima (default engine)__
#'
#' TODO
#'
#' __forecast::Arima__
#'
#' [forecast::Arima()] Function Parameters:
#' ```{r echo = FALSE}
#' str(forecast::Arima)
#' ```
#' The nonseasonal ARIMA terms (`order`) and seasonal ARIMA terms (`seasonal`) are provided to [forecast::Arima()] via `arima_reg()` parameters.
#' Other options and argument can be set using `set_engine()`.
#'
#' Parameter Notes:
#' - `xreg` - This is supplied via the parsnip / modeltime `fit()` interface
#'  (so don't provide this manually). See Fit Details (below).
#' - `method` - The default is set to "ML". This method is more robust at the expense of speed.
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
#' @seealso [fit.arima_reg()], [set_engine()]
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
#' # Model Spec
#' model_spec <- arima_reg(
#'         period                   = 12,
#'         non_seasonal_ar          = 3,
#'         non_seasonal_differences = 1,
#'         non_seasonal_ma          = 3,
#'         seasonal_ar              = 1,
#'         seasonal_differences     = 0,
#'         seasonal_ma              = 1
#'     ) %>%
#'     set_engine("forecast::Arima")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#'
#' @export
arima_reg <- function(mode = "regression", period = NULL,
                      non_seasonal_ar = NULL, non_seasonal_differences = NULL, non_seasonal_ma = NULL,
                      seasonal_ar = NULL, seasonal_differences = NULL, seasonal_ma = NULL) {

    args <- list(
        period                    = rlang::enquo(period),
        non_seasonal_ar           = rlang::enquo(non_seasonal_ar),
        non_seasonal_differences  = rlang::enquo(non_seasonal_differences),
        non_seasonal_ma           = rlang::enquo(non_seasonal_ma),
        seasonal_ar               = rlang::enquo(seasonal_ar),
        seasonal_differences      = rlang::enquo(seasonal_differences),
        seasonal_ma               = rlang::enquo(seasonal_ma)
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
                             period = NULL, non_seasonal_ar = NULL, non_seasonal_differences = NULL, non_seasonal_ma = NULL,
                             seasonal_ar = NULL, seasonal_differences = NULL, seasonal_ma = NULL,
                             fresh = FALSE, ...) {

    parsnip::update_dot_check(...)

    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(
        period                     = rlang::enquo(period),
        non_seasonal_ar            = rlang::enquo(non_seasonal_ar),
        non_seasonal_differences   = rlang::enquo(non_seasonal_differences),
        non_seasonal_ma            = rlang::enquo(non_seasonal_ma),
        seasonal_ar                = rlang::enquo(seasonal_ar),
        seasonal_differences       = rlang::enquo(seasonal_differences),
        seasonal_ma                = rlang::enquo(seasonal_ma)
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
        message("Used `engine = 'forecast::auto.arima'` for translation.")
        engine <- "forecast::auto.arima"
    }
    x <- parsnip::translate.default(x, engine, ...)

    x
}


