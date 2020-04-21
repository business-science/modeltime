#' General Interface for ARIMA Regression Models
#'
#' `arima_reg()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `forecast`.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param period A seasonal frequency. If none is present, use 1.
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
#'  to determine the _mode_ of the model. For `linear_reg()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"forecast"`  (the default)
#' }
#'
#' __Main Arguments__
#'
#' The main arguments for the model are:
#'
#'  - `period`: A seasonal frequency. If none is present, use 1.
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
#'  set using `set_engine()`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions.
#'
#'  If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#'
#' @section Engine Details:
#'
#' The standardized parameter names in parsnip can be mapped to their original
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
#' Other options and argument can be set using `set_engine()`.
#'
#' __forecast__
#'
#' ```{r echo = FALSE}
#' str(forecast::Arima)
#' ```
#'
#' @section Fit Details:
#'
#' __xreg (Exogenous Regressors)__
#'
#'  The `xreg` paramater is populated using the `fit()` or `fit_xy()` function.
#'
#'  - `fit(y ~ x)` will pass x on as an exogenous regressor.
#'  - `fit_xy(x, y)` will pass x on as an exogenous regressor.
#'
#'  Note that date or date-time class values are excluded from `xreg`.
#'
#'  If no `xreg` is used, simply use:
#'
#'  - `fit(y ~ 1)` will ignore xreg.
#'  - `fit_xy(x = NULL, y)` will ignore xreg.
#'
#'
#' @seealso [fit.arima_reg()], [set_engine()]
#'
#' @examples
#' # TODO
#'
#' @export
arima_reg <- function(mode = "regression", period = "auto", p = 0, d = 0, q = 0, P = 0, D = 0, Q = 0) {

    args <- list(
        period = rlang::enquo(period),
        p = rlang::enquo(p),
        d = rlang::enquo(d),
        q = rlang::enquo(q),
        P = rlang::enquo(P),
        D = rlang::enquo(D),
        Q = rlang::enquo(Q)
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
                             period = 1, p = 0, d = 0, q = 0, P = 0, D = 0, Q = 0,
                             fresh = FALSE, ...) {
    parsnip::update_dot_check(...)

    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(
        period = rlang::enquo(period),
        p = rlang::enquo(p),
        d = rlang::enquo(d),
        q = rlang::enquo(q),
        P = rlang::enquo(P),
        D = rlang::enquo(D),
        Q = rlang::enquo(Q)
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


