# forecast::auto.arima Implementations
# - These are wrappers that handle date information for the forecasting functions

# FIT -----

#' Low-Level ARIMA function for translating modeltime to forecast
#'
#' @inheritParams arima_reg
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param max.p The maximum order of the non-seasonal auto-regressive (AR) terms.
#' @param max.d The maximum order of integration for non-seasonal differencing.
#' @param max.q The maximum order of the non-seasonal moving average (MA) terms.
#' @param max.P The maximum order of the seasonal auto-regressive (SAR) terms.
#' @param max.D The maximum order of integration for seasonal differencing.
#' @param max.Q The maximum order of the seasonal moving average (SMA) terms.
#' @param ... Additional arguments passed to `forecast::auto.arima`
#'
#' @export
auto_arima_fit_impl <- function(x, y, period = "auto",
                                max.p = 5, max.d = 2, max.q = 5,
                                max.P = 2, max.D = 1, max.Q = 2, ...) {

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
    predictor   <- janitor::clean_names(predictor)
    xreg_recipe <- prepare_xreg_recipe_from_predictors(predictor, prepare = TRUE)
    xreg_matrix <- juice_xreg_recipe(xreg_recipe, format = "matrix")

    # FIT
    outcome <- stats::ts(outcome, frequency = period)

    if (!is.null(xreg_matrix)) {
        xreg_matrix <- as.matrix(xreg_matrix)
        fit_arima   <- forecast::auto.arima(outcome,
                                            max.p = max.p, max.d = max.d, max.q = max.q,
                                            max.P = max.P, max.D = max.D, max.Q = max.Q,
                                            xreg = xreg_matrix, ...)
    } else {
        fit_arima <- forecast::auto.arima(outcome,
                                          max.p = max.p, max.d = max.d, max.q = max.q,
                                          max.P = max.P, max.D = max.D, max.Q = max.Q,
                                          ...)
    }

    # RETURN
    ret <- list(
        model      = list(
            model_1 = fit_arima
        ),
        data       = tibble::tibble(
            !! idx_col  := idx,
            .value      =  as.numeric(fit_arima$x),
            .fitted     =  as.numeric(fit_arima$fitted),
            .resid      =  as.numeric(fit_arima$residuals)
        ),
        extras = list(
            xreg_recipe = xreg_recipe
        )
    )

    structure(ret, class = "auto_arima_fit_impl")

}

#' @export
print.auto_arima_fit_impl <- function(x, ...) {
    print(x$model$model_1)
    invisible(x)
}


# PREDICT ----
# - auto.arima produces an Arima model

#' @export
predict.auto_arima_fit_impl <- function(object, new_data, ...) {
    Arima_predict_impl(object, new_data, ...)
}

