# forecast::Arima Implementations
# - These are wrappers that handle date information for the forecasting functions

# FIT -----

#' Low-Level ARIMA function for translating modeltime to forecast
#'
#' @inheritParams arima_reg
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param p The order of the non-seasonal auto-regressive (AR) terms. Often denoted "p" in pdq-notation.
#' @param d The order of integration for non-seasonal differencing. Often denoted "d" in pdq-notation.
#' @param q The order of the non-seasonal moving average (MA) terms. Often denoted "q" in pdq-notation.
#' @param P The order of the seasonal auto-regressive (SAR) terms. Often denoted "P" in PDQ-notation.
#' @param D The order of integration for seasonal differencing. Often denoted "D" in PDQ-notation.
#' @param Q The order of the seasonal moving average (SMA) terms. Often denoted "Q" in PDQ-notation.
#' @param ... Additional arguments passed to `forecast::Arima`
#'
#' @export
Arima_fit_impl <- function(x, y, period = "auto", p = 0, d = 0, q = 0, P = 0, D = 0, Q = 0, ...) {

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

    # FIT
    outcome_ts <- stats::ts(outcome, frequency = period)

    if (!is.null(xreg_matrix)) {
        fit_arima   <- forecast::Arima(outcome_ts, order = c(p, d, q), seasonal = c(P, D, Q), xreg = xreg_matrix, ...)
    } else {
        fit_arima <- forecast::Arima(outcome_ts, order = c(p, d, q), seasonal = c(P, D, Q), ...)
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

    structure(ret, class = "Arima_fit_impl")

}

#' @export
print.Arima_fit_impl <- function(x, ...) {
    print(x$model$model_1)
    invisible(x)
}


# PREDICT ----


#' @export
predict.Arima_fit_impl <- function(object, new_data, ...) {
    Arima_predict_impl(object, new_data, ...)
}

#' Low-Level ARIMA function for translating modeltime to forecast
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `forecast::forcast.Arima()`
#'
#' @export
Arima_predict_impl <- function(object, new_data, ...) {

    # PREPARE INPUTS
    model       <- object$model$model_1
    idx_train   <- object$data %>% timetk::tk_index()
    xreg_recipe <- object$extras$xreg_recipe
    h_horizon   <- nrow(new_data)

    # XREG
    new_data    <- janitor::clean_names(new_data)
    xreg_matrix <- bake_xreg_recipe(xreg_recipe, new_data, format = "matrix")

    # PREDICTIONS
    if (!is.null(xreg_matrix)) {
        preds_forecast <- forecast::forecast(model, h = h_horizon, xreg = xreg_matrix, ...)
    } else {
        preds_forecast <- forecast::forecast(model, h = h_horizon, ...)
    }

    # Return predictions as numeric vector
    preds <- tibble::as_tibble(preds_forecast) %>% purrr::pluck(1)

    return(preds)

}







