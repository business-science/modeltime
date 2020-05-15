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

    # print(head(x))

    # OUTCOME VEC

    # Expect outcomes = vector
    # Expect predictor = data.frame or NULL
    outcome    <- y
    predictor  <- x

    # PERIOD

    # Determine Period
    idx <- NULL
    tryCatch({
        # Try to get a period from a user-provided index
        idx_col <- timetk::tk_get_timeseries_variables(predictor)[1]
        idx     <- predictor %>% timetk::tk_index() # Will generate an error if no time series index
        if (tolower(period) == "auto" | is.character(period)) {
            period  <- timetk::tk_get_frequency(idx, period, message = TRUE)
        }
    }, error = function(e) {
        rlang::abort("No date or date-time variable provided. Please supply a date or date-time variable as a predictor or set `period` to a numeric value.")

    })


    # XREG

    # Drop outcome and any date features
    xreg_df <- predictor %>%
        dplyr::select_if(~ ! timetk::is_date_class(.))

    xreg_matrix <- prep_xreg_matrix_from_df_fit(xreg_df)

    # FIT

    # Prep ts object
    outcome_ts <- stats::ts(outcome, frequency = period)

    # Fit
    if (!is.null(xreg_matrix)) {
        if (ncol(xreg_matrix) > 0) {
            xreg_matrix <- as.matrix(xreg_matrix)
            fit_arima   <- forecast::auto.arima(outcome_ts,
                                                max.p = max.p, max.d = max.d, max.q = max.q,
                                                max.P = max.P, max.D = max.D, max.Q = max.Q,
                                                xreg = xreg_matrix, ...)
        } else {
            fit_arima   <- forecast::auto.arima(outcome_ts,
                                                max.p = max.p, max.d = max.d, max.q = max.q,
                                                max.P = max.P, max.D = max.D, max.Q = max.Q,
                                                ...)
        }
    } else {
        fit_arima <- forecast::auto.arima(outcome_ts,
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
            xreg_terms = c(colnames(xreg_matrix))
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

