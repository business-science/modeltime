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
        # If not possible, period = 1
        rlang::abort("No date or date-time variable provided. Please supply a date or date-time variable as a predictor or set `period` to a numeric value.")
        # period <- 1
    })


    # XREG

    # Drop outcome and any date features
    xreg_df <- predictor %>%
        dplyr::select_if(~ ! timetk::is_date_class(.))

    xreg_matrix <- prep_xreg_matrix_from_df(xreg_df)

    # FIT

    # Prep ts object
    outcome_ts <- stats::ts(outcome, frequency = period)

    # Fit
    if (!is.null(xreg_matrix)) {
        if (ncol(xreg_matrix) > 0) {
            xreg_matrix <- as.matrix(xreg_matrix)
            fit_arima   <- forecast::Arima(outcome_ts, order = c(p, d, q), seasonal = c(P, D, Q), xreg = xreg_matrix, ...)
        } else {
            fit_arima   <- forecast::Arima(outcome_ts, order = c(p, d, q), seasonal = c(P, D, Q), ...)
        }
    } else {
        fit_arima <- forecast::Arima(outcome_ts, order = c(p, d, q), seasonal = c(P, D, Q), ...)
    }

    # RETURN
    ret <- list(
        model      = fit_arima,
        index      = tibble::tibble(!! idx_col := idx),
        xreg_terms = c(colnames(xreg_matrix))
    )

    structure(ret, class = "Arima_fit_impl")

}

#' @export
print.Arima_fit_impl <- function(x, ...) {
    print(x[1])
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

    model       <- object$model
    idx_train   <- object$index %>% timetk::tk_index()
    xreg_terms  <- object$xreg_terms
    h_horizon   <- nrow(new_data)

    # XREG

    # Drop outcome and any date features
    xreg_df <- new_data %>%
        dplyr::select_if(~ ! timetk::is_date_class(.))

    # Prep as matrix
    xreg_matrix <- NULL
    if (ncol(xreg_df) > 0) {

        xreg_model_frame   <- hardhat::model_frame(~ ., xreg_df)
        xreg_model_matrix  <- hardhat::model_matrix(xreg_model_frame$terms, xreg_model_frame$data)

        xreg_matrix <- xreg_model_matrix %>%
            as.matrix()

        xreg_matrix <- xreg_matrix[,xreg_terms]

        # print(xreg_matrix)

        if (length(xreg_matrix) == 0) {
            xreg_matrix <- NULL
        } else if (ncol(xreg_matrix) == 0) {
            xreg_matrix <- NULL
        }

    }

    # PREDICTIONS

    if (!is.null(xreg_matrix)) {
        preds_forecast <- forecast::forecast(object$model, h = h_horizon, xreg = xreg_matrix, ...)
    } else {
        preds_forecast <- forecast::forecast(object$model, h = h_horizon, ...)
    }

    # Return
    preds <- tibble::as_tibble(preds_forecast) %>% purrr::pluck(1)

    return(preds)

}







