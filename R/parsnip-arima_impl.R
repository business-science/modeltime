

# MODELING -----


#' Low-Level ARIMA function for translating modeltime to forecast
#'
#' @inheritParams arima_reg
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param ... Additional arguments passed to `forecast::Arima`
#'
#' @export
Arima_fit_impl <- function(x, y, period = "auto", p = 0, d = 0, q = 0, P = 0, D = 0, Q = 0, ...) {

    # print(head(x))

    # OUTCOME VEC ----

    # Expect outcomes = vector
    # Expect predictor = data.frame or NULL
    outcome    <- y
    predictor  <- x

    # PERIOD ----

    # Determine Period
    idx <- NULL
    if (tolower(period) == "auto" | is.character(period)) {
        tryCatch({
            # Try to get a period from a user-provided index
            period <- timetk::tk_get_frequency(idx, period, message = TRUE)
        }, error = function(e) {
            # If not possible, period = 1
            rlang::abort("No date or date-time variable provided. Please supply a date or date-time variable as a predictor or set `period` to a numeric value.")
            # period <- 1
        })
    }

    # XREG ----

    # Drop outcome and any date features
    xreg_df <- predictor %>%
        dplyr::select_if(~ ! timetk:::is_date_class(.))

    xreg_matrix <- prep_xreg_matrix_from_df(xreg_df)

    # FIT ----

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
        index      = idx,
        xreg_terms = c(colnames(xreg_matrix))
    )

    structure(ret, class = "Arima_fit_impl")

}

#' @export
print.Arima_fit_impl <- function(x, ...) {
    print(x[1])
    invisible(x)
}


# PREDICTION ----


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
    idx_train   <- object$index
    xreg_terms  <- object$xreg_terms
    h_horizon   <- nrow(new_data)

    # XREG ----

    # Drop outcome and any date features
    xreg_df <- new_data %>%
        dplyr::select_if(~ ! timetk:::is_date_class(.))

    # Prep as matrix
    xreg_matrix <- NULL
    if (ncol(xreg_df) > 0) {

        xreg_model_frame   <- hardhat::model_frame(~ ., xreg_df)
        xreg_model_matrix  <- hardhat::model_matrix(xreg_model_frame$terms, xreg_model_frame$data)

        xreg_matrix <- xreg_model_matrix %>%
            as.matrix()

        xreg_matrix <- xreg_matrix[,xreg_terms]

        if (ncol(xreg_matrix) == 0) xreg_matrix <- NULL

    }

    # PREDICTIONS ----

    if (!is.null(xreg_matrix)) {
        preds_forecast <- forecast::forecast(object$model, h = h_horizon, xreg = xreg_matrix, ...)
    } else {
        preds_forecast <- forecast::forecast(object$model, h = h_horizon, ...)
    }

    # Return
    preds <- tibble::as_tibble(preds_forecast) %>% purrr::pluck(1)

    return(preds)

}







# UTILITIES -----

prep_xreg_matrix_from_df <- function(xreg_df) {
    xreg_matrix <- NULL
    if (ncol(xreg_df) > 0) {

        # Checks
        validate_non_unique_contrasts(xreg_df)
        validate_unused_factor_levels(xreg_df)

        xreg_model_frame   <- hardhat::model_frame(~ ., xreg_df)
        xreg_model_matrix  <- hardhat::model_matrix(xreg_model_frame$terms, xreg_model_frame$data)

        xreg_matrix <- xreg_model_matrix %>%
            drop_columns_with_single_value() %>%
            as.matrix()

    }
}



drop_columns_with_single_value <- function(data) {

    results_tbl <- check_non_unique_contrasts(data)

    names_failed <- results_tbl %>%
        dplyr::filter(fail_check) %>%
        dplyr::pull(key)

    data %>%
        dplyr::select(-dplyr::one_of(names_failed))

}


# CHECKS ----

check_non_unique_contrasts <- function(data) {

    ret <- data %>%
        purrr::map_dfr(~ length(unique(.))) %>%
        tidyr::gather(key = "key", value = "unique_count", dplyr::everything()) %>%
        dplyr::mutate(fail_check = ifelse(unique_count == 1, TRUE, FALSE))

    ret

}

check_unused_factor_levels <- function(data) {
    ret_factor_count <- data %>%
        purrr::map_dfr(.f = function(x) {
            if (is.factor(x)) {
                length(levels(x))
            } else {
                0
            }
        }) %>%
        tidyr::gather(key = "key", value = "factor_count", dplyr::everything())

    ret_unique_count <- check_non_unique_contrasts(data) %>%
        dplyr::select(-fail_check)

    ret <- dplyr::left_join(ret_factor_count, ret_unique_count, by = "key") %>%
        dplyr::mutate(fail_check = ifelse(factor_count > unique_count, TRUE, FALSE))

    ret
}



validate_non_unique_contrasts <- function(data) {

    result_tbl <- check_non_unique_contrasts(data) %>%
        dplyr::filter(fail_check)

    if (nrow(result_tbl) > 0) {
        bad_cols   <- glue::single_quote(result_tbl$key)
        bad_values <- purrr::map(result_tbl$unique_count, glue_quote_collapse)
        bad_msg    <- glue::glue("{bad_cols}: {bad_values} unique value")
        bad_msg    <- glue::glue_collapse(bad_msg, sep = "\n")

        rlang::abort(glue::glue(
            "All variables must have more than one unique value, but the following do not:",
            "\n",
            "{bad_msg}")
        )
    }

}

validate_unused_factor_levels <- function(data) {

    result_tbl <- check_unused_factor_levels(data) %>%
        dplyr::filter(fail_check)

    if (nrow(result_tbl) > 0) {
        bad_cols     <- glue::single_quote(result_tbl$key)
        bad_values_1 <- purrr::map(result_tbl$factor_count, glue_quote_collapse)
        bad_values_2 <- purrr::map(result_tbl$unique_count, glue_quote_collapse)
        bad_msg      <- glue::glue("{bad_cols}: levels {bad_values_1} > levels used {bad_values_2}")
        bad_msg      <- glue::glue_collapse(bad_msg, sep = "\n")

        rlang::abort(glue::glue(
            "All factor variables must use all levels, but the following do not:",
            "\n",
            "{bad_msg}")
        )
    }

}



glubort <- function(..., .sep = "", .envir = parent.frame()) {
    rlang::abort(glue::glue(..., .sep = .sep, .envir = .envir))
}

glue_quote_collapse <- function(x) {
    glue::glue_collapse(glue::single_quote(x), sep = ", ")
}


