# MODELTIME RESIDUALS TESTS ----

#' Apply Statistical Tests to Residuals
#'
#' This is a convenience function to calculate some statistical tests on the residuals models. Currently, the
#' following statistics are calculated: the shapiro.test to check the normality of the residuals, the box-pierce
#' and ljung-box tests and the durbin watson test to check the autocorrelation of the residuals. In all cases
#' the p-values are returned.
#'
#'
#' @param object A `tibble` extracted from modeltime::modeltime_residuals().
#' @param new_data A `tibble` to predict and calculate residuals on.
#'  If provided, overrides any calibration data.
#' @param lag The statistic will be based on lag autocorrelation coefficients. Default: 1
#'   (Applies to Box-Pierce, Ljung-Box, and Durbin-Watson Tests)
#' @param fitdf Number of degrees of freedom to be subtracted. Default: 0
#'   (Applies Box-Pierce and Ljung-Box Tests)
#' @param ... Not currently used
#'
#' @details
#'
#' __Shapiro-Wilk Test__
#'
#' The Shapiro-Wilk tests the Normality of the residuals.
#' The Null Hypothesis is that the residuals are normally distributed.
#' A low P-Value below a given significance level indicates the values are NOT Normally Distributed.
#'
#' If the __p-value > 0.05 (good)__, this implies that the distribution
#' of the data are not significantly different from normal distribution.
#' In other words, we can assume the normality.
#'
#' __Box-Pierce and Ljung-Box Tests Tests__
#'
#' The Ljung-Box and Box-Pierce tests are methods that test for the absense
#' of autocorrelation in residuals. A low p-value below a given significance level
#' indicates the values are autocorrelated.
#'
#' If the __p-value > 0.05 (good)__, this implies that the residuals
#' of the data are are independent.
#' In other words, we can assume the residuals are not autocorrelated.
#'
#' For more information about the parameters associated with the Box Pierce and Ljung Box tests check
#' ?Box.Test
#'
#' __Durbin-Watson Test__
#'
#' The Durbin-Watson test is a method that tests for the absense of autocorrelation in residuals.
#' The Durbin Watson test reports a test statistic, with a value from 0 to 4, where:
#'
#'  * __2 is no autocorrelation (good)__
#'  * From 0 to <2 is positive autocorrelation (common in time series data)
#'  * From >2 to 4 is negative autocorrelation (less common in time series data)
#'
#' @seealso
#' [stats::shapiro.test()], [stats::Box.test()]
#'
#' @return A tibble with with the p-values of the calculated statistical tests.
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#' library(parsnip)
#' library(rsample)
#'
#' # Data
#' m750 <- m4_monthly %>% filter(id == "M750")
#'
#' # Split Data 80/20
#' splits <- initial_time_split(m750, prop = 0.9)
#'
#' # --- MODELS ---
#'
#' # Model 1: prophet ----
#' model_fit_prophet <- prophet_reg() %>%
#'     set_engine(engine = "prophet") %>%
#'     fit(value ~ date, data = training(splits))
#'
#'
#' # ---- MODELTIME TABLE ----
#'
#' models_tbl <- modeltime_table(
#'     model_fit_prophet
#' )
#'
#' # ---- RESIDUALS ----
#'
#' # In-Sample
#' models_tbl %>%
#'     modeltime_calibrate(new_data = training(splits)) %>%
#'     modeltime_residuals() %>%
#'     modeltime_residuals_test()
#'
#' # Out-of-Sample
#' models_tbl %>%
#'     modeltime_calibrate(new_data = testing(splits)) %>%
#'     modeltime_residuals() %>%
#'     modeltime_residuals_test()
#'
#'
#' @name modeltime_residuals_test
NULL

#' @export
#' @rdname modeltime_residuals_test
modeltime_residuals_test <- function(object,
                                      new_data = NULL,
                                      lag = 1,
                                      fitdf = 0,
                                      ...) {

    if (!is_residuals(object)) {
        if (is.null(new_data)) {
            rlang::abort("Object must be extracted from modeltime::modeltime_residuals() or include 'new_data'.")
        }
    }

    UseMethod("modeltime_residuals_test")
}

#' @export
modeltime_residuals_test.default <- function(object,
                                              new_data = NULL,
                                              lag = 1,
                                              fitdf = 0,
                                              ...) {
    rlang::abort(stringr::str_glue("Received an object of class: {class(object)[1]}. Expected an object of class:\n 1. 'tbl_df' - A tibble extracted from modeltime::modeltime_residuals(). '."))
}


#' @export
modeltime_residuals_test.tbl_df <- function(object,
                                             new_data = NULL,
                                             lag = 1,
                                             fitdf = 0,
                                             ...) {

    if (is_residuals(object) & !is.null(new_data)) {

        message(stringr::str_glue('Using {ifelse(deparse(substitute(object)) == ".", "lhs tibble", deparse(substitute(object)))} to calculate p-values. Cannot calibrate new_data based on residuals tibble.'))

    }

    data <- object

    # Handle New Data ----
    if (!is.null(new_data) & !is_residuals(object)) {

        data <- data %>%
            modeltime_calibrate(new_data = new_data) %>%
            modeltime_residuals()
    }

    # Description Storage ----

    .model_desc <- data %>%
                   dplyr::group_by(.model_id) %>%
                   dplyr::summarise(.model_desc = dplyr::first(.model_desc)) %>%
                   dplyr::ungroup()


    # Residuals  Tests Extraction ----

    ret <- data %>%
        dplyr::group_by(.model_id) %>%
        dplyr::select(.model_id, .residuals) %>%
        tidyr::nest() %>%
        dplyr::mutate(
            shapiro_wilk  = purrr::map(data, ~stats::shapiro.test(.x$.residuals)$p.value),
            box_pierce    = purrr::map(data, ~stats::Box.test(.x$.residuals, lag = lag, type = 'Box-Pierce', fitdf = fitdf)$p.value),
            ljung_box     = purrr::map(data, ~stats::Box.test(.x$.residuals, lag = lag,  type = 'Ljung-Box',  fitdf = fitdf)$p.value),
            durbin_watson = purrr::map_dbl(data, ~durbin_watson_test(.x$.residuals, lag = lag))

        ) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(shapiro_wilk:durbin_watson) %>%
        dplyr::ungroup()

    # Recombine Description ----

    ret <- ret %>%
           dplyr::inner_join(.model_desc, by = '.model_id') %>%
           dplyr::relocate(.model_desc, .after = '.model_id')


    return(ret)
}


# DURBIN-WATSON ----

durbin_watson_test <- function(x, lag = 1) {

    sum((x - timetk::lag_vec(x, lag = lag))^2, na.rm = TRUE) / sum(x^2, na.rm = TRUE)

}

