# MODELTIME RESIDUALS TESTS ----

#' Extract Residuals Tests
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
#' @param quiet Hide errors (`TRUE`, the default), or display them as they occur?
#' @param box_pierce_lag the statistic will be based on lag autocorrelation coefficients. Default: 1
#' @param box_pierce_fitdf number of degrees of freedom to be subtracted. Default: 0
#' @param ljung_box_lag the statistic will be based on lag autocorrelation coefficients. Default: 1
#' @param ljung_box_fitdf number of degrees of freedom to be subtracted. Default: 0
#' @param ... Not currently used
#'
#' @details For more information about the parameters associated with the Box Pierce and Ljung Box tests check
#' ?Box.Test
#'
#' @return A tibble with with the p-values of the calculated statistical tests.
#'
#' @examples
#' library(tidyverse)
#' library(lubridate)
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
#' # Model 1: auto_arima ----
#' model_fit_arima <- arima_reg() %>%
#'     set_engine(engine = "auto_arima") %>%
#'     fit(value ~ date, data = training(splits))
#'
#'
#' # ---- MODELTIME TABLE ----
#'
#' models_tbl <- modeltime_table(
#'     model_fit_arima
#' )
#'
#' # ---- RESIDUALS ----
#'
#' # In-Sample
#' models_tbl %>%
#'     modeltime_calibrate(new_data = training(splits)) %>%
#'     modeltime_residuals() %>%
#'     modeltime_residuals_tests()
#'
#' # Out-of-Sample
#' models_tbl %>%
#'     modeltime_calibrate(new_data = testing(splits)) %>%
#'     modeltime_residuals() %>%
#'     modeltime_residuals_tests()
#'
#'
#' @name modeltime_residuals_tests
NULL

#' @export
#' @rdname modeltime_residuals_tests
#'
modeltime_residuals_tests <- function(object,
                                      new_data = NULL,
                                      quiet = TRUE,
                                      box_pierce_lag = 1,
                                      box_pierce_fitdf = 0,
                                      ljung_box_lag = 1,
                                      ljung_box_fitdf = 0,
                                      ...) {

    if (!is_residuals(object)) {
        if (is.null(new_data)) {
            rlang::abort("Object must be extracted from modeltime::modeltime_residuals() or include 'new_data'.")
        }
    }

    UseMethod("modeltime_residuals_tests")
}

#' @export
modeltime_residuals_tests.default <- function(object,
                                              new_data = NULL,
                                              quiet = TRUE,
                                              box_pierce_lag = 1,
                                              box_pierce_fitdf = 0,
                                              ljung_box_lag = 1,
                                              ljung_box_fitdf = 0,
                                              ...) {
    rlang::abort(stringr::str_glue("Received an object of class: {class(object)[1]}. Expected an object of class:\n 1. 'tbl_df' - A tibble extracted from modeltime::modeltime_residuals(). '."))
}


#' @export
modeltime_residuals_tests.tbl_df <- function(object,
                                             new_data = NULL,
                                             quiet = TRUE,
                                             box_pierce_lag = 1,
                                             box_pierce_fitdf = 0,
                                             ljung_box_lag = 1,
                                             ljung_box_fitdf = 0,
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

    #Description Storage

    .model_desc <- data %>%
                   dplyr::group_by(.model_id) %>%
                   dplyr::summarise(.model_desc = dplyr::first(.model_desc)) %>%
                   dplyr::ungroup()


    # Residuals  Tests Extraction ----

    ret <- data %>%
        dplyr::group_by(.model_id) %>%
        dplyr::select(.residuals) %>%
        tidyr::nest() %>%
        dplyr::mutate(
            shapiro_wilk  = purrr::map(data, ~stats::shapiro.test(.x$.residuals)$p.value),
            box_pierce    = purrr::map(data, ~stats::Box.test(.x$.residuals, lag = box_pierce_lag, type = 'Box-Pierce', fitdf = box_pierce_fitdf)$p.value),
            ljung_box     = purrr::map(data, ~stats::Box.test(.x$.residuals, lag = ljung_box_lag,  type = 'Ljung-Box',  fitdf = ljung_box_fitdf)$p.value),
            durbin_watson = purrr::map(data, ~car::durbinWatsonTest(.x$.residuals))

        ) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(shapiro_wilk:durbin_watson) %>%
        dplyr::ungroup()


    ret <- ret %>%
           dplyr::inner_join(.model_desc, by = '.model_id') %>%
           dplyr::relocate(.model_desc, .after = '.model_id')


    return(ret)
}


