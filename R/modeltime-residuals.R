# MODELTIME RESIDUALS ----

#' Extract Residuals Information
#'
#' This is a convenience function to unnest model residuals
#'
#' @param object A Modeltime Table
#' @param new_data A `tibble` to predict and calculate residuals on.
#'  If provided, overrides any calibration data.
#' @param quiet Hide errors (`TRUE`, the default), or display them as they occur?
#' @param ... Not currently used.
#'
#' @return A tibble with residuals.
#'
#' @examples
#' library(dplyr)
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
#'     plot_modeltime_residuals(.interactive = FALSE)
#'
#' # Out-of-Sample
#' models_tbl %>%
#'     modeltime_calibrate(new_data = testing(splits)) %>%
#'     modeltime_residuals() %>%
#'     plot_modeltime_residuals(.interactive = FALSE)
#'
#'
#' @name modeltime_residuals
NULL

#' @export
#' @rdname modeltime_residuals
modeltime_residuals <- function(object, new_data = NULL, quiet = TRUE, ...) {
    if (!is_calibrated(object)) {
       if (is.null(new_data)) {
           rlang::abort("Modeltime Table must be calibrated (see 'modeltime_calibrate()') or include 'new_data'.")
       }
    }

    UseMethod("modeltime_residuals")
}

#' @export
modeltime_residuals.default <- function(object, new_data = NULL, quiet = TRUE, ...) {
    rlang::abort(stringr::str_glue("Received an object of class: {class(object)[1]}. Expected an object of class:\n 1. 'mdl_time_tbl' - A Model Time Table made with 'modeltime_table()' and calibrated with 'modeltime_calibrate()'."))
}


#' @export
modeltime_residuals.mdl_time_tbl <- function(object, new_data = NULL, quiet = TRUE, ...) {

    data <- object

    # Handle New Data ----
    if (!is.null(new_data)) {
        data <- data %>%
            modeltime_calibrate(new_data = new_data)
    }


    # Residuals Extraction ----
    ret <- data %>%
        dplyr::ungroup() %>%
        dplyr::select(-".model") %>%
        tidyr::unnest(.calibration_data) %>%
        dplyr::rename(.index = 4)


    return(ret)
}




