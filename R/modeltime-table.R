#' Scale forecast analysis with a Modeltime Table
#'
#' Designed to perform forecasts at scale using models created with
#' `modeltime`, `parsnip`, `workflows`, and regression modeling extensions
#' in the `tidymodels` ecosystem.
#'
#' @param ... Fitted `parsnip` model or `workflow` objects
#'
#' @details
#'
#' This function:
#'
#' 1. Creates a table of models
#' 2. Validates that all objects are models (parsnip or workflows objects) and
#'  all models have been fitted (trained)
#' 3. Provides an ID and Description of the models
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
#' # ---- CALIBRATE ----
#'
#' calibration_tbl <- models_tbl %>%
#'     modeltime_calibrate(new_data = testing(splits))
#'
#' # ---- ACCURACY ----
#'
#' calibration_tbl %>%
#'     modeltime_accuracy()
#'
#' # ---- FORECAST ----
#'
#' calibration_tbl %>%
#'     modeltime_forecast(
#'         new_data    = testing(splits),
#'         actual_data = m750
#'     )
#'
#' @export
#' @name modeltime_table
modeltime_table <- function(...) {

    ret <- tibble::tibble(
        .model = list(...)
    ) %>%
        tibble::rowid_to_column(var = ".model_id")

    # CHECKS
    validate_model_classes(ret, accept_classes = c("model_fit", "workflow"))
    validate_models_are_trained(ret)

    # CREATE MODELTIME OBJECT
    ret <- ret %>%
        dplyr::mutate(.model_desc = purrr::map_chr(.model, .f = get_model_description))

    class(ret) <- c("mdl_time_tbl", class(ret))

    return(ret)
}

#' @export
print.mdl_time_tbl <- function(x, ...) {
    cat("# Modeltime Table\n")
    class(x) <- class(x)[!(class(x) %in% c("mdl_time_tbl"))]
    print(x, ...)
}


#' Update the model description by model id in a Modeltime Table
#'
#' @param object A Modeltime Table
#' @param .model_id A numeric value matching the .model_id that you want to update
#' @param .new_model_desc Text describing the new model description
#'
#' @export
update_model_description <- function(object, .model_id, .new_model_desc) {
    UseMethod("update_model_description", object)
}

#' @export
update_model_description.mdl_time_tbl <- function(object, .model_id, .new_model_desc) {

    .id <- .model_id

    object %>%
        dplyr::mutate(.model_desc = ifelse(.model_id == .id, .new_model_desc, .model_desc))
}

#' Combine multiple Modeltime Tables into a single Modeltime Table
#'
#' @param ... Multiple Modeltime Tables (class `mdl_time_tbl`)
#'
#' @details
#' This function combines multiple Modeltime Tables.
#'
#'  - The `.model_id` will automatically be renumbered to ensure
#' each model has a unique ID.
#'  - Only the `.model_id`, `.model`, and `.model_desc` columns will be returned.
#'
#' __Re-Training Models on the Same Datasets__
#'
#' One issue can arise if your models are trained on different datasets.
#' If your models have been trained on different datasets, you can run
#' [modeltime_refit()] to train all models on the same data.
#'
#' __Re-Calibrating Models__
#'
#' If your data has been calibrated using [modeltime_calibrate()],
#' the `.test` and `.calibration_data` columns will be removed.
#' To re-calibrate, simply run [modeltime_calibrate()] on the newly
#' combined Modeltime Table.
#'
#' @examples
#' library(modeltime)
#' library(tidymodels)
#' library(tidyverse)
#' library(timetk)
#' library(lubridate)
#'
#' # Setup
#' m750 <- m4_monthly %>% filter(id == "M750")
#'
#' splits <- time_series_split(m750, assess = "3 years", cumulative = TRUE)
#'
#' model_fit_arima <- arima_reg() %>%
#'     set_engine("auto_arima") %>%
#'     fit(value ~ date, training(splits))
#'
#' model_fit_prophet <- prophet_reg() %>%
#'     set_engine("prophet") %>%
#'     fit(value ~ date, training(splits))
#'
#' # Multiple Modeltime Tables
#' model_tbl_1 <- modeltime_table(model_fit_arima)
#' model_tbl_2 <- modeltime_table(model_fit_prophet)
#'
#' # Combine
#' combine_modeltime_tables(model_tbl_1, model_tbl_2)
#'
#' @export
combine_modeltime_tables <- function(...) {

    dots <- list(...)

    # CHECKS
    meta_data <- tibble::enframe(dots, name = ".id", value = ".model_table")
    validate_modeltime_table_classes(meta_data, accept_classes = c("mdl_time_tbl"))

    # UNCALIBRATE
    meta_ncol_check_tbl      <- meta_data %>% check_ncols(.model_table, accept_ncol = 3)
    meta_data_fail_check_tbl <- meta_ncol_check_tbl %>% dplyr::filter(fail_check)
    if (nrow(meta_data_fail_check_tbl) > 0) {
        message(stringr::str_glue("Some Modeltime Tables have more than 3 columns, likely due to calibration.
                                  - Models: {stringr::str_c(meta_data_fail_check_tbl$.id, collapse = ', ')} have more than 3 columns.
                                  Calibration data is being removed. Please use `modeltime_calibrate()` to re-calibrate following combining modeltime tables."))

        dots <- meta_ncol_check_tbl %>%
            dplyr::mutate(.model_table_clean = purrr::map(.model_table, function(df) {
                df %>%
                    dplyr::select(.model_id, .model, .model_desc)
            })) %>%
            dplyr::pull(.model_table_clean)

    }


    # CREATE MODELTIME OBJECT
    combined_tbl <- dplyr::bind_rows(dots)

    # Renumber .model_ids
    ret <- combined_tbl %>%
        dplyr::mutate(.model_id = dplyr::row_number())

    class(ret) <- c("mdl_time_tbl", class(ret)) %>% unique()

    return(ret)

}



