# MODELTIME TALBE HELPERS ----

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



#' Extract model by model id in a Modeltime Table
#'
#' @param object A Modeltime Table
#' @param .model_id A numeric value matching the .model_id that you want to update
#'
#' @export
pluck_modeltime_model <- function(object, .model_id) {
    UseMethod("pluck_modeltime_model", object)
}

#' @export
pluck_modeltime_model.mdl_time_tbl <- function(object, .model_id) {

    .id <- .model_id

    object %>%
        dplyr::filter(.model_id == .id) %>%
        purrr::pluck(".model", 1)
}

