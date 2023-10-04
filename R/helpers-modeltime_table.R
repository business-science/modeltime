# MODELTIME TALBE HELPERS ----


# COMBINING MODELTIME TABLES -----

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
#' @seealso
#' - [combine_modeltime_tables()]: Combine 2 or more Modeltime Tables together
#' - [add_modeltime_model()]: Adds a new row with a new model to a Modeltime Table
#' - [drop_modeltime_model()]: Drop one or more models from a Modeltime Table
#' - [update_modeltime_description()]: Updates a description for a model inside a Modeltime Table
#' - [update_modeltime_model()]: Updates a model inside a Modeltime Table
#' - [pull_modeltime_model()]: Extracts a model from a Modeltime Table
#'
#' @examples
#' library(tidymodels)
#' library(timetk)
#' library(dplyr)
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

# ADD MODEL -----

#' Add a Model into a Modeltime Table
#'
#' @param object Multiple Modeltime Tables (class `mdl_time_tbl`)
#' @param model A model of class `model_fit` or a fitted `workflow` object
#' @param location Where to add the model. Either "top" or "bottom". Default: "bottom".
#'
#' @seealso
#' - [combine_modeltime_tables()]: Combine 2 or more Modeltime Tables together
#' - [add_modeltime_model()]: Adds a new row with a new model to a Modeltime Table
#' - [drop_modeltime_model()]: Drop one or more models from a Modeltime Table
#' - [update_modeltime_description()]: Updates a description for a model inside a Modeltime Table
#' - [update_modeltime_model()]: Updates a model inside a Modeltime Table
#' - [pull_modeltime_model()]: Extracts a model from a Modeltime Table
#'
#' @examples
#' \donttest{
#' library(tidymodels)
#'
#' model_fit_ets <- exp_smoothing() %>%
#'     set_engine("ets") %>%
#'     fit(value ~ date, training(m750_splits))
#'
#' m750_models %>%
#'     add_modeltime_model(model_fit_ets)
#' }
#'
#' @export
add_modeltime_model <- function(object, model, location = "bottom") {

    object_1 <- object
    object_2 <- modeltime_table(model)

    if (location == "bottom") {
        ret <- combine_modeltime_tables(object_1, object_2)
    } else {
        ret <- combine_modeltime_tables(object_2, object_1)
    }

    return(ret)

}

# DROP MODEL -----

#' Drop a Model from a Modeltime Table
#'
#' @param object A Modeltime Table (class `mdl_time_tbl`)
#' @param .model_id A numeric value matching the .model_id that you want to drop
#'
#' @seealso
#' - [combine_modeltime_tables()]: Combine 2 or more Modeltime Tables together
#' - [add_modeltime_model()]: Adds a new row with a new model to a Modeltime Table
#' - [drop_modeltime_model()]: Drop one or more models from a Modeltime Table
#' - [update_modeltime_description()]: Updates a description for a model inside a Modeltime Table
#' - [update_modeltime_model()]: Updates a model inside a Modeltime Table
#' - [pull_modeltime_model()]: Extracts a model from a Modeltime Table
#'
#' @examples
#' \donttest{
#' library(tidymodels)
#'
#'
#' m750_models %>%
#'     drop_modeltime_model(.model_id = c(2,3))
#' }
#'
#' @export
drop_modeltime_model <- function(object, .model_id) {

    if (!rlang::is_bare_numeric(.model_id)){
        rlang::abort(".model_id must be numeric")
    }

    if (!is_modeltime_table(object)){
        rlang::abort("object must be a 'modeltime_table'")
    }

    ret <- object %>%
           dplyr::filter(!(.model_id %in% !!.model_id))

    return(ret)

}

# UPDATE MODEL ----

#' Update the model by model id in a Modeltime Table
#'
#'
#' @param object A Modeltime Table
#' @param .model_id A numeric value matching the .model_id that you want to update
#' @param .new_model A fitted workflow, model_fit, or mdl_time_ensmble object
#'
#' @seealso
#' - [combine_modeltime_tables()]: Combine 2 or more Modeltime Tables together
#' - [add_modeltime_model()]: Adds a new row with a new model to a Modeltime Table
#' - [drop_modeltime_model()]: Drop one or more models from a Modeltime Table
#' - [update_modeltime_description()]: Updates a description for a model inside a Modeltime Table
#' - [update_modeltime_model()]: Updates a model inside a Modeltime Table
#' - [pull_modeltime_model()]: Extracts a model from a Modeltime Table
#'
#' @examples
#' \donttest{
#' library(tidymodels)
#'
#' model_fit_ets <- exp_smoothing() %>%
#'     set_engine("ets") %>%
#'     fit(value ~ date, training(m750_splits))
#'
#' m750_models %>%
#'     update_modeltime_model(1, model_fit_ets)
#' }
#'
#' @export
update_modeltime_model <- function(object, .model_id, .new_model) {
    UseMethod("update_modeltime_model", object)
}


#' @export
update_modeltime_model.mdl_time_tbl <- function(object, .model_id, .new_model) {

    # Check .new_model is a trained model
    if (!is_trained(.new_model)) {
        rlang::abort("Could not update the Modeltime Model. The '.new_model' is not a trained model of class 'workflow', 'model_fit', or 'mdl_time_ensemble'.")
    }

    .id <- .model_id

    .desc <- get_model_description(.new_model)

    object %>%
        dplyr::mutate(.model = ifelse(.model_id == .id, list(.new_model), .model)) %>%
        update_model_description(.model_id = .id, .new_model_desc = .desc)

}



# UPDATE MODEL DESCRIPTION ----

#' Update the model description by model id in a Modeltime Table
#'
#' The `update_model_description()` and `update_modeltime_description()` functions
#' are synonyms.
#'
#' @param object A Modeltime Table
#' @param .model_id A numeric value matching the .model_id that you want to update
#' @param .new_model_desc Text describing the new model description
#'
#' @seealso
#' - [combine_modeltime_tables()]: Combine 2 or more Modeltime Tables together
#' - [add_modeltime_model()]: Adds a new row with a new model to a Modeltime Table
#' - [drop_modeltime_model()]: Drop one or more models from a Modeltime Table
#' - [update_modeltime_description()]: Updates a description for a model inside a Modeltime Table
#' - [update_modeltime_model()]: Updates a model inside a Modeltime Table
#' - [pull_modeltime_model()]: Extracts a model from a Modeltime Table
#'
#' @examples
#'
#' m750_models %>%
#'     update_modeltime_description(2, "PROPHET - No Regressors")
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

#' @export
#' @rdname update_model_description
update_modeltime_description <- update_model_description




# PULL / PLUCK MODELTIME MODEL ----

#' Extract model by model id in a Modeltime Table
#'
#' The `pull_modeltime_model()` and `pluck_modeltime_model()` functions are synonymns.
#'
#' @param object A Modeltime Table
#' @param .model_id A numeric value matching the .model_id that you want to update
#'
#' @seealso
#' - [combine_modeltime_tables()]: Combine 2 or more Modeltime Tables together
#' - [add_modeltime_model()]: Adds a new row with a new model to a Modeltime Table
#' - [drop_modeltime_model()]: Drop one or more models from a Modeltime Table
#' - [update_modeltime_description()]: Updates a description for a model inside a Modeltime Table
#' - [update_modeltime_model()]: Updates a model inside a Modeltime Table
#' - [pull_modeltime_model()]: Extracts a model from a Modeltime Table
#'
#' @examples
#'
#' m750_models %>%
#'     pluck_modeltime_model(2)
#'
#' @name pluck_modeltime_model

#' @export
#' @rdname pluck_modeltime_model
pluck_modeltime_model <- function(object, .model_id) {
    UseMethod("pluck_modeltime_model", object)
}

#' @export
#' @rdname pluck_modeltime_model
pluck_modeltime_model.mdl_time_tbl <- function(object, .model_id) {

    .id <- .model_id

    object %>%
        dplyr::filter(.model_id == .id) %>%
        purrr::pluck(".model", 1)
}

#' @export
#' @rdname pluck_modeltime_model
pull_modeltime_model <- pluck_modeltime_model

