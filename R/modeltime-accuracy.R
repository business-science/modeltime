

#' @export
modeltime_accuracy <- function(object, new_data = NULL, ...) {
    UseMethod("modeltime_accuracy")
}

#' @export
modeltime_accuracy.default <- function(object, new_data = NULL, ...) {
    rlang::abort(stringr::str_glue("Received an object of class: {class(object)[1]}. Expected an object of class 'workflow' that has been fitted (trained) or 'model_fit' (a fitted parsnip model)."))
}

#' @export
modeltime_accuracy.model_spec <- function(object, new_data = NULL, ...) {
    rlang::abort("Model spec must be trained using the 'fit()' function.")
}

#' @export
modeltime_accuracy.model_fit <- function(object, new_data = NULL, ...) {

    model_fit <- object

    data <- model_fit$fit$data

    data_formatted <- data %>%
        tibble::add_column(.type = "Training", .before = 1) %>%
        dplyr::group_by(.type) %>%
        dplyr::summarize(
            MAE   = yardstick::mae_vec(.value, .fitted),
            MAPE  = yardstick::mape_vec(.value, .fitted),
            MASE  = yardstick::mase_vec(.value, .fitted),
            SMAPE = yardstick::smape_vec(.value, .fitted),
            RMSE  = yardstick::rmse_vec(.value, .fitted),
            RSQ   = yardstick::rsq_vec(.value, .fitted)
        ) %>%
        dplyr::ungroup()

    if (!is.null(new_data)) {
        predictions_tbl <- modeltime_forecast(object, new_data = new_data, actual_data = new_data, conf_interval = NULL)

        test_tbl <- predictions_tbl %>%
            tidyr::pivot_wider(names_from = .id, values_from = .value) %>%
            tibble::add_column(.type = "Test", .before = 1) %>%
            dplyr::group_by(.type) %>%
            dplyr::summarize(
                MAE   = mae_vec(actual, prediction),
                MAPE  = mape_vec(actual, prediction),
                MASE  = mase_vec(actual, prediction),
                SMAPE = smape_vec(actual, prediction),
                RMSE  = rmse_vec(actual, prediction),
                RSQ   = rsq_vec(actual, prediction)
            ) %>%
            dplyr::ungroup()

        data_formatted <- dplyr::bind_rows(data_formatted, test_tbl)

    }


    return(data_formatted)


}



