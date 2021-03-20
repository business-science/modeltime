# MODELTIME REFIT ----

#' Refit one or more trained models to new data
#'
#' This is a wrapper for `fit()` that takes a
#' Modeltime Table and retrains each model on _new data_ re-using the parameters
#' and preprocessing steps used during the training process.
#'
#' @param object A Modeltime Table
#' @param data A `tibble` that contains data to retrain the model(s) using.
#' @param control Under construction. Will be used to control refitting.
#' @param ... Under construction. Additional arguments to control refitting.
#'
#'
#' @return
#' A Modeltime Table containing one or more re-trained models.
#'
#' @details
#'
#' Refitting is an important step prior to forecasting time series models.
#' The `modeltime_refit()` function makes it easy to recycle models,
#' retraining on new data.
#'
#' __Recycling Parameters__
#'
#' Parameters are recycled during retraining using the following criteria:
#'
#' - __Automated models__ (e.g. "auto arima") will have parameters recalculated.
#' - __Non-automated models__ (e.g. "arima") will have parameters preserved.
#' - All preprocessing steps will be reused on the data
#'
#' __Refit__
#'
#' The `modeltime_refit()` function is used to retrain models trained with `fit()`.
#'
#' __Refit XY__
#'
#' The XY format is not supported at this time.
#'
#'
#'
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
#' model_fit_auto_arima <- arima_reg() %>%
#'     set_engine(engine = "auto_arima") %>%
#'     fit(value ~ date, data = training(splits))
#'
#'
#' # ---- MODELTIME TABLE ----
#'
#' models_tbl <- modeltime_table(
#'     model_fit_auto_arima
#' )
#'
#' # ---- CALIBRATE ----
#' # - Calibrate on training data set
#'
#' calibration_tbl <- models_tbl %>%
#'     modeltime_calibrate(new_data = testing(splits))
#'
#'
#' # ---- REFIT ----
#' # - Refit on full data set
#'
#' refit_tbl <- calibration_tbl %>%
#'     modeltime_refit(m750)
#'
#'
#' @name modeltime_refit
#' @importFrom parsnip fit fit_xy
NULL

#' @export
#' @rdname modeltime_refit
modeltime_refit <- function(object, data, ..., control = NULL) {
    UseMethod("modeltime_refit", object)
}

#' @export
modeltime_refit.mdl_time_tbl <- function(object, data, ..., control = NULL) {

    new_data <- data
    data     <- object # object is a Modeltime Table

    # Save current model descriptions
    model_desc_user_vec          <- object$.model_desc
    model_desc_modeltime_old_vec <- object$.model %>% purrr::map_chr(get_model_description)

    # Safely refit
    safe_modeltime_refit <- purrr::safely(mdl_time_refit, otherwise = NULL, quiet = FALSE)

    # Implement progressr for progress reporting
    p <- progressr::progressor(steps = nrow(data))

    ret <- data %>%
        dplyr::ungroup() %>%
        dplyr::mutate(.model = purrr::map2(
            .x         = .model,
            .y         = .model_id,
            .f         = function(obj, id) {

                p(stringr::str_glue("Model ID = {id} / {max(data$.model_id)}"))

                ret <- safe_modeltime_refit(
                    obj,
                    data    = new_data,
                    control = control,
                    ...
                )

                ret <- ret %>% purrr::pluck("result")

                return(ret)
            })
        )

    validate_models_are_not_null(ret)

    # Safely refit
    # safe_get_model_description <- purrr::safely(get_model_description, otherwise = "TODO", quiet = FALSE)


    # Get new Model Descriptions
    ret <- ret %>%
        dplyr::mutate(.model_desc_user = model_desc_user_vec) %>%
        dplyr::mutate(.model_desc_old  = model_desc_modeltime_old_vec) %>%
        dplyr::mutate(.model_desc_new  = purrr::map_chr(.model, .f = get_model_description)) %>%

        # Description Logic
        dplyr::mutate(.model_desc = ifelse(
            .model_desc_old == .model_desc_new,
            # TRUE - Let User Choice Alone
            .model_desc_user,
            # FALSE - Model Algorithm Parameters Have Changed
            # - Reflect Updated Model Params in Description
            paste0("UPDATE: ", .model_desc_new)
            )
        ) %>%

        # Clean up columns
        dplyr::select(-.model_desc_user, -.model_desc_old, -.model_desc_new)


    return(ret)

}

# #' @export
# modeltime_refit_xy.mdl_time_tbl <- function(object, x, y, ..., control = NULL) {
#     rlang::abort("Only models and workflows trained using `fit()` are supported at this time.")
# }


# REFIT ----

#' Modeltime Refit Helpers
#'
#' Used for low-level refitting of modeltime, parnsip and workflow models
#' These functions are not intended for user use.
#'
#' @inheritParams modeltime_refit
#'
#' @return A tibble with forecast features
#'
#' @keywords internal
#'
#' @export
mdl_time_refit <- function(object, data, ..., control = NULL) {
    UseMethod("mdl_time_refit", object)
}

#' @export
mdl_time_refit.default <- function(object, data, ..., control = NULL) {
    glubort("No method for an object of class: {class(object)[1]}. .")
}

#' @export
mdl_time_refit.workflow <- function(object, data, ..., control = NULL) {

    model_spec    <- object %>% workflows::pull_workflow_spec()
    model_preproc <- object %>% workflows::pull_workflow_preprocessor()

    if (inherits(model_preproc, "formula")) {
        # Formula preprocessor
        ret <- workflows::workflow() %>%
            workflows::add_model(model_spec) %>%
            workflows::add_formula(model_preproc) %>%
            fit(data)
    } else {
        # Recipe preprocessor
        ret <- workflows::workflow() %>%
            workflows::add_model(model_spec) %>%
            workflows::add_recipe(model_preproc) %>%
            fit(data)
    }

    return(ret)

}

#' @export
mdl_time_refit.model_fit <- function(object, data, ...,  control = NULL) {

    model_spec <- object$spec

    form <- object %>% pull_parsnip_preprocessor()

    ret <- model_spec %>%
        parsnip::fit(form, data = data)

    return(ret)

}

#' @export
mdl_time_refit.recursive <- function(object, data, ..., control = NULL) {

    if (inherits(object, "model_fit")) {

        # Swap out train_tail
        train_tail_old <- object$spec$train_tail

        object$spec$train_tail <- data %>%
            dplyr::slice_tail(n = nrow(train_tail_old))

        # Refit
        object <- mdl_time_refit.model_fit(object, data, ..., control = control)

        # Reconstruct class
        .class        <- class(object)
        class(object) <- c(.class[1], "recursive", .class[2])


    } else {

        # Get transformer
        transformer <- object$fit$fit$spec$transform

        # Create new train tail
        train_tail_old <- object$fit$fit$spec$train_tail

        train_tail_new <- data %>%
            dplyr::slice_tail(n = nrow(train_tail_old))

        # Refit
        object <- mdl_time_refit.workflow(object, data, ..., control = control)

        # Make Recursive
        object <- recursive(object, transform = transformer, train_tail = train_tail_new)

        # Need to overwrite transformer
        object$fit$fit$spec$transform <- transformer

    }

    return(object)


}


#' @export
mdl_time_refit.recursive_panel <- function(object, data, ..., control = NULL) {

    if (inherits(object, "model_fit")) {

        # Swap out train_tail
        train_tail_old <- object$spec$train_tail

        object$spec$train_tail <- data %>%
            dplyr::slice_tail(n = nrow(train_tail_old))

        # Refit
        object <- mdl_time_refit.model_fit(object, data, ..., control = control)

        # Reconstruct class
        .class        <- class(object)
        class(object) <- c(.class[1], "recursive_panel", .class[2])


    } else {

        # Get transformer
        transformer <- object$fit$fit$spec$transform

        # Create new train tail
        train_tail_old <- object$fit$fit$spec$train_tail


        ### NEED TO KNOW THE ID HERE ----
        train_tail_new <- data %>%
            dplyr::slice_tail(n = nrow(train_tail_old)) # CHANGE TO panel_tail()

        # Refit
        object <- mdl_time_refit.workflow(object, data, ..., control = control)

        # Make Recursive
        object <- recursive(object, transform = transformer, train_tail = train_tail_new)

        # Need to overwrite transformer
        object$fit$fit$spec$transform <- transformer

    }

    return(object)


}



# # REFIT XY ----
#
# #' @export
# mdl_time_refit_xy.workflow <- function(object, x, y, control = NULL, ...) {
#     rlang::abort("Using 'mdl_time_refit_xy()' on a workflow object is not allowed. Try using 'modeltime_refit()'.")
# }
#
# #' @export
# mdl_time_refit_xy.model_fit <- function(object, x, y, control = NULL, ...) {
#
#     if (is.null(control)) {
#         control <- parsnip::control_parsnip()
#     }
#
#     model_spec <- object$spec
#
#     ret <- model_spec %>%
#         fit_xy(x = x, y = y, control = control, ...)
#
#     return(ret)
#
# }
#
# #' @export
# mdl_time_refit_xy.default <- function(object, x, y, control = NULL, ...) {
#     rlang::abort(paste0("No method for class '", class(object)[1], "'."))
# }
