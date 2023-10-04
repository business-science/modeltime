# MODELTIME REFIT ----

#' Refit one or more trained models to new data
#'
#' This is a wrapper for `fit()` that takes a
#' Modeltime Table and retrains each model on _new data_ re-using the parameters
#' and preprocessing steps used during the training process.
#'
#' @param object A Modeltime Table
#' @param data A `tibble` that contains data to retrain the model(s) using.
#' @param ... Additional arguments to control refitting.
#'
#'  __Ensemble Model Spec (`modeltime.ensemble`):__
#'
#'    When making a meta-learner with `modeltime.ensemble::ensemble_model_spec()`,
#'    used to pass `resamples` argument containing results
#'    from `modeltime.resample::modeltime_fit_resamples()`.
#'
#' @param control Used to control verbosity and parallel processing.
#'  See [control_refit()].
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
#' @seealso
#' [control_refit()]
#'
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
modeltime_refit <- function(object, data, ..., control = control_refit()) {
    UseMethod("modeltime_refit", object)
}

#' @export
modeltime_refit.mdl_time_tbl <- function(object, data, ..., control = control_refit()) {

    new_data <- data
    data     <- object # object is a Modeltime Table

    # Backwards compatibility
    if (is.null(control)) control <- control_refit()

    # Save current model descriptions
    model_desc_user_vec          <- object$.model_desc
    model_desc_modeltime_old_vec <- object$.model %>% purrr::map_chr(get_model_description)

    # Parallel or Sequential
    if ((control$cores > 1) && control$allow_par) {
        ret <- modeltime_refit_parallel(object, data = new_data, control = control, ...)
    } else {
        ret <- modeltime_refit_sequential(object, data = new_data, control = control, ...)
    }

    validate_models_are_not_null(ret, msg_main = "Some models failed during fitting: modeltime_refit()")

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

modeltime_refit_sequential <- function(object, data, ..., control) {

    t1 <- Sys.time()

    new_data <- data
    data     <- object # object is a Modeltime Table

    # Safely refit
    safe_modeltime_refit <- purrr::safely(mdl_time_refit, otherwise = NULL, quiet = TRUE)

    # BEGIN LOOP
    # if (control$verbose) {
    #     t <- Sys.time()
    #     message(stringr::str_glue(" Beginning Sequential Loop | {round(t-t1, 3)} seconds"))
    # }

    ret <- data %>%
        dplyr::ungroup() %>%
        dplyr::mutate(.model = purrr::map2(
            .x         = .model,
            .y         = .model_id,
            .f         = function(obj, id) {

                if (control$verbose) {
                    cli::cli_alert_info(cli::col_grey("Refitting Model: Model ID {id}"))
                }

                ret <- safe_modeltime_refit(
                    obj,
                    data    = new_data,
                    control = control,
                    ...
                )

                res <- ret %>% purrr::pluck("result")

                if (!is.null(ret$error)) message(stringr::str_glue("Model {id} Error: {ret$error}"))

                if (control$verbose) {
                    if (is.null(res)) {
                        cli::cli_alert_danger(cli::col_grey("Model Failed Refitting: Model ID {id}"))
                    } else {
                        cli::cli_alert_success(cli::col_grey("Model Successfully Refitted: Model ID {id}"))
                    }
                }

                return(res)
            })
        )

    # PRINT TOTAL TIME
    if (control$verbose) {
        t <- Sys.time()
        message(stringr::str_glue("Total time | {round(t-t1, 3)} seconds"))
    }

    return(ret)

}

modeltime_refit_parallel <- function(object, data, ..., control) {

    t1 <- Sys.time()

    new_data <- data
    data     <- object # object is a Modeltime Table

    # Parallel Detection
    is_par_setup <- foreach::getDoParWorkers() > 1

    # If parallel processing is not set up, set up parallel backend
    par_setup_info <- setup_parallel_processing(control, is_par_setup, t1)
    clusters_made  <- par_setup_info$clusters_made
    cl             <- par_setup_info$cl
    t              <- par_setup_info$t

    # Setup Foreach
    `%op%` <- get_operator(allow_par = control$allow_par)

    # Safely refit
    safe_modeltime_refit <- purrr::safely(mdl_time_refit, otherwise = NULL, quiet = FALSE)

    ret <- data %>% dplyr::ungroup()

    # BEGIN LOOP
    if (control$verbose) {
        t <- Sys.time()
        message(stringr::str_glue(" Beginning Parallel Loop | {round(t-t1, 3)} seconds"))
    }

    mod_list <- foreach::foreach(
            id                  = ret$.model_id,
            .inorder            = TRUE,
            .packages           = control$packages,
            .verbose            = FALSE
        ) %op% {

            model <- ret %>%
                dplyr::filter(.model_id == id) %>%
                dplyr::select(.model) %>%
                dplyr::pull()

            mod <- safe_modeltime_refit(model[[1]], new_data, control = control)

            res <- mod %>%
                purrr::pluck("result")

            err <- mod %>%
                purrr::pluck("error")

            return(list(res = res, err = err))

        }

    # Collect models
    models <- mod_list %>% purrr::map(~ .x$res)

    # Collect errors
    error_messages <- mod_list %>% purrr::map(~ .x$err)
    purrr::iwalk(
        error_messages,
        function (e, id) {
            if (!is.null(e)) message(stringr::str_glue("Model {id} Error: {e}"))
        }
    )

    # Recombine models with modeltime table
    ret <- ret %>%
        dplyr::mutate(.model = models)


    # Finish Parallel Backend. Close clusters if we set up internally.
    finish_parallel_processing(control, clusters_made, cl, t1)

    # PRINT TOTAL TIME
    if (control$verbose) {
        t <- Sys.time()
        message(stringr::str_glue(" Total time | {round(t-t1, 3)} seconds"))
    }

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
    cli::cli_abort("No method for an object of class: {.obj_type_friendly {object}}. .")
}

#' @export
mdl_time_refit.workflow <- function(object, data, ..., control = NULL) {

    ret <- object %>% fit(data)

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
        chunk_size_old <- object$fit$fit$spec$chunk_size

        train_tail_new <- data %>%
            dplyr::slice_tail(n = nrow(train_tail_old))

        # Refit
        object <- mdl_time_refit.workflow(object, data, ..., control = control)

        # Make Recursive
        object <- recursive(object,
                            transform = transformer,
                            train_tail = train_tail_new,
                            chunk_size = chunk_size_old)

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

        n <- object$spec$train_tail %>%
            dplyr::count(!! rlang::sym(object$spec$id)) %>%
            dplyr::pull(n) %>%
            stats::median(na.rm = TRUE)

        object$spec$train_tail <- data %>%
            panel_tail(
                id = !! object$spec$id,
                n  = n
            )

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

        # print("Spec ID")
        # print(object$fit$fit$spec$id)

        n <- object$fit$fit$spec$train_tail %>%
            dplyr::count(!! rlang::sym(object$fit$fit$spec$id)) %>%
            dplyr::pull(n) %>%
            stats::median(na.rm = TRUE)

        train_tail_new <- data %>%
            panel_tail(
                id = !! object$fit$fit$spec$id,
                n  = n
            )

        id_old <- object$fit$fit$spec$id
        chunk_size_old <- object$fit$fit$spec$chunk_size

        # Refit
        object <- mdl_time_refit.workflow(object, data, ..., control = control)

        # Make Recursive
        object <- recursive(
            object,
            transform  = transformer,
            train_tail = train_tail_new,
            id         = id_old,
            chunk_size = chunk_size_old
        )

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
