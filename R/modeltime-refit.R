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
modeltime_refit <- function(object, data, ..., control = control_refit()) {
    UseMethod("modeltime_refit", object)
}

#' @export
modeltime_refit.mdl_time_tbl <- function(object, data, ..., control = control_refit()) {

    new_data <- data
    data     <- object # object is a Modeltime Table

    # Save current model descriptions
    model_desc_user_vec          <- object$.model_desc
    model_desc_modeltime_old_vec <- object$.model %>% purrr::map_chr(get_model_description)

    # Safely refit
    safe_modeltime_refit <- purrr::safely(mdl_time_refit, otherwise = NULL, quiet = FALSE)

    # Implement progressr for progress reporting
    p <- progressr::progressor(steps = nrow(data))

    if (control$cores > 1){
        message("Starting parallel backend...")
        cl <- parallel::makeCluster(control$cores)
        doSNOW::registerDoSNOW(cl)
    } else {
        foreach::registerDoSEQ()
    }

    get_operator <- function(allow_par = TRUE) {
        is_par <- foreach::getDoParWorkers() > 1

        cond <- allow_par && is_par
        if (cond) {
            res <- foreach::`%dopar%`
        } else {
            res <- foreach::`%do%`
        }
        return(res)
    }

    `%op%` <- get_operator(allow_par = control$allow_par)

    ret <- data %>%
        dplyr::ungroup()

    progress <- function(n) {
        message(stringr::str_glue("Refitting model id {n} - {ret %>%
                                       dplyr::filter(.model_id == n) %>%
                                       dplyr::select(.model_desc) %>%
                                       dplyr::pull()}"))
    }

    opts <- list(progress=progress)

    models <- foreach::foreach(id = seq_len(nrow(ret)),
                               .inorder = FALSE,
                               .options.snow = if (control$verbose) opts else NULL, #Parallel Printing
                               #.export = c("safe_modeltime_refit"),
                               .packages = control$packages) %op% {


                                   model <- ret %>%
                                       dplyr::filter(.model_id == id) %>%
                                       dplyr::select(.model) %>%
                                       dplyr::pull()

                                   #Sequential Printing

                                   if (control$verbose){
                                       message(stringr::str_glue("Refitting model id {id} - {ret %>%
                                       dplyr::filter(.model_id == id) %>%
                                       dplyr::select(.model_desc) %>%
                                       dplyr::pull()}"))
                                   }

                                   res <- safe_modeltime_refit(model[[1]], new_data, control) %>% purrr::pluck("result")

                                   return(res)


                               }


    if (control$cores > 1) {doParallel::stopImplicitCluster()
                            parallel::stopCluster(cl)
                            message("Finishing parallel backend")}

    message("Done. Have a good day.")

    ret <- ret %>%
           dplyr::mutate(.model = models)

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

        # Refit
        object <- mdl_time_refit.workflow(object, data, ..., control = control)

        # Make Recursive
        object <- recursive(
            object,
            transform  = transformer,
            train_tail = train_tail_new,
            id         = id_old
        )

        # Need to overwrite transformer
        object$fit$fit$spec$transform <- transformer

    }

    return(object)


}



# Control Refit
#
#' Control aspects of the modeltime_refit process.
#'
#' @param allow_par Logical to allow parallel computation
#' @param cores Number of cores for computation
#' @param packages Packages that the user wants to send to the workers during parallelization.
#' @param verbose Logical to control printing.
#'
#' @return
#' A List with the information.

#' @export
control_refit <- function(allow_par = TRUE,
                          cores = 1,
                          packages = c("modeltime", "parsnip", "dplyr", "stats",
                                       "lubridate", "tidymodels", "timetk"),
                          verbose = FALSE) {
    # add options for  seeds per resample

    val_class_and_single(verbose, "logical", "control_refit()")
    val_class_and_single(allow_par, "logical", "control_refit()")
    val_class_and_single(cores, "numeric", "control_refit()")

    class_cores <- check_class_integer(cores)

    load_namespace(packages, full_load = packages)

    if (class_cores == F) {rlang::abort("Argument 'cores' should be a single integer value in `control_refit()`")}

    res <- list(allow_par = allow_par,
                cores = cores,
                verbose = verbose)

    class(res) <- c("control_refit")
    res
}

#' @export
print.control_refit <- function(x, ...) {
    cat("refit control object\n")
    invisible(x)
}


# val_class_and_single
#
#' These are not intended for use by the general public.
#'
#' @param x An object
#' @param cls A character vector of possible classes
#' @param where A character string for the calling function
#'
#' @return
#' Control information

#' @export
val_class_and_single <- function (x, cls = "numeric", where = NULL) {
    cl <- match.call()
    fine <- check_class_and_single(x, cls)
    cls <- paste(cls, collapse = " or ")
    if (!fine) {
        msg <- glue::glue("Argument '{deparse(cl$x)}' should be a single {cls} value")
        if (!is.null(where)) {
            msg <- glue::glue(msg, " in `{where}`")
        }
        rlang::abort(msg)
    }
    invisible(NULL)
}

# check_class_and_single
#
#' These are not intended for use by the general public.
#'
#' @param x An object
#' @param cls A character vector of possible classes
#'
#' @return
#' Control information

#' @export
check_class_and_single <- function (x, cls = "numeric") {
    isTRUE(inherits(x, cls) & length(x) == 1)
}

# check_class_integer
#
#' These are not intended for use by the general public.
#'
#' @param x A number
#'
#' @return
#' A logical value

#' @export
check_class_integer <- function(x){
    if (x %% 1 == 0) TRUE else FALSE
}

# load_namespace
#
#' These are not intended for use by the general public.
#'
#' @param x A vector
#' @param full_load A vector
#'
#' @return
#' Control information

#' @export
load_namespace <- function(x, full_load) {
    if (length(x) == 0) {
        return(invisible(TRUE))
    }

    x_full <- x[x %in% full_load]
    x <- x[!(x %in% full_load)]

    loaded <- purrr::map_lgl(x, isNamespaceLoaded)
    x <- x[!loaded]

    if (length(x) > 0) {
        did_load <- purrr::map_lgl(x, requireNamespace, quietly = TRUE)
        if (any(!did_load)) {
            bad <- x[!did_load]
            msg <- paste0("'", bad, "'", collapse = ", ")
            stop(paste("These packages could not be loaded:", msg), call. = FALSE)
        }
    }

    if (length(x_full) > 0) {
        purrr::map(x_full,
                   ~ try(suppressPackageStartupMessages(attachNamespace(.x)), silent = TRUE))
    }

    invisible(TRUE)
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
