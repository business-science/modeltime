#' Refit one or more trained models to new data
#'
#' This is a wrapper for `fit()` and `fit_xy()` that takes a
#' trained parsnip model or workflow and retrains on _new data_ using the parameters
#' and preprocessing (formulas or recipes) used during the training process:
#' - Automated models (e.g. "auto arima") will have parameters recalculated.
#' - Non-automated models (e.g. "arima") will have parameters preserved.
#' - All preprocessing steps will be reused on the data
#'
#' @param object A fitted model object that is either (1) a workflow that has been fit by [fit.workflow()] or
#'  (2) a parsnip model that has been fit using [fit.model_spec()]
#' @param data A `tibble`
#' @param control Either [control_parsnip()] or [control_workflow()] depending on the object.
#'  If NULL, created automatically.
#' @param ... Additional arguments passed to [fit()].
#' @param x A matrix or data frame of predictors.
#' @param y A vector, matrix or data frame of outcome data.
#'
#'
#' @return
#' Depending on the object provided, either:
#' - Single Parsnip Model: A fitted parsnip model (`model_fit`) is returned.
#' - Single Workflow: A trained `workflow` is returned.
#' - A Modeltime Table containing one or more trained models: A trained `mdl_time_tbl` table
#'  with one or more models that have been retrained is returned.
#'
#' @details
#' TODO
#'
#'
#'
#' @examples
#' # TODO
#'
#' @name modeltime_refit
#' @importFrom parsnip fit fit_xy
NULL

#' @export
#' @rdname modeltime_refit
modeltime_refit <- function(object, data, control = NULL, ...) {
    UseMethod("modeltime_refit", object)
}

#' @export
#' @rdname modeltime_refit
modeltime_refit_xy <- function(object, x, y, control = NULL, ...) {
    UseMethod("modeltime_refit_xy", object)
}

# REFIT ----

#' @export
modeltime_refit.workflow <- function(object, data, ..., control = NULL) {

    if (is.null(control)) {
        control <- workflows::control_workflow(control_parsnip = NULL)
    }

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
modeltime_refit.model_fit <- function(object, data, control = NULL, ...) {

    if (is.null(control)) {
        control <- parsnip::control_parsnip()
    }

    model_spec <- object$spec

    # Deterimine Interface
    fit_interface <- object$spec$method$fit$interface

    if (fit_interface == "formula") {

        # Formula interface includes preprocessing internally to the object's fit method
        # - Need to extract formula using find_formula()

        form <- object$fit %>% find_formula()
        if (is.null(form)) {
            rlang::abort("Could not 'refit()' model. The model formula could not be located. Consider using a 'workflow()' instead to ensure model refitting is possible.")
        }

        ret <- model_spec %>%
            parsnip::fit(form, data = data, control = control, ...)

    } else {

        # fit_interface is either "data.frame" or "matrix"

        # check for formula in object$peproc$terms
        if ("terms" %in% names(object$preproc)) {
            if (inherits(object$preproc$terms, "formula")) {

                form <- object$preproc$terms

                ret <- model_spec %>%
                    fit(form, data = data)
            }
        } else {
            rlang::abort("Could not 'refit()' model. The model formula could not be located. Consider using a 'workflow()' instead to ensure model refitting is possible.")

        }

    }

    return(ret)

}

#' @export
modeltime_refit.default <- function(object, data, control = NULL, ...) {
    rlang::abort(paste0("No method for class '", class(object)[1], "'."))
}

# REFIT XY ----

#' @export
modeltime_refit_xy.workflow <- function(object, x, y, control = NULL, ...) {
    rlang::abort("Using 'modeltime_refit_xy()' on a workflow object is not allowed. Try using 'modeltime_refit()'.")
}

#' @export
modeltime_refit_xy.model_fit <- function(object, x, y, control = NULL, ...) {

    if (is.null(control)) {
        control <- parsnip::control_parsnip()
    }

    model_spec <- object$spec

    ret <- model_spec %>%
        fit_xy(x = x, y = y, control = control, ...)

    return(ret)

}

#' @export
modeltime_refit_xy.default <- function(object, x, y, control = NULL, ...) {
    rlang::abort(paste0("No method for class '", class(object)[1], "'."))
}
