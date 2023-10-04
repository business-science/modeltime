# PARSNIP HELPERS ----

#' Pulls the Formula from a Fitted Parsnip Model Object
#'
#' @param object A fitted parsnip model `model_fit` object
#'
#' @return A formula using `stats::formula()`
#'
#' @export
pull_parsnip_preprocessor <- function(object) {
    UseMethod("pull_parsnip_preprocessor", object)
}

#' @export
pull_parsnip_preprocessor.model_fit <- function(object) {

    form <- NULL

    fit_interface <- object$spec$method$fit$interface

    if (fit_interface == "formula") {

        # Formula interface includes preprocessing internally to the object's fit method
        # - Need to extract formula using find_formula()

        form <- object %>% find_parsnip_formula_form()

    } else {

        # fit_interface is either "data.frame" or "matrix"

        form <- object %>% find_parsnip_formula_matrix()

    }

    if (is.null(form)) {
        rlang::abort("The model formula could not be located. Consider using a 'workflow()' instead to ensure model refitting is possible.")
    }

    return(form)

}

find_parsnip_formula_form <- function(object) {

    if (is_modeltime_model(object)){
        object <- object$fit$models$model_1
    } else {
        object <- object$fit
    }

    form <- NULL

    if (isS4(object)) {
        # S4 Class

        slots <- methods::slotNames(object)

        for (slot_name in slots) {
            sl <- methods::slot(object, slot_name)
            if (rlang::is_formula(sl)) {
                form <- stats::formula(sl)
            }
        }
    } else {
        # S3 Class

        check_formula_tbl <- object %>%
            purrr::map_dfr(~ rlang::is_formula(.)) %>%
            tidyr::gather() %>%
            dplyr::filter(value)

        formula_found <- FALSE

        if (nrow(check_formula_tbl) >= 1) {
            formula_found <- TRUE
        }

        if (!formula_found){
            check_formula_second_level <- object %>%
                purrr::map_dfr(~ rlang::is_formula(.)) %>%
                tidyr::gather() %>%
                dplyr::filter(key == "formula")

            if (length(check_formula_second_level) > 1){
                check_formula_tbl <- object$formula %>%
                    purrr::map_dfr(~ rlang::is_formula(.)) %>%
                    tidyr::gather() %>%
                    dplyr::filter(value)
            }
        }

        if (nrow(check_formula_tbl) >= 1) {
            formula_found <- TRUE
        }

        if (formula_found) {
            form <- stats::formula(object[[ check_formula_tbl$key[1] ]])
        }
    }

    return(form)
}

find_parsnip_formula_matrix <- function(object) {

    form <- NULL

    # check for formula in object$peproc$terms
    if ("terms" %in% names(object$preproc)) {
        if (inherits(object$preproc$terms, "formula")) {
            form <- stats::formula(object$preproc$terms)
        }
    }

    return(form)
}


# ARCHIVED ----
# - These functions are *currently used* and will be replaced in the future

# For Formula Objects
find_formula <- function(object) {

    form <- NULL

    if (isS4(object)) {
        # S4 Class

        slots <- methods::slotNames(object)

        for (slot_name in slots) {
            sl <- methods::slot(object, slot_name)
            if (rlang::is_formula(sl)) {
                form <- stats::formula(sl)
            }
        }
    } else {
        # S3 Class

        check_formula_tbl <- object %>%
            purrr::map_dfr(~ rlang::is_formula(.)) %>%
            tidyr::gather() %>%
            dplyr::filter(value)

        formula_found <- FALSE
        if (nrow(check_formula_tbl) == 1) {
            formula_found <- TRUE
        }

        if (formula_found) {
            form <- stats::formula(object[[check_formula_tbl$key]])
        }
    }

    return(form)
}

find_formula_lhs <- function(object) {

    form <- NULL
    lhs  <- NULL

    form <- find_formula(object)
    if (!is.null(form)) {
        lhs <- rlang::f_lhs(form)
    }

    return(lhs)
}


