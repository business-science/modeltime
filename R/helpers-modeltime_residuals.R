# HELPERS FOR MODELTIME RESIDUALS ----

#' Extracts modeltime residuals data from a Modeltime Model
#'
#' If a modeltime model contains `data` with residuals information,
#' this function will extract the data frame.
#'
#' @param object A fitted `parsnip` / `modeltime` model or `workflow`
#'
#' @return A `tibble` containing the model timestamp, actual, fitted, and residuals data
#'
#'
#' @export
pull_modeltime_residuals <- function(object) {
    UseMethod("pull_modeltime_residuals")
}

#' @export
pull_modeltime_residuals.model_fit <- function(object) {

    if (is_modeltime_model(object)) {
        ret <- object$fit$data %>% tibble::as_tibble()
    } else {
        ret <- NA
    }

    return(ret)

}

#' @export
pull_modeltime_residuals.workflow <- function(object) {

    if (is_modeltime_model(object)) {
        ret <- object$fit$fit$fit$data %>% tibble::as_tibble()
    } else {
        ret <- NA
    }

    return(ret)

}

