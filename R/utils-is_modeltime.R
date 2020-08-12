#' Test if object contains a fitted modeltime model
#'
#' This function returns `TRUE` for trained workflows and parsnip objects
#' that contain modeltime models
#'
#' @param object An object to detect if contains a fitted modeltime model
#'
#' @export
is_modeltime <- function(object) {
    UseMethod("is_modeltime", object)
}

#' @export
is_modeltime.default <- function(object) {
    FALSE
}

#'@export
is_modeltime.workflow <- function(object) {
    inherits(object$fit$fit$fit, "modeltime_bridge")
}

#' @export
is_modeltime.model_fit <- function(object) {
    inherits(object$fit, "modeltime_bridge")
}


