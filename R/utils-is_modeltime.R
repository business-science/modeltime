#' Test if object contains a fitted modeltime model
#'
#' This function returns `TRUE` for trained workflows and parsnip objects
#' that contain modeltime models
#'
#' @param object An object to detect if contains a fitted modeltime model
#'
#' @keywords internal
#' @export
is_modeltime_model <- function(object) {
    UseMethod("is_modeltime_model", object)
}

#' @export
is_modeltime_model.default <- function(object) {
    FALSE
}

#'@export
is_modeltime_model.workflow <- function(object) {
    inherits(object$fit$fit$fit, "modeltime_bridge")
}

#' @export
is_modeltime_model.model_fit <- function(object) {
    inherits(object$fit, "modeltime_bridge")
}

#' Test if object is a Modeltime Table
#'
#' This function returns `TRUE` for objects that contain class `mdl_time_tbl`
#'
#' @param object An object to detect if is a Modeltime Table
#'
#' @keywords internal
#' @export
is_modeltime_table <- function(object) {
    inherits(object, "mdl_time_tbl")
}

#' Test if a Modeltime Table has been calibrated
#'
#' This function returns `TRUE` for objects that contains columns
#' ".type" and ".calibration_data"
#'
#' @param object An object to detect if is a Calibrated Modeltime Table
#'
#' @keywords internal
#' @export
is_calibrated <- function(object) {
    all(c(".type", ".calibration_data") %in% names(object) )
}



#' Test if a table contains residuals.
#'
#' This function returns `TRUE` for objects that contains the column name '.residuals'.
#'
#' @param object An object to detect if it provides from modeltime::modeltime_residuals().
#'
#' @keywords internal
#' @export
is_residuals <- function(object) {
    all(c(".residuals") %in% names(object) )
}
